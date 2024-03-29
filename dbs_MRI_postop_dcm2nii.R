# In this script I first summarize raw (DICOM) MRI data from a chosen parent directory and then convert them
# to NIfTI format (via Roden`s dcm2niix, https://github.com/rordenlab/dcm2niix) in the BIDS folder structure

# set working directory (works in RStudio only)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# list packages to be used
pkgs <- c("dplyr", # for object manipulations
          "divest", # R interface to dcm2niix
          "tidyverse", # for more object manipulations
          "RNifti", # R-native NIfTI tools
          "english" # rewriting digits to words for nicer file names
          )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}


# ----------- prepare parameters for data extraction and control -----------

n <- 79 # number of patients
n_scans <- list( t1 = 176 , rs = 203 ) # expected number of raw scans in anatomical (T1) and functional (RS) images
fold <- "data/raw" # folder that holds the raw data


# ----------- dicom-to-nifti conversion -----------

# get directory name for each included patient
pats <- dir( path = paste(getwd(), fold, sep = "/") )

# extract folder names for each session for each patient
ses <- list()
for ( i in pats ) ses[[i]] <- dir( path = paste(fold, i, sep = "/") )

# intrinsically read and convert all raw DICOM files to NIfTI
nii <- list()
for ( i in pats ) {
  for ( j in ses[[i]] ) {
    
    # read via divest::readDicom
    # interactive = F so that I don't need to manually confirm each convertion
    # verbose = -2 so that no messages are printed (not even warnings, can be changed)
    nii[[i]][[j]][["anat"]][["t1w"]] <- readDicom( path = paste(fold, i, j, "anat/T1_MPRAGE", sep = "/"), interactive = F, verbosity = -2 )
    nii[[i]][[j]][["func"]][["rs_on"]] <- readDicom( path = paste(fold, i, j, "funct/RS_ON", sep = "/"), interactive = F, verbosity = -2 )
    nii[[i]][[j]][["func"]][["rs_off"]] <- readDicom( path = paste(fold, i, j, "funct/RS_OFF", sep = "/"), interactive = F, verbosity = -2 )
    
    # closing all connections to prevent the following error
    # Error in textConnection("output", "w", local = TRUE) : all connections are in use
    closeAllConnections()
  }
}


# ----------- read headers -----------

# because the scans come from different scanners, the headers contain different amount of information
# I will manually define which information are of interest for summaries
vars <- c( paste0( "patient", c("Identifier","Name","BirthDate","Age","Sex","Weight") ), # patient info
           paste0("imagedim_",1:4), paste0("pixdim_",1:4), # image dimensions
           "modality", "manufacturer", "scannerModelName", # scanner information
           "imageType", "seriesNumber", "seriesDescription", "sequenceName", "protocolName", # scanning session information
           paste0( "study",c("Date","Time") ), # when was the scan acquired
           "fieldStrength", "flipAngle", "echoTime", "repetitionTime", "inversionTime", # basic information about the sequence
           paste0( "slice", c("Thickness","Spacing") ), # slice information (timing not included because it's a vector)
           paste0( "phaseEncoding", c("Steps","Lines","Direction","Sign") ), # phase encoding
           "pixelBandwidth", "dwellTime", paste0( "effective", c("EchoSpacing","ReadoutTime") ) # remaining parameters
           )

# prepare a data.frame to hold all these information for all patient/sessions in the data
attr <- list(
  # data frame for the anatomical data headers
  anat = list( t1w = matrix(
    data = NA, nrow = length( unlist(ses) ), ncol = length(vars), dimnames = list( names( unlist(ses) ), vars)
  ) %>% as.data.frame %>%
    rownames_to_column( "id") %>%
    mutate( id = substr( id, 1, 6 ) ) %>% # since some of the patients have more than one session, need to trim the 'id' variable down
    add_column( session = unlist(ses), .after = "id" )
  ),
  # list to hold resting state data headers
  func = list()
)

# copy the structure of anat data frame to resting state as well
for ( i in c("rs_on","rs_off") ) attr$func[[i]] <- attr$anat$t1w

# loop through patient/sessions, MRI type and variables to fill-in all attributes
for ( i in names(attr) ) {
  for ( j in names(attr[[i]]) ) {
    for ( k in 1:nrow(attr[[i]][[j]]) ) {
      
      # prepare dummy variables for each row, i.e., patient id and session
      pid = attr[[i]][[j]]$id[k]
      sid = attr[[i]][[j]]$session[k]
      
      # if there ain't no images, continue
      if ( ( nii[[pid]][[sid]][[i]][[j]] %>% length ) == 0 ) next
      
      # otherwise continue
      else {
      
        # prepare a dummy variable containing image and pixel dimensions
        with( attributes(nii[[pid]][[sid]][[i]][[j]][[1]]),
              dims <<- c(imagedim,pixdim) %>%
                `names<-` ( c( paste0( "imagedim_", 1:case_when( i == "anat" ~ 3 , i == "func" ~ 4 ) ),
                               paste0( "pixdim_", 1:case_when( i == "anat" ~ 3 , i == "func" ~ 4 ) ) ) )
        )
        
        # loop through all variables (columns)
        for ( l in vars ) {
          
          # fill-in image dimensions
          if ( grepl( "dim_", l) ) attr[[i]][[j]][ k , l ] <- dims[l]
          
          # then fill-in the rest of headers
          else if ( l %in% names( attributes(nii[[pid]][[sid]][[i]][[j]][[1]]) ) ) {
            attr[[i]][[j]][ k , l ] <- attributes(nii[[pid]][[sid]][[i]][[j]][[1]])[[l]] %>% as.character
            
            # if the variable (column) ain't included in headers, proceed to the next one
          } else next
        }
      }
    }
    
    # remove the dummy variables
    rm(pid,sid,dims)
    
  }
}

# pull the tables together
for ( i in names(attr) ) {
  for ( j in names(attr[[i]]) ) {
    # add a column with the type of MRI scan
    attr[[i]][[j]] <- attr[[i]][[j]] %>% add_column( type = j, .after = "session" )
  }
  # pull all MRIs within anatomical and functional folders
  attr[[i]] <- do.call( rbind.data.frame , attr[[i]] )
}

# collapse anatomical and functional MRI headers to a single table, tidy it up and save as csv
attr <- do.call( rbind.data.frame, attr ) %>%
  arrange( id ) %>%
  `rownames<-` ( 1:nrow(.) ) %>%
  write.table( file = "data/header_info.csv", sep = ",", row.names = F )


# ----------- save .niix in a BIDS format -----------

# check whether there is a subfolder for bids in the data, it not create one
if ( !dir.exists("data/bids") ) dir.create("data/bids")

# create each subfolder and fill-in with the .nii.gz files
for ( i in pats ) {
  # create patient's folder if ain't already there
  if ( !dir.exists( paste0( "data/bids/sub-prague-",i ) ) ) dir.create( paste0( "data/bids/sub-prague-",i ) )
  
  for ( j in 1:length(nii[[i]]) ) {
    # create a folder for the session
    if ( !dir.exists( paste0( "data/bids/sub-prague-",i,"/ses-postop-",english(j) ) ) ) dir.create( paste0("data/bids/sub-prague-",i,"/ses-postop-",english(j) ) )
    
    for ( k in names(nii[[i]][[j]]) ) {
      # create a folder for the image type
      if ( !dir.exists( paste0( "data/bids/sub-prague-",i,"/ses-postop-",english(j),"/",k ) ) ) dir.create( paste0("data/bids/sub-prague-",i,"/ses-postop-",english(j),"/",k ) )
      
      for ( l in names(nii[[i]][[j]][[k]]) )
        
        # create a dummy folder for each image
        if ( !dir.exists( paste0( "data/bids/sub-prague-",i,"/ses-postop-",english(j),"/",k,"/",l ) ) ) dir.create( paste0("data/bids/sub-prague-",i,"/ses-postop-",english(j),"/",k,"/",l ) )
      
        # convert from DICOM to nii via dcm2niix in the shell
        system( paste0( "dcm2niix -f %f -o ",
                        # set-up output directory
                        getwd(), "/data/bids/sub-prague-", i, "/ses-postop-", english(j), "/", k, "/", l, " ",
                        # write the path to the original image
                        paste( getwd(), fold, i, ses[[i]][[j]], ifelse( k == "func", "funct", k),
                               case_when( l == "t1w" ~ "T1_MPRAGE",
                                          l == "rs_on" ~ "RS_ON",
                                          l == "rs_off" ~ "RS_OFF" ),
                               sep = "/" ) ) )
      
    }
  }
}

# rename the .nii and .json files
for ( i in dir("data/bids",recursive = T) ) {
  
  file.copy( from = paste0( "data/bids/", i ),
             to = paste0( "data/bids/", paste( strsplit( i, "/" )[[1]][1:3], collapse = "/" ), "/",
                          paste( strsplit( i, "/" )[[1]][c(1,4)], collapse = "_" ),
                          ".", strsplit( i, "[.]" )[[1]][2] ) )
  
}

# delete the dummy folders
for ( i in paste0( "data/bids/", ( dir( "data/bids", recursive = T )[ grepl( "T1_MPRAGE|RS_", dir( "data/bids", recursive = T ) ) & grepl( ".nii", dir( "data/bids", recursive = T ) ) ] %>% sub( "/[^/]*$", "", . ) ) ) ) unlink(i, recursive = T)


# ----------- defacing via spm_deface -----------

# since fsl_deface from FSL didn't work properly in my machine (it didn't touch the face but skimmed ears and
# some parts of the brain instead), neither did mri_deface from FreeSurfer (it didn't converge in some patients,
# it sliced parts of frontal lobes in others), and pydeface (see https://pypi.org/project/pydeface/) sliced the
# face too often below the eye, trying spm_deface now

# extract all t1w files' names
t1w.files <- list.files( "data/bids", recursive = T ) %>% as.data.frame() %>% slice( which( grepl("t1w", . ) & grepl(".nii", . ) ) )

# write a matlab script for spm_deface
writeLines( paste0( "spm_deface( {\n",
                    paste( paste0("'", getwd(), "/data/bids/",t1w.files[,1],"'"), collapse = "\n"),
                    "\n} )"
                    ), con = "conduct_spmdeface.m"
            )

# go to MatLab and run the code there, works well this time


# ----------- build folder structure for data suitable for sharing -----------

# first create a parent directory for shareables
if ( !dir.exists("data/4share") ) dir.create("data/4share")

# next fill it in with patient folders
for ( i in pats ) {
  # create patient's folder if ain't already there
  if ( !dir.exists( paste0( "data/4share/sub-prague-",i ) ) ) dir.create( paste0( "data/4share/sub-prague-",i ) )
    
    for ( j in c("anat","func") ) {
      # create a folder for the image type
      if ( !dir.exists( paste0( "data/4share/sub-prague-",i,"/",j ) ) ) dir.create( paste0("data/4share/sub-prague-",i,"/",j ) )
      
  }
}

# next sort the data by hand, check whether defacing worked as intended, fill-in the 4share directory by data
# from the bids directory
all.nms <- list.files("data/4share", recursive = T ) %>%
  as.data.frame() %>%
  rename( "old_nms" = ".") %>%
  slice( starts_with( "sub-prague", vars = old_nms ) ) %>%
  mutate( new_nms = gsub("anon_","",old_nms ) %>% gsub("ses-postop-one_","",.) %>% gsub("ses-postop-two_","",.) )

# rename all the files
for ( i in 1:nrow(all.nms) ) file.rename( from = paste0("data/4share/",all.nms[i,"old_nms"]),
                                          to = paste0("data/4share/",all.nms[i,"new_nms"])
                                          )


# ---- session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sessions/dbs_MRI_postop_dcm2nii.txt" )
