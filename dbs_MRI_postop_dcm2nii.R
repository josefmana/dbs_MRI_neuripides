# In this script I first summarize raw (DICOM) MRI data from a chosen parent directory and then convert them
# to NIfTI format (via Roden`s dcm2niix, https://github.com/rordenlab/dcm2niix) in the BIDS folder structure

# In the latest run (2022-07-28) I ran in R version 4.2.0 (2022-04-22), on aarch64-apple-darwin20 (64-bit)
# platform under macOS Monterey 12.4. the following versions of packages employed: dplyr_1.0.9, tidyverse_1.3.1,
# divest_0.10.2, and RNifti_1.4.1

# set working directory (works in RStudio only)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# list packages to be used
pkgs <- c("dplyr", # for object manipulations
          "tidyverse", # for more object manipulations
          "divest", # R interfece to dcm2niix
          "RNifti" # R-native NIfTI tools
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


# ----------- data extraction -----------

# get directory name for each included patient
pats <- dir( path = paste(getwd(), fold, sep = "/") )

# extract folder names for each session for each patient
ses <- list()
for ( i in pats ) ses[[i]] <- dir( path = paste(fold, i, sep = "/") )

# create a list containing the whole tree structure of the raw data
tree <- list()

# go through the data structure as it is to extract paths for each file containing an image
# WILL NEDD TO REMAKE IT ONCE I'LL START WORKING WITH LESS STRUCTURED DATA (i.e., data directly from the MRI place)
for ( i in pats ) {
  for ( j in ses[[i]] ) {
    tree[[i]][[j]][["anat"]] <- dir( path = paste(fold, i, j, "anat/T1_MPRAGE", sep = "/"), full.names = T, recursive = T )
    tree[[i]][[j]][["func"]][["rs_on"]] <- dir( path = paste(fold, i, j, "funct/RS_ON", sep = "/"), full.names = T, recursive = T )
    tree[[i]][[j]][["func"]][["rs_off"]] <- dir( path = paste(fold, i, j, "funct/RS_OFF", sep = "/"), full.names = T, recursive = T )
  }
}

# look for discrepancies in number of scans (expected vs present in the folders)
disc <- unlist(ses) %>%
  as.data.frame %>%
  rownames_to_column( "patient" ) %>%
  rename( "session" = "." ) %>%
  mutate( patient = substr( patient, 1, 6),
          anat = NA, rs_on = NA, rs_off = NA
          )

# loop through each session/patient and compare n_scans (expected) against session length (observed)
# save names of session/patients with unexpected number of scans
for ( i in 1:nrow(disc) ) with(
  disc, {
    if ( length( tree[[patient[i]]][[session[i]]][["anat"]] ) != n_scans[["t1"]] ) disc$anat[i] <<- length( tree[[patient[i]]][[session[i]]][["anat"]] )
    if ( length( tree[[patient[i]]][[session[i]]][["func"]][["rs_on"]] ) != n_scans[["rs"]] ) disc$rs_on[i] <<- length( tree[[patient[i]]][[session[i]]][["func"]][["rs_on"]] )
    if ( length( tree[[patient[i]]][[session[i]]][["func"]][["rs_off"]] ) != n_scans[["rs"]] ) disc$rs_off[i] <<- length( tree[[patient[i]]][[session[i]]][["func"]][["rs_off"]] )
  }
)

# print and write the table of dicrepancies as csv
disc[ which( rowSums( !is.na(disc[,3:5]) ) != 0 ) , ]
disc[ which( rowSums( !is.na(disc[,3:5]) ) != 0 ) , ] %>% write.table( . , "discrepancies.csv", row.names = F, sep = "," , na = "ok" )

