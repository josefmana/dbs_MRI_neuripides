setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir <- getwd()

library(oro.dicom)
library(dplyr)
library(tidyverse)
library(openxlsx)

n = 79 # number of patients to convert from dicom to nifti
n_scans <- list(T1 = 176,  # number of scans in T1w sequence
                RS = 203)  # number of scans in resting state sequence

f <- "unzipped" # file which contains the data
dicom_path = paste0(dir, "/", f) # path to dicom data
pat_dirs <- dir(path = dicom_path) # names of directories for single patients
tree <- list()
for (i in pat_dirs) tree[[i]] <- as.list(dir(path = paste(dicom_path, i, "DICOM", sep = "/")))
tree <- unlist(tree) %>%
  data.frame() %>%
  rownames_to_column(var = "sub") %>%
  mutate(sub = substr(sub, 1, 6)) %>%
  rename("date" = ".")

isTRUE(all.equal (n, length(unique(tree$sub)))) ## Is the number of patients equal to the number
                                                ## of folders?

                                                ## continue iff TRUE, find the source of the 
                                                ## discrepancy otherwise

################################x##################
## get paths to all images of all patients/sessions

img <- list(all = list(), anat = list(), rs_on = list(), rs_off = list())
for (i in unique(tree$sub)) {
  for (k in names(img)) img[[k]][[i]] <- list()
  for (j in tree$date[ tree$sub == i]) {
    img$all[[i]][[j]] <- dir(path = file.path(dicom_path, i, "DICOM", j), full.names = T, recursive = T)
    img$anat[[i]][[j]] <- img$all[[i]][[j]][ grepl( "T1_MPRAGE" , img$all[[i]][[j]] ) ]
    img$rs_on[[i]][[j]] <- img$all[[i]][[j]][ grepl( "RS_ON" , img$all[[i]][[j]] ) ]
    img$rs_off[[i]][[j]] <- img$all[[i]][[j]][ grepl( "RS_OFF" , img$all[[i]][[j]] ) ]
  }
}
# look for discrepancies in the number of scans in single T1w sessions
dum_df <- data.frame(patient = tree$sub, date = tree$date, session = NA, n_scans = NA)
disc <- list(anat = dum_df, rs_on = dum_df, rs_off = dum_df)
for (i in names(disc)) {
  for (j in 1:nrow(disc[[i]])) {
    if (i == "anat") s <- "T1" else s <- "RS"
    sub <- disc[[i]]$patient[j]
    date <- disc[[i]]$date[j]
    if (n_scans[[s]] != length(img[[i]][[sub]][[date]])) {
      disc[[i]][ j , "session" ] <- i
      disc[[i]][ j , "n_scans" ] <- length(img[[i]][[sub]][[date]])
    }
  }
}
# print and save discrepancies
d <- na.omit(do.call(rbind.data.frame, disc))
print(d)
write.table(d, "discrepancies.csv", row.names = F, sep = ",")

              ## continue after all discrepancies are tackled in one way or 
              ## another
              ## rerun from "Get paths to all T1w images" to double-check if
              ## discrepancies were identified

# read data from the first file of each DICOM series for each patient/session
dicom <- list(anat = list(), rs_on = list(), rs_off = list())
system.time(
  for (i in names(dicom)) {
    for (j in names(img[[i]])) {
      for (k in names(img[[i]][[j]])) {
        tryCatch(
          dicom[[i]][[j]][[k]] <- readDICOM(img[[i]][[j]][[k]][1])$hdr,
          error = function(e) {print(paste0("Error - subject: ", j, ", session: ", k, ", type: ", i))}
        )
      }
    }
  }
)
# create table for each mesure
leg <- data.frame(
  var = c("name", "sex", "date",
          "study_time", "series_time", "acquisition_time", "content_time",
          "machine", "protocol", "series_name", "sequence_name",
          "slice_thickness", "repetition_time", "echo_time", "inversion_time"),
  nam = c("PatientsName", "PatientsSex", "StudyDate",
          "StudyTime", "SeriesTime", "AcquisitionTime", "ContentTime",
          "ManufacturersModelName", "ProtocolName", "SeriesDescription", "SequenceName",
          "SliceThickness", "RepetitionTime", "EchoTime", "InversionTime")
)
dum_m <- matrix(
  data = NA, nrow = nrow(tree), ncol = nrow(leg) + 2,
  dimnames = list(1:nrow(tree), c("folder_subject_name", "folder_session_date", leg$var))
)
tab <- list(anat = dum_m, rs_on = dum_m, rs_off = dum_m)
system.time(
  for (i in names(tab)) {
    for (j in 1:nrow(tree)) {
      sub <- tree$sub[j]
      date <- tree$date[j]
      tab[[i]][j, "folder_subject_name"] <- sub
      tab[[i]][j, "folder_session_date"] <- date
      for (k in 1:nrow(leg)) {
        var <- leg$var[k]
        nam <- leg$nam[k]
        tryCatch(
          tab[[i]][j, var] <- with(dicom[[i]][[sub]][[date]][[1]], value[name == nam]),
          error = function(e) {tab[[i]][j, var] <- NA}
        )
      }
    }
  }
)
# change times to good format
for (i in names(tab)) {
  tab[[i]] <- as.data.frame(tab[[i]])
  tab[[i]]$date <- paste0(substr(tab[[i]]$date, 1, 4), "-", substr(tab[[i]]$date, 5, 6), "-", substr(tab[[i]]$date, 7, 8))
  for (j in c("study_time", "series_time", "acquisition_time", "content_time")) {
    tab[[i]][[j]] <- paste0(
      substr(tab[[i]][[j]], 1, 2), ":", substr(tab[[i]][[j]], 3, 4), ":", substr(tab[[i]][[j]], 5, 6)
    )
  }
}
# save xlsx
write.xlsx(tab, "pre-postNEURIPIDES_mri_dicom_summary.xlsx", rowNames = F)
