## --------------------------------
## Purpose of script: Make long-form CSVS (one row per time-point) from per-trial data
##
## Author: Shanaa Modchalingam
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: See README for more information
##
## --------------------------------

rm(list = ls()) # clean environment

library(data.table)
library(tidyverse)
library(future)

##### Set this variable #####
measures_to_expand <- c("hand_pos", "Indicator_position")

##### Helper functions #####
# create a directory if it doesn't exist
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

# convert a single cell to a numeric vector
convert_cell_to_numvec <- function(v) {
  # split by commas:
  v <- strsplit(v, "_")
  # convert to numeric:
  v <- lapply(v, FUN = as.numeric)

  return(v)
}

##### Main functions #####
# lengthen all data in data/raw directory
lengthen_all_data <- function(measures_to_expand) {
  # get all participant paths
  ppt_paths <- list.dirs("data/raw", recursive = FALSE, full.names = TRUE)

  multiprocess_list <- vector("list", length(ppt_paths))

  # lengthen each participant
  for (i in 1:length(ppt_paths)) {
    multiprocess_list[[i]] <- future(lengthen_participant(ppt_paths[i], measures_to_expand))
  }

  # evaluate futures
  future_output <- value(multiprocess_list)

  # print completion message
  print("All data has been lengthened")
}

# lengthen a single participant
lengthen_participant <- function(ppt_path, measures_to_expand) {
  # load the trial_results.csv
  trial_results <- fread(paste0(ppt_path, "/S001/trial_results.csv"))

  # create a directory for the long-form data
  to_save_dir <- paste0("data/processed", substr(ppt_path, 9, nchar(ppt_path)))

  create_dir(to_save_dir)

  # save a copy of the trial_results.csv
  fwrite(trial_results, paste0(to_save_dir, "/trial_results.csv"))

  multiprocess_list <- vector("list", length(measures_to_expand))

  for (i in 1:length(measures_to_expand)) {
    # lengthen the measure
    multiprocess_list[[i]] <- future(
      lengthen_measure(trial_results, 
        measures_to_expand[i], 
        measures_to_expand, 
        paste0(to_save_dir, "/", measures_to_expand[i], ".csv")))
  }

  # evaluate futures
  future_output <- value(multiprocess_list)

  # return anything
  return(1)
}

# lengthen a single measure
lengthen_measure <- function(trial_results, measure, measures_to_expand, to_save_dir) {
  # remove measure from the measures_to_expand list
  measures_to_expand <- measures_to_expand[measures_to_expand != measure]

  # from trial_results, remove columns that begin with any string in the list measures_to_expand
  trial_results <- trial_results %>%
    select(-starts_with(measures_to_expand))

  # if number of columns is not 4, throw error
  if (sum(startsWith(colnames(trial_results), measure)) != 4) {
    stop("Error: Double check the measure to be lengthened (also ensure there isn't another column that starts with the same string)")
  }
  
  # convert the string lists to numeric vectors
  trial_results <- trial_results %>%
    mutate(across(starts_with(measure), convert_cell_to_numvec)) %>%
    unnest(cols = starts_with(measure))

  # save the measure
  fwrite(trial_results, to_save_dir)

  # return anything
  return(1)
}

##### Run #####
plan(multisession)
lengthen_all_data(measures_to_expand)