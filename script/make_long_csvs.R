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
  # make vector:
  v <- as.vector(unlist(v))

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
  # get columns that begin with the measure string
  measure_cols <- trial_results %>%
    select(starts_with(measure)) %>%
    as_tibble()

  # from trial_results, remove columns that begin with any string in the list measures_to_expand
  trial_results <- trial_results %>%
    select(-starts_with(measures_to_expand))

  # if number of columns is not 4, throw error
  if (ncol(measure_cols) != 4) {
    stop("Error: Double check the measure to be lengthened (also ensure there isn't another column that starts with the same string)")
  }

  # make empty list
  per_frame_x_list <- list()
  per_frame_y_list <- list()
  per_frame_z_list <- list()
  per_frame_timepoint_list <- list()

  per_frame_x_list <- populate_list(measure_cols, per_frame_x_list, measure, "_x")
  per_frame_y_list <- populate_list(measure_cols, per_frame_y_list, measure, "_y")
  per_frame_z_list <- populate_list(measure_cols, per_frame_z_list, measure, "_z")
  per_frame_timepoint_list <- populate_list(measure_cols, per_frame_timepoint_list, measure, "_time_stamp") 

  # Add the lists to the trial_results as new columns
  trial_results <- trial_results %>%
    add_column( x = per_frame_x_list, 
      y = per_frame_y_list, 
      z = per_frame_z_list, 
      time_stamp = per_frame_timepoint_list)

  # unnest the lists
  unnested_df <- trial_results %>%
    unnest(cols = c(x, y, z, time_stamp))
  

  # rename the columns
  unnested_df <- unnested_df %>%
    rename(!!paste(measure,"_x", sep="") := x, 
      !!paste(measure,"_y", sep="") := y, 
      !!paste(measure,"_z", sep="") := z, 
      !!paste(measure,"_time_stamp", sep="") := time_stamp)

  # save the measure
  fwrite(unnested_df, to_save_dir)

  # return anything
  return(1)
}

# populate a single list
populate_list <- function(measure_cols, list, measure, suffix) {
  # convert each row to a list
  for (i in 1:nrow(measure_cols)) {
    list[[i]] <- convert_cell_to_numvec(
      as.character(measure_cols[i, paste(measure, suffix, sep = "")])
    )
  }

  return(list)
}

##### Run #####
plan(multisession)
lengthen_all_data(measures_to_expand)

