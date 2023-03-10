# Expand per-trial csv to create per-time-point csv
Script for expanding per-trial data from the SMCL unity experiment framework

## Steps
1. Make a copy of this repo (download or clone)
1. Populate the data/raw folder with your data
1. Open the R project via RStudio
1. In R studio, open the make_long_csvs.R script
1. Ensure you have the following 3 packages installed: tidyverse, data.table, future
1. In the R script, change the "measures_to_expand" list to include the columns you want to expand
1. Click "Source" (near the top of RStudio)
1. New data will be in the data/processed folder

## Notes
- Each of the measures to expand should be separated by an _ (underscore)
- For each measure, there should be _x, _y, _z and _timestamp columns
- Each folder in the data/raw folder should have with it: a single folder names S001, and within that, a single file names trial_results.csv
The above is the default data logging behaviour for our unity framework.