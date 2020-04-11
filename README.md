# biobank_accel_processing
This repository contains code for transforming the 5-second level UK Biobank accelerometry 
data and deriving a number of scalar summaries.

Because the UK Biobank data must be stored locally, the data path will vary for each individual. 
To that end we have structured the data processing 
code contained herein to run dependent on an R script which defined three data directories.
In addition, for deriving scalar summaries, we require a name for a .rds file which contains the UK Biobank team 
derived accelerometry data quality variables. This file should be named "make_data_directories.R" and 
stored within the code subdirectory of this repository. An example "make_data_directories.R" is below:

```{r}
## data for raw 5-second level data
data_path_5sec                <- "raw_data_directory"
## directory for saving the minute level data
data_path_processed           <- "/processed_data_directory"
## directory for ukb quality flags 
data_path_accel_quality_flags <- "/data_qualtiy_directory"
## name of data file with data quality flags
data_accel_quality_flags      <- "accel.rds"
```

