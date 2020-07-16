## read in data path information
source(file.path(".","make_data_directories.R"))

## If packages are not already installed, first install them and then
## load all requisite packages
pckgs <- c("remotes","readr","tidyr","dplyr","lubridate","stringr")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
  install.packages(x)
  require(x, character.only=TRUE)
})

## Obtain file names for all files in the directory with 5-second level data
data_5_sec_ls <- list.files(data_path_5sec)

## total number of subejcts to loop over 
nid <- length(data_5_sec_ls)
## extract all subject ids
eid <- str_extract(data_5_sec_ls, "[0-9]+")

## create vector of names for wide-format data 
accel_mat_nms  <- paste0("accel_mat_",1:nid)
impute_mat_nms <- paste0("impute_mat_",1:nid)

## unique minutes 
utimes <- sprintf("%02d:%02d", rep(0:23, each=60), rep(0:59, 24))

## loop over subjects
for(i in 1:nid){
  ## read in subject i's data
  accel_i <- read.table(file.path(data_path_5sec, data_5_sec_ls[i]), header=TRUE, sep=",")
  
  ## verify the sampling rate for the data is 5 seconds
  (interval_i <- str_replace(names(accel_i)[1], "(.*sampleRate\\.+)([0-9]+\\.[aA-zZ]+)","\\2"))
  if(interval_i != "5.seconds") next
  
  ## get portion of the column names which indicates start and stop dates/times
  raw_date_i <- str_extract_all(names(accel_i)[1], "\\.{3,3}([0-9]+\\.){5,5}[0-9]+")[[1]]
  ## extract start and stop dates/times
  date_i     <- str_replace_all(raw_date_i, "\\.{3,3}","") %>% 
    str_replace_all("\\.","-") %>%
    str_replace_all("([0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2})-([0-9]{2,2})-([0-9]{2,2})-([0-9]{2,2})","\\1 \\2:\\3:\\4")
  ## create a vector of date-times corresponding to observed data
  ## note that this sequence of dates does NOT account for DST (uses UTC)
  date_time_vec     <- seq(ymd_hms(date_i[1], tz="UTC"),ymd_hms(date_i[2], tz="UTC"), by="5 secs")
  date_time_vec_chr <- as.POSIXlt(date_time_vec, tz="UTC")

  ## separate out the date and time components of the date/time vector
  time_vec <- sprintf("%02d:%02d", hour(date_time_vec), minute(date_time_vec))
  time_vec <- factor(time_vec, levels = utimes)
  date_vec <- sprintf("%04d-%02d-%02d",year(date_time_vec), month(date_time_vec),day(date_time_vec))
  
  ## combine aceleration and date data
  adf_i  <- data.frame("eid"=eid[i], accel_i, time_vec, date_vec, stringsAsFactors = FALSE)
  colnames(adf_i)[2] <- "acceleration"
  adf_i <- adf_i %>% 
      ## group at the minute level
      group_by(date_vec, time_vec) %>%
      ## obtain mean and number of not imputed minutes
      summarize("acceleration" = mean(acceleration,na.rm=TRUE),
                "imputed" = sum(1-imputed,na.rm=TRUE),
                "eid" = eid[1], 
                .groups="drop") 
  
  ## transform to wide format for accelration and imputation separately using tidyr::spread
  accel_mat_i  <- adf_i %>% select(-imputed) %>% spread(time_vec,acceleration, drop=FALSE)
  impute_mat_i <- adf_i %>% select(-acceleration) %>% spread(time_vec,imputed, drop=FALSE)
  
  ## assign subject i's data to the corresponding names 
  assign(accel_mat_nms[i], accel_mat_i)
  assign(impute_mat_nms[i], impute_mat_i) 
    
  ## clear up the workspace a bit
  rm(accel_i, interval_i, raw_date_i, date_i, 
     date_time_vec,date_time_vec_chr,time_vec,date_vec,
     adf_i,accel_mat_i, impute_mat_i)
  
  print(i)
  gc()
  
}

## combine the data
accel_mat_nms  <- ls()[grepl("accel_mat_[0-9]+", ls())]
impute_mat_nms <- ls()[grepl("impute_mat_[0-9]+", ls())]

eval(parse(text=paste0("accel_mat <- bind_rows(", paste0(accel_mat_nms, collapse=","),")")))
eval(parse(text=paste0("impute_mat <- bind_rows(", paste0(impute_mat_nms, collapse=","),")")))

## save the combined  data
write_rds(accel_mat, path=file.path(data_path_processed, "1min_accel.rds"))
write_rds(impute_mat, path=file.path(data_path_processed, "1min_impute.rds"))
