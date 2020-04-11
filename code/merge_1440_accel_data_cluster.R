## read in data path information
source(file.path(".","code","make_data_directories.R"))

## logical for whether to delete the batch job files once data merging is complete
delete_batch <- TRUE

## If packages are not already installed, first install them and then
## load all requisite packages
pckgs <- c("dplyr","data.table")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(pckgs)

## all minute level data to be combined 
accel_sep_ls <- list.files(data_path_processed)

## load all the minute level data
for(i in seq_along(accel_sep_ls)){
    load(file.path(data_path_processed, accel_sep_ls[i]))
    print(i)
}
rm(i, accel_sep_ls, data_path_out)

## combine the data
accel_mat_nms  <- ls()[grepl("accel_mat_[0-9]+", ls())]
impute_mat_nms <- ls()[grepl("impute_mat_[0-9]+", ls())]

eval(parse(text=paste0("accel_mat <- bind_rows(", paste0(accel_mat_nms, collapse=","),")")))
eval(parse(text=paste0("impute_mat <- bind_rows(", paste0(impute_mat_nms, collapse=","),")")))

rm(list=c(accel_mat_nms, impute_mat_nms))


accel_mat <- 
    accel_mat %>% 
    arrange(eid, date_vec)

impute_mat <- 
    impute_mat %>% 
    arrange(eid, date_vec)

## save the combined data
write_rds(accel_mat, path=file.path(data_path_processed, "1min_accel.rds"))
write_rds(impute_mat, path=file.path(data_path_processed,"1min_impute.rds"))

## delete individual files
if(delete_batch){
    file.remove(file.path(data_path_processed, accel_sep_ls))
}
