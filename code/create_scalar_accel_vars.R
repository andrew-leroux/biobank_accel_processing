## read in data path information
source(file.path(".","code","make_data_directories.R"))

###################
#  load packages  #
###################
pckgs <- c("lubridate", "readr", "dplyr", "refund")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
  install.packages(x)
  require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

####################################
#  Read in UKB data quality flags  #
####################################

## read in data with data quality flags
xdf     <- read_rds(file.path(data_path_accel_quality_flags, data_accel_quality_flags))

# subset to just those individuals with "good" quality data as measured by UKB
xdf <- xdf %>% 
  mutate(
    ## create an indicator of whether accelerometry data was deeemed "good" by UKB team
    "good_accel_data_ukb" = invitation_to_physical_activity_study_acceptance %in% "Completed" &
                            data_quality_good_wear_time %in% "Yes" &
                            data_quality_good_calibration %in% "Yes" & 
                            data_quality_calibrated_on_own_data %in% "Yes"
      ) %>% 
    filter(good_accel_data_ukb == TRUE)


## load minute level accelerometry data
accel_mat  <- read_rds(path=file.path(data_path_processed, "1min_accel.rds"))
impute_mat <- read_rds(path=file.path(data_path_processed, "1min_impute.rds"))

## re-name acceleration columns to be easier to work with in matrix/data frame operations
accel_cols <- paste0("MIN",1:1440)
colnames(accel_mat) <- colnames(impute_mat) <- c("date","eid",accel_cols)


## combine accel and impute mat to estimate "good" days of data where
## "good" days of accelerometry data which are defined as 
## 95% of observed data.

## calcualate percent of observed day data which are good, then define a "good" 
## day as those with less than 5% imputed data
impute_mat$pct_good_obs <- rowSums(impute_mat[,paste0("MIN",1:1440)], na.rm=TRUE)/(1440*12)
impute_mat$good_day <- as.numeric(impute_mat$pct_good_obs >= 0.95)

## merge the good day information with acceleration data
accel_mat <- 
  impute_mat %>% 
  dplyr::select("eid","date","good_day","pct_good_obs") %>% 
  left_join(accel_mat, ., by=c("eid","date")) %>% 
  group_by(eid) %>% 
  mutate("dow" = wday(date, label=TRUE),
         "n_good_days" = sum(good_day),
         "n_good_wkdays" = sum((dow %in% c("Mon","Tue","Wed","Thu","Fri")) & (good_day==1) ),
         "n_good_wknd" = sum((dow %in% c("Sat","Sun"))) & (good_day==1)) %>%
  ungroup() 

## subset acceleration data to just those "good" days PLUS
## participants who have >= 3 "good" days of data PLUS 
## participants who have full data for our predictor variables of interest
accel_mat <- 
  accel_mat %>% 
  filter(eid %in% intersect(eid, xdf$eid) & n_good_days >= 3 & good_day == 1)  


## clean up the workspace
## no longer need imputation data
rm(list=c("impute_mat"))

#########################
# Impute "missing" data #
#########################

## perform fpca on the log transformed acceleration data
## we will use this for two purposes:
##   1) to impute the (relatively small) number of "missing" data
##   2) calculate surrogate measures based on PCA 
X        <- as.matrix(accel_mat[,paste0("MIN",1:1440)])
fit_fpca <- fpca.face(log(1+X), knots=50)

## impute missing acceleration data among good days using fpca predicted values
## truncate below by 0 since ENMO is bounded below by 0 by definition
inx_na    <- is.na(X)
X[inx_na] <- pmax(0,exp(fit_fpca$Yhat[inx_na]) + 1)
rm(list=c("fit_fpca", "inx_na"))

lX       <- log(1+X)
#########################################
# Calculate scalar features of interest #
#########################################


## TAC: Total daily acceleration
## TLAC: Total daily log(1+acceleration)
## Sed_Mins: Total daily minutes sedentary/sleep (Note that we cannot disentale sedentary and sleep minutes without first estimating sleep time)
## MVPA_Mins: Total daily minutes spent at or above 100miligs (This has been identified as a potential cutpoint for MVPA in certain populations)
## LIPA_Mins: Total daily minutes spent in between 30 and 100 miligs (Area between "sedentary" and "MVPA")
## DARE: Daytime activity ratio estiamte, proportion of total activity acculuated between 6AM and 8PM
features_mat <- 
  accel_mat %>% 
  dplyr::select(-one_of(c(paste0("MIN",1:1440))))

features_mat$TAC <- rowSums(X)
features_mat$TLAC <- rowSums(lX)
features_mat$Sed_Mins <- rowSums(X < 30)
features_mat$MVPA_Mins <- rowSums(X >= 100)
features_mat$LIPA_Mins <- rowSums(X < 100 & X >= 30)
features_mat$DARE      <- rowMeans(lX[,481:1200])/(rowMeans(lX[,481:1200]) + rowMeans(lX[,-c(481:1200)]))

## calcualte log acceleration accumulated in each 2-hour window of the day
tlen       <- 120
nt         <- floor(1440/tlen)
# create a list of indices for binning into 2-hour windows
inx_col_ls <- split(1:1440, rep(1:nt,each=tlen))
X_2hr      <- sapply(inx_col_ls, function(x) rowSums(lX[,x]))
colnames(X_2hr) <- paste0("TLAC_",c(1:12))

features_mat <- data.frame(features_mat, X_2hr)
rm(tlen, nt, inx_col_ls, X_2hr)

## PC surrogate measures
features_mat$PC1 <- rowMeans(lX[,(5*60+1):(24*60)])
features_mat$PC2 <- rowMeans(lX[,(10*60+1):(24*60)]) - rowMeans(lX[,(5*60+1):(10*60)])
features_mat$PC3 <- rowMeans(lX[,(8*60+1):(16*60)]) - rowMeans(lX[,c((5*60+1):(8*60), (16*60+1):(24*60))])
features_mat$PC4 <- rowMeans(lX[,c((5*60+1):(8*60), (11.5*60+1):(18*60))]) - rowMeans(lX[,c((8*60+1):(11.5*60), (18*60+1):(24*60))])
features_mat$PC5 <- rowMeans(lX[,c((8*60+1):(9*60), (14*60+1):(19*60))]) - rowMeans(lX[,c((5*60+1):(8*60), (9*60+1):(14*60), (19*60+1):(24*60))])
features_mat$PC6 <- rowMeans(lX[,c((7*60+1):(9*60), (13*60+1):(17*60), (21*60+1):(24*60))]) - rowMeans(lX[,c( (5*60+1):(7*60), (9*60+1):(13*60), (17*60+1):(21*60))])
features_mat$PC6_NHANES <- rowMeans(lX[,c((8*60+1):(10*60),(15*60+1):(17*60),(22*60+1):(24*60))]) - 
  rowMeans(lX[,c((5*60+1):( 7*60),(11*60+1):(13*60),(18*60+1):(20*60))])


## fragmentation measures
## SBout: average length of sedentary/sleep bouts
## ABout: average length of active bouts
## SATP: sedentary/sleep to active transition probability
## ASTP: active to sendetary/sleep transition probability
aX <- X >= 30
bout_mat <- apply(aX, 1, function(x){
  mat <- rle(x)
  sed <- mat$lengths[which(mat$values == FALSE)]
  act <- mat$lengths[which(mat$values == TRUE)]
  
  sed.m <- ifelse(length(sed) == 0, NA, mean(sed))
  act.m <- ifelse(length(act) == 0, NA, mean(act))
  
  c(sed.m, act.m)
})
features_mat$SBout <- bout_mat[1,]
features_mat$ABout <- bout_mat[2,]
features_mat$SATP  <- 1/features_mat$SBout
features_mat$ASTP  <- 1/features_mat$ABout
rm(aX, bout_mat)





## Calculate variables associated with timing of PA
## M10: average log acceleration accumulated during the 10 most active hours of the day
## L5: average log acceleration accumulated during the 5 least active hours of the day
## relA: relative amplitude = (M10-L5)/(M10 + L5)
# create transpose of the log(1+acceleration)
# we work with the transpose because colSums is MUCH faster than rowSums
# even using colSums, this takes a LONG time to run
tlX <- t(lX)

# create a list of indices for binning into 10- and 5- hour moving sums
inx_10hr_ls <- lapply(1:(1440-10*60 + 1), function(x) x:(x+(10*60-1)))
inx_5hr_ls  <- lapply(1:(1440-5*60 + 1), function(x) x:(x+(5*60-1)))
# obtain the moving sums
X_10hr     <- sapply(inx_10hr_ls, function(x) colMeans(tlX[x,,drop=FALSE]))
X_5hr      <- sapply(inx_5hr_ls, function(x) colMeans(tlX[x,,drop=FALSE]))
# calculate M10 and L5
M10 <- apply(X_10hr, 1, function(x){
  inx_x <- which.max(x)
  mx    <- x[inx_x]
  tx    <- inx_x + 300
  c(mx, tx)
})
L5 <- apply(X_5hr, 1, function(x){
  inx_x <- which.min(x)
  mx    <- x[inx_x]
  tx    <- inx_x + 150
  c(mx, tx)
})
rm(inx_10hr_ls,inx_5hr_ls, X_10hr, X_5hr, tlX)


features_mat <- data.frame(features_mat, 
                           "M10" = M10[1,], "M10_t" = M10[2,],
                           "L5" = L5[1,], "L5_t" = L5[2,])
features_mat$relA    <- (features_mat$M10 - features_mat$L5)/(features_mat$M10 + features_mat$L5)
rm(X_10min, M10min, tX, inx_10min_ls, M10, L5,X)

## combine data matrices
accel_mat <- left_join(accel_mat, features_mat, by=c("eid","date"))

## get individual averages
## note that the timing of L5 and M10 have to be handled separately
accel_vars_daily <- c("TAC","TLAC",paste0("TLAC_",1:12), 
                      "Sed_Mins","LIPA_Mins", "MVPA_Mins",
                      paste0("PC",1:6),"PC6_NHANES",
                      "SBout","ABout","SATP","ASTP",
                      "DARE","M10","L5","relA", paste0("MIN",1:1440))
accel_mat_ind <- 
  accel_mat %>% 
  dplyr::select(one_of(c("eid",accel_vars_daily))) %>% 
  group_by(eid) %>% 
  summarize_all(.funs=list("mean"=mean, "sd"=sd), na.rm=TRUE) %>% 
  ungroup()

accel_mat_t_vars <- 
  accel_mat %>% 
  select(eid, "M10_t","L5_t")

## a function for calcualting the average timing of variables (in this case the M10 and L5)
get_mean_sd_t <- function(tind, tmin=0, tmax=1, search_len=1000){
  tgrid <- seq(tmin, tmax, len=search_len)
  dmat  <- outer(tind, tgrid, FUN="-")
  dmat  <- pmin(dmat^2, (1440-abs(dmat))^2)
  dsum <- colSums(dmat)
  dmin <- which.min(dsum)
  c(tgrid[dmin], sqrt(dsum[dmin]/(length(tind)-1)))
}


uid <- unique(accel_mat_t_vars$eid)
nid <- length(uid)
accel_mat_t_vars_ind <- data.frame(matrix(NA_real_, ncol=5, nrow=nid))
colnames(accel_mat_t_vars_ind) <- c("eid", "M10_t_mean","M10_t_sd","L5_t_mean", "L5_t_sd")
accel_mat_t_vars_ind[,1] <- uid
for(i in 1:nid){
  df_i <- subset(accel_mat_t_vars, eid == uid[i])
  accel_mat_t_vars_ind[i,2:3] <- get_mean_sd_t(df_i$M10_t, tmax=1440, search_len=1441)
  accel_mat_t_vars_ind[i,4:5] <- get_mean_sd_t(df_i$L5_t, tmax=1440, search_len=1441)
  if(i %% 1000 == 0) print(i)
}

accel_mat_ind <- 
  accel_mat_ind %>% 
  left_join(accel_mat_t_vars_ind, by="eid") 

## save data frame with day-level features
write_rds(features_mat, path=file.path(data_path_processed, "accel_features.rds"))
## save data frame with subject specific averages
write_rds(accel_mat_ind, path=file.path(data_path_processed, "accel_features_ind.rds"))
