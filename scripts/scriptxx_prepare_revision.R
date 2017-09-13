####################################################################################################
####################################################################################################
## Prepare data for time series clipping
## Contact remi.dannunzio@fao.org
## 2017/05/19 -- Cambodia
####################################################################################################
####################################################################################################
options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)

#######################################################################
##############################     SETUP YOUR DATA 
#######################################################################

## Set your working directory
setwd("C:/Users/dannunzio/Documents/countries/cambodia/workshop_20170911/data/results_ce/")
setwd("/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/cambodia/workshop_20170911/data/results_ce/")

## Read the datafiles
pts_results <- read.csv("results_day2/all_grp_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_120917_104259_CSV.csv")
pts_origin  <- read.csv("results_day1/sae_design_Change_FC14_16_31_august/pts_aa_change_1416_CE_2017-09-12.csv")

## Confusion matrix
table(pts_results$map_class,pts_results$ref_class)
table(pts_results$operator)

pts_results[pts_results$operator == "",]$operator <- "the_ghost"

## Check commissions and omissions
check <- pts_results[
    (pts_results$ref_class != pts_results$map_class)
    ,]

## See how did what
table(pts_results$operator)#,useNA = "always")
table(check$operator)#,useNA = "always")


## Setup points to check
out <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class != pts_results$map_class)
    ,
    ]$id,
  ]

## List of operators who have to check something
list_op <- unique(check$operator)

## Export as csv file
for(op in list_op){
  tmp <- out[out$id %in% pts_results[pts_results$operator == op,]$id,]
  write.csv(tmp,paste0("results_day2/check_",op,"20170912.csv"),row.names=F)
}
