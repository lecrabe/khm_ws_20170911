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
pts_results <- read.csv("results_day4/results_aa_cmb_2014_16_20170914.csv")
pts_origin  <- read.csv("results_day1/sae_design_Change_FC14_16_31_august/pts_aa_change_1416_CE_2017-09-12.csv")

## Confusion matrix
table(pts_results$map_class,pts_results$ref_class)
table(pts_results$operator)

################# Points to check where Deforestation Commissions
gp5_Com <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class %in% c("Non Forest stable") &  
    !(pts_results$map_class %in% c("Non Forest stable"))
     ) 
    ,
    ]$id,
  ]

################# Points to check where Map and Reference say Enhancement VS Forest
gp6_Enh <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class %in% c("Forest stable") &  
    (pts_results$map_class %in% c("Enhancement"))
    ) 
    ,
    ]$id,
  ]

#################### Export
write.csv(gp5_Com,paste0("results_day4/check_gp5_Com_20170914.csv"),row.names=F)
write.csv(gp6_Enh,paste0("results_day4/check_gp6_Enh_20170914.csv"),row.names=F)

