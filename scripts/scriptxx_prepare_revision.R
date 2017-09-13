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
pts_results <- read.csv("results_day3/all_groups_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_130917_152246_CSV.csv")
pts_origin  <- read.csv("results_day1/sae_design_Change_FC14_16_31_august/pts_aa_change_1416_CE_2017-09-12.csv")

## Confusion matrix
table(pts_results$map_class,pts_results$ref_class)
table(pts_results$operator)

pts_results[pts_results$operator == "",]$operator <- "sampreap"

## Check commissions and omissions
check <- pts_results[
    (pts_results$ref_class != pts_results$map_class)
    ,]

## See how did what
table(pts_results$operator)#,useNA = "always")
(table(check$operator))
(table(pts_results$operator))#,useNA = "always")

write.csv(pts_results,paste0("results_day3/results_aa_cmb_2014_16_20170913.csv"),row.names=F)

table(check$operator,check$map_class)
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
  write.csv(tmp,paste0("results_day2/check_",op,"20170913.csv"),row.names=F)
}

################# Points to check where Map and Reference say Forest VS Non Forest
gp1_FNF <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class == "Non Forest stable" &  
     pts_results$map_class == "Forest stable") 
    |
    (pts_results$ref_class == "Forest stable" &  
     pts_results$map_class == "Non Forest stable")
    ,
    ]$id,
  ]

################# Points to check where Map and Reference say Forest VS Non Forest
gp2_DgE <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class %in% c("Forest stable","Non Forest stable") &  
     pts_results$map_class %in% c("Degradation","Enhancement")) 
    ,
    ]$id,
  ]

################# Points to check where Map and Reference say Forest VS Non Forest
gp3_Dfo <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class %in% c("Non Forest stable") &  
       pts_results$map_class %in% c("Deforestation crops",
                                    "Deforestation other",
                                    "Deforestation rubber"
                                    )) 
    ,
    ]$id,
  ]

################# Points to check where Map and Reference say Forest VS Non Forest
gp4_Omi <- pts_origin[
  pts_origin$id %in% pts_results[
    (pts_results$ref_class %in% c("Forest stable") &  
       pts_results$map_class %in% c("Deforestation crops",
                                    "Deforestation other",
                                    "Deforestation rubber",
                                    "Management"))
    |
      (pts_results$ref_class %in% c("Deforestation other") &  
         pts_results$map_class %in% c("Deforestation rubber",
                                      "Forest stable",
                                      "Non Forest stable")) 
    |
      (pts_results$ref_class %in% c("Deforestation crops") &  
         pts_results$map_class %in% c("Forest stable",
                                      "Non Forest stable"))
      
    ,
    ]$id,
  ]

#################### Export
write.csv(gp1_FNF,paste0("results_day3/check_gp1_FNF_20170913.csv"),row.names=F)
write.csv(gp2_DgE,paste0("results_day3/check_gp2_DgE_20170913.csv"),row.names=F)
write.csv(gp3_Dfo,paste0("results_day3/check_gp3_Dfo_20170913.csv"),row.names=F)
write.csv(gp4_Omi,paste0("results_day3/check_gp4_Omi_20170913.csv"),row.names=F)
