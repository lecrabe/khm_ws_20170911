####################################################################################################
####################################################################################################
## Integrate revisions from final check of CE database
## Contact remi.dannunzio@fao.org
## 2017/09/14 -- Cambodia
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
#setwd("C:/Users/dannunzio/Documents/countries/cambodia/workshop_20170911/data/results_ce/")
setwd("/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/cambodia/workshop_20170911/data/results_ce/")

## Read the datafiles
pts_results <- read.csv(paste0("results_day3/results_aa_cmb_2014_16_20170913.csv"))
pts_origin  <- read.csv("results_day1/sae_design_Change_FC14_16_31_august/pts_aa_change_1416_CE_2017-09-12.csv")

## Who
table(pts_results$operator)

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

rev_gr1_FNF <- read.csv("results_day4/grp1_FNF_remilinux_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_140917_095918_CSV.csv")
rev_gr2_DgE <- read.csv("results_day4/grp2_DgE_ALL_remilinux_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_140917_094351_CSV.csv")
rev_gr3_Dfo <- read.csv("results_day4/grp3_Dfo_remilinux_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_140917_100241_CSV.csv")
rev_gr4_Omi <- read.csv("results_day4/grp4_Omi_remilinux_collectedData_earthsae_aa_change_1416_CE_2017-09-12_on_140917_103146_CSV.csv")

rev_gr1_FNF <- rev_gr1_FNF[rev_gr1_FNF$id %in% gp1_FNF$id,]
rev_gr2_DgE <- rev_gr2_DgE[rev_gr2_DgE$id %in% gp2_DgE$id,]
rev_gr3_Dfo <- rev_gr3_Dfo[rev_gr3_Dfo$id %in% gp3_Dfo$id,]
rev_gr4_Omi <- rev_gr4_Omi[rev_gr4_Omi$id %in% gp4_Omi$id,]

final <- pts_results

final[final$id %in% gp1_FNF$id,] <- rev_gr1_FNF
final[final$id %in% gp2_DgE$id,] <- rev_gr2_DgE
final[final$id %in% gp3_Dfo$id,] <- rev_gr3_Dfo
final[final$id %in% gp4_Omi$id,] <- rev_gr4_Omi

table(final$map_class,final$ref_class)

#################### Export
write.csv(final,paste0("results_day4/results_aa_cmb_2014_16_20170914.csv"),row.names=F)
