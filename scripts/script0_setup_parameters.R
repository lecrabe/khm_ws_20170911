### click on SOURCE



####################################################################################
#######    object: SETUP YOUR LOCAL PARAMETERS                  ####################
#######    Update : 2017/09/11                                  ####################
#######    contact: remi.dannunzio@fao.org                      ####################
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or 
# software or in the documentation accompanying it, for program maintenance and 
# upgrading as well as for any # damage that may arise from them. FAO also declines 
# any responsibility for updating the data and assumes no responsibility for errors 
# and omissions in the data provided. Users are, however, kindly asked to report any 
# errors or deficiencies in this product to FAO.
####################################################################################

#################### SET OPTIONS AND NECESSARY PACKAGES
options(stringsAsFactors = FALSE)

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(foreign)
library(dplyr)

############### DOWNLOAD WORKSHOP DATA
############### SET WORKING ENVIRONMENT
rootdir <- "~/khm_ws_20170911/data/"
dir.create(rootdir)
setwd(rootdir)
rootdir <- paste0(getwd(),"/")

system("wget https://www.dropbox.com/s/8wuirpisett6gbd/data.rar?dl=0")
system("unrar e data.rar?dl=0" )


