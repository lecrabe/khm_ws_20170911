####################################################################################
#######    object: CLEAN AND COMBINE SHAPEFILES                 ####################
#######    Update : 2017/09/11                                  ####################
#######    contact: remi.dannunzio@fao.org                      ####################
####################################################################################
options(stringsAsFactors = FALSE)

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(foreign)
library(dplyr)


rootdir <- "/media/dannunzio/hdd_remi/cambodia/national_data/"
setwd(rootdir)
rootdir <- paste0(getwd(),"/")

#################### SETUP INPUT NAMES
name_t1 <- "FC2014_1.shp"
name_t2 <- "FC2016_August_31_2017_area5ha_3.shp"

attr_t1 <- "FC2014"
attr_t2 <- "FC2016"

#################### GET BASENAME AND PATH TO FILE
base_t1 <- basename(name_t1)
base_t2 <- basename(name_t2)

path_t1 <- dirname(base_t1)
path_t2 <- dirname(base_t2)

#################### READ SHAPEFILES
shp_t1 <- readOGR(dsn=paste0(path_t1,"/",base_t1),layer = substr(base_t1,1,nchar(base_t1)-4) )
shp_t2 <- readOGR(dsn=paste0(path_t2,"/",base_t2),layer = substr(base_t2,1,nchar(base_t2)-4) )

#################### DETERMINE EXTENT OF BOTH SHAPEFILES
ext <- extent(shp_t1)

#################### EXTRACT DBF AND CHECK DISTRIBUTION OF CLASSES
dbf_t1 <- shp_t1@data
dbf_t2 <- shp_t2@data

write.dbf(dbf_t1,paste0(path_t1,"/bckup_",substr(base_t1,1,nchar(base_t1)-4),".dbf"))
write.dbf(dbf_t2,paste0(path_t2,"/bckup_",substr(base_t2,1,nchar(base_t2)-4),".dbf"))

names(dbf_t1)
names(dbf_t2)

table(dbf_t2[,attr_t2])
table(dbf_t1[,attr_t1])

#################### GENERATE UNIQUE POLYGON ID AND STANDARDIZE NAMES
dbf_t1$polyid <- row(dbf_t1)[,1]
dbf_t2$polyid <- row(dbf_t2)[,1]

dbf_t1 <- dbf_t1[,c("polyid",attr_t1)]
dbf_t2 <- dbf_t2[,c("polyid",attr_t2)]

names(dbf_t1) <- c("polyid_t1","class_t1")
names(dbf_t2) <- c("polyid_t2","class_t2")


#################### DETERMINE LIST OF CLASSES FOR EACH DATASET
list_class_t1 <- levels(as.factor((dbf_t1$class_t1)))
list_class_t2 <- levels(as.factor((dbf_t2$class_t2)))
list_class   <- unique(list_class_t2,list_class_t1)
list_class   <- list_class[order(list_class)]


#################### CREATE NUMERIC CODE FOR EACH CLASS
code_class <- data.frame(cbind(list_class,1:length(list_class)))
names(code_class) <- c("class","code")
write.csv(code_class,"code_class.csv",row.names = F)

#################### MERGE THESE CODES IN DBF
dbf_t1 <- merge(dbf_t1,code_class,by.x="class_t1",by.y="class",all.x=T)
dbf_t2 <- merge(dbf_t2,code_class,by.x="class_t2",by.y="class",all.x=T)

#################### EXPORT THE HARMONIZED SHAPEFILES
out_dbf_t1 <- arrange(dbf_t1,polyid_t1)
out_dbf_t2 <- arrange(dbf_t2,polyid_t2)

write.dbf(out_dbf_t1,paste0(path_t1,"/",substr(base_t1,1,nchar(base_t1)-4),".dbf"))
write.dbf(out_dbf_t2,paste0(path_t2,"/",substr(base_t2,1,nchar(base_t2)-4),".dbf"))

#################### RASTERIZE FIRST SHAPEFILE AT 10m RESOLUTION
system(sprintf("gdal_rasterize -a %s -l %s -co COMPRESS=LZW -te %s %s %s %s -tr %s %s -ot UInt16 %s %s",
               "code",
               substr(base_t1,1,nchar(base_t1)-4),
               ext@xmin,ext@ymin,ext@xmax,ext@ymax,
               30,30,
               base_t1,
               "shp_t1.tif"
))


#################### RASTERIZE SECOND SHAPEFILE AT 10m RESOLUTION
system(sprintf("gdal_rasterize -a %s -l %s -co COMPRESS=LZW -te %s %s %s %s -tr %s %s -ot UInt16 %s %s",
               "code",
               substr(base_t2,1,nchar(base_t2)-4),
               ext@xmin,ext@ymin,ext@xmax,ext@ymax,
               10,10,
               base_t2,
               "shp_t2.tif"
))


#################### COMBINE BOTH RASTERS INTO A 2_DATES_CODE RASTER
system(sprintf("gdal_calc.py -A %s -B %s --type=UInt16 --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
               "shp_t1.tif",
               "shp_t2.tif",
               "change_t1t2.tif",
               "A*100+B"
))


#################### COMPUTE OCCURENCE OF THE TRANSITION RASTER
system(sprintf("oft-stat -i %s -o %s -um %s -nostd",
               "change_t1t2.tif",
               "stats_change.txt",
               "change_t1t2.tif"
               ))


#################### READ STATISTICS AND RESEPARATE EACH DATE COMPONENT
df <- read.table("stats_change.txt")[,1:2]
names(df) <- c("chg_code","pix_count")

df$code_t1 <- as.numeric(substr(as.character(10000 + df$chg_code),2,3))
df$code_t2 <- as.numeric(substr(as.character(10000 + df$chg_code),4,5))


#################### ORGANIZE AS A TRANSITION MATRIX
tmp             <- data.frame(tapply(df$pix_count*10*10/10000,df[,c("code_t1","code_t2")],sum))
names(tmp)      <- c(list_class_t2)
tmp$code_t1     <- c(list_class_t1)
tmp[is.na(tmp)] <- 0

#################### WHEN TRANSITIONS ARE NOT OCCURRING, FILL WITH ZEROS
matrix <- matrix(0,nrow=length(list_class)+1,ncol=length(list_class)+1)

for(i in 1:length(list_class)+1){
  for(j in 1:length(list_class)+1){
    tryCatch({
      print(paste0(i,j))
    matrix[i,j] <- tmp[tmp$code_t1 == c("nodata",list_class_t2)[i] ,c("nodata",list_class_t2)[j]]
    }, error=function(e)cat("Not relevant\n")
    )
  }
}

matrix <- data.frame(matrix)
names(matrix)<-c("nodata",list_class)
rownames(matrix) <- c("nodata",list_class)
matrix

#################### EXPORT AS CSV FILE
write.csv(matrix,"transition_t1_t2.csv")
