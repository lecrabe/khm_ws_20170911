####################################################################################
#######    object: CLEAN AND COMBINE SHAPEFILES                 ####################
#######    Update : 2017/09/11                                  ####################
#######    contact: remi.dannunzio@fao.org                      ####################
####################################################################################
rootdir <- "~/khm_ws_20170911/data/"
setwd(rootdir)
rootdir <- paste0(getwd(),"/")
                  
#################### READ SHAPEFILES
shp14 <- readOGR(dsn="FC2014_1.shp",layer = "FC2014_1" )
shp16 <- readOGR(dsn="FC2016.shp",layer = "FC2016" )


#################### DETERMINE EXTENT OF BOTH SHAPEFILES
ext <- extent(shp14)
extent(shp16)


#################### EXTRACT DBF AND CHECK DISTRIBUTION OF CLASSES
dbf14 <- shp14@data
dbf16 <- shp16@data

names(dbf14)
names(dbf16)

table(dbf16$FC2016)
table(dbf14$FC2014)


#################### GENERATE UNIQUE POLYGON ID AND STANDARDIZE NAMES
dbf14$polyid <- row(dbf14)[,1]
dbf16$polyid <- row(dbf16)[,1]

dbf14 <- dbf14[,c("polyid","FC2014")]
dbf16 <- dbf16[,c("polyid","FC2016")]

names(dbf14) <- c("polyid_14","class14")
names(dbf16) <- c("polyid_16","class16")


#################### DETERMINE LIST OF CLASSES FOR EACH DATASET
list_class14 <- unique(dbf14$class14)
list_class16 <- unique(dbf16$class16)
list_class   <- unique(list_class16,list_class14)
list_class   <- list_class[order(list_class)]


#################### CREATE NUMERIC CODE FOR EACH CLASS
code_class <- data.frame(cbind(list_class,1:length(list_class)))
names(code_class) <- c("class","code")
write.csv(code_class,"code_class.csv",row.names = F)

#################### MERGE THESE CODES IN DBF
dbf14 <- merge(dbf14,code_class,by.x="class14",by.y="class",all.x=T)
dbf16 <- merge(dbf16,code_class,by.x="class16",by.y="class",all.x=T)


#################### EXPORT THE HARMONIZED SHAPEFILES
shp14@data <- arrange(dbf14,polyid_14)
shp16@data <- arrange(dbf16,polyid_16)

writeOGR(shp14,"shp2014.shp","shp2014",driver="ESRI Shapefile",overwrite_layer = T)
writeOGR(shp16,"shp2016.shp","shp2016",driver="ESRI Shapefile",overwrite_layer = T)


#################### RASTERIZE FIRST SHAPEFILE AT 10m RESOLUTION
system(sprintf("gdal_rasterize -a %s -l %s -co COMPRESS=LZW -te %s %s %s %s -tr %s %s -ot UInt16 %s %s",
               "code",
               "shp2014",
               ext@xmin,ext@ymin,ext@xmax,ext@ymax,
               10,10,
               "shp2014.shp",
               "shp2014.tif"
))


#################### RASTERIZE SECOND SHAPEFILE AT 10m RESOLUTION
system(sprintf("gdal_rasterize -a %s -l %s -co COMPRESS=LZW -te %s %s %s %s -tr %s %s -ot UInt16 %s %s",
               "code",
               "shp2016",
               ext@xmin,ext@ymin,ext@xmax,ext@ymax,
               10,10,
               "shp2016.shp",
               "shp2016.tif"
))


#################### COMBINE BOTH RASTERS INTO A 2_DATES_CODE RASTER
system(sprintf("gdal_calc.py -A %s -B %s --type=UInt16 --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
               "shp2014.tif",
               "shp2016.tif",
               "change_1416.tif",
               "A*100+B"
))


#################### COMPUTE OCCURENCE OF THE TRANSITION RASTER
system(sprintf("oft-stat -i %s -o %s -um %s -nostd",
               "change_1416.tif",
               "stats_change.txt",
               "change_1416.tif"
               ))


#################### READ STATISTICS AND RESEPARATE EACH DATE COMPONENT
df <- read.table("stats_change.txt")[,1:2]
names(df) <- c("chg_code","pix_count")

df$code14 <- as.numeric(substr(as.character(10000 + df$chg_code),2,3))
df$code16 <- as.numeric(substr(as.character(10000 + df$chg_code),4,5))


#################### ORGANIZE AS A TRANSITION MATRIX
tmp <- data.frame(tapply(df$pix_count*10*10/10000,df[,c("code14","code16")],sum))
names(tmp) <- c(list_class16)
tmp$code14 <- c(list_class14)
tmp[is.na(tmp)]<- 0

#################### WHEN TRANSITIONS ARE NOT OCCURRING, FILL WITH ZEROS
matrix<-matrix(0,nrow=length(list_class)+1,ncol=length(list_class)+1)

for(i in 1:length(list_class)+1){
  for(j in 1:length(list_class)+1){
    tryCatch({
      print(paste0(i,j))
    matrix[i,j] <- tmp[tmp$code14 == c("nodata",list_class16)[i] ,c("nodata",list_class16)[j]]
    }, error=function(e)cat("Not relevant\n")
    )
  }
}

matrix <- data.frame(matrix)
names(matrix)<-c("nodata",list_class)
rownames(matrix) <- c("nodata",list_class)
matrix

#################### EXPORT AS CSV FILE
write.csv(matrix,"transition_1416.csv")

matrix <- as.matrix(read.csv("transition_1416.csv")[,-1])
rownames(matrix) <- colnames(matrix)

#################### PLOT MATRIX WITH COLOR GRADIENTS
matrix <- matrix/sum(matrix)
matrix <- matrix / max(matrix)


