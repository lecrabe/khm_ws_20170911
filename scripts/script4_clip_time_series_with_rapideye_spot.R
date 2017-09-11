####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, NDVI trend 
## Contact remi.dannunzio@fao.org
## 2017/005/15 - Cambodia
####################################################################################################
####################################################################################################

# Options -----------------------------------------------------------------


options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(dplyr)
library(rgeos)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

#################### SET WORKING ENVIRONMENT
rootdir <- "/home/dannunzio/Documents/cambodia/"   #####   CHANGE TO MY VALUE HERE
setwd(rootdir)

#### Name of the directory where your Landsat data is
lsat_dir <- paste0(rootdir,"time_series/landsat/")

#### Name of the directory where your Sentinel data is
stnl_dir <- paste0(rootdir,"time_series/sentinel/")

#### Name of the directory where your Rapideye data is
rpdy_dir <- paste0(rootdir,"time_series/rapideye/")

#### Name of the directory where your data will be stored in output
dest_dir <- paste0(rootdir,"clip_time_series_test/")

dir.create(dest_dir)

# Preparation of files ----------------------------------------------------

# ## Read the datafile and setup the correct names for the variables
pts <- read.csv("/home/dannunzio/Documents/cambodia/day3/cambodia_CE_2017-05-18.csv")  #####  CHANGE TO MY VALUE HERE

head(pts)
names(pts)

map_code <- "map_class"
point_id <- "id"
xcoord   <- "XCoordinate"
ycoord   <- "YCoordinate"

##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################

################# Create spatial point file 
pt_df_geo <- SpatialPointsDataFrame(
  coords = pts[,c(xcoord,ycoord)],
  data   = data.frame(pts[,c(point_id,map_code)]),
  proj4string=CRS("+init=epsg:4326")
)


################# Create spatial point file in UTM
proj_utm <- proj4string(raster(paste0(rpdy_dir,list.files(rpdy_dir,pattern=glob2rx("*.tif"))[1])))

pt_df_utm <- spTransform(pt_df_geo,proj_utm)


################ Create the index of the Landsat tiles
list_lsat <- list.files(lsat_dir,pattern="lsat_2013")
lp <- list()

for(file in list_lsat){
  raster <- raster(paste(lsat_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF PRIMER ERROR
lsat_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_lsat), 
  match.ID = F
)

head(lsat_idx)
names(lsat_idx@data) <- "bb"
lsat_idx@data$bb <- substr(lsat_idx@data$bb,27,(nchar(lsat_idx@data$bb)-4))
lsat_idx@data
plot(lsat_idx)

################ Create the index of the Sentinel tiles
list_s2 <- list.files(stnl_dir,pattern="s2_2016")
lp<-list()

for(file in list_s2){
  raster <- raster(paste(stnl_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF  SEGUNDO ERROR
stnl_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_s2), 
  match.ID = F
)

names(stnl_idx@data) <- "bb"
stnl_idx@data$bb <- substr(stnl_idx@data$bb,25,(nchar(stnl_idx@data$bb)-4))
stnl_idx@data
plot(stnl_idx,add=T)

################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- proj4string(lsat_idx) <- proj4string(stnl_idx)  <- CRS("+init=epsg:4326")


################ Create the index of the Rapideye tiles
list_rpdy <- list.files(rpdy_dir,pattern="rpdy_2016")
lp<-list()

for(file in list_rpdy){
  raster <- raster(paste(rpdy_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
rpdy_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_rpdy), 
  match.ID = F
)

names(rpdy_idx@data) <- "bb"
proj4string(rpdy_idx) <- proj_utm
plot(rpdy_idx)
plot(pt_df_utm,add=T)

################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)
pts_rpdy <- over(pt_df_utm,rpdy_idx)

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)
pts<-cbind(pts,pts_rpdy$bb)

################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- 1000/111321

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,point_id])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(outbox) <- CRS("+init=epsg:4326")

## Transform the list into a SPDF
outbox_utm <- spTransform(outbox,proj_utm)


################# Create the 0.5 ha box (70/2 = 35m shift from center)
lp<-list()
ysize <- 35/111321

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,1])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
inbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(inbox) <- CRS("+init=epsg:4326")
inbox_utm <- spTransform(inbox,proj_utm)

proj4string(inbox) <- proj4string(outbox) <- CRS("+init=epsg:4326")
proj4string(inbox_utm) <- proj4string(outbox_utm) <- proj_utm


################ Create the list of ID's to process
list_ids <- pts[,point_id]

# ID to process 

listdone <- list() 
listdone <- read.table(text=list.files(dest_dir),as.is=T,fill=T,sep="_")[,2]
listdone <- gsub(".png","",listdone)
listodo  <- list_ids[!(list_ids %in% listdone)]

head(pts)
#####################################################################################
#####################################################################################
#####################################################################################

# Loop through all IDs ----------------------------------------------------


######################################################################################################
################# Loop through the IDs
##    example.... the_id = "585"
dev.off()

## The export image will be in a 3 (height) x 2 (width) grid box
dim_v_grid <- 3
dim_h_grid <- 3

for(the_id in listodo[1:10]){
  
  ####################################################################
  ################# Open the image output file
  
  out_name <- paste(dest_dir,"pt_",the_id,".png",sep="")
  png(file=  out_name,
      width= 400*dim_h_grid,
      height=400*dim_v_grid)
  
  ## Check which point is being processed
  (the_pt <- pts[pts[,point_id]==the_id,])
  
  ####################################################################
  ##### Delimitations of the plot in geographic coordinates
  one_poly <- outbox[outbox@data[,point_id]==the_id,]
  in_poly  <-   inbox[inbox@data[,point_id]==the_id,]
  
  margins <- extent(
    one_poly@bbox["x","min"]-100/111321,
    one_poly@bbox["x","max"]+100/111321,
    one_poly@bbox["y","min"]-100/111321,
    one_poly@bbox["y","max"]+100/111321)
  
  ####################################################################
  ##### Delimitations of the plot in UTM coordinates
  one_poly_utm <- outbox_utm[outbox_utm@data[,point_id]==the_id,]
  in_poly_utm  <-   inbox_utm[inbox_utm@data[,point_id]==the_id,]
  
  margins_utm <- extent(
    one_poly_utm@bbox["x","min"]-100,
    one_poly_utm@bbox["x","max"]+100,
    one_poly_utm@bbox["y","min"]-100,
    one_poly_utm@bbox["y","max"]+100)
  
  ####################################################################
  ################# Find the corresponding indexes
  lsat_bbox <- the_pt[,"pts_lsat$bb"]
  stnl_bbox <- the_pt[,"pts_stnl$bb"]
  rpdy_bbox <- the_pt[,"pts_rpdy$bb"]
  
  ################# Set the layout
  #dev.off()
  ## The export image will be in a 4 (height) x 5 (width) grid box
  par(mfrow = c(dim_v_grid,dim_h_grid))
  par(mar=c(1,0,1,0))
  
  ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndvi_trend) <- c("year","mean")
  i <- 1
  year <- "2013"
  ####################################################################
  ################# Clip the landsat time series
  for(year in c(2013:2015)){
    print(year)
    plot(margins,axes=F,xlab="",ylab="")
    tryCatch({
      lsat <- brick(paste(lsat_dir,"median_hul_clip_lsat_",year,"_",lsat_bbox,".tif",sep=""))
      lsat_clip<-crop(lsat,one_poly)
      
      swir <- raster(lsat_clip,4)
      nir  <- raster(lsat_clip,3)
      red  <- raster(lsat_clip,2)
      green<- raster(lsat_clip,1)
      ndvi <- (nir-red)/(nir+red)
      #nbr  <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      i <- i + 1
      
      #Plot natural colours composite (NIR-RED-GREEN == 4-3-2 in L7 nomenclature)
      stack <- stack(swir,nir,red)
      plotRGB(stack,stretch="hist",add=T)
      #plot(ndvi,add=T)
    },error=function(e){cat("Configuration impossible \n")})
    
    lines(in_poly,col="red",lwd=2)
    title(main=paste("landsat_",year,sep=""),font.main=1200)
    
  }
  
  
  
  ####################################################################
  ################# Clip the RapidEye tile 
  
  for(year in c(2016)){
    plot(margins_utm,axes=F,xlab="",ylab="")
    print(year)
    the_pt
    tryCatch({
      rpdy <- brick(paste(rpdy_dir,rpdy_bbox,sep=""))
      rpdy_clip<-crop(rpdy,one_poly_utm)
      
      blu <- raster(rpdy_clip,1)
      grn <- raster(rpdy_clip,2)
      red <- raster(rpdy_clip,3)
      nir <- raster(rpdy_clip,4)
      
      #ndvi <- (nir-red)/(nir+red)
      
      R <- red
      G <- grn
      B <- blu
      
      stackNat <- stack(R,G,B)
      ndvi <- (nir-red)/(nir+red)
      #nbr  <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly_utm),stat='mean')
      i <- i + 1
      #stackVeg <- stack(nir,ndvi,grn)
      #stackNIR <- stack(nir,red,grn)
      
      plotRGB(stackNat,stretch="hist",add=T)
      
      
    },error=function(e){cat("Configuration impossible \n")})
    lines(in_poly_utm,col="red",lwd=2)
    #plot(in_poly,add=T,col="red")
    
    title(main=paste0("rapideye_",year),font.main=200)
  }
  
  
  
  ####################################################################
  ################# Clip the sentinel tile 
  for(year in c(2016:2017)){
    plot(margins,axes=F,xlab="",ylab="")
    print(year)
    the_pt
    tryCatch({
      stnl <- brick(paste(stnl_dir,"median_hul_clip_s2_",year,"_",stnl_bbox,".tif",sep=""))
      stnl_clip<-crop(stnl,one_poly)
      
      blu <- raster(stnl_clip,1)
      grn <- raster(stnl_clip,2)
      red <- raster(stnl_clip,3)
      nir <- raster(stnl_clip,4)
      
      #ndvi <- (nir-red)/(nir+red)
      
      R <- red
      G <- grn
      B <- blu
      
      stackNat <- stack(R,G,B)
      ndvi <- (nir-red)/(nir+red)
      #nbr  <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      i <- i + 1
      #stackVeg <- stack(nir,ndvi,grn)
      #stackNIR <- stack(nir,red,grn)
      
      plotRGB(stackNat,stretch="hist",add=T)
      
      
    },error=function(e){cat("Configuration impossible \n")})
    lines(in_poly,col="red",lwd=2)
    #plot(in_poly,add=T,col="red")
    
    title(main=paste0("sentinel_",year),font.main=200)
  }
  
  ####################################################################
  ################# function to all pixel stack 
  
  par(mar=c(2,2,2,2))
  tryCatch({
    plot(ndvi_trend,
         # yaxt='n',
         # xaxt='n',
         xlab="year",
         ylab="",
         ylim=c(0,1)
    )
    
    title(main="mean ndvi",font.main=200)
  },error=function(e){cat("Configuration impossible \n")})
  ####################################################################
  ### Close the image file
  dev.off()
  
  
  ####################################################################
  ### End the points loop
}

the_pt
