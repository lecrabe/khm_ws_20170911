####################################################################################
#######    object: RECLASSIFY SHAPEFILES                        ####################
#######    Update : 2017/09/11                                  ####################
#######    contact: remi.dannunzio@fao.org                      ####################
####################################################################################
rootdir <- "~/khm_ws_20170911/data/"
setwd(rootdir)
rootdir <- paste0(getwd(),"/")

#################### READ STATISTICS AND RESEPARATE EACH DATE COMPONENT
df <- read.table("stats_change.txt")[,1:2]
names(df) <- c("chg_code","pix_count")

df$code_t1 <- as.numeric(substr(as.character(10000 + df$chg_code),2,3))
df$code_t2 <- as.numeric(substr(as.character(10000 + df$chg_code),4,5))

#################### READ CODE/CLASS CONVERSION TABLE
code_ipcc      <- read.csv("code_ipcc.csv")
tmp_code_class <- read.csv("code_class.csv")
code_class     <- merge(tmp_code_class,code_ipcc)
  
df1 <- merge(df,code_class[,c("code","class","ipcc_code","ipcc_class")],by.x="code_t1",by.y="code",all.x=T)
names(df1) <- c("code_t1","chg_code" ,"pix_count","code_t2","class_t1","ipcc_code_t1","ipcc_class_t1")

df1 <- merge(df1,code_class[,c("code","class","ipcc_code","ipcc_class")],by.x="code_t2",by.y="code",all.x=T)
names(df1) <- c("code_t2","code_t1","chg_code" ,"pix_count","class_t1","ipcc_code_t1","ipcc_class_t1","class_t2","ipcc_code_t2","ipcc_class_t2")

df1$ipcc_chge <- df1$ipcc_code_t1 *10 + df1$ipcc_code_t2

df1$final <- 0

table(df1$ipcc_class_t1,df1$ipcc_class_t2,useNA = "always")

unique(df1[df1$ipcc_class_t2!="F",]$class_t2)

nat_forest <- c("B","D","E","Ff","P","Se","M","Mr")
sec_forest <- c("Tp","Pp","Fr")
non_forest <- c("Bt","Bu","G","Hr","R","S","W","Ws","Po")
crop       <- c("Hc")
rubber     <- c("Rp")

################## FOREST STABLE
df1[df1$class_t1 %in% nat_forest & df1$class_t2 %in% nat_forest,]$final <- 1

################## STABLE NON FOREST
df1[df1$class_t1 %in% c(non_forest,crop,rubber) & df1$class_t2 %in% c(non_forest,crop,rubber),]$final <- 2

################## DEFORESTATION - other
df1[df1$class_t1 %in% nat_forest & df1$class_t2 %in% non_forest,]$final <- 3
df1[df1$class_t1 %in% sec_forest & df1$class_t2 %in% non_forest,]$final <- 3

################## DEFORESTATION - crops
df1[df1$class_t1 %in% nat_forest & df1$class_t2 %in% crop,]$final <- 4
df1[df1$class_t1 %in% sec_forest & df1$class_t2 %in% crop,]$final <- 4

################## DEFORESTATION - rubber
df1[df1$class_t1 %in% nat_forest & df1$class_t2 %in% rubber,]$final <- 5
df1[df1$class_t1 %in% sec_forest & df1$class_t2 %in% rubber,]$final <- 5

################## FOREST DEGRADATION
df1[df1$class_t1 %in% nat_forest & df1$class_t2 %in% sec_forest,]$final <- 6

################## ENHANCEMENT
df1[df1$class_t1 %in% sec_forest & df1$class_t2 %in% nat_forest,]$final <- 7
df1[df1$class_t1 %in% c(non_forest,crop,rubber) & df1$class_t2 %in% nat_forest,]$final <- 7
df1[df1$class_t1 %in% c(non_forest,crop,rubber) & df1$class_t2 %in% sec_forest,]$final <- 7

################## MANAGEMENT
df1[df1$class_t1 %in% sec_forest & df1$class_t2 %in% sec_forest,]$final <- 8

df1[df1$final == 0,c("class_t1","class_t2")]
table(df1$final)

write.table(df1[,c("chg_code","final")],"reclass.txt",sep = " ",row.names = F,col.names = F)
write.csv(df1,"all_transitions.csv",row.names = F)


#################### RECLASSIFY THE CHANGE RASTER
system(sprintf("(echo %s; echo 1; echo 1; echo 2; echo 0) | oft-reclass -oi  %s  %s",
               "reclass.txt",
               "tmp_final_chge_t1_t2.tif",
               "change_t1t2.tif"
))

#################### SIEVE RESULTS
system(sprintf("gdal_sieve.py -st %s %s %s",
               3,
               "tmp_final_chge_t1_t2.tif",
               "tmp_sieve_final_chge_t1_t2.tif"))

####################  CREATE A PSEUDO COLOR TABLE
cols <- col2rgb(c("darkgreen","grey","yellow","pink","red","orange","lightgreen","blue"))

pct <- data.frame(cbind(c(1:8),
                        cols[1,],
                        cols[2,],
                        cols[3,]
))
pct
write.table(pct,paste0("color_table.txt"),row.names = F,col.names = F,quote = F)


################################################################################
## Add pseudo color table to result
system(sprintf("(echo %s) | oft-addpct.py %s %s",
               "color_table.txt",
               "tmp_sieve_final_chge_t1_t2.tif",
               "tmp_pct_sieve_final_chge_t1_t2.tif"
))

system(sprintf("gdal_translate -ot byte -co COMPRESS=LZW %s %s",
               "tmp_pct_sieve_final_chge_t1_t2.tif",
               "final_change_t1_t2.tif"))

#################### DELETE TEMP FILES
system(sprintf(paste0("rm tmp*.tif")))



