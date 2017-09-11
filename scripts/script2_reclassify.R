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

df$code14 <- as.numeric(substr(as.character(10000 + df$chg_code),2,3))
df$code16 <- as.numeric(substr(as.character(10000 + df$chg_code),4,5))

#################### READ CODE/CLASS CONVERSION TABLE
code_class <- read.csv("code_class_agg.csv")

df1 <- merge(df,code_class[,c("code","class","ipcc_code","ipcc_class")],by.x="code14",by.y="code",all.x=T)
names(df1) <- c("code14","chg_code" ,"pix_count","code16","class14","ipcc_code_14","ipcc_class_14")

df1 <- merge(df1,code_class[,c("code","class","ipcc_code","ipcc_class")],by.x="code16",by.y="code",all.x=T)
names(df1) <- c("code16","code14","chg_code" ,"pix_count","class14","ipcc_code_14","ipcc_class_14","class16","ipcc_code_16","ipcc_class_16")

df1$ipcc_chge <- df1$ipcc_code_14 *10 + df1$ipcc_code_16

df1$final <- 0

table(df1$ipcc_class_14,df1$ipcc_class_16,useNA = "always")

unique(df1[df1$ipcc_class_16!="F",]$class16)

nat_forest <- c("B","D","E","Ff","Se","M","Mr")
sec_forest <- c("Tp","Pp","Fr")
non_forest <- c("Bt","Bu","G","Hc","Hr","R","S","W","Ws","Rp","Po")

################## FOREST STABLE
df1[df1$class14 %in% nat_forest & df1$class16 %in% nat_forest,]$final <- 1

################## STABLE NON FOREST
df1[df1$class14 %in% non_forest & df1$class16 %in% non_forest,]$final <- 2

################## DEFORESTATION
df1[df1$class14 %in% nat_forest & df1$class16 %in% non_forest,]$final <- 3
df1[df1$class14 %in% sec_forest & df1$class16 %in% non_forest,]$final <- 3

################## FOREST DEGRADATION
df1[df1$class14 %in% nat_forest & df1$class16 %in% sec_forest,]$final <- 4

################## ENHANCEMENT
df1[df1$class14 %in% sec_forest & df1$class16 %in% nat_forest,]$final <- 5
df1[df1$class14 %in% non_forest & df1$class16 %in% nat_forest,]$final <- 5
df1[df1$class14 %in% non_forest & df1$class16 %in% sec_forest,]$final <- 5

################## MANAGEMENT
df1[df1$class14 %in% sec_forest & df1$class16 %in% sec_forest,]$final <- 6


df1[df1$final == 0,c("class14","class16")]
table(df1$final)

write.table(df1[,c("chg_code","final")],"reclass.txt",sep = " ",row.names = F,col.names = F)
write.csv(df1,"all_transitions.csv",row.names = F)


#################### RECLASSIFY THE CHANGE RASTER
system(sprintf("(echo %s; echo 1; echo 1; echo 2; echo 0) | oft-reclass -oi  %s  %s",
               "reclass.txt",
               "tmp_final_chge_1416.tif",
               "change_1416.tif"
))

#################### SIEVE RESULTS
system(sprintf("gdal_sieve.py -st %s %s %s",
               3,
               "tmp_final_chge_1416.tif",
               "tmp_sieve_final_chge_1416.tif"))

#################### COMPRESS RESULTS
system(sprintf("gdal_translate -ot byte -co COMPRESS=LZW %s %s",
               "tmp_sieve_final_chge_1416.tif",
               "final_change_1416.tif"))

#################### DELETE TEMP FILES
system(sprintf(paste0("rm tmp*.tif")))



