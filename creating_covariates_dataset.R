#creating a useful dataset for covariates
library(dplyr)
library(lubridate)
library(tidyr)
library(googlesheets)
library(mapview)

#loading in data with z-score
my_sheets <- gs_ls() #Will have to authenticate Google here (in browser, very easy)
core <- gs_title("z_score_data") #get whole document
z_scores <- core %>% gs_read_csv(ws = "Sheet1") #work with individual worksheet

z_scores$Lake.ID<-as.character(z_scores$Lake.ID)


temp_covs<-read.csv('data/coreTemps_Berkeley_9_18.csv', header=T)
temp_covs$date<-as.Date(temp_covs$date)
c3crop<-read.csv("data/core_LULC_C3crop.csv", header=T)
c4crop<-read.csv("data/core_LULC_C4crop.csv", header=T)
c3past<-read.csv("data/core_LULC_C3past.csv", header=T)
c4past<-read.csv("data/core_LULC_C4past.csv", header=T)
urban<-read.csv("data/core_LULC_Urban.csv", header=T)

#taking the mean temperature anomoly by year
temp_covs1<-aggregate(.~year(date),data=temp_covs, FUN=mean, na.action=na.pass)

colnames(temp_covs1)[1]<-"year"
temp_covs2<-temp_covs1[,c(-2)]

#converting temperature to vertical format
temp_covs3<-temp_covs2 %>% gather(key=year)
colnames(temp_covs3)[2:3]<-c("Lake.ID", "mean_annual_temp_anomaly")
temp_covs3$Lake.ID<-gsub("X","",temp_covs3$Lake.ID)

#adding the LULC covariate data
c3crop$year<-year(c3crop$date)
c3crop<-c3crop[-1]
c3crop1<-c3crop %>% gather(key=year)
colnames(c3crop1)[2]<-"Lake.ID"
colnames(c3crop1)[3]<-"%c3crop"
c3crop1$Lake.ID<-gsub("X","",c3crop1$Lake.ID)
covar_data<-full_join(temp_covs3,c3crop1,by=c("Lake.ID", "year") )

c4crop$year<-year(c4crop$date)
c4crop<-c4crop[-1]
c4crop1<-c4crop %>% gather(key=year)
colnames(c4crop1)[2]<-"Lake.ID"
colnames(c4crop1)[3]<-"%c4crop"
c4crop1$Lake.ID<-gsub("X","",c4crop1$Lake.ID)
covar_data<-full_join(covar_data,c4crop1,by=c("Lake.ID", "year") )


c3past$year<-year(c3past$date)
c3past<-c3past[-1]
c3past1<-c3past %>% gather(key=year)
colnames(c3past1)[2]<-"Lake.ID"
colnames(c3past1)[3]<-"%c3past"
c3past1$Lake.ID<-gsub("X","",c3past1$Lake.ID)
covar_data<-full_join(covar_data,c3past1,by=c("Lake.ID", "year") )

c4past$year<-year(c4past$date)
c4past<-c4past[-1]
c4past1<-c4past %>% gather(key=year)
colnames(c4past1)[2]<-"Lake.ID"
colnames(c4past1)[3]<-"%c4past"
c4past1$Lake.ID<-gsub("X","",c4past1$Lake.ID)
covar_data<-full_join(covar_data,c4past1,by=c("Lake.ID", "year") )

urban$year<-year(urban$date)
urban<-urban[-1]
urban1<-urban %>% gather(key=year)
colnames(urban1)[2]<-"Lake.ID"
colnames(urban1)[3]<-"%urban"
urban1$Lake.ID<-gsub("X","",urban1$Lake.ID)
covar_data<-full_join(covar_data,urban1,by=c("Lake.ID", "year") )

covar_data[,"%total_crop"]<-(covar_data$`%c3crop`+covar_data$`%c4crop`)

covar_data[,"%total_past"]<-(covar_data$`%c3past`+covar_data$`%c4past`)


#adding z-score data
covar_data<-full_join(covar_data, z_scores, by=c("year", "Lake.ID"))

#removing years before 1850 and lakes without a Z score marked with X
covar_data1<-covar_data[covar_data$year>=1850 & covar_data$Lake.ID %in% as.character(md$Lake.ID[md$'Z Score'=="X"]),]

#testing to see if all years included
tapply(covar_data1$year, covar_data1$Lake.ID, function(x){length(x)})

write.csv(covar_data1, "data/covariates_data_9_16_17.csv", row.names=F)

