library(dplyr)

#calculating static variables for RT analysis
covar_data<-read.csv("data/covariates_data_9_19_17.csv", header=T)


#calculating mean temperature anomoly from 1950-2000
static_data<-covar_data %>% group_by(Lake.ID) %>% summarise(Temp.anomoly=mean(mean_annual_temp_anomaly[year>=1950 & year<=2000], na.rm=T), Percent.urban=mean(X.urban, na.rm=T), Percent.crop=mean(X.total_crop, na.rm=T), Percent.past=mean(X.total_past, na.rm=T))

write.csv(static_data, "data/static_data_9_19_17.csv", row.names=F)
