library(googlesheets)
library(dplyr)
library(tidyr)
library(raster)

##### Get Google Docs master info from web #####
my_sheets <- gs_ls() #Will have to authenticate Google here (in browser, very easy)
core <- gs_title("Datatemplate_CORE_Variables") #get whole document
md <- core %>% gs_read_csv(ws = "Sheet1") #work with individual worksheet

#DONT NEED TO DO THIS FOR DATATEMPLATE FILEcreate unique ID for lakes
# md1<-md_import %>% mutate(lake.id = group_indices_(md_import, .dots=c("Lake.Name", "Country", "Latitude", "Longitude")))
# 
# md<-md1[!duplicated(md1[,c("Lake.Name","lake.id","Country","Area", "Latitude", "Longitude", "Elevation")]),c("Lake.Name","lake.id","Country","Area", "Latitude", "Longitude", "Elevation")]


# Climate comparisons info 
# https://climatedataguide.ucar.edu/climate-data/global-temperature-data-sets-overview-comparison-table

# Global surface temperatures: BEST: Berkeley Earth Surface Temperatures
# Monthly Land Average Temperature (TAVG; 1753 - Recent) (200 Mb)
br = brick('data/Complete_TAVG_LatLong1.nc', varname="temperature") # variables are temperature and climatology 
lakes = md$Lake.ID
lakenames = md$Lake.Name
# Create output dataframe 
dates<-seq.Date(as.Date('1750-01-01'),as.Date('2014-12-01'),by = 'month')

output = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1)

for (m in 1:length(dates)) {
  print(m)
  month = br[[m]] #choose month/year
  month@file@nodatavalue<--999
  vals<-raster::extract(month, SpatialPoints(md[!is.na(md$'Latitude.(dec.deg)'),c("Longitude.(dec.deg)","Latitude.(dec.deg)")], proj4string = CRS(proj4string(lulc_bricks[[1]][[200]]))))
  output[m,1]<-as.character(dates[m])
  output[m,2:(length(vals)+1)]<-t(vals)
}

output<-data.frame(output)
colnames(output)[1]<-"date"
for (i in 2:length(output)){
  colnames(output)[i]<-data.frame(md[!is.na(md$'Latitude.(dec.deg)'),"Lake.ID"])[i-1,]
}

# Write output file 
write.csv(output,'data/coreTemps_Berkeley_9_18.csv',row.names=F)

#####
# Global surface temperature data:  CRUTEM4
# Years of Record: 1850/01 to 2016/07

br1 = brick('data/air.mon.anom.nc', varname="air")
plot(br1[[1]])
#much worse coverage than Berkeley


