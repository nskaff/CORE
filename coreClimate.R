library(googlesheets)
library(dplyr)
library(tidyr)
library(raster)

##### Get Google Docs master info from web #####
my_sheets <- gs_ls() #Will have to authenticate Google here (in browser, very easy)
core <- gs_title("Dataset_covariates") #get whole document
md_import <- core %>% gs_read_csv(ws = "Sheet1") #work with individual worksheet
#create unique ID for lakes
md1<-md_import %>% mutate(lake.id = group_indices_(md_import, .dots=c("Lake.Name", "Country", "Latitude", "Longitude")))

md<-md1[!duplicated(md1[,c("Lake.Name","lake.id","Country","Area", "Latitude", "Longitude", "Elevation")]),c("Lake.Name","lake.id","Country","Area", "Latitude", "Longitude", "Elevation")]


# Climate comparisons info 
# https://climatedataguide.ucar.edu/climate-data/global-temperature-data-sets-overview-comparison-table

# Global surface temperatures: BEST: Berkeley Earth Surface Temperatures
# Monthly Land Average Temperature (TAVG; 1753 - Recent) (200 Mb)
br = brick('data/Complete_TAVG_LatLong1.nc', varname="temperature") # variables are temperature and climatology 
lakes = md$lake.id
lakenames = md$Lake.Name
# Create output dataframe 
dates<-seq.Date(as.Date('1750-01-01'),as.Date('2016-01-01'),by = 'month')
output = data.frame(date =dates ,z = NA)

# Pull data monthly from 1750-2016
for (m in 1:length(dates)) {
  print(m)
  month = br[[m]] #choose month/year
  output$z[m] = unlist(month@z)
  
  vals = getValues(month)
  coord <- xyFromCell(month,1:ncell(month))
  month <- cbind(coord,vals)
  
  # Loop through lakes 
  for (i in 1:length(lakes)){
    indx = which(md$lake.id %in% lakes[i])
    lat = md$`Latitude`[indx]
    long = md$`Longitude`[indx]
    
    df = month[which(abs(month[,1] - long) <=0.5 & abs(month[,2] - lat) <=0.5),]
    if (length(df) == 3){
      output[m,i+2] = df[3]
    } else {
      output[m,i+2] = mean(df[,3],na.rm=T)
    }
  }
}
names(output)[-c(1:2)] = paste(lakenames, lakes, sep="_") # add column names of lakes 

# Write output file 
write.csv(output,'data/coreTemps_Berkeley.csv',row.names=F,quote=F)

#####
# Global surface temperature data:  CRUTEM4
# Years of Record: 1850/01 to 2016/07

br1 = brick('data/air.mon.anom.nc', varname="air")
plot(br1[[1]])
#much worse coverage than Berkeley


