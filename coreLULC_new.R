library(googlesheets)
library(ncdf4)
library(raster)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)

#Calculating land cover
#there are several different methodologies they used to estimate land-cover. I'm using the one that seems to estimate cropland and pastureland the best. However, I haven't done research into the various types. More info can be found here:http://climate.atmos.uiuc.edu/ISAM_Landuse/land-cover_doc_c20130831.pdf

#data from here: http://climate.atmos.uiuc.edu/ISAM_Landuse/ 
#using 	land-cover_rf_landcover_s1770_e2007_c20130831.tar.gz


##### Get Google Docs master info from web #####
my_sheets <- gs_ls() #Will have to authenticate Google here (in browser, very easy)
core <- gs_title("Datatemplate_CORE_Variables") #get whole document
md <- core %>% gs_read_csv(ws = "Sheet1") 


#importing land use
lulc_files<-list.files("data/RF_AREAVEG", pattern='*.nc', full.names=TRUE)
varnames<-c("C3crop", "C4crop", "C3past", "C4past", "Urban")

lulc_stack<-list()
for (i in 1:length(varnames)){
lulc_stack[[i]]<-stack(lulc_files, varname=varnames[i])

}

lulc_bricks<-list()
for (i in 1:length(varnames)){
lulc_bricks[[i]] = brick(lulc_stack[[i]]) 
}



####calculating this shit based on coordinates rather than polygons

# Create output dataframe 

dates<-seq.Date(as.Date('1770-01-01'),as.Date('2007-01-01'),by = 'year')

output_list<-list(
    output_C3crop = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1),
    output_C4crop = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1),
    output_C3past = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1),
    output_C4past = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1),
    output_Urban = matrix(nrow=length(dates), ncol=nrow(md[!is.na(md$'Latitude.(dec.deg)'),])+1))

  for (j in 1:length(varnames)){
    for (m in 1:length(dates)) {
        print(m)
        #have to get rid of the 0-360 longitude with rotate
          month = rotate(lulc_bricks[[j]][[m]])
          vals<-raster::extract(month, SpatialPoints(md[!is.na(md$'Latitude.(dec.deg)'),c("Longitude.(dec.deg)","Latitude.(dec.deg)")]))
          output_list[[j]][m,1]<-as.character(dates[m])
          output_list[[j]][m,2:(length(vals)+1)] <- t(vals)
          
          }
    }




#calculating value of non-NA pixel nearest to points in order to fill in lakes with missing values
for (j in 1:length(varnames)){
  for (m in 1:length(dates)) {
  
xy <- md[!is.na(md$'Latitude.(dec.deg)'),c("Longitude.(dec.deg)","Latitude.(dec.deg)")]

#enter all the lakes where you want to take the nearest pixel
xy<-xy[c(3,4,105),]

r <- rotate(lulc_bricks[[j]][[m]])

na_vals = apply(X = xy, MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])

#enter all the lakes where you want to take the nearest pixel
# output_list[[j]][,c(3+1, 4+1, 105+1)]<-as.numeric(output_list[[j]][,c(3+1, 4+1, 105+1)])
output_list[[j]][m,c(3+1, 4+1, 105+1)] = t(na_vals)
 print(m) 
}
}



for (j in 1:length(varnames)){
  output_list[[j]]<-data.frame(output_list[[j]])
  for (i in 1:length(vals)){
    colnames(output_list[[j]])[1]<-"date"
    colnames(output_list[[j]])[i+1]<-data.frame(md[!is.na(md$'Latitude.(dec.deg)'),"Lake.ID"])[i,]
    
  }
}



for (j in 1:length(varnames)){
  # Write output file 
  write.csv(output_list[[j]],paste('data/',"core_LULC_",varnames[j],".csv", sep=""),row.names=F,quote=F)
}





# #shapefile based analysis, #not working quite right#
# #import shapefiles and bind them together
# #need to decide if you really want to use mercator projection
# lake_shpfiles<-list.files(path="data/Shapefiles",pattern=".shp$")
# lake_filenames<-sub(".shp","",lake_shpfiles)
# 
# #problem where some of the shapefiles are missing a project. need to define them.
# proj_example<-readOGR(dsn="data/Shapefiles/",layer="Aguasverdes_Spain")
# shape_file_list<-list()
# for(i in 1:length(lake_filenames)){
#   shape_file_list[[i]]<-readOGR(dsn="data/Shapefiles/",layer=lake_filenames[i], p4s = proj4string(proj_example))
#   
# }
# 
# 
# buffers_1000_list<-list()
# buffers_500_list<-list()
# for (i in 1:length(lake_filenames)){
#   #buffering in meters
#   buffer_1000<-gBuffer(shape_file_list[[i]], width=1000, quadsegs=100)
# 
#   buffer_500<-gBuffer(shape_file_list[[i]], width=1000, quadsegs=100)
# 
#   #removing lake area
#   buffers_1000_list[[i]]<-erase(buffer_1000, shape_file_list[[i]])
#   buffers_500_list[[i]]<-erase(buffer_500, shape_file_list[[i]])
# 
# }
# 
# # #merging all the shapefiles together
# # combined_shp<-shape_file_list[[1]]
# # for (i in 2:length(lake_filenames)){
# #   combined_shp<-union(combined_shp,shape_file_list[[i]])
# #   print(i)
# #   print(nrow(combined_shp))
# # }
# # 
# # 
# # #creating buffers around each shapefile and removing the lake itself
# # buffer_1000_full <- gBuffer(combined_shp, width=1000, quadsegs=100,byid=T, id = )
# # buffer_500_full <- gBuffer(combined_shp, width=500, quadsegs=100,byid=T)
# # #
# # #
# # buffer_1000<-erase(buffer_1000_full, combined_shp,byid=T) # using raster:erase
# # buffer_500<-erase(buffer_500_full, combined_shp,byid=T)
# # 
# 
# 
# lakes = md$lake.id
# lakenames = md$Lake.Name
# # Create output dataframe 
# dates<-seq.Date(as.Date('1770-01-01'),as.Date('2007-01-01'),by = 'year')
# 
# 
# output = data.frame(date =dates)
# 
# .rs.unloadPackage("tidyr")
# 
# # Pull data yearly from 1770-2007
# for (i in 1:length(varnames)){
#   for (m in 1:length(dates)) {
#   print(m)
#   year = rotate(lulc_bricks[[i]][[m]])
#   
#   vals_1000 = extract(year, buffers_1000_list[[i]])
#   vals_500= extract(year, buffers_500_list[[i]])
#   print(vals_1000)
#   print(vals_500)
#   #unlist(lapply(vals_500, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
#   
#   
#   #output[m,]
#   }  
# 
# }
# names(output)[-c(1:2)] = paste(lakenames, lakes, sep="_") # add column names of lakes 
# 
# # Write output file 
# write.csv(output,'data/coreTemps_Berkeley.csv',row.names=F,quote=F)
# 
# 
# 
# 
