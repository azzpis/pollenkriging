library(dismo)
library(dplyr)
library(ggplot2)
library(raster)
#library(rgdal)
library(terra)
library(sf)
library(sp)


####use this script for past climate and only for non-site values for future. There is another script for that.


 for(m in c(
             'bio01','bio02','bio03','bio04','bio05','bio06','bio07','bio08',
             'bio09',
             'bio10',
             'bio11',
             'bio12',
             'bio13','bio14','bio15','bio16','bio17',
             'bio18','bio19',
             'tas',
             'pr',
             'tasmax',
             'tasmin'
              )){
  grid <- vect("grid_cropped.shp")
  
  
  # #load all the layers
  rastlist <- list.files(path = paste0("E:/CHELSA_future/2071-2100/",m), pattern='.tif', all.files=TRUE, full.names=T)
 
  #extract value from sites
  for (j in c(#10,#25,
              50
              #,75,100
              )){
    df_all_year = data.frame()
    for (i in 1:length(rastlist)){
      print(i)
      rescale = aggregate(rast(rastlist[i]), fact = j)
      lake_env_2 <- crop(rescale, grid)
      lake_env_2 = stars::st_as_stars(lake_env_2)
      lake_env_2 = st_as_sf(lake_env_2)
      lake_env_2$cell_id = seq(1, nrow(lake_env_2),1)
      lake_env_2$run = paste(rastlist[i])
      names(lake_env_2)[1] = c('value') 
      lake_env_2$geometry = NULL
      df_all_year = rbind(df_all_year, lake_env_2)
      
    }
    
    write.table(df_all_year, paste0('E:/CHELSA_future/v2/2071-2100/',m,'_',j,'res.txt'))
  }
  
}

