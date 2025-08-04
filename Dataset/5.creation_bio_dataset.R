library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


#remove the gridcells 
res = 50 # define the resolution

to_remove = read.table(paste0('Covariates/gridcells_to_be_removed_',res,'.txt')) #those were extracted from the 'glacier extent' and 'sea level' maps available in the CHELSA Trace21k 

#now we use those data to reshape the climate data and set as -999 all the cells to remove 

df = read.table('df_ready_to_krige_vfinal_corr_fact_add_sites.txt', header = T)

years = seq(500,20000, by=500)


#now we logit transform aies values 

df_3 = df
df_3 = df_3[!is.na(df_3$Abies_perc),]
df_zeros = df_3[df_3$Abies_perc ==0,]
df_not_zeros = df_3[df_3$Abies_perc >0,] #we remove the zeros
df_not_zeros = df_not_zeros[!is.na(df_not_zeros$Abies_perc),]
df_not_zeros = df_not_zeros[order(df_not_zeros$Label, df_not_zeros$CalYrBP),] #reorder the data 
min(df_not_zeros$Abies_perc); max(df_not_zeros$Abies_perc)
df_not_zeros$Abies_perc = df_not_zeros$Abies_perc/100 #our original dataset goes from 0 to 100%, here I just ri-dimension it to 0-1
df_not_zeros$Abies_perc = log(df_not_zeros$Abies_perc/(1-df_not_zeros$Abies_perc)) #transformation

df_zeros$Abies_perc = -20
df_3 = rbind(df_not_zeros, df_zeros)

#here you must be sure that it is in the right order!
names(df_3)[which(names(df_3) %in% c('LatDD'))] = c('lat')
names(df_3)[which(names(df_3) %in% c('LonDD'))] = c('lon')

#trasform the coordinates in metric (necessary for geomodels)
df_3 = st_as_sf(df_3, coords = c('lon','lat'), crs = st_crs('WGS84'))
df_3 <- st_transform(df_3, crs = st_crs("epsg:3035"))
df_3$lat = st_coordinates(df_3)[,2]
df_3$lon = st_coordinates(df_3)[,1]
df_3 = data.frame(df_3)

#this script is necessary to extrapolate data from given resolution

#load the reference file 
grid = st_read(paste0("Covariates/polygon_",res,".shp"))
grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84")
grid$CHELSA_Tra = NULL #remove the variables

grid$cell_id = as.numeric(rownames(grid))
plot(grid['cell_id'])

#transform to metric 
grid <- st_transform(grid, crs = st_crs("epsg:3035"))
grid

#find in which gridcell the points fall
df_4 = st_as_sf(df_3, coords = c('lon','lat'), crs ="epsg:3035" )
names(df_4)
df_4 = df_4[,-(which(names(df_4) %in% c('Abies','Name','Pollen.sum','lon','lat')))]

grid_match = st_join(df_4, grid)

#check that grid match worked by looking if they interect 
table(is.na(grid_match$cell_id))

not_intersected = grid_match[is.na(grid_match$cell_id),]


#now we create the dataset of covariables and extract those values for our points, 
#first we load the variables 
files_bio = list.files(path = 'Covariates/Covariates.percentages/Covariates', 
                       recursive = T, pattern = paste0(res,'res.txt'))
files_bio
files_bio = files_bio[c(1:19)] #take care of taking only the past!
big_df = list()
total_df = list()
i = 1
j = 500

pb = txtProgressBar(min = 0, max =29400, initial = 0)

for (i in 1:length(files_bio)){
  bio01 = read.table(paste0('Covariates/Covariates.percentages/Covariates/', files_bio[i]))
  names(bio01) = c('value','lon','lat','file')
  bio01$variable =  substr(bio01$file,start =17, stop = 21 )
  bio01$year =  substr(bio01$file,start =23, stop = 26 )
  bio01 = bio01[,-4]
  unique(bio01$year)
  bio01 = bio01 %>% mutate(year = case_when(
    year=="20_V" ~ 0,  year=="19_V" ~ 100,  year=="18_V" ~ 200,   year== "17_V" ~300,  year=="16_V" ~400,  year== "15_V" ~500,  year== "14_V" ~600,  year== "13_V" ~700,
    year== "12_V" ~800,  year=="11_V" ~900,  year== "10_V" ~1000,  year== "9_V1"~1100,  year== "8_V1"~1200,  year=="7_V1" ~1300,  year=="6_V1" ~1400,  year=="5_V1" ~1500,
    year== "4_V1" ~1600,  year== "3_V1" ~1700,  year== "2_V1"  ~1800,  year== "1_V1" ~1900,  year== "0_V1" ~2000,  year==".1_V" ~2100,  year==".2_V" ~2200,  year== ".3_V"~2300,
    year==".4_V"~2400,  year== ".5_V"~2500,  year==".6_V"~2600,  year== ".7_V"~2700,  year== ".8_V"~2800,  year== ".9_V"~2900,  year== ".10_" ~3000,  year== ".11_"~3100,
    year== ".12_" ~3200,  year== ".13_" ~3300,  year== ".14_"~3400,  year==  ".15_"~ 3500,year==  ".16_" ~3600,  year== ".17_" ~3700,  year==  ".18_"~3800,  year== ".19_" ~3900,
    year==  ".20_" ~4000,  year==  ".21_"~4100,   year== ".22_" ~4200 ,   year==  ".23_" ~4300 ,  year==  ".24_" ~4400 ,  year==".25_"~ 4500  ,  year==  ".26_"~4600  ,
    year==  ".27_" ~4700  ,  year== ".28_" ~4800  ,  year== ".29_"~4900 ,  year==  ".30_"~ 5000,   year==  ".31_" ~ 5100,   year==  ".32_"~ 5200,  year==  ".33_" ~  5300,  
    year==  ".34_" ~5400  ,  year== ".35_" ~5500 ,  year==  ".36_" ~ 5600,   year==  ".37_" ~ 5700 ,  year==  ".38_"~ 5800  ,  year== ".39_"~5900  ,  year== ".40_" ~6000 ,
    year==  ".41_" ~ 6100,    year==  ".42_" ~6200,  year==  ".43_" ~6300 ,  year==  ".44_"~ 6400 ,  year==  ".45_" ~ 6500 ,   year==  ".46_"~6600  ,  year==  ".47_" ~6700  ,
    year==  ".48_" ~6800  ,  year==  ".49_" ~6900 ,  year==  ".50_" ~ 7000 ,  year==  ".51_" ~ 7100  ,  year==  ".52_"~7200 ,  year==  ".53_" ~ 7300,    year==  ".54_"~7400  ,
    year== ".55_"~7500  ,  year==  ".56_"~ 7600 ,  year== ".57_" ~7700 ,  year==  ".58_" ~ 7800,   year== ".59_"~ 7900 ,  year== ".60_" ~ 8000 ,   year==  ".61_"~8100  ,
    year==  ".62_"~8200  ,  year==  ".63_" ~8300  ,  year==  ".64_" ~8400  ,  year==  ".65_" ~8500,  year== ".66_" ~8600 ,  year== ".67_" ~ 8700 ,   year==  ".68_" ~8800  ,
    year==  ".69_" ~8900  ,  year==  ".70_"~9000  ,  year== ".71_" ~9100 ,  year== ".72_" ~ 9200 ,   year==  ".73_" ~9300  ,  year== ".74_" ~9400  ,  year== ".75_" ~9500  ,
    year== ".76_" ~9600  ,  year==  ".77_" ~9700  ,  year== ".78_" ~9800  ,  year== ".79_" ~9900 ,  year== ".80_"~10000 ,  year==  ".81_"~10100 ,  year==  ".82_" ~10200 ,
    year==  ".83_"~10300 ,  year==  ".84_" ~10400 ,  year==  ".85_" ~10500 ,  year==  ".86_" ~10600 ,  year==  ".87_" ~10700 ,  year==  ".88_" ~10800,  year==  ".89_"~10900 ,  year==  ".90_" ~11000 ,
    year==  ".91_" ~11100 ,  year==  ".92_" ~11200 ,  year==  ".93_" ~11300,  year==  ".94_" ~ 11400,   year== ".95_"~11500 ,  year== ".96_" ~11600 ,  year==  ".97_" ~11700,
    year== ".98_" ~ 11800 ,  year==".99_"~11900 ,  year==".100" ~12000 ,  year==  ".101"~12100 ,  year==  ".102"~ 12200,  year==  ".103" ~12300,  year==  ".104" ~12400 ,  year==  ".105" ~12500 ,
    year==  ".106" ~12600 ,  year==  ".107" ~12700 ,  year==  ".108" ~12800,  year==  ".109"  ~12900,   year==  ".110" ~13000 ,  year==  ".111" ~ 13100,  year==  ".112" ~13200 ,
    year== ".113" ~13300 ,  year==  ".114" ~13400 ,  year== ".115" ~13500 ,  year== ".116"~13600,  year== ".117" ~13700,   year==  ".118" ~ 13800,  year== ".119" ~13900,
    year== ".120" ~14000 ,  year== ".121" ~ 14100 ,  year== ".122" ~ 14200 ,  year== ".123" ~14300 ,  year==  ".124" ~14400 ,  year==  ".125" ~14500,  year==  ".126" ~14600 ,
    year==  ".127" ~ 14700 ,  year==  ".128" ~14800 ,  year==  ".129" ~14900,  year==  ".130" ~15000 ,  year==  ".131" ~ 15100 ,  year==  ".132" ~15200 ,  year==  ".133" ~15300 ,
    year==  ".134"~15400,  year==  ".135" ~15500,   year==  ".136" ~15600 ,  year==  ".137" ~15700 ,  year==  ".138" ~15800 ,  year== ".139" ~15900 ,  year==  ".140" ~16000 ,
    year== ".141" ~16100 ,  year== ".142" ~16200,  year== ".143" ~16300,  year== ".144"~ 16400 ,  year==".145"~16500 ,  year== ".146"~ 16600,   year== ".147" ~16700 ,
    year==  ".148" ~16800 ,  year==  ".149" ~16900 ,  year==  ".150" ~17000 ,  year== ".151" ~17100 ,  year== ".152"~17200 ,  year==  ".153"~ 17300,   year== ".154" ~17400 ,
    year==  ".155" ~17500 ,  year== ".156" ~17600 ,  year== ".157" ~17700,  year== ".158" ~17800 ,  year== ".159" ~17900 ,  year==  ".160" ~18000 ,  year==".161" ~18100 ,
    year== ".162" ~18200,  year==".163" ~ 18300 ,  year==".164" ~18400 ,  year== ".165" ~18500 ,  year==  ".166" ~18600,  year==  ".167" ~ 18700,  year==  ".168" ~ 18800,
    year==  ".169" ~ 18900,  year==  ".170"~ 19000 ,  year== ".171" ~19100 ,  year== ".172" ~19200 ,  year== ".173" ~19300 ,  year== ".174"~19400 ,  year==".175"~19500,  year==".176" ~ 19600,   year==".177" ~19700,
    year==".178"~ 19800 ,  year==".179" ~19900 ,  year==".180" ~20000
  ))
  table(bio01$year)
  
  final_bio_data = list()
  n <- vector()
  unique_coords = unique(bio01[,c('lon','lat')])
  for (j in 1:nrow(unique(bio01[,c('lon','lat')]))) {
      take_coords = unique_coords[j,]
      x <- bio01[bio01$lon  == take_coords$lon & bio01$lat  == take_coords$lat,]
      
      time.mid <- seq(250, 19750, 500)
      value.bin <- rep(NA, length(time.mid))
      half.win <- 250
      n <- append(n,values=(time.mid))
      
      for (k in 1:length(time.mid)) {
        age.lo <- time.mid[k] - half.win
        age.hi <- time.mid[k] + half.win
        ageID <- which(x$year >= age.lo & x$year < age.hi)
        value.bin[k] = mean(x$value[ageID], na.rm=TRUE)
      }
      
      final.bin <- as.data.frame(value.bin)
      final.bin$year <- time.mid+250
      final.bin$lat <- rep(x[1,'lat'], length(time.mid))
      final.bin$lon <- rep(x[1,'lon'], length(time.mid))
      final.bin$variable = rep(unique(x$variable), length(time.mid))
      final_bio_data[[j]] =  final.bin
      setTxtProgressBar(pb,j)
  }
  
  final_bio_data = bind_rows(final_bio_data)
  
  bio01 = final_bio_data
  bio01 = st_as_sf(bio01, coords = c('lon','lat'), crs= 'WGS84')
  bio01 <- st_transform(bio01, crs = st_crs("epsg:3035"))
  bio01$lon = st_coordinates(bio01)[,1]
  bio01$lat = st_coordinates(bio01)[,2]
  
  bio_data = list()
  bio_data_2 = list()
  count = 1
  for (f in years){
    to_remove_yr = to_remove[to_remove$i ==f,]
    to_remove_yr = to_remove_yr[!to_remove_yr$cell_id %in% grid_match$cell_id,] 
    new_grid = grid
    year_df = bio01[bio01$year ==f,]
    grid_match_2 = st_join(year_df, new_grid)
    names(grid_match_2)[2] = 'CalYrBP'
    names(grid_match_2)[1] = 'value'
    grid_match_2 = as.data.frame(grid_match_2)
    grid_match_2$value[grid_match_2$cell_id %in% to_remove_yr$cell_id] = -999
    values = merge(as.data.frame(grid_match),grid_match_2, by = c('cell_id', 'CalYrBP'))
    
    bio_data[[count]] =values
    bio_data_2[[count]] = grid_match_2
    count = count+1
    
  }
  bio_data = do.call(rbind, bio_data)
  bio_data_2 = do.call(rbind, bio_data_2)
  big_df[[i]]=  bio_data
  total_df[[i]] = bio_data_2
  print(i)
} #for loop that extract all covariable data

big_df = do.call(rbind, big_df)
total_df = do.call(rbind, total_df)

head(big_df)

big_df$lon = NULL; big_df$lat = NULL
big_df$geometry.y = NULL
big_df = st_as_sf(big_df)
geometries = st_coordinates(big_df)
big_df$lat= geometries[,2]; big_df$lon= geometries[,1]
big_df = data.frame(big_df)
big_df <- big_df[order(big_df$cell_id, big_df$CalYrBP),]
class(big_df); class(total_df)
big_df$geometry.x = NULL
total_df$geometry = NULL
write.table(big_df, paste0('big_df_',res,'_cerealia_corr_fact_add_sites.txt'), col.names = T)
write.table(total_df, paste0('total_df_',res,'_cerealia_corr_fact_add_sites.txt'))

