library(dplyr)
df_sites = read.table("Covariates/future.126.txt")


#summer moisture availability
df_sites$summer_moist = (df_sites$pr_Jun + df_sites$pr_Jul + df_sites$pr_Aug)

#minimum winter temperature
store_min = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tasmin_Jan', 'tasmin_Feb', 'tasmin_Mar', 'tasmin_Apr', 'tasmin_May', 'tasmin_Jun', 'tasmin_Jul',
                                    'tasmin_Aug', 'tasmin_Sep', 'tasmin_Oct', 'tasmin_Nov', 'tasmin_Dec')]
  which_is_min = min(take_precipitations)
  store_min[[i]] = which_is_min 
}
store_min = do.call(rbind, store_min)

df_sites$Winter_tasmin = store_min


#coldest mean
store_coldest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                    'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_min = min(take_precipitations)
  store_coldest_mean[[i]] = which_is_min 
}
store_coldest_mean = do.call(rbind, store_coldest_mean)

df_sites$Coldest_mean = store_coldest_mean

#warmest mean
store_warmest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                   'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_max = max(take_precipitations)
  store_warmest_mean[[i]] = which_is_max
}
store_warmest_mean = do.call(rbind, store_warmest_mean)

df_sites$Warmest_mean = store_warmest_mean

#month range
df_sites$month_range = df_sites$Warmest_mean - df_sites$Coldest_mean

#finally the distances
#df_sites = merge(df_sites, distances[,c('distance', 'CalYrBP', 'cell_id')], by  = c('cell_id','CalYrBP'))

#add Eu-4Trees variables
library(dplyr)
df_sites = df_sites %>%
  rowwise() %>%
  mutate(Winter_temperature = base::mean(tas_Jan, tas_Feb, tas_Dec, na.rm = T ), 
         Summer_temperature = base::mean(tas_Jun, tas_Jul, tas_Aug, na.rm = T),
         Winter_precipitation = base::sum(pr_Jan, pr_Feb, pr_Dec, na.rm = T), 
         Summer_precipitation = base::sum(pr_Jun, pr_Jul, pr_Aug, na.rm = T),
         Mean_coldest_month = base::min(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         Mean_warmest_month = base::max(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         continentality = Mean_warmest_month-Mean_coldest_month )

df_sites$Winter_temperature[is.nan(df_sites$Winter_temperature)] = NA
df_sites$Summer_temperature[is.nan(df_sites$Summer_temperature)] = NA
df_sites$Winter_precipitation[is.nan(df_sites$Winter_precipitation)] = NA
df_sites$Summer_precipitation[is.nan(df_sites$Summer_precipitation)] = NA
df_sites$Mean_coldest_month[df_sites$Mean_coldest_month == Inf] = NA
df_sites$Mean_warmest_month[df_sites$Mean_warmest_month == Inf] = NA
df_sites$continentality[df_sites$continentality == Inf] = NA

#
#calculate GDD
days = c(31,31,28.25,30,31,30,31,31,30,31,30,31)
store_GDD = c()
for (i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  if(is.na(take_row$pr_Jan)){
    store_GDD[i] = NA
  }else{
    take_mean_temperatures = take_row %>% select(tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec )
    store_GDD[i] = sum(take_mean_temperatures[which(take_mean_temperatures >=5)]*days[which(take_mean_temperatures >=5)])
  }
}
df_sites$GDD = store_GDD

#permafrost (Levavasseur, 2011)
#continuous bio01 <= -8 + Coldest_mean <= -20
#discontinuous  <=-8 bio01 <=-4

df_sites$permafrost = 'Absent'
df_sites = df_sites %>% mutate(permafrost = case_when(
  bio01 <= -8 & Coldest_mean <= -20 ~ 'Continous',
  bio01 <= -4 & bio01 >= -8 ~'Discontinous'
))


#save
write.table(df_sites, "/Covariates/future.126.complete.txt")


################
library(dplyr)
df_sites = read.table("/Covariates/future.370.txt")


#summer moisture availability
df_sites$summer_moist = (df_sites$pr_Jun + df_sites$pr_Jul + df_sites$pr_Aug)

#minimum winter temperature
store_min = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tasmin_Jan', 'tasmin_Feb', 'tasmin_Mar', 'tasmin_Apr', 'tasmin_May', 'tasmin_Jun', 'tasmin_Jul',
                                    'tasmin_Aug', 'tasmin_Sep', 'tasmin_Oct', 'tasmin_Nov', 'tasmin_Dec')]
  which_is_min = min(take_precipitations)
  store_min[[i]] = which_is_min 
}
store_min = do.call(rbind, store_min)

df_sites$Winter_tasmin = store_min


#coldest mean
store_coldest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                    'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_min = min(take_precipitations)
  store_coldest_mean[[i]] = which_is_min 
}
store_coldest_mean = do.call(rbind, store_coldest_mean)

df_sites$Coldest_mean = store_coldest_mean

#warmest mean
store_warmest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                    'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_max = max(take_precipitations)
  store_warmest_mean[[i]] = which_is_max
}
store_warmest_mean = do.call(rbind, store_warmest_mean)

df_sites$Warmest_mean = store_warmest_mean

#month range
df_sites$month_range = df_sites$Warmest_mean - df_sites$Coldest_mean

#finally the distances
#df_sites = merge(df_sites, distances[,c('distance', 'CalYrBP', 'cell_id')], by  = c('cell_id','CalYrBP'))

#add Eu-4Trees variables
library(dplyr)
df_sites = df_sites %>%
  rowwise() %>%
  mutate(Winter_temperature = base::mean(tas_Jan, tas_Feb, tas_Dec, na.rm = T ), 
         Summer_temperature = base::mean(tas_Jun, tas_Jul, tas_Aug, na.rm = T),
         Winter_precipitation = base::sum(pr_Jan, pr_Feb, pr_Dec, na.rm = T), 
         Summer_precipitation = base::sum(pr_Jun, pr_Jul, pr_Aug, na.rm = T),
         Mean_coldest_month = base::min(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         Mean_warmest_month = base::max(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         continentality = Mean_warmest_month-Mean_coldest_month )

df_sites$Winter_temperature[is.nan(df_sites$Winter_temperature)] = NA
df_sites$Summer_temperature[is.nan(df_sites$Summer_temperature)] = NA
df_sites$Winter_precipitation[is.nan(df_sites$Winter_precipitation)] = NA
df_sites$Summer_precipitation[is.nan(df_sites$Summer_precipitation)] = NA
df_sites$Mean_coldest_month[df_sites$Mean_coldest_month == Inf] = NA
df_sites$Mean_warmest_month[df_sites$Mean_warmest_month == Inf] = NA
df_sites$continentality[df_sites$continentality == Inf] = NA

#
#calculate GDD
days = c(31,31,28.25,30,31,30,31,31,30,31,30,31)
store_GDD = c()
for (i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  if(is.na(take_row$pr_Jan)){
    store_GDD[i] = NA
  }else{
    take_mean_temperatures = take_row %>% select(tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec )
    store_GDD[i] = sum(take_mean_temperatures[which(take_mean_temperatures >=5)]*days[which(take_mean_temperatures >=5)])
  }
}
df_sites$GDD = store_GDD

#permafrost (Levavasseur, 2011)
#continuous bio01 <= -8 + Coldest_mean <= -20
#discontinuous  <=-8 bio01 <=-4

df_sites$permafrost = 'Absent'
df_sites = df_sites %>% mutate(permafrost = case_when(
  bio01 <= -8 & Coldest_mean <= -20 ~ 'Continous',
  bio01 <= -4 & bio01 >= -8 ~'Discontinous'
))


#save
write.table(df_sites, "Covariates/future.370.complete.txt")

###############
library(dplyr)
df_sites = read.table("Covariates/future.585.txt")


#summer moisture availability
df_sites$summer_moist = (df_sites$pr_Jun + df_sites$pr_Jul + df_sites$pr_Aug)

#minimum winter temperature
store_min = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tasmin_Jan', 'tasmin_Feb', 'tasmin_Mar', 'tasmin_Apr', 'tasmin_May', 'tasmin_Jun', 'tasmin_Jul',
                                    'tasmin_Aug', 'tasmin_Sep', 'tasmin_Oct', 'tasmin_Nov', 'tasmin_Dec')]
  which_is_min = min(take_precipitations)
  store_min[[i]] = which_is_min 
}
store_min = do.call(rbind, store_min)

df_sites$Winter_tasmin = store_min


#coldest mean
store_coldest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                    'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_min = min(take_precipitations)
  store_coldest_mean[[i]] = which_is_min 
}
store_coldest_mean = do.call(rbind, store_coldest_mean)

df_sites$Coldest_mean = store_coldest_mean

#warmest mean
store_warmest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('tas_Jan', 'tas_Feb', 'tas_Mar', 'tas_Apr', 'tas_May', 'tas_Jun', 'tas_Jul',
                                    'tas_Aug', 'tas_Sep', 'tas_Oct', 'tas_Nov', 'tas_Dec')]
  which_is_max = max(take_precipitations)
  store_warmest_mean[[i]] = which_is_max
}
store_warmest_mean = do.call(rbind, store_warmest_mean)

df_sites$Warmest_mean = store_warmest_mean

#month range
df_sites$month_range = df_sites$Warmest_mean - df_sites$Coldest_mean

#finally the distances
#df_sites = merge(df_sites, distances[,c('distance', 'CalYrBP', 'cell_id')], by  = c('cell_id','CalYrBP'))

#add Eu-4Trees variables
library(dplyr)
df_sites = df_sites %>%
  rowwise() %>%
  mutate(Winter_temperature = base::mean(tas_Jan, tas_Feb, tas_Dec, na.rm = T ), 
         Summer_temperature = base::mean(tas_Jun, tas_Jul, tas_Aug, na.rm = T),
         Winter_precipitation = base::sum(pr_Jan, pr_Feb, pr_Dec, na.rm = T), 
         Summer_precipitation = base::sum(pr_Jun, pr_Jul, pr_Aug, na.rm = T),
         Mean_coldest_month = base::min(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         Mean_warmest_month = base::max(tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec, tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, na.rm = T),
         continentality = Mean_warmest_month-Mean_coldest_month )

df_sites$Winter_temperature[is.nan(df_sites$Winter_temperature)] = NA
df_sites$Summer_temperature[is.nan(df_sites$Summer_temperature)] = NA
df_sites$Winter_precipitation[is.nan(df_sites$Winter_precipitation)] = NA
df_sites$Summer_precipitation[is.nan(df_sites$Summer_precipitation)] = NA
df_sites$Mean_coldest_month[df_sites$Mean_coldest_month == Inf] = NA
df_sites$Mean_warmest_month[df_sites$Mean_warmest_month == Inf] = NA
df_sites$continentality[df_sites$continentality == Inf] = NA

#
#calculate GDD
days = c(31,31,28.25,30,31,30,31,31,30,31,30,31)
store_GDD = c()
for (i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  if(is.na(take_row$pr_Jan)){
    store_GDD[i] = NA
  }else{
    take_mean_temperatures = take_row %>% select(tas_Jan, tas_Feb, tas_Mar, tas_Apr, tas_May, tas_Jun, tas_Jul, tas_Aug,tas_Sep, tas_Oct, tas_Nov,tas_Dec )
    store_GDD[i] = sum(take_mean_temperatures[which(take_mean_temperatures >=5)]*days[which(take_mean_temperatures >=5)])
  }
}
df_sites$GDD = store_GDD

#permafrost (Levavasseur, 2011)
#continuous bio01 <= -8 + Coldest_mean <= -20
#discontinuous  <=-8 bio01 <=-4

df_sites$permafrost = 'Absent'
df_sites = df_sites %>% mutate(permafrost = case_when(
  bio01 <= -8 & Coldest_mean <= -20 ~ 'Continous',
  bio01 <= -4 & bio01 >= -8 ~'Discontinous'
))


#save
write.table(df_sites, "Covariates/future.585.complete.txt")
