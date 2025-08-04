
#this script is needed to produce two nice datasets needed for kriging
tasmin = read.table('big_df_50_cerealia_tasmin_corr_fact_add_sites.txt')
tasmax = read.table('big_df_50_cerealia_tasmax_corr_fact_add_sites.txt')
pr = read.table('big_df_50_cerealia_pr_corr_fact_add_sites.txt')
bio_df = read.table('big_df_50_cerealia_corr_fact_add_sites.txt')

aber = tasmin[tasmin$Label == 'ABER',]
duplicates = tasmin[duplicated(tasmin),]

#we creates two datasets first, one with bios and one with monthly data

bio_df$Pollen.sum = NULL

#bios
bios = unique(bio_df$variable)

column_df = bio_df[bio_df$variable == bios[1],]

names(column_df)
names(column_df)[which(names(column_df) == 'value')] = 'bio01'
column_df = column_df[,-which(names(column_df) == 'variable')]
i = bios[2]

store_column = list()
count = 1
for (i in bios[-1]){
  print(i)
  select_bio = bio_df[bio_df$variable == i,]
  names(select_bio)[which(names(select_bio) == 'value')] = i
  select_bio = select_bio[,-which(names(select_bio) == 'variable')]
  #column_df = merge(column_df, select_bio)
  store_column[[count]] = data.frame(select_bio[,c(i)])
  count = count+1
  gc()
}
store_column = do.call(cbind, store_column)
names(store_column) = c(bios[-1])
column_df = cbind(column_df, store_column)
bios_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]

#monthly data - precipitation
months = c('P_Jan', 'P_Feb', 'P_Mar', 'P_Apr', 'P_May', 'P_Jun', 'P_Jul',
           'P_Aug', 'P_Sep', 'P_Oct', 'P_Nov', 'P_Dec')


column_df = pr[pr$month == 1,]
column_df$month = NULL

names(column_df)
names(column_df)[which(names(column_df) == 'value')] = months[1]
column_df = column_df[,-which(names(column_df) == 'variable')]

store_column = list()
count = 1

for (i in 2:12){
  print(i)
  select_bio = pr[pr$month == i,]
  names(select_bio)[which(names(select_bio) == 'value')] =  months[i]
  select_bio = select_bio[,-which(names(select_bio) == 'variable')]
  select_bio$month = NULL
  #column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'Label', 'lon','lat', 'Abies_perc'))
  store_column[[count]] = data.frame(select_bio[,months[i]])
  count = count+1
  gc()
}
store_column = do.call(cbind, store_column)
names(store_column) = c(months[-1])
column_df = cbind(column_df, store_column)

pr_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]

#monthly data - tas min
months = c('Tasmin_Jan', 'Tasmin_Feb', 'Tasmin_Mar', 'Tasmin_Apr', 'Tasmin_May', 'Tasmin_Jun', 'Tasmin_Jul',
           'Tasmin_Aug', 'Tasmin_Sep', 'Tasmin_Oct', 'Tasmin_Nov', 'Tasmin_Dec')


column_df = tasmin[tasmin$month == 1,]
column_df$month = NULL

names(column_df)
names(column_df)[which(names(column_df) == 'value')] = months[1]
column_df = column_df[,-which(names(column_df) == 'variable')]

store_column = list()
count = 1

for (i in 2:12){
  print(i)
  select_bio = tasmin[tasmin$month == i,]
  names(select_bio)[which(names(select_bio) == 'value')] =  months[i]
  select_bio = select_bio[,-which(names(select_bio) == 'variable')]
  select_bio$month = NULL
  #column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'Label', 'lon','lat', 'Abies_perc'))
  store_column[[count]] = data.frame(select_bio[,months[i]])
  count = count+1
  gc()
}
store_column = do.call(cbind, store_column)
names(store_column) = c(months[-1])
column_df = cbind(column_df, store_column)

tasmin_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]


#monthly data - tas max
months = c('Tasmax_Jan', 'Tasmax_Feb', 'Tasmax_Mar', 'Tasmax_Apr', 'Tasmax_May', 'Tasmax_Jun', 'Tasmax_Jul',
           'Tasmax_Aug', 'Tasmax_Sep', 'Tasmax_Oct', 'Tasmax_Nov', 'Tasmax_Dec')


column_df = tasmax[tasmax$month == 1,]
column_df$month = NULL

names(column_df)
names(column_df)[which(names(column_df) == 'value')] = months[1]
column_df = column_df[,-which(names(column_df) == 'variable')]

store_column = list()
count = 1

for (i in 2:12){
  print(i)
  select_bio = tasmax[tasmax$month == i,]
  names(select_bio)[which(names(select_bio) == 'value')] =  months[i]
  select_bio = select_bio[,-which(names(select_bio) == 'variable')]
  select_bio$month = NULL
  #column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'Label', 'lon','lat', 'Abies_perc'))
  store_column[[count]] = data.frame(select_bio[,months[i]])
  count = count+1
  gc()
}
store_column = do.call(cbind, store_column)
names(store_column) = c(months[-1])
column_df = cbind(column_df, store_column)

tasmax_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]

#now we merge all together 
df_sites = merge(pr_df, tasmax_df, by = c('CalYrBP', 'cell_id', 'Label', 'lon','lat', 'Abies_perc'))
df_sites = merge(df_sites, tasmin_df, by = c('CalYrBP', 'cell_id', 'Label', 'lon','lat', 'Abies_perc'))
df_sites = merge(df_sites, bios_df, by = c('CalYrBP', 'cell_id', 'Label','lon','lat', 'Abies_perc'))

table(bios_df$CalYrBP)
df_sites = df_sites[!duplicated(df_sites),]


#now we do the average
df_sites$Tas_Jan = (df_sites$Tasmin_Jan + df_sites$Tasmax_Jan)/2
df_sites$Tas_Feb = (df_sites$Tasmin_Feb + df_sites$Tasmax_Feb)/2
df_sites$Tas_Mar = (df_sites$Tasmin_Mar + df_sites$Tasmax_Mar)/2
df_sites$Tas_Apr = (df_sites$Tasmin_Apr + df_sites$Tasmax_Apr)/2
df_sites$Tas_May = (df_sites$Tasmin_May + df_sites$Tasmax_May)/2
df_sites$Tas_Jun = (df_sites$Tasmin_Jun + df_sites$Tasmax_Jun)/2
df_sites$Tas_Jul = (df_sites$Tasmin_Jul + df_sites$Tasmax_Jul)/2
df_sites$Tas_Aug = (df_sites$Tasmin_Aug + df_sites$Tasmax_Aug)/2
df_sites$Tas_Sep = (df_sites$Tasmin_Sep + df_sites$Tasmax_Sep)/2
df_sites$Tas_Oct = (df_sites$Tasmin_Oct + df_sites$Tasmax_Oct)/2
df_sites$Tas_Nov = (df_sites$Tasmin_Nov + df_sites$Tasmax_Nov)/2
df_sites$Tas_Dec = (df_sites$Tasmin_Dec + df_sites$Tasmax_Dec)/2

#now we transform back the two monthly temperatures
df_sites$Tas_Jan = ((df_sites$Tas_Jan)/10 - 273)
df_sites$Tas_Feb = ((df_sites$Tas_Feb)/10 - 273)
df_sites$Tas_Mar = ((df_sites$Tas_Mar)/10 - 273)
df_sites$Tas_Apr = ((df_sites$Tas_Apr)/10 - 273)
df_sites$Tas_May = ((df_sites$Tas_May)/10 - 273)
df_sites$Tas_Jun = ((df_sites$Tas_Jun)/10 - 273)
df_sites$Tas_Jul = ((df_sites$Tas_Jul)/10 - 273)
df_sites$Tas_Aug = ((df_sites$Tas_Aug)/10 - 273)
df_sites$Tas_Sep = ((df_sites$Tas_Sep)/10 - 273)
df_sites$Tas_Oct = ((df_sites$Tas_Oct)/10 - 273)
df_sites$Tas_Nov = ((df_sites$Tas_Nov)/10 - 273)
df_sites$Tas_Dec = ((df_sites$Tas_Dec)/10 - 273)


df_sites$Tasmin_Jan = ((df_sites$Tasmin_Jan)/10 - 273)
df_sites$Tasmin_Feb = ((df_sites$Tasmin_Feb)/10 - 273)
df_sites$Tasmin_Mar = ((df_sites$Tasmin_Mar)/10 - 273)
df_sites$Tasmin_Apr = ((df_sites$Tasmin_Apr)/10 - 273)
df_sites$Tasmin_May = ((df_sites$Tasmin_May)/10 - 273)
df_sites$Tasmin_Jun = ((df_sites$Tasmin_Jun)/10 - 273)
df_sites$Tasmin_Jul = ((df_sites$Tasmin_Jul)/10 - 273)
df_sites$Tasmin_Aug = ((df_sites$Tasmin_Aug)/10 - 273)
df_sites$Tasmin_Sep = ((df_sites$Tasmin_Sep)/10 - 273)
df_sites$Tasmin_Oct = ((df_sites$Tasmin_Oct)/10 - 273)
df_sites$Tasmin_Nov = ((df_sites$Tasmin_Nov)/10 - 273)
df_sites$Tasmin_Dec = ((df_sites$Tasmin_Dec)/10 - 273)


df_sites$Tasmax_Jan = ((df_sites$Tasmax_Jan)/10 - 273)
df_sites$Tasmax_Feb = ((df_sites$Tasmax_Feb)/10 - 273)
df_sites$Tasmax_Mar = ((df_sites$Tasmax_Mar)/10 - 273)
df_sites$Tasmax_Apr = ((df_sites$Tasmax_Apr)/10 - 273)
df_sites$Tasmax_May = ((df_sites$Tasmax_May)/10 - 273)
df_sites$Tasmax_Jun = ((df_sites$Tasmax_Jun)/10 - 273)
df_sites$Tasmax_Jul = ((df_sites$Tasmax_Jul)/10 - 273)
df_sites$Tasmax_Aug = ((df_sites$Tasmax_Aug)/10 - 273)
df_sites$Tasmax_Sep = ((df_sites$Tasmax_Sep)/10 - 273)
df_sites$Tasmax_Oct = ((df_sites$Tasmax_Oct)/10 - 273)
df_sites$Tasmax_Nov = ((df_sites$Tasmax_Nov)/10 - 273)
df_sites$Tasmax_Dec = ((df_sites$Tasmax_Dec)/10 - 273)

#summer moisture availability
df_sites$summer_moist = (df_sites$P_Jun + df_sites$P_Jul + df_sites$P_Aug)

#minimum winter temperature
store_min = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tasmin_Jan', 'Tasmin_Feb', 'Tasmin_Mar', 'Tasmin_Apr', 'Tasmin_May', 'Tasmin_Jun', 'Tasmin_Jul',
                                    'Tasmin_Aug', 'Tasmin_Sep', 'Tasmin_Oct', 'Tasmin_Nov', 'Tasmin_Dec')]
  which_is_min = min(take_precipitations)
  store_min[[i]] = which_is_min 
}
store_min = do.call(rbind, store_min)

df_sites$Winter_tasmin = store_min


#coldest mean
store_coldest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tas_Jan', 'Tas_Feb', 'Tas_Mar', 'Tas_Apr', 'Tas_May', 'Tas_Jun', 'Tas_Jul',
                                    'Tas_Aug', 'Tas_Sep', 'Tas_Oct', 'Tas_Nov', 'Tas_Dec')]
  which_is_min = min(take_precipitations)
  store_coldest_mean[[i]] = which_is_min 
}
store_coldest_mean = do.call(rbind, store_coldest_mean)

df_sites$Coldest_mean = store_coldest_mean

#warmest mean
store_warmest_mean = list()
for(i in 1:nrow(df_sites)){
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tas_Jan', 'Tas_Feb', 'Tas_Mar', 'Tas_Apr', 'Tas_May', 'Tas_Jun', 'Tas_Jul',
                                    'Tas_Aug', 'Tas_Sep', 'Tas_Oct', 'Tas_Nov', 'Tas_Dec')]
  which_is_max = max(take_precipitations)
  store_warmest_mean[[i]] = which_is_max
}
store_warmest_mean = do.call(rbind, store_warmest_mean)

df_sites$Warmest_mean = store_warmest_mean

#month range
df_sites$month_range = df_sites$Warmest_mean - df_sites$Coldest_mean

#add Eu-4Trees variables
library(dplyr)
df_sites = df_sites %>%
  rowwise() %>%
  mutate(Winter_temperature = base::mean(Tas_Jan, Tas_Feb, Tas_Dec, na.rm = T ), 
         Summer_temperature = base::mean(Tas_Jun, Tas_Jul, Tas_Aug, na.rm = T),
         Winter_precipitation = base::sum(P_Jan, P_Feb, P_Dec, na.rm = T), 
         Summer_precipitation = base::sum(P_Jun, P_Jul, P_Aug, na.rm = T),
         Mean_coldest_month = base::min(Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec, Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, na.rm = T),
         Mean_warmest_month = base::max(Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec, Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, na.rm = T),
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
  if(is.na(take_row$P_Jan)){
    store_GDD[i] = NA
  }else{
    take_mean_temperatures = take_row %>% select(Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec )
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
write.table(df_sites, 'df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt')

# now we do the same for the covariable dataset 
tasmin = read.table('total_df_50_cerealia_tasmin_corr_fact_add_sites.txt')
tasmax = read.table('total_df_50_cerealia_tasmax_corr_fact_add_sites.txt')
pr = read.table('total_df_50_cerealia_pr_corr_fact_add_sites.txt')
bio_df = read.table('total_df_50_cerealia_corr_fact_add_sites.txt')

bios = unique(bio_df$variable)
covariables_df = bio_df[bio_df$variable == bios[1],]
names(covariables_df)[1] = bios[1]
covariables_df = covariables_df[,-3]
i = bios[2]
for (i in bios[-1]){
  print(i)
  select_bio = bio_df[bio_df$variable == i,]
  names(select_bio)[1] = i
  select_bio = select_bio[,-3]
  covariables_df = merge(covariables_df, select_bio, by = c('CalYrBP', 'cell_id', 'lon', 'lat'))
  gc()
}
covariables_df <- covariables_df[order(covariables_df$cell_id,covariables_df$CalYrBP ),]
bios_df =covariables_df


#monthly data - precipitation
months = c('P_Jan', 'P_Feb', 'P_Mar', 'P_Apr', 'P_May', 'P_Jun', 'P_Jul',
           'P_Aug', 'P_Sep', 'P_Oct', 'P_Nov', 'P_Dec')

column_df = pr[pr$month == 1,]
column_df = column_df[!duplicated(column_df),]
column_df$month = NULL

names(column_df)
names(column_df)[1] = months[1]
column_df = column_df[,-3]

for (i in 2:12){
  print(i)
  select_bio = pr[pr$month == i,]
  names(select_bio)[1] =  months[i]
  select_bio = select_bio[,-3]
  select_bio$month = NULL
  column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'lon','lat'))
  gc()
}
pr_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]

#monthly data - tas min
months = c('Tasmin_Jan', 'Tasmin_Feb', 'Tasmin_Mar', 'Tasmin_Apr', 'Tasmin_May', 'Tasmin_Jun', 'Tasmin_Jul',
           'Tasmin_Aug', 'Tasmin_Sep', 'Tasmin_Oct', 'Tasmin_Nov', 'Tasmin_Dec')

column_df = tasmin[tasmin$month == 1,]
column_df = column_df[!duplicated(column_df),]
column_df$month = NULL

names(column_df)
names(column_df)[1] = months[1]
column_df = column_df[,-3]

for (i in 2:12){
  print(i)
  select_bio = tasmin[tasmin$month == i,]
  names(select_bio)[1] =  months[i]
  select_bio = select_bio[,-3]
  select_bio$month = NULL
  column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'lon','lat'))
  gc()
}
tasmin_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]


#monthly data - tas max
months = c('Tasmax_Jan', 'Tasmax_Feb', 'Tasmax_Mar', 'Tasmax_Apr', 'Tasmax_May', 'Tasmax_Jun', 'Tasmax_Jul',
           'Tasmax_Aug', 'Tasmax_Sep', 'Tasmax_Oct', 'Tasmax_Nov', 'Tasmax_Dec')

column_df = tasmax[tasmax$month == 1,]
column_df = column_df[!duplicated(column_df),]
column_df$month = NULL

names(column_df)
names(column_df)[1] = months[1]
column_df = column_df[,-3]

for (i in 2:12){
  print(i)
  select_bio = tasmax[tasmax$month == i,]
  names(select_bio)[1] =  months[i]
  select_bio = select_bio[,-3]
  select_bio$month = NULL
  column_df = merge(column_df, select_bio, by = c('CalYrBP', 'cell_id', 'lon','lat'))
  gc()
}
tasmax_df <- column_df[order(column_df$CalYrBP, column_df$cell_id),]


#now we merge all together 
df_sites = merge(pr_df, tasmax_df, by = c('CalYrBP', 'cell_id', 'lon','lat'))
df_sites = merge(df_sites, tasmin_df, by = c('CalYrBP', 'cell_id', 'lon','lat'))
df_sites = merge(df_sites, bios_df, by = c('CalYrBP', 'cell_id', 'lon','lat'))


#now we do the average
df_sites$Tas_Jan = (df_sites$Tasmin_Jan + df_sites$Tasmax_Jan)/2
df_sites$Tas_Feb = (df_sites$Tasmin_Feb + df_sites$Tasmax_Feb)/2
df_sites$Tas_Mar = (df_sites$Tasmin_Mar + df_sites$Tasmax_Mar)/2
df_sites$Tas_Apr = (df_sites$Tasmin_Apr + df_sites$Tasmax_Apr)/2
df_sites$Tas_May = (df_sites$Tasmin_May + df_sites$Tasmax_May)/2
df_sites$Tas_Jun = (df_sites$Tasmin_Jun + df_sites$Tasmax_Jun)/2
df_sites$Tas_Jul = (df_sites$Tasmin_Jul + df_sites$Tasmax_Jul)/2
df_sites$Tas_Aug = (df_sites$Tasmin_Aug + df_sites$Tasmax_Aug)/2
df_sites$Tas_Sep = (df_sites$Tasmin_Sep + df_sites$Tasmax_Sep)/2
df_sites$Tas_Oct = (df_sites$Tasmin_Oct + df_sites$Tasmax_Oct)/2
df_sites$Tas_Nov = (df_sites$Tasmin_Nov + df_sites$Tasmax_Nov)/2
df_sites$Tas_Dec = (df_sites$Tasmin_Dec + df_sites$Tasmax_Dec)/2

#now we transform back the two monthly temperatures
df_sites$Tas_Jan = ((df_sites$Tas_Jan)/10 - 273)
df_sites$Tas_Feb = ((df_sites$Tas_Feb)/10 - 273)
df_sites$Tas_Mar = ((df_sites$Tas_Mar)/10 - 273)
df_sites$Tas_Apr = ((df_sites$Tas_Apr)/10 - 273)
df_sites$Tas_May = ((df_sites$Tas_May)/10 - 273)
df_sites$Tas_Jun = ((df_sites$Tas_Jun)/10 - 273)
df_sites$Tas_Jul = ((df_sites$Tas_Jul)/10 - 273)
df_sites$Tas_Aug = ((df_sites$Tas_Aug)/10 - 273)
df_sites$Tas_Sep = ((df_sites$Tas_Sep)/10 - 273)
df_sites$Tas_Oct = ((df_sites$Tas_Oct)/10 - 273)
df_sites$Tas_Nov = ((df_sites$Tas_Nov)/10 - 273)
df_sites$Tas_Dec = ((df_sites$Tas_Dec)/10 - 273)


df_sites$Tasmin_Jan = ((df_sites$Tasmin_Jan)/10 - 273)
df_sites$Tasmin_Feb = ((df_sites$Tasmin_Feb)/10 - 273)
df_sites$Tasmin_Mar = ((df_sites$Tasmin_Mar)/10 - 273)
df_sites$Tasmin_Apr = ((df_sites$Tasmin_Apr)/10 - 273)
df_sites$Tasmin_May = ((df_sites$Tasmin_May)/10 - 273)
df_sites$Tasmin_Jun = ((df_sites$Tasmin_Jun)/10 - 273)
df_sites$Tasmin_Jul = ((df_sites$Tasmin_Jul)/10 - 273)
df_sites$Tasmin_Aug = ((df_sites$Tasmin_Aug)/10 - 273)
df_sites$Tasmin_Sep = ((df_sites$Tasmin_Sep)/10 - 273)
df_sites$Tasmin_Oct = ((df_sites$Tasmin_Oct)/10 - 273)
df_sites$Tasmin_Nov = ((df_sites$Tasmin_Nov)/10 - 273)
df_sites$Tasmin_Dec = ((df_sites$Tasmin_Dec)/10 - 273)


df_sites$Tasmax_Jan = ((df_sites$Tasmax_Jan)/10 - 273)
df_sites$Tasmax_Feb = ((df_sites$Tasmax_Feb)/10 - 273)
df_sites$Tasmax_Mar = ((df_sites$Tasmax_Mar)/10 - 273)
df_sites$Tasmax_Apr = ((df_sites$Tasmax_Apr)/10 - 273)
df_sites$Tasmax_May = ((df_sites$Tasmax_May)/10 - 273)
df_sites$Tasmax_Jun = ((df_sites$Tasmax_Jun)/10 - 273)
df_sites$Tasmax_Jul = ((df_sites$Tasmax_Jul)/10 - 273)
df_sites$Tasmax_Aug = ((df_sites$Tasmax_Aug)/10 - 273)
df_sites$Tasmax_Sep = ((df_sites$Tasmax_Sep)/10 - 273)
df_sites$Tasmax_Oct = ((df_sites$Tasmax_Oct)/10 - 273)
df_sites$Tasmax_Nov = ((df_sites$Tasmax_Nov)/10 - 273)
df_sites$Tasmax_Dec = ((df_sites$Tasmax_Dec)/10 - 273)

#adjust 
df_sites$Tas_Jan[df_sites$Tas_Jan == -372.9] = -999
df_sites$Tas_Feb[df_sites$Tas_Feb == -372.9] = -999
df_sites$Tas_Mar[df_sites$Tas_Mar == -372.9] = -999
df_sites$Tas_Apr[df_sites$Tas_Apr == -372.9] = -999
df_sites$Tas_May[df_sites$Tas_May == -372.9] = -999
df_sites$Tas_Jun[df_sites$Tas_Jun == -372.9] = -999
df_sites$Tas_Jul[df_sites$Tas_Jul == -372.9] = -999
df_sites$Tas_Aug[df_sites$Tas_Aug == -372.9] = -999
df_sites$Tas_Sep[df_sites$Tas_Sep == -372.9] = -999
df_sites$Tas_Oct[df_sites$Tas_Oct == -372.9] = -999
df_sites$Tas_Nov[df_sites$Tas_Nov == -372.9] = -999
df_sites$Tas_Dec[df_sites$Tas_Dec == -372.9] = -999


df_sites$Tasmin_Jan[df_sites$Tasmin_Jan == -372.9] = -999
df_sites$Tasmin_Feb[df_sites$Tasmin_Feb == -372.9] = -999
df_sites$Tasmin_Mar[df_sites$Tasmin_Mar == -372.9] = -999
df_sites$Tasmin_Apr[df_sites$Tasmin_Apr == -372.9] = -999
df_sites$Tasmin_May[df_sites$Tasmin_May == -372.9] = -999
df_sites$Tasmin_Jun[df_sites$Tasmin_Jun == -372.9] = -999
df_sites$Tasmin_Jul[df_sites$Tasmin_Jul == -372.9] = -999
df_sites$Tasmin_Aug[df_sites$Tasmin_Aug == -372.9] = -999
df_sites$Tasmin_Sep[df_sites$Tasmin_Sep == -372.9] = -999
df_sites$Tasmin_Oct[df_sites$Tasmin_Oct == -372.9] = -999
df_sites$Tasmin_Nov[df_sites$Tasmin_Nov == -372.9] = -999
df_sites$Tasmin_Dec[df_sites$Tasmin_Dec == -372.9] = -999


df_sites$Tasmax_Jan[df_sites$Tasmax_Jan == -372.9] = -999
df_sites$Tasmax_Feb[df_sites$Tasmax_Feb == -372.9] = -999
df_sites$Tasmax_Mar[df_sites$Tasmax_Mar == -372.9] = -999
df_sites$Tasmax_Apr[df_sites$Tasmax_Apr == -372.9] = -999
df_sites$Tasmax_May[df_sites$Tasmax_May == -372.9] = -999
df_sites$Tasmax_Jun[df_sites$Tasmax_Jun == -372.9] = -999
df_sites$Tasmax_Jul[df_sites$Tasmax_Jul == -372.9] = -999
df_sites$Tasmax_Aug[df_sites$Tasmax_Aug == -372.9] = -999
df_sites$Tasmax_Sep[df_sites$Tasmax_Sep == -372.9] = -999
df_sites$Tasmax_Oct[df_sites$Tasmax_Oct == -372.9] = -999
df_sites$Tasmax_Nov[df_sites$Tasmax_Nov == -372.9] = -999
df_sites$Tasmax_Dec[df_sites$Tasmax_Dec == -372.9] = -999

df_sites[1,]
#summer moisture availability
df_sites$summer_moist = (df_sites$P_Jun + df_sites$P_Jul + df_sites$P_Aug)

#minimum winter temperature
store_min = list()
for(i in 1:nrow(df_sites)){
  print(i)
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tasmin_Jan', 'Tasmin_Feb', 'Tasmin_Mar', 'Tasmin_Apr', 'Tasmin_May', 'Tasmin_Jun', 'Tasmin_Jul',
                                    'Tasmin_Aug', 'Tasmin_Sep', 'Tasmin_Oct', 'Tasmin_Nov', 'Tasmin_Dec')]
  which_is_min = min(take_precipitations)
  store_min[[i]] = which_is_min 
}
store_min = do.call(rbind, store_min)

df_sites$Winter_tasmin = store_min

#coldest mean
store_coldest_mean = list()
for(i in 1:nrow(df_sites)){
  print(i)
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tas_Jan', 'Tas_Feb', 'Tas_Mar', 'Tas_Apr', 'Tas_May', 'Tas_Jun', 'Tas_Jul',
                                    'Tas_Aug', 'Tas_Sep', 'Tas_Oct', 'Tas_Nov', 'Tas_Dec')]
  which_is_min = min(take_precipitations)
  store_coldest_mean[[i]] = which_is_min 
}
store_coldest_mean = do.call(rbind, store_coldest_mean)

df_sites$Coldest_mean = store_coldest_mean

#warmest mean
store_warmest_mean = list()
for(i in 1:nrow(df_sites)){
  print(i)
  take_row = df_sites[i,]
  take_precipitations = take_row[,c('Tas_Jan', 'Tas_Feb', 'Tas_Mar', 'Tas_Apr', 'Tas_May', 'Tas_Jun', 'Tas_Jul',
                                    'Tas_Aug', 'Tas_Sep', 'Tas_Oct', 'Tas_Nov', 'Tas_Dec')]
  which_is_max = max(take_precipitations)
  store_warmest_mean[[i]] = which_is_max
}
store_warmest_mean = do.call(rbind, store_warmest_mean)

df_sites$Warmest_mean = store_warmest_mean

#month range
df_sites$month_range = df_sites$Warmest_mean - df_sites$Coldest_mean

library(dplyr)
#add EU-4Trees variables
df_sites = df_sites %>%
  rowwise() %>%
  mutate(Winter_temperature = base::mean(Tas_Jan, Tas_Feb, Tas_Dec, na.rm = T ), 
         Summer_temperature = base::mean(Tas_Jun, Tas_Jul, Tas_Aug, na.rm = T),
         Winter_precipitation = base::sum(P_Jan, P_Feb, P_Dec, na.rm = T), 
         Summer_precipitation = base::sum(P_Jun, P_Jul, P_Aug, na.rm = T),
         Mean_coldest_month = base::min(Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec, Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, na.rm = T),
         Mean_warmest_month = base::max(Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec, Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, na.rm = T),
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
  print(i)
  take_row = df_sites[i,]
  if(is.na(take_row$P_Jan)){
    store_GDD[i] = NA
  }else{
    take_mean_temperatures = take_row %>% select(Tas_Jan, Tas_Feb, Tas_Mar, Tas_Apr, Tas_May, Tas_Jun, Tas_Jul, Tas_Aug,Tas_Sep, Tas_Oct, Tas_Nov,Tas_Dec )
    store_GDD[i] = sum(take_mean_temperatures[which(take_mean_temperatures >=5)]*days[which(take_mean_temperatures >=5)])
  }
}
df_sites$GDD = store_GDD

#permafrost (Levavasseur, 2011)
#continuous bio01 <= -8 + Coldest_mean <= -20
#discontinuous  <=-8 bio01 <=-4
names(df_sites)[41] ='bio01'
df_sites$permafrost = 'Absent'
df_sites = df_sites %>% mutate(permafrost = case_when(
  bio01 <= -8 & Coldest_mean <= -20 ~ 'Continous',
  bio01 <= -4 & bio01 >= -8 ~'Discontinous'
))
#save
write.table(df_sites, 'covariables_all_variables_100km_corr_fact_add_sites.txt')

