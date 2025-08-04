library(dplyr)
library(stringr)

setwd("E:/CHELSA_future/v2")

files <- list.files(pattern = "_50res.txt",
                    recursive = TRUE, full.names = TRUE)

files <- files[c(1:19,43:115)]

folder_names = list.dirs(recursive = F)


### Create today's climate dataframe ###
today_folder = folder_names[1]
for(i in today_folder){
  
  files <- list.files(path = paste0(i,'/'),
                      pattern = "_50res.txt",
                      recursive = F, full.names = T)
  
  this_year_df = data.frame(cell_id = seq(1, 7320), Year =  gsub('./','',i))
  
  #read all datasets
  for(j in files){

    data = read.table(j)
    data = data.table::data.table(data)
    variable = gsub(paste0(i,'/'),'', j)
    variable = gsub('_50res.txt','', variable)
    print(variable)
    names(data)[which(names(data)=='value')] = variable
    data$Year = gsub('./','',i)

    
    if(variable %in% c('tasmin','tasmax','pr','tas')){
      month = gsub(paste0("E:/CHELSA future/1981-2010/",variable,"/CHELSA_",variable,"_"),'',data$run)
      month = gsub(paste0("_",unique(data$Year),"_V.2.1.tif"),'',month)
      
      data$Month = month
      data = data %>% mutate(Month = case_when(
        Month == '01' ~ 'Jan',
        Month == '02' ~ 'Feb',
        Month == '03' ~ 'Mar',
        Month == '04' ~ 'Apr',
        Month == '05' ~ 'May',
        Month == '06' ~ 'Jun',
        Month == '07' ~ 'Jul',
        Month == '08' ~ 'Aug',
        Month == '09' ~ 'Sep',
        Month == '10' ~ 'Oct',
        Month == '11' ~ 'Nov',
        Month == '12' ~ 'Dec'
      ))
      for(m in c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')){
        print(m)
        mothly_data = data[data$Month == m,]
        names(mothly_data)[1] = paste0(variable, '_', m)
        data2 = mothly_data[,-c(3,5)]
        this_year_df = merge(data2,this_year_df, by = c('cell_id','Year'))
      }
    } else{
      data = data[,-3]
      this_year_df = merge(data,this_year_df, by = c('cell_id','Year'))
    }
  }
}

today_data  = this_year_df


### Create future climate dataframe ###
future_folders = folder_names[c(2:4)]

all_data = list()
count_2 = 1
i = future_folders[1]
for(i in future_folders){
  files <- list.files(path = paste0(i,'/'),
                      pattern = "_50res.txt",
                      recursive = F, full.names = T)
  
  for(j in files){
    data = read.table(j)
    data = data.table::data.table(data)
    data$climate_model = str_extract(data$run, "ssp[0-9]{3}")
    
    dir_part <- dirname(data$run)
    variable = unique(basename(dir_part))
    print(variable)
    if(variable %in% c('tasmin','tasmax','pr','tas')){
      month = basename(data$run)
      month <- str_extract(month, "(?<=_)[0-9]{2}(?=_[0-9]{4}_[0-9]{4})")
      
      data$Month = month
      data = data %>% mutate(Month = case_when(
        Month == '01' ~ 'Jan',
        Month == '02' ~ 'Feb',
        Month == '03' ~ 'Mar',
        Month == '04' ~ 'Apr',
        Month == '05' ~ 'May',
        Month == '06' ~ 'Jun',
        Month == '07' ~ 'Jul',
        Month == '08' ~ 'Aug',
        Month == '09' ~ 'Sep',
        Month == '10' ~ 'Oct',
        Month == '11' ~ 'Nov',
        Month == '12' ~ 'Dec'
      ))
      for(o in c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')){
        monthly_data = data[data$Month ==o,]
        print(o)
        data_to_append = list()
        count = 1
        for(m in unique(data$cell_id)){

          take_cell_id = monthly_data[monthly_data$cell_id ==m,]
          collapsed_data = take_cell_id %>%
            group_by(cell_id, climate_model) %>%
            summarise(value = mean(value), .groups = 'drop')
          
          collapsed_data$Year = gsub('./','',i)
          data_to_append[[count]] = collapsed_data
          count = count+1
        }
        data_to_append = do.call(rbind,data_to_append)
        data_to_append$Variable = paste0(variable, '_', o)
        all_data[[count_2]] = data_to_append
        count_2 = count_2+1
      }
      
    }else{
      data_to_append = list()
      count = 1
      for(m in unique(data$cell_id)){
        take_cell_id = data[data$cell_id ==m,]
        collapsed_data = take_cell_id %>%
          group_by(cell_id, climate_model) %>%
          summarise(value = mean(value), .groups = 'drop')
        
        collapsed_data$Year = gsub('./','',i)
        data_to_append[[count]] = collapsed_data
        count = count+1
      }
      data_to_append = do.call(rbind,data_to_append)
      data_to_append$Variable = variable
      all_data[[count_2]] = data_to_append
      count_2 = count_2+1
    }
    
  }
  
}

all_data_2 =  data.table::rbindlist(all_data)


final_126 = data.table::data.table(cell_id = rep(seq(1,7320,1),3), Year = c(rep("2011-2040", 7320), rep("2041-2070", 7320),rep("2071-2100", 7320)))
final_370 = data.table::data.table(cell_id = rep(seq(1,7320,1),3), Year = c(rep("2011-2040", 7320), rep("2041-2070", 7320),rep("2071-2100", 7320)))
final_585 = data.table::data.table(cell_id = rep(seq(1,7320,1),3), Year = c(rep("2011-2040", 7320), rep("2041-2070", 7320),rep("2071-2100", 7320)))

#now we create three datasets
for(i in unique(all_data_2$climate_model)){
  take_data = all_data_2[all_data_2$climate_model == i,]
  
  for(j in unique(take_data$Variable)){
    take_variable = take_data[take_data$Variable == j,]
    names(take_variable)[which(names(take_variable) == 'value')] = j
    take_variable = take_variable[, c('climate_model', 'Variable'):=NULL]
    if(i == "ssp126"){
      final_126 = merge(take_variable, final_126, by = c('cell_id', 'Year'))
    }else if(i == "ssp370"){
      final_370 = merge(take_variable, final_370)
    }else if (i == "ssp585"){
      final_585 = merge(take_variable, final_585)
    }

  }
}

all_data_126 = rbind(today_data, final_126)
all_data_370 = rbind(today_data, final_370)
all_data_585 = rbind(today_data, final_585)

write.table(all_data_126, 'C:/Users/pistone/OneDrive - Eidg. Forschungsanstalt WSL/Working_directory_kriging_new_sites/Covariates/future.126.txt')
write.table(all_data_370, 'C:/Users/pistone/OneDrive - Eidg. Forschungsanstalt WSL/Working_directory_kriging_new_sites/Covariates/future.370.txt')
write.table(all_data_585, 'C:/Users/pistone/OneDrive - Eidg. Forschungsanstalt WSL/Working_directory_kriging_new_sites/Covariates/future.585.txt')
