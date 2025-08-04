######### In this script we predict the future distribution and abundances of Abies by using Spatio-temporal regression kriging ###
library(sf)
library(dplyr)
library(ggplot2)

future.scenarios = c('126','370','585')

for(o in future.scenarios){
  print(o)
  case_future_scenario = o
  finished = F
  
  column_df = read.table('df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt') #Abies percentage already logit transfomed and covariables values at gridcell in which the corresponding site lies
  covariables_df = read.table('covariables_all_variables_100km_corr_fact_add_sites.txt') #covariables values on the whole grid for each time step
    
  column_df = column_df %>% mutate(ind = case_when(
    Abies_perc == -20 ~ 0,
    Abies_perc != -20 ~ 1
  ))
    
  covariables_df = covariables_df[covariables_df$P_Jan == -999,]
    
  grid = st_read(paste0("polygon_50.shp"))
  grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84")
  grid = st_transform(grid, crs= st_crs('epsg:3035'))
  grid$CHELSA_Tra = NULL #remove the variables
  grid$cell_id = as.numeric(rownames(grid))
  points_centers = st_coordinates(st_centroid(grid))
    
  #select years of interest
  year_500 = column_df[column_df$CalYrBP == 500,]
  year_LH = column_df[column_df$CalYrBP %in% seq(4500, 500, -500),]
    
  #remove the sea
  covariable_500 = covariables_df[covariables_df$CalYrBP == 500,]
  covariable_LH = covariables_df[covariables_df$CalYrBP %in% seq(4500, 500, -500),]
    
  sea = covariable_500[covariable_500$P_Jan == -999,]
    
  #load future climate data
  if(o == '126'){
    data_future = read.table("Future/future.126.complete.txt")
  } else if (o == '370'){
    data_future = read.table("Future/future.370.complete.txt")
  } else if (o == '585'){
    data_future = read.table("Future/future.585.complete.txt")
  }
    
  data_future$Year = NULL
    
  #for now we merge the data (today included)
  data_future_2 = data_future %>%
    group_by(cell_id) %>%
    summarise(across(everything(), list(mean)))
    
  names(data_future_2) = gsub('_1', '', names(data_future_2))
    
  data_future_2[data_future_2$cell_id %in% sea$cell_id,c(2:68)] = NA
  
    
  ###### Load kriging results future ######### TO FIX ###########
  load("RelAb_VarSel_STRK_future/kriging_results.RData")
  form_RelAb = formula('Abies_perc ~ prediction+bio09+Summer_temperature+Winter_temperature+bio03')
    
  future_results_rk = interpolated_residuals[41,]
    
    
  #load past indicator kiging 
  load('Binary_component_result_VarSel_IK//IK_results.RData')
  years = seq(20000, 500, by = -500)
  krigings = IK_results
  #manually adjust sume years that did not performed well in IK procedure 
  to_bind = list()
  for(i in 1:40){
    if(i == 1){
      this_year = krigings[[i]]
      this_year$CalYrBP = years[i]
      to_bind[[i]] = this_year
    }else{
      this_year = krigings[[(i-1)]]
      this_year$CalYrBP = years[i]
      to_bind[[i]] = this_year
    }
  }
      
  #create the dataset and some formatting
  IK_results = do.call(rbind, to_bind)
  IK_results$cell_id = rep(seq(1, 7320),40)
  IK_results$x = NULL
  IK_results$y = NULL
  IK_results$geometry = NULL
  IK_results$prediction[IK_results$prediction <0]= 0
  IK_results$prediction[IK_results$prediction >1]= 1
    
  IK_results_500 = IK_results[IK_results$CalYrBP ==500,]
  IK_results_LH = IK_results[IK_results$CalYrBP %in% seq(4500, 500, -500),]
    
  data_future_2$prediction = krigings[[40]]$prediction
      
  year_500 = merge(IK_results_500, year_500)
  year_LH = merge(IK_results_LH, year_LH)

  year_LH = year_LH[!year_LH$Abies_perc == -20,]
    
  #now we load the formula for the RelAb regression
  model.glm = lm(form_RelAb, data = year_LH, na.action = "na.fail")
    
  predicted_lm = predict(model.glm, newdata = data_future_2)
    
  predicted = predicted_lm+future_results_rk
  predicted = (exp(predicted)/(1+exp(predicted)))*100
    
  grid_RK = grid
  grid_RK$predicted = predicted
  grid_RK$predicted_kriging = future_results_rk
  grid_RK$predicted_regression = predicted_lm
    
  grid_RK$predicted_cropped = predicted
    
    
  save(grid_RK, file = paste0("RelAb_VarSel_STRK_future/results_future_",o, '.RData'))
    
  my_breaks = c(0.01, 1, 5, 10, 50 , 100)
    
  g = ggplot(grid_RK)+
      geom_sf(mapping = aes(fill = predicted), color = NA)+
      scale_fill_viridis_c(option = 'H',
                           na.value = NA,
                           breaks = my_breaks, labels = my_breaks, limits = c(0.1,100),
                           oob = scales::squish,
                           trans = "log10",
                           guide = guide_colourbar(barheight = 10, nbin = 7),
                           name = 'Abies %'
      )
    
    ggsave(paste0("RelAb_VarSel_STRK_future/results_future_",o,'.png'),g, unit = 'cm',
           width = 20, height = 20)
    
}

