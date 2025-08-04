library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(GeoModels)

form = formula('Abies_perc ~ prediction+bio09+Summer_temperature+Winter_temperature+bio03')

#load the data
column_df = read.table('df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt') #Abies percentage already logit transfomed and covariables values at gridcell in which the corresponding site lies
covariables_df = read.table('covariables_all_variables_100km_corr_fact_add_sites.txt') #covariables values on the whole grid for each time step
    
grid = st_read(paste0("polygon_50.shp"))
grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84")
grid = st_transform(grid, crs= st_crs('epsg:3035'))
grid$CHELSA_Tra = NULL #remove the variables
grid$cell_id = as.numeric(rownames(grid))
points_centers = st_coordinates(st_centroid(grid))
    
covariables_df[covariables_df == -999] = NA
covariables_df = covariables_df[order(covariables_df$cell_id),]
    
years = seq(20000, 500, by = -500)

load('Binary_component_result_VarSel_IK/IK_results.RData')

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
    
column_df = merge(column_df, IK_results, by = c('cell_id','CalYrBP'))
column_df = column_df[!is.na(column_df$prediction),] #remove the site that fell outside the grid. It is in the UK and never recorded Abies
covariables_df = merge(covariables_df, IK_results, by = c('CalYrBP','cell_id'))
#covariables_df = covariables_df[!is.na(covariables_df$prediction),] #remove the site that fell outside the grid. It is in the UK and never recorded Abies
    
##################### Fitting the variogram for training of st-RK ##########################
#!! This means that you need to supply the model only with data that you consider for the initial training!
# All the dataset is used to inform the model about the distribution/relationships.
df_RK = column_df
df_RK = df_RK[!df_RK$Abies_perc==-20,]
    
###### hindcasting ##########
#df_RK = df_RK[!df_RK$CalYrBP >11500, ]
#EH = seq(11500, 9000, -500)
years = years[!years>8500]
results_2 = list()
count_2 = 1

for(o in years){
  print(o)
  training = df_RK[df_RK$CalYrBP %in% seq(o+3000, (o+500), by = -500),]
  
  #training = training[order(training$CalYrBP),]
  test = df_RK[df_RK$CalYrBP %in% o,]
      
  covariables_test = covariables_df[covariables_df$CalYrBP %in% o,]
  covariables_test = covariables_test[order(covariables_test$cell_id),]
      
  # Regressions
  model.glm = lm(form, data = training, na.action = "na.fail")
      
  #residuals
  training$residuals = model.glm$residuals
      
  coordx_dyn = list()
  store_residuals = list()
  count = 1
  for(i in seq(o+3000, (o+500), by = -500)){
    df_RK_this_run_year = training[training$CalYrBP==i,] 
    coordx_dyn[[count]] = as.matrix(df_RK_this_run_year[,c('lon','lat')])
    store_residuals[[count]] = df_RK_this_run_year$residuals
    count = count+1
  }

  ###### variogram building #############
  coordt = 0:(length(unique(training$CalYrBP)))
      
  corrmodel = "Matern_Matern"
  model = "Gaussian"
      
  #fitting
  var_b = var(unlist(store_residuals))
  fixed_b = list(nugget=0, scale_s=45000, sill = 2.4, smooth_s = 0.5)
  start_b = list(mean = 0, scale_t=5, smooth_t = 2)
      
  fitted = GeoFit(data = store_residuals , coordx_dyn = coordx_dyn,
                  coordt = coordt[-length(coordt)],
                  corrmodel = corrmodel,
                  start=start_b,
                  maxdist = 500000,
                  fixed = fixed_b)
      
  param_est=as.list(c(fitted$param,fixed_b))
      
  vario = GeoVariogram(data=store_residuals,coordx_dyn=coordx_dyn, coordt=coordt[-length(coordt)], maxdist = 500000)
      
  #check that the parameters are all positive, they should be positive
  if(param_est$scale_t <0){
    print(paste0('Negative scale_t: failed. Using scale_t = 0.5'))
    param_est$scale_t = 0.5
  } else if(param_est$smooth_t<0){
    print(paste0('Negative smooth_t: failed. Using smooth_t = 0.5'))
    param_est$smooth_t = 0.5
  }  
      
  ST_RK = GeoKrig(data = store_residuals,
                  coordx_dyn = coordx_dyn,
                  coordt = coordt[-length(coordt)],
                  corrmodel = corrmodel,
                  model = model,
                  loc = points_centers,
                  param = param_est,
                  maxdist = 500000,
                  time = coordt)
      
  interpolated_residuals = ST_RK$pred
      
  #now we need to predict the data with glm and sum the results to the prediction from ST-RK
  lm_formula = model.glm
  predicted_lm = predict(lm_formula, newdata = covariables_test)
      
  predicted_RK = interpolated_residuals[length(coordt),]
      
  #now we need to sum those two
  predicted = predicted_lm+predicted_RK
  predicted = (exp(predicted)/(1+exp(predicted)))*100
  grid_RK = grid
      
  grid_RK$predicted = predicted
  grid_RK$predicted_lm = predicted_lm
  grid_RK$predicted_rk = predicted_RK
  grid_RK$CalYrBP = o
      
  differences = merge(test[,c('cell_id','Abies_perc')], grid_RK, by = 'cell_id')
  differences$Abies_perc = (exp(differences$Abies_perc)/(1+exp(differences$Abies_perc)))*100
      
  results_2[[count_2]] = grid_RK
  count_2 = count_2 +1
}
  
results = do.call(rbind, results_2)
    
save(results, file = paste0("RelAb_VarSel_STRK//sliding_window.RData"))
