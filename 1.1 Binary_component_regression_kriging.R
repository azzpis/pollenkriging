

library(dplyr)
library(sf)
library(ggplot2)
library(GeoModels)
library(rnaturalearthdata)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf")

#load + preparation of the data---------------------------
grid = st_read(paste0("polygon_50.shp"))
grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84")
grid = st_transform(grid, crs= st_crs('epsg:3035'))
grid$CHELSA_Tra = NULL #remove the variables
grid$cell_id = as.numeric(rownames(grid))
points_centers = st_coordinates(st_centroid(grid))

column_df = read.table('df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt') #Abies percentage already logit transfomed and covariables values at gridcell in which the corresponding site lies

covariables_df = read.table('covariables_all_variables_100km_corr_fact_add_sites.txt') #covariables values on the whole grid for each time step

points_centers = st_coordinates(st_centroid(grid))


# set sea or areas covered by ice as NA
covariables_df[covariables_df == -999] = NA
covariables_df = covariables_df[order(covariables_df$cell_id),] #reorder the dataset

years = seq(20000, 500, by = -500) #define times

#transform the data from continuous to binary
column_df = column_df %>% mutate(Abies_perc = case_when(
  Abies_perc == -20 ~ 0,
  Abies_perc != -20 ~ 1
))

#for loop for the binary regression kriging. Take care it may require some ram and time. 
i = years[1]
store_logit_regression = list()
IK_results = list()
store_vario = list()
store_param = list()
count = 1

for(i in years){
  print(i)
  
  #take data of interest for this year
  df_year = column_df[column_df$CalYrBP ==i,]
  df_year_all = covariables_df[covariables_df$CalYrBP ==i,]
  coordx = df_year[,c('lon','lat')]
  
  ###################### Logit regression ##############################
  #I had to do the logit regression because GeoWLS does not allow covariates
  logit_regression = glm(Abies_perc ~ Winter_tasmin+Summer_precipitation+Summer_temperature,
                         family = binomial(link= 'logit'), data = df_year)
  
  store_logit_regression[[count]] = logit_regression
  
  data2 = logit_regression$residuals
  
  ##################### Fitting the variogram ##########################
  var_b = var(unlist(data2))
  fixed_b = list(nugget = 0, mean = 0)
  start_b = list(scale=50000, sill = 1) #nb, take care here.
  maxdist = 1000000
  
  fitted = GeoWLS(data =data2 , coordx = coordx,
                  corrmodel = 'Exp',
                  start=start_b,
                  fixed = fixed_b, 
                  maxdist = maxdist)
  
  fitted
  fitted$param
  param_est=as.list(c(fitted$param,fixed_b))
  
  vario = GeoVariogram(data=data2,coordx = coordx, maxdist = maxdist)
  
  store_vario[[count]] = vario
  store_param[[count]] = param_est
  
  ######## plot ##################
  data_variogram = data.frame(vario = vario$variograms, bins = c(vario$bins[-1]))
  maxdist = 1000000
  distance = seq(0, maxdist, length.out = 100)
  sill = param_est$sill
  range = param_est$scale
  nugget = param_est$nugget
  
  
  theoretical_exp = function(distance, sill, range, nugget){
    nugget+sill*(1- exp(-distance/range))
  }
  curve <- theoretical_exp(distance, sill, range, nugget)
  curve = data.frame(bins = distance,
                     space = curve)
  
  g1 = ggplot(data_variogram)+
    geom_point(mapping = aes(x = bins, y = vario))+
    geom_line(data = curve, aes(x = bins, y = space))
  
  ggsave(filename = paste0('Binary_component_result_VarSel_IK/variograms/',i,'.jpeg'), g1, width = 6, height = 5, dpi = 300)
  
  ########## Kriging ###########
  param_est=as.list(c(fitted$param,fixed_b))
  print(param_est)
  RK = GeoKrig(data = data2,  # It is a Residual Kriging
               coordx = coordx,
               corrmodel='Exp',
               model = 'Gaussian',
               loc = points_centers,
               #points_centers,
               param = param_est,
               maxdist = maxdist)
  
  prediction = predict(logit_regression, newdata = df_year_all)
  
  total_prediction = prediction + RK$pred
  
  grid_pred = grid
  #grid_pred$prediction = total_prediction
  grid_pred$prediction = 1/(1+exp(-total_prediction))
  
  IK_results[[count]] = grid_pred
  
  g2 = ggplot(grid_pred)+
    geom_sf(mapping = aes(fill = prediction))+
    scale_fill_viridis_c(option = 'H', na.value = NA)
  
  ggsave(filename = paste0('Binary_component_result_VarSel_IK/results/',i,'.jpeg'), g2, width = 6, height = 5, dpi = 300)
  
  g3 = ggplot(grid_pred[grid_pred$prediction>0.2,])+
    geom_sf(mapping = aes(fill = prediction))+
    scale_fill_viridis_c(option = 'H', na.value = NA)+
    geom_sf(data = world, fill = NA, colour = 'black') +
    coord_sf(crs = "epsg:3035", xlim = c(2494406, 7087972), ylim = c(1309552, 4453060))
  
  ggsave(filename = paste0('Binary_component_result_VarSel_IK/results_cropped/',i,'.jpeg'), g3, width = 6, height = 5, dpi = 300)
  
  count = count + 1
  
}

save(store_logit_regression, file = 'Binary_component_result_VarSel_IK/logit_reg.RData')
save(IK_results, file ='Binary_component_result_VarSel_IK/IK_results.RData')
save(store_vario, file ='Binary_component_result_VarSel_IK/vario.RData')
save(store_param, file ='Binary_component_result_VarSel_IK/param.RData')

