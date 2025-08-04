#K fold cross validation of the binary component regression kriging 

#load the data
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

covariables_df[covariables_df == -999] = NA #remove gridcells on sea and covered by ice
covariables_df = covariables_df[order(covariables_df$cell_id),]

# trasform data from continuous to binary
column_df = column_df %>% mutate(Abies_perc = case_when(
  Abies_perc == -20 ~ 0,
  Abies_perc != -20 ~ 1
))


#now the procedure
library(caret)

years = seq(20000, 500, by = -500)
k = 5
results_k_folds_years = list()
count = 1
for(i in years){
  print(i)
  df_year = column_df[column_df$CalYrBP ==i,]
  df_year_all = covariables_df[covariables_df$CalYrBP ==i,]
  
  results_k_folds = list()
  for(j in 1:100){
    print(j)
    folds <- createFolds(df_year$Abies_perc, k = k, list = TRUE, returnTrain = FALSE)
    data_cv = list()
    for(a in 1:5){
      
      test_data <- df_year[folds[[a]], ]
      train_data <- df_year[-folds[[a]], ]
      
      logit_regression = glm(Abies_perc ~ Winter_tasmin+Summer_precipitation+Summer_temperature,
                             family = binomial(link= 'logit'), 
                             data = train_data)
      
      data2 = logit_regression$residuals
      coordx = train_data[, c('lon','lat')]
      
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
      
      param_est=as.list(c(fitted$param,fixed_b))
      
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
      df_year_all_2 = df_year_all[!is.na(df_year_all$P_Apr),]
      grid_pred$prediction = 1/(1+exp(-total_prediction))
      grid_pred = grid_pred[grid_pred$cell_id %in% df_year_all_2$cell_id,]
      grid_pred = grid_pred[grid_pred$cell_id %in% test_data$cell_id,]
      grid_pred = merge(grid_pred, test_data[,c('CalYrBP', 'cell_id', 'Label','lon', 'lat', 'Abies_perc')])
      
      data_cv[[a]] = grid_pred
      
    }
    results_k_folds[[j]] = data_cv
  }
  results_k_folds_years[[count]] = results_k_folds
  count = count +1
}

save(results_k_folds_years, file = 'Binary_component_result_VarSel_IK/Cross_validation_results.RData')

bind_years = list()
for(i in 1:40){
  print(i)
  take_results = results_k_folds_years[[i]]
  bind_runs = list()
  for(j in 1:100){
    take_results_run = do.call(rbind, take_results[[j]])
    take_results_run$run = j
    bind_runs[[j]] = take_results_run
  }
  bind_runs = do.call(rbind, bind_runs)
  bind_years[[i]] = bind_runs
}
bind_years= do.call(rbind, bind_years)


Brier_score_years = data.frame()
for(i in years){
  this_year = bind_years[bind_years$CalYrBP == i,]
  Brier_score = (sum((this_year$prediction - this_year$Abies_perc)^2))/(nrow(this_year))
  Brier_score_years = rbind(Brier_score_years, data.frame(Brier_score = Brier_score, years = i))
}

write.table(Brier_score_years, file = 'Binary_component_result_VarSel_IK/CV_results.txt' )

ggplot(Brier_score_years)+
  geom_line(mapping = aes(x = years, y = Brier_score))

