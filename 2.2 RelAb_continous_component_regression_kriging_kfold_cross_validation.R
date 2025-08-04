#load the data
library(sp)
library(dplyr)
library(sf)
library(GeoModels)

set.seed(2083)

form = formula('Abies_perc ~ prediction+bio09+Summer_temperature+Winter_temperature+bio03')

kfolds = 5

#load the data
### Add the predicted probability of presence for IK ####
### Now we choose where the probability of occurrence comes from 

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


#add the PoO to the dataset
load('Binary_component_result_VarSel_IK/IK_results.RData')

years = seq(20000, 500, by = -500)
krigings = IK_results

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


##################### Fitting the variogram for training of st-RK ##########################
#!! This means that you need to supply the model only with data that you consider for the initial training!
# All the dataset is used to inform the model about the distribution/relathionships.
df_RK = column_df
df_RK = df_RK[!df_RK$Abies_perc==-20,]

#now we do the k_fold cv
# WARNING: running it requires: 36 GB RAM and 2 days!

success = T
sites = unique(df_RK$Label)
store_kfold_results = list()
length_run = 1

while(success){
  print(paste0('try n. ', length_run,' ', Sys.time()))
  folds <- caret::createFolds(sites, k = kfolds, list = TRUE, returnTrain = FALSE)
  data_cv = list()
  
  for(a in 1:length(folds)){
    print(paste('Fold:',a, 'of', length(folds), '.'))
    test_data <- df_RK[df_RK$Label %in% sites[folds[[a]]], ]
    train_data <- df_RK[!df_RK$Label %in% sites[folds[[a]]], ]
    
    #check that there is at least one point for each year
    year_counts <- table(train_data$CalYrBP)
    
    if(length(unique(df_RK$CalYrBP)) != length(year_counts)){
      print('Every year is not represented. Skip to the next iteration.')
      next
    }
    
    
    #now we build regression model and extract the residuals for each 'age'
    LG = which(years %in% seq(20000, 12000, -500))
    EH = which(years %in% seq(11500, 9000, -500))
    MH = which(years %in% seq(8500, 5000, -500))
    LH = which(years %in% seq(4500, 500, -500))
    
    df_RK_LG = train_data[train_data$CalYrBP %in% years[LG],]
    df_RK_EH = train_data[train_data$CalYrBP %in% years[EH],]
    df_RK_MH = train_data[train_data$CalYrBP %in% years[MH],]
    df_RK_LH = train_data[train_data$CalYrBP %in% years[LH],]
    
    covariables_df_LG = covariables_df[covariables_df$CalYrBP %in% years[LG],]
    covariables_df_EH = covariables_df[covariables_df$CalYrBP %in% years[EH],]
    covariables_df_MH = covariables_df[covariables_df$CalYrBP %in% years[MH],]
    covariables_df_LH = covariables_df[covariables_df$CalYrBP %in% years[LH],]
    
    covariables_df_LG = covariables_df_LG[order(covariables_df_LG$cell_id),]
    covariables_df_EH = covariables_df_EH[order(covariables_df_EH$cell_id),]
    covariables_df_MH = covariables_df_MH[order(covariables_df_MH$cell_id),]
    covariables_df_LH = covariables_df_LH[order(covariables_df_LH$cell_id),]
    
    # Regressions
    model.glm.LG = lm(form, data = df_RK_LG, na.action = "na.fail")
    model.glm.EH = lm(form, data = df_RK_EH, na.action = "na.fail")
    model.glm.MH = lm(form, data = df_RK_MH, na.action = "na.fail")
    model.glm.LH = lm(form, data = df_RK_LH, na.action = "na.fail")
    
    #residuals
    df_RK_LG$residuals = model.glm.LG$residuals
    df_RK_EH$residuals = model.glm.EH$residuals
    df_RK_MH$residuals = model.glm.MH$residuals
    df_RK_LH$residuals = model.glm.LH$residuals
    
    all_together = do.call(rbind, list(df_RK_LG, df_RK_EH, df_RK_MH, df_RK_LH))
    
    coordx_dyn = list()
    store_residuals = list()
    count = 1
    for(i in years){
      df_RK_this_run_year = all_together[all_together$CalYrBP==i,] 
      coordx_dyn[[count]] = as.matrix(df_RK_this_run_year[,c('lon','lat')])
      store_residuals[[count]] = df_RK_this_run_year$residuals
      count = count+1
    }
    
    ###### variogram building #############
    coordt = 0:(length(store_residuals)-1)
    
    corrmodel = "Matern_Matern"
    model = "Gaussian"
    
    #fitting
    var_b = var(unlist(store_residuals))
    fixed_b = list(nugget=0, scale_s=45000, sill = 2.4, smooth_s = 0.5)
    start_b = list(mean = 0, scale_t=5, smooth_t = 2) #nb, take care here.
    
    fitted = GeoFit(data = store_residuals , coordx_dyn = coordx_dyn,
                    coordt = coordt,
                    corrmodel = corrmodel,
                    start=start_b,
                    maxdist = 500000,
                    fixed = fixed_b)
    
    param_est=as.list(c(fitted$param,fixed_b))
    
    vario = GeoVariogram(data=store_residuals,coordx_dyn=coordx_dyn, coordt=coordt, maxdist = 500000)
    
    #check that the parameters are all positive
    if(param_est$scale_t <0){
      print(paste0('Negative scale_t: failed. Using scale_t = 0.5'))
      param_est$scale_t = 0.5
    } else if(param_est$smooth_t<0){
      print(paste0('Negative smooth_t: failed. Using smooth_t = 0.5'))
      param_est$smooth_t = 0.5
    }         
    
    ST_RK = GeoKrig(data = store_residuals,
                    coordx_dyn = coordx_dyn,
                    coordt = coordt,
                    corrmodel = corrmodel,
                    model = model,
                    loc = points_centers,
                    param = param_est,
                    maxdist = 500000,
                    time = coordt)
    
    interpolated_residuals = ST_RK$pred
    
    #set store lists
    results = list()
    count = 1
    
    #Start with the cycles
    for (j in c(1, 18, seq(23,40,1))) {
      
      
      #last glacial maximum
      if(j == 1){
        #now we produce the dataset and coordinates used to make the prediction
        sampled = years[LG]
        to_keep = match(sampled, years)
        df_RK_this_run= train_data[train_data$CalYrBP %in% sampled,]
        
        covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
        covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        
        # Regression
        model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
        
        count_2 = 1
        results_2 = list()
        for(m in which(years %in% sampled)){
          
          #now we need to predict the data with glm and sum the results to the prediction from ST-RK
          lm_formula = model.glm
          predicted_lm = predict(lm_formula, newdata = covariables_df_RK[covariables_df_RK$CalYrBP == years[m],])
          
          predicted_RK = interpolated_residuals[m,]
          
          #now we need to sum those two
          predicted = predicted_lm+predicted_RK
          predicted = (exp(predicted)/(1+exp(predicted)))*100
          
          grid_RK = grid
          grid_RK$predicted = predicted
          grid_RK$CalYrBP = years[m]
          
          grid_RK = merge(grid_RK, test_data, by = c('CalYrBP','cell_id'))
          
          results_2[[count_2]] = grid_RK
          count_2 = count_2 +1
        }
        results_2 = do.call(rbind, results_2)
        results[[count]] = results_2
        count = count+1
      } else if(j %in% c(18)){
        #now we produce the dataset and coordinates used to make the prediction
        sampled = years[18:22]
        to_keep = match(sampled, years)
        df_RK_this_run= train_data[train_data$CalYrBP %in% sampled,]
        covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
        covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        # Regression
        model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
        
        # check regression results
        summary(model.glm)
        
        count_2 = 1
        results_2 = list()
        for(m in which(years %in% sampled)){
          
          #now we need to predict the data with glm and sum the results to the prediction from ST-RK
          lm_formula = model.glm
          predicted_lm = predict(lm_formula, newdata = covariables_df_RK[covariables_df_RK$CalYrBP == years[m],])
          
          predicted_RK = interpolated_residuals[m,]
          
          #now we need to sum those two
          predicted = predicted_lm+predicted_RK
          predicted = (exp(predicted)/(1+exp(predicted)))*100
          
          grid_RK = grid
          grid_RK$predicted = predicted
          grid_RK$CalYrBP = years[m]
          
          grid_RK = merge(grid_RK, test_data, by = c('CalYrBP','cell_id'))
          
          results_2[[count_2]] = grid_RK
          count_2 = count_2 +1
        }
        results_2 = do.call(rbind, results_2)
        results[[count]] = results_2
        count = count+1
      } else { #Holocene 
        #now we produce the dataset and coordinates used to make the prediction
        sampled = years[(j-5):(j)]
        to_keep = match(sampled, years)
        df_RK_this_run= train_data[train_data$CalYrBP %in% sampled,]
        covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
        covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        # Regression
        model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
        
        
        #now we need to predict the data with glm and sum the results to the prediction from ST-RK
        lm_formula = model.glm
        predicted_lm = predict(lm_formula, newdata = covariables_df_RK[covariables_df_RK$CalYrBP == years[j],])
        
        predicted_RK = interpolated_residuals[j,]
        
        #now we need to sum those two
        predicted = predicted_lm+predicted_RK
        predicted = (exp(predicted)/(1+exp(predicted)))*100
        
        grid_RK = grid
        grid_RK$predicted = predicted
        grid_RK$CalYrBP = years[j]
        
        grid_RK = merge(grid_RK, test_data, by = c('CalYrBP','cell_id'))
        
        results[[count]] = grid_RK
        count = count+1
        
      }   
    }
    results = do.call(rbind, results)
    data_cv[[a]] = results
  }
  
  data_cv = do.call(rbind, data_cv)
  store_kfold_results[[length_run]] = data_cv
  
  length_run = length_run+1
  
  if (length_run == 21){
    save(store_kfold_results, file = paste0("RelAb_VarSel_STRK//5_fold_CV_20.RData"))
  }
  if (length_run == 51){
    save(store_kfold_results, file = paste0("RelAb_VarSel_STRK/5_fold_CV_50.RData"))
  }
  
  if (length_run == 101){
    success = F
  }
  save(store_kfold_results, file = paste0("RelAb_VarSel_STRK/5_fold_CV.RData"))
}
