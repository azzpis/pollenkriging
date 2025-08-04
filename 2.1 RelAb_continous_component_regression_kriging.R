library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(GeoModels)

#load the data
column_df = read.table('df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt') #Abies percentage already logit transfomed and covariables values at gridcell in which the corresponding site lies
covariables_df = read.table('covariables_all_variables_100km_corr_fact_add_sites.txt') #covariables values on the whole grid for each time step
    
grid = st_read(paste0("polygon_50.shp")) #grid
grid = grid %>% st_set_crs("+proj=longlat +datum=WGS84") #trasformation in metric 
grid = st_transform(grid, crs= st_crs('epsg:3035'))
grid$CHELSA_Tra = NULL #remove the variables
grid$cell_id = as.numeric(rownames(grid))
points_centers = st_coordinates(st_centroid(grid))
    
#some housekeeping 
covariables_df[covariables_df == -999] = NA
covariables_df = covariables_df[order(covariables_df$cell_id),]
    
years = seq(20000, 500, by = -500)
    
#load the regression formula
form = formula('Abies_perc ~ prediction+bio09+Summer_temperature+Winter_temperature+bio03')

### Add the predicted probability of presence from the kriging of the binary component ####
### Now we choose from which scnario the probability of occurrence comes from 

load('Binary_component_result_VarSel_IK/IK_results.RData') #VarSel_Surv

krigings = IK_results
    
#formatting the data
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
    
#add the IK results to the datasets
IK_results = do.call(rbind, to_bind)
IK_results$cell_id = rep(seq(1, 7320),40)
IK_results$x = NULL
IK_results$y = NULL
IK_results$geometry = NULL
IK_results$prediction[IK_results$prediction <0]= 0 #here we set the probabilities less than zero as zeros 
IK_results$prediction[IK_results$prediction >1]= 1 #here we set the probabilities higher than one as one 
    
#now we add the PoO to the original dataset and to the gridcells to be interpolated
column_df = merge(column_df, IK_results, by = c('cell_id','CalYrBP'))
column_df = column_df[!is.na(column_df$prediction),] #remove the site that fell outside the grid. It is in the UK and never recorded Abies
covariables_df = merge(covariables_df, IK_results, by = c('CalYrBP','cell_id'))
    
    
##################### Variogram fitting ##########################
df_RK = column_df #data at sites
df_RK = df_RK[!df_RK$Abies_perc==-20,] #remove the zeros !! This is important because we are interpolating only the positive values!
    
#now we build regression model and extract the residuals for each 'era'
LG = which(years %in% seq(20000, 12000, -500)) #Late glacial
EH = which(years %in% seq(11500, 9000, -500)) #Early Holocene
MH = which(years %in% seq(8500, 5000, -500)) #Mid Holocene
LH = which(years %in% seq(4500, 500, -500)) #Late Holocene
    
#subset by 'era'
df_RK_LG = df_RK[df_RK$CalYrBP %in% years[LG],]
df_RK_EH = df_RK[df_RK$CalYrBP %in% years[EH],]
df_RK_MH = df_RK[df_RK$CalYrBP %in% years[MH],]
df_RK_LH = df_RK[df_RK$CalYrBP %in% years[LH],]
    
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
    
#bind the residuals
all_together = do.call(rbind, list(df_RK_LG, df_RK_EH, df_RK_MH, df_RK_LH))
    
#format the residuals for GeoKrig. You have to produce two lists, each one containing the data at site for a time bin, and the other must have at the same position the coordinate of that site. 
#it has to go from the oldest to the yougest. 

coordx_dyn = list()
store_residuals = list()
count = 1
for(i in years){
  df_RK_this_run_year = all_together[all_together$CalYrBP==i,] 
  coordx_dyn[[count]] = as.matrix(df_RK_this_run_year[,c('lon','lat')])
  store_residuals[[count]] = df_RK_this_run_year$residuals
  count = count+1
}
    
#check normality
# hist(unlist(store_residuals), breaks = 30, probability = TRUE, main = "Histogram with Density")
# lines(density(unlist(store_residuals)), col = "blue", lwd = 2)
# qqnorm(unlist(store_residuals))
# qqline(unlist(store_residuals), col = "red")
# shapiro.test(unlist(store_residuals))
# library(nortest)
# ad.test(unlist(store_residuals))
# library(moments)
# skewness(unlist(store_residuals))
# kurtosis(unlist(store_residuals))

#remember to detatch the packages if running the normality check

###### variogram building #############
coordt = 0:(length(store_residuals)-1) #a vector that has an element for each time bin considered. Here from 0 to 39 (40 time bins total)
    
corrmodel = "Matern_Matern"
model = "Gaussian"
    
#fitting the variogram 
var_b = var(unlist(store_residuals))
fixed_b = list(nugget=0, scale_s=45000, sill = 2.4, smooth_s = 0.5)
start_b = list(mean = 0, scale_t=5, smooth_t = 2) #nb, take care here.
    
fitted = GeoFit(data = store_residuals, #residuals
                coordx_dyn = coordx_dyn, #coodinates of the sites
                coordt = coordt, #temporal coordinates
                corrmodel = corrmodel, #correlation model
                start=start_b, #starting parameter 
                maxdist = 500000, #maximum distance 
                fixed = fixed_b) #fixed parameters
    
param_est=as.list(c(fitted$param,fixed_b))
    
vario = GeoVariogram(data=store_residuals, coordx_dyn=coordx_dyn, coordt=coordt, maxdist = 500000)
    
subDir = paste0("RelAb_VarSel_STRK")
if(file.exists(subDir)){
  print(paste0('Folder exist!'))
} else {
  print(paste0('Does not exist! Creating it..'))
  dir.create(subDir)
}
save(vario, file = paste0("RelAb_VarSel_STRK/vario.RData"))
save(fitted, file = paste0("RelAb_VarSel_STRK/fitted.RData"))

#Plot the covariogram and save it
png(paste0("RelAb_VarSel_STRK/covariogram.png"))
GeoCovariogram(fitted,vario=vario,fix.lagt=1,fix.lags=1,show.vario = T)
dev.off()
    
#separability index
C0 = var(unlist(store_residuals))
cov_time = C0 - vario$variogramt
cov_spat = C0 - vario$variograms
cov_spatime = C0 - matrix(vario$variogramst,ncol=length(cov_time),byrow=T)
    
cov_time_x_spat = outer(cov_spat, cov_time, "*")
sep = cov_spatime*C0 - cov_time_x_spat
    
png(paste0("RelAb_VarSel_STRK/separability.png"))
plot(vario$centers/1000,sep[,1],xlim=c(0,500),ylim=range(sep), type="b",col="red",lwd=2,xlab="Dist_VarSel_Exp (km)",ylab="separation index")
points(vario$centers/1000,sep[,15],xlim=c(0,500),ylim=range(sep), type="b",col="pink",lwd=2,)
points(vario$centers/1000,sep[,20],xlim=c(0,500),ylim=range(sep), type="b",col="purple",lwd=2,)
points(vario$centers/1000,sep[,37],xlim=c(0,500),ylim=range(sep), type="b",col="forestgreen",lwd=2,)
points(vario$centers/1000,sep[,30],xlim=c(0,500),ylim=range(sep), type="b",col="blue",lwd=2,)
abline(h=0,lty=2)
dev.off()

    
#check that the parameters are all positive. It should not happen. 
if(param_est$scale_t <0){
  print(paste0('Negative scale_t: failed. Using scale_t = 0.5'))
  param_est$scale_t = 0.5
  } else if(param_est$smooth_t<0){
    print(paste0('Negative smooth_t: failed. Using smooth_t = 0.5'))
    param_est$smooth_t = 0.5
  }  

#### Kriging ###########
ST_RK = GeoKrig(data = store_residuals, #residuals
                coordx_dyn = coordx_dyn, #coordinates sites
                coordt = coordt, #temporal coordinate
                corrmodel = corrmodel, #correlation models
                model = model, # random field
                loc = points_centers, #locations to be predicted
                param = param_est, #estimated parameters from fitting
                maxdist = 500000, #maximum distance 
                time = coordt) #temporal instants to be predicted (may be larger than coordt if a prediction is required)

#take the results
interpolated_residuals = ST_RK$pred


#Here we compute the second regressions and the prediction of the values. 
#at the end, the corresponding interpolated residual is added. 
#there are three main 'cases': 
#1) the late glacial: a single regression is made. Then, for each year, a single prediction is computed and the residuals are added correspondingly. 
#2) First time bins of the Holcocene: a single regression is made. Then, for each year, a single prediction is computed and the residuals are added correspondingly. 
#3) Holocene: a regression is made considering a time window of 6 time bind (3k years). Then, for the youngest time bin, a prediction is computed and the residuals are added correspondingly. 

#set store lists
store_RK = list()
results = list()
store_variogram = list()
store_fit_vario = list()
store_glm = list()
count = 1
    
    
#Start with the cycles
for (j in c(1, 18, seq(23,40,1))) {
  print(paste('RK Kriging:',j, 'of', length(years), '.'))
      
  #1) late glacial
  if(j == 1){
    sampled = years[LG]
    to_keep = match(sampled, years)
    df_RK_this_run= df_RK[df_RK$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        
    # Regression
    model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
    store_glm[[count]] = model.glm
        
    # check regression results
    #summary(model.glm)
        
    #calculate values for each year
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
      grid_RK$predicted = predicted #save the final prediction
      grid_RK$predicted_lm = predicted_lm #save the results from the regression
      grid_RK$predicted_rk = predicted_RK #save the results from kriging
      grid_RK$CalYrBP = years[m]
      results_2[[count_2]] = grid_RK
      count_2 = count_2 +1
    }
    
    results_2 = do.call(rbind, results_2)
    results[[count]] = results_2
    count = count+1
        
  } else if(j %in% c(18)){ #2) Early stages of the Holocene 
        
    sampled = years[18:22]
    to_keep = match(sampled, years)
    df_RK_this_run= df_RK[df_RK$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        
    # Regression
    model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
    store_glm[[count]] = model.glm
        
    # check regression results
    #summary(model.glm)
        
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
      grid_RK$predicted = predicted #save the final prediction
      grid_RK$predicted_lm = predicted_lm #save the results from the regression
      grid_RK$predicted_rk = predicted_RK #save the results from kriging
      grid_RK$CalYrBP = years[m]
      results_2[[count_2]] = grid_RK
      count_2 = count_2 +1
    }
    
    results_2 = do.call(rbind, results_2)
    results[[count]] = results_2
    count = count+1
        
  } else { # 3) Holocene 
    sampled = years[(j-5):(j)]
    to_keep = match(sampled, years)
    df_RK_this_run= df_RK[df_RK$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df[covariables_df$CalYrBP %in% sampled,]
    covariables_df_RK = covariables_df_RK[order(covariables_df_RK$cell_id),]
        
    # Regression
    model.glm = lm(form, data = df_RK_this_run, na.action = "na.fail")
    store_glm[[count]] = model.glm
        
    #now we need to predict the data with glm and sum the results to the prediction from ST-RK
    lm_formula = model.glm
    predicted_lm = predict(lm_formula, newdata = covariables_df_RK[covariables_df_RK$CalYrBP == years[j],])
    predicted_RK = interpolated_residuals[j,]
        
    #now we need to sum those two
    predicted = predicted_lm+predicted_RK
    predicted = (exp(predicted)/(1+exp(predicted)))*100
    grid_RK = grid
    grid_RK$predicted = predicted #save the final prediction
    grid_RK$predicted_lm = predicted_lm #save the results from the regression
    grid_RK$predicted_rk = predicted_RK #save the results from kriging
    grid_RK$CalYrBP = years[j]
        
    results[[count]] = grid_RK
    count = count+1
        
  }   
}
    
    
#collapse the results
results = do.call(rbind, results)
    
#save
save(results, file = paste0("RelAb_VarSel_STRK/RK_results.RData"))
    
#plots
subDir2 = paste0('RelAb_VarSel_STRK/rk_plots')
if (file.exists(subDir2)){
  print(paste0('Folder exist!'))
} else {
  print(paste0('Does not exist! Creating it..'))
  dir.create(subDir2)
}
my_breaks = c(0.01, 1, 5, 10, 50 , 100)
    
for(i in c(1:40)){
  data_year = results[results$CalYrBP == years[i],]
  grid_rk = grid
  grid_rk$predicted_RK = data_year$predicted
  ggplot(grid_rk, mapping = aes(fill = predicted_RK))+geom_sf(color = NA)+
  scale_fill_viridis_c(option = 'H',
                       na.value = NA, 
                       breaks = my_breaks, labels = my_breaks, limits = c(0.1,100), 
                       oob = scales::squish, 
                       trans = "log10", 
                       guide = guide_colourbar(barheight = 10, nbin = 7)
        )
  ggsave(filename=paste0("RelAb_VarSel_STRK/rk_plots/",years[i],".jpeg"), units = 'px', width = 2000, height = 1800, dpi = 300)
      
}


