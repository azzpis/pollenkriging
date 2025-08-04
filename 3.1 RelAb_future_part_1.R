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
    
#now we build regression model and extract the residuals for each 'age'
LG = which(years %in% seq(20000, 12000, -500))
EH = which(years %in% seq(11500, 9000, -500))
MH = which(years %in% seq(8500, 5000, -500))
LH = which(years %in% seq(4500, 500, -500))
  
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
    
subDir = paste0("RelAb_VarSel_STRK_future/")
if (file.exists(subDir)){
  print(paste0('Folder exist!'))
} else {
  print(paste0('Does not exist! Creating it..'))
  dir.create(subDir)
}

save(vario, file = paste0("RelAb_VarSel_STRK_future/vario.RData"))
save(fitted, file = paste0("RelAb_VarSel_STRK_future/fitted.RData"))
    
png(paste0("RelAb_VarSel_STRK_future/covariogram.png"))
GeoCovariogram(fitted,vario=vario,fix.lagt=1,fix.lags=1,show.vario = T)
dev.off()
    
#separability index - Denis: I corrected the computations
C0 = var(unlist(store_residuals))
cov_time = C0 - vario$variogramt
cov_spat = C0 - vario$variograms
cov_spatime = C0 - matrix(vario$variogramst,ncol=length(cov_time),byrow=T)
    
#Then, we compute the sep index:
cov_time_x_spat = outer(cov_spat, cov_time, "*")
sep = cov_spatime*C0 - cov_time_x_spat

png(paste0("RelAb_VarSel_STRK_future/separability.png"))
plot(vario$centers/1000,sep[,1],xlim=c(0,500),ylim=range(sep), type="b",col="red",lwd=2,xlab="Dist_VarSel_Exp (km)",ylab="separation index")
points(vario$centers/1000,sep[,15],xlim=c(0,500),ylim=range(sep), type="b",col="pink",lwd=2,)
points(vario$centers/1000,sep[,20],xlim=c(0,500),ylim=range(sep), type="b",col="purple",lwd=2,)
points(vario$centers/1000,sep[,37],xlim=c(0,500),ylim=range(sep), type="b",col="forestgreen",lwd=2,)
points(vario$centers/1000,sep[,30],xlim=c(0,500),ylim=range(sep), type="b",col="blue",lwd=2,)
abline(h=0,lty=2)
dev.off()

#check that the parameters are all positive, they must be positive
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
                time = c(coordt, length(coordt))) #here we do the future prediction of the residual surface! It is simply done by adding a further time step!!
    
interpolated_residuals = ST_RK$pred
    
save(interpolated_residuals, file = paste0("RelAb_VarSel_STRK_future/kriging_results.RData"))

