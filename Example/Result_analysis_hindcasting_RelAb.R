## analyse slifing window chaining results ##
library(dplyr)
library(ggplot2)
library(Metrics)
library(ggpubr)

Spearman_corr = function(original, predicted){
  cor(predicted, original, method = 'spearman')
}
Median_abs_err = function(original, predicted){
  Metrics::mdae(original, predicted)
}

relative_error = function(original, predicted){
  error = (predicted-original)/(original)
  median(error)
}

mare = function(original, predicted){ #abs_median_relative_error
  rel_abs_err = abs((predicted-original)/(original))
  median(rel_abs_err)
}

rel_change = function(original, predicted){
  change = ((predicted / original)-1) #relative change https://en.wikipedia.org/wiki/Relative_change
  median(change)
}

folders = list.dirs(recursive = F)
folders = folders[4]

sliding_window = list()
count = 1
for(i in folders){
  load(paste0(i,'/sliding_window.RData'))
  results_sw = results
  results_sw$model = i
  sliding_window[[count]] = results_sw
  count = count+1
}
sliding_window = do.call(rbind, sliding_window)

column_df = read.table('df_sites_all_variables_kernel_100km_corr_fact_add_sites.txt') #Abies rel abundances already logit transfomed and covariables values at gridcell in which the corresponding site lies
column_df = column_df[!column_df$Abies_perc == -20,]

data_merged_sw = merge(sliding_window, column_df[,c('cell_id','CalYrBP','Abies_perc')], by = c('cell_id','CalYrBP'))
data_merged_sw$Abies_perc = (exp(data_merged_sw$Abies_perc)/(1+exp(data_merged_sw$Abies_perc)))*100 #trasform back logit trasformed rel abundances

data_merged_sw$geometry = NULL

ggplot(data = data_merged_sw)+
  geom_point(mapping = aes(x = Abies_perc, y = predicted, color = CalYrBP))+
  geom_segment(aes(x = 0, xend = 70, y = 0, yend = 70))+theme_bw()+
  xlab('Observed Abies %')+ylab('Predicted Abies %')


########################################

calc_sw = data_merged_sw %>% 
  group_by(CalYrBP,model) %>%
  summarise(MDAE = Median_abs_err(Abies_perc, predicted),
            Spear_cor = Spearman_corr(Abies_perc, predicted),
            rel_err = relative_error(Abies_perc, predicted),
            mare = mare(Abies_perc, predicted), 
            rmse = rmse(Abies_perc, predicted))

write.table(calc_sw, file = 'results_sliding_window_analised.txt')

g1 = ggplot()+
  geom_line(calc_sw, mapping = aes(x = CalYrBP, y = rel_err))+
  scale_x_reverse()+theme_bw()+ggtitle('Absolute median relative error')
g1

g2 = ggplot()+
  geom_line(calc_sw, mapping = aes(x = CalYrBP, y = Spear_cor))+
  scale_x_reverse()+theme_bw()+ggtitle('Spearman correlation')

g3 = ggplot()+
  geom_line(calc_sw, mapping = aes(x = CalYrBP, y = MDAE))+
  scale_x_reverse()+theme_bw()+ggtitle('Median absolute error')

g4 = ggplot()+
  geom_line(calc_sw, mapping = aes(x = CalYrBP, y = rel_err))+
  scale_x_reverse()+theme_bw()+ggtitle('Relative error')

ggarrange(plotlist = list(g1,g3,g4, g2), nrow = 2, ncol = 2)

