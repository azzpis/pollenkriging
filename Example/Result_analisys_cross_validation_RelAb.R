#analyse 5kfold resutls 
library(dplyr)
library(ggplot2)
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
dirs = list.dirs(recursive = F)
dirs = dirs[c(4)]

df_all = list()
count = 1
for(j in dirs){
  print(j)
  load(paste0(j,'/5_fold_CV.RData'))
  take_df = store_kfold_results
  
  for(i in 1:length(take_df)){
    this_year = take_df[[i]]
    this_year$run = i
    this_year$model = j
    this_year = this_year[,c('CalYrBP', 'cell_id', 'predicted', 'Abies_perc', 'run', 'model')]
    df_all[[count]] = this_year
    count = count+1
  }
}

df_all = data.table::rbindlist(df_all) 

#trasform back the Abies RelAb (from logit)
df_all$Abies_perc = (exp(df_all$Abies_perc )/(1+exp(df_all$Abies_perc )))*100
unique(df_all$model)


analysed_MM = df_all %>%
  group_by(CalYrBP, model) %>%
  summarise(Spear_cor = Spearman_corr(Abies_perc, predicted), 
            MDAE = Median_abs_err(Abies_perc, predicted), 
            rel_err = relative_error(Abies_perc, predicted),
            mare = mare(Abies_perc, predicted))


g1 = ggplot(analysed_MM)+
  geom_line(mapping =aes(x = CalYrBP, y = rel_err, color = model))+
  theme_bw()+
  geom_vline(xintercept = c(11500), linetype =1, color = 'black')+
  geom_vline(xintercept = c(8200,4200,18500, 15000, 12900), linetype =2, color = 'black')+
  scale_x_reverse(limits = c(20000,0), name = 'CalYrBP')+
  ylab('Median absolute relative error')

g2 = ggplot(analysed_MM)+
  geom_line(mapping =aes(x = CalYrBP, y = Spear_cor, color = model))+
  scale_x_reverse()+theme_bw()+
  geom_vline(xintercept = c(11500), linetype =1, color = 'black')+
  geom_vline(xintercept = c(8200,4200,18500, 15000, 12900), linetype =2, color = 'black')+
  ylab('Spearman correlation')+
  geom_hline(yintercept = 0)



ggarrange(plotlist = list(g1,g2), common.legend = T, legend = 'bottom', ncol = 2, nrow =1)
#ggsave(paste0("5_fold_runs_average.png"), units = 'px', width = 6000, height =  3508, dpi = 300)

write.table(analysed_MM, file = 'CV_results.txt')

library(plotly)
ggplotly(g2)


###### RUNS independently###########3
analysed_MM_run = df_all %>%
  group_by(CalYrBP, model, run) %>%
  summarise(Spear_cor = Spearman_corr(Abies_perc, predicted), MDAE = Median_abs_err(Abies_perc, predicted), 
            rel_err = relative_error(Abies_perc, predicted),
            mare = mare(Abies_perc, predicted))

g1 = ggplot()+
  geom_line(analysed_MM_run, mapping =aes(x = CalYrBP, y = rel_err, color = model, group = interaction(model, run)), alpha = 0.1)+
  geom_line(analysed_MM, mapping =aes(x = CalYrBP, y = rel_err, color = model))+
  theme_bw()+
  geom_vline(xintercept = c(11500), linetype =1, color = 'black')+
  geom_vline(xintercept = c(8200,4200,18500, 15000, 12900), linetype =2, color = 'black')+
  scale_x_reverse(limits = c(20000,0), name = 'CalYrBP')+
  ylab('Relative error')

g2 = ggplot()+
  geom_line(analysed_MM_run, mapping =aes(x = CalYrBP, y = Spear_cor, color = model, group = interaction(model, run)), alpha = 0.1)+
  geom_line(analysed_MM, mapping =aes(x = CalYrBP, y = Spear_cor, color = model))+
  theme_bw()+
  geom_vline(xintercept = c(11500), linetype =1, color = 'black')+
  geom_vline(xintercept = c(8200,4200,18500, 15000, 12900), linetype =2, color = 'black')+
  scale_x_reverse(limits = c(20000,0), name = 'CalYrBP')+
  ylab('Spearman correlation')+
  geom_hline(yintercept = 0)


ggarrange(plotlist = list(g1,g2), common.legend = T, legend = 'bottom', ncol = 2, nrow =1)
#ggsave(paste0("/5_fold_runs.png"), units = 'px', width = 6000, height =  3508, dpi = 300)

write.table(analysed_MM_run, file = 'CV_results_runs.txt')
