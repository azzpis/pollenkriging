#percentages workflow

#load the datasets
MAP = read.csv("old.file.2.csv")
new.sites = read.csv('additiona.sites.formatted.csv')


##### HOUSE KEEPING
new.sites = new.sites[!new.sites$Label == 'HOZE',]
names(MAP)
names(new.sites)[10] = 'Pollen.sum'
new.sites$CalYrBP = NULL
names(new.sites)[11] = 'CalYrBP'
new.sites$Country = NULL
new.sites$Region = NULL
new.sites$Pollen_sum = NULL
names(new.sites)

MAP = rbind(MAP, new.sites)
MAP$CalYrBP.2 = as.numeric(as.character(MAP$CalYrBP))
MAP_NA = MAP[is.na(MAP$CalYrBP.2),]
MAP_NA = MAP_NA[!is.na(MAP_NA$CalYrBP),]

sites = data.frame(sort(unique(MAP$Name)))

#adjust some sites formatting
MAP$CalYrBP[MAP$Label == 'TRIFO2'] = MAP$CalYrBP[MAP$Label == 'TRIFO2']*1000

#remove site in the mediterranean sea
MAP = MAP[!MAP$Label == 'MAILL',]
MAP = MAP[!MAP$Label == 'ALGEN',]

##########################################
#how many counts 
# library(dplyr)
# average.pollen.count = MAP %>%
#   select(Label, Pollen.sum) %>%
#   group_by(Label) %>%
#   summarise(average = median(Pollen.sum))
# 
# sites = average.pollen.count[average.pollen.count$average <100,]
# sites = MAP[MAP$Label %in% sites$Label,]
# sites = sites[!sites$Abies ==0,]


############################################
#set the empirical limit according to Lang et al. 2023
#the empirical limit is reached if we have continuous presence in
#at least 5 five adjacent samples spanning at least 1000 years without more than one interruption
i = 'PERGU'
j = 1

MAP_EL = MAP[!MAP$CalYrBP >21000,]

years_el = data.frame()

add.samples = function(site_df, samples){
  data.segment = site_df[c(samples, (samples[length(samples)]+1)),]
  return(data.segment)
}

count = 1
for (i in sort(unique(MAP_EL$Label))){
  print(paste(count, 'of 250'))
  site_df = MAP_EL[MAP_EL$Label == i,]
  site_df = site_df[!is.na(site_df$CalYrBP),]
  site_df = site_df[order(site_df$CalYrBP, decreasing = T),]
  
  for(j in 1:nrow(site_df)){
    if(j >= nrow(site_df)-3){
      break
    }
    
    #take the minimum of year
    age.above = site_df[j, c('CalYrBP')]-1000
    
    #check how many samples
    samples = which(site_df$CalYrBP<= site_df[j, c('CalYrBP')] & site_df$CalYrBP > age.above)
    age = ((site_df[j, c('CalYrBP')] - site_df[samples[length(samples)], c('CalYrBP')]) <1000)
    
    #if not sufficient, add samples until 5 
    while(length(samples) <5 | isTRUE(age)){
      new.segment = add.samples(site_df, samples)
      if(is.na(new.segment[nrow(new.segment), 'X'])){
        break
      } else if(new.segment$CalYrBP[1] - (new.segment$CalYrBP[nrow(new.segment)] >= 1000) & nrow(new.segment)>=5){
        samples = c(samples, (samples[length(samples)]+1))
        break
      }else{
        samples = c(samples, (samples[length(samples)]+1))
      }
      if(length(samples) == nrow(site_df)){
        break
      }
    }
    
    #check 2: is there more than one zero? is the timespan larger than 1000
    take_above_rows = site_df[samples,]
    above_sum = (sum(take_above_rows$Abies ==0, na.rm = T)>1)
    age = (take_above_rows[1, c('CalYrBP')] - (take_above_rows[nrow(take_above_rows), c('CalYrBP')])<1000)
    
    if(samples[length(samples)] == nrow(site_df) & isTRUE(age) & isFALSE(above_sum)){
      age = FALSE
    }
    if(isTRUE(age)){
      this_site = data.frame(Label = unique(site_df$Label), Name = unique(site_df$Name), Year = take_above_rows[nrow(take_above_rows),c('CalYrBP')], Segment = 0)
      years_el = rbind(years_el, this_site)
    } else if(isTRUE(above_sum)){
      this_site = data.frame(Label = unique(site_df$Label), Name = unique(site_df$Name), Year = take_above_rows[nrow(take_above_rows),c('CalYrBP')], Segment = 0)
      years_el = rbind(years_el, this_site)
    }else{ 
      take_year = take_above_rows[!take_above_rows$Abies ==0,]
      take_year = take_year[order(take_year$CalYrBP),]
      this_site = data.frame(Label = unique(site_df$Label), Name = unique(site_df$Name), Year = take_year[nrow(take_year),c('CalYrBP')], Segment = 1)
      years_el = rbind(years_el, this_site)
    }
    
  }
  
  count = count+1
}

years_el2 = years_el
years_el = years_el[!years_el$Segment ==0,]
table(is.na(years_el$Year))
el = data.frame()
i = 'OHRID'
for (i in unique(years_el$Label)){
  select_site = years_el[years_el$Label ==i, ]
  if (sum(select_site$Year)<0){
    el_site = select_site[select_site$Year == min(select_site$Year ),]
  }else{
    el_site = data.frame(select_site[select_site$Year == max(select_site$Year ),], abies.presence = nrow(select_site))
  }
  el = rbind(el, el_site)
}
el = el[!duplicated(el),]

names(el)[c(1,3)] = c('filename','year')

write.table(el, 'empirical.limits.txt')

# the following commented code is just a mid-way check; you can skip it. 


library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
#study.site = read.csv("Table1.site.metadata.csv", sep = ';')

#el = merge(el, study.site, by = 'filename', all= T)
#
# el = el %>%
#   mutate(category_yr = case_when(
#     year > 19000 & year <20000 ~ 20000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 18000 & year <19000 ~ 19000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 17000 & year <18000 ~ 18000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 16000 & year <17000 ~ 17000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 15000 & year <16000 ~ 16000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 14000 & year <15000 ~ 15000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 13000 & year <14000 ~ 14000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 12000 & year <13000 ~ 13000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 11000 & year <12000 ~ 12000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 10000 & year <11000 ~ 11000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 9000 & year <10000 ~ 10000,
#     year > 19000 & year <20000 ~ 20000,
#     year > 8000 & year <9000 ~ 9000,
# #     year > 19000 & year <20000 ~ 20000,
# #     year > 7000 & year <8000 ~ 8000,
# #     year > 19000 & year <20000 ~ 20000,
# #     year > 6000 & year <7000 ~ 7000,
# #     year > 19000 & year <20000 ~ 20000,
# #     year > 5000 & year <6000 ~ 6000,
# #     year > 19000 & year <20000 ~ 20000,
# #     year > 4000 & year <5000 ~ 5000,
# #     year > 3000 & year <4000 ~ 4000,
# #     year > 2000 & year <3000 ~ 3000,
# #     year > 1000 & year <2000 ~ 2000,
# #     year > 0 & year <1000 ~ 1000
# #   ))
#

######## APPLY THE EMPIRICAL LIMIT ##################3
i = unique(el$filename)[2]
#el$category_yr[is.na(el$category_yr)] =0
for (i in unique(MAP$Label)){
  if (i %in% unique(el$filename)){
    MAP$Abies[MAP$Label == i & MAP$CalYrBP > el$year[el$filename == i ]] = 0
  }
  else{
    MAP$Abies[MAP$Label==i] = 0
  }
}

# #check how many observations per time step
# MAP_check = MAP
# MAP_check = MAP_check[!is.na(MAP_check$CalYrBP),]
# library(dplyr)
# MAP_check = MAP_check %>% mutate(bin = case_when(
#   CalYrBP >19500  & CalYrBP <= 20000 ~ 1,
#   CalYrBP >19000  & CalYrBP <= 19500 ~ 2,
#   CalYrBP >18500  & CalYrBP <= 19000 ~ 3,
#   CalYrBP >18000  & CalYrBP <= 18500 ~ 4,
#   CalYrBP >17500  & CalYrBP <= 18000 ~ 5,
#   CalYrBP >17000  & CalYrBP <= 17500 ~ 6,
#   CalYrBP >16500  & CalYrBP <= 17000 ~ 7,
#   CalYrBP >16000  & CalYrBP <= 16500 ~ 8,
#   CalYrBP >15500  & CalYrBP <= 16000 ~ 9,
#   CalYrBP >15000  & CalYrBP <= 15500 ~ 10,
#   CalYrBP >14500  & CalYrBP <= 15000 ~ 11,
#   CalYrBP >14000  & CalYrBP <= 14500 ~ 12,
#   CalYrBP >13500  & CalYrBP <= 14000 ~ 13,
#   CalYrBP >13000  & CalYrBP <= 13500 ~ 14,
#   CalYrBP >12500  & CalYrBP <= 13000 ~ 15,
#   CalYrBP >12000  & CalYrBP <= 12500 ~ 16,
#   CalYrBP >11500  & CalYrBP <= 12000 ~ 17,
#   CalYrBP >11000  & CalYrBP <= 11500 ~ 18,
#   CalYrBP >10500  & CalYrBP <= 11000 ~ 19,
#   CalYrBP >10000  & CalYrBP <= 10500 ~ 20,
#   CalYrBP >9500  & CalYrBP <= 10000 ~ 21,
#   CalYrBP >9000  & CalYrBP <= 9500 ~ 22,
#   CalYrBP >8500  & CalYrBP <= 9000 ~ 23,
#   CalYrBP >8000  & CalYrBP <= 8500 ~ 24,
#   CalYrBP >7500  & CalYrBP <= 8000 ~ 25,
#   CalYrBP >7000  & CalYrBP <= 7500 ~ 26,
#   CalYrBP >6500  & CalYrBP <= 7000 ~ 27,
#   CalYrBP >6000  & CalYrBP <= 6500 ~ 28,
#   CalYrBP >5500  & CalYrBP <= 6000 ~ 29,
#   CalYrBP >5000  & CalYrBP <= 5500 ~ 30,
#   CalYrBP >4500  & CalYrBP <= 5000 ~ 31,
#   CalYrBP >4000  & CalYrBP <= 4500 ~ 32,
#   CalYrBP >3500  & CalYrBP <= 4000 ~ 33,
#   CalYrBP >3000  & CalYrBP <= 3500 ~ 34,
#   CalYrBP >2500  & CalYrBP <= 3000 ~ 35,
#   CalYrBP >2000  & CalYrBP <= 2500 ~ 36,
#   CalYrBP >1500  & CalYrBP <= 2000 ~ 37,
#   CalYrBP >1000  & CalYrBP <= 1500 ~ 38,
#   CalYrBP >500  & CalYrBP <= 1000 ~ 39,
#   CalYrBP <= 500 ~ 40,
# ))
# 
# 
# obs.lake.bint = MAP_check %>%
#   dplyr::group_by(Label, bin) %>%
#   dplyr::summarise(n= n())
# 
# library(ggplot2)
# ggplot(obs.lake.bint, mapping = aes(x = bin, y = n, group = bin))+
#   geom_boxplot()+ylim(0,180)
# 
# median = obs.lake.bint %>%
#   dplyr::group_by(bin) %>%
#   summarise(median = median(n))
# 
# #take the oldest samples
# old_samples = data.frame()
# for(i in unique(MAP$Label)){
#   take_site = MAP[MAP$Label == i,]
#   take_site = take_site[order(take_site$CalYrBP, decreasing = T),]
#   old_samples = rbind(old_samples, this_site = data.frame(name = i, CalYrBP = take_site[1, c('CalYrBP')]))
# }

#   

######## DATA BINNING ##############
# Bin data in 500 year bins
MAP_Abies <- data.frame(matrix(NA, nrow=length(unique(MAP$Label))*40, ncol=7))
n <- vector()
sites = unique(MAP$Label)
j= sites[1]
i = 40
for (j in sites) {
  
  x <- MAP[MAP$Label == j,]
  
  time.mid <- seq(250, 19750, 500)
  Abies.bin <- rep(NA, length(time.mid))
  Polsum.bin <- rep(NA, length(time.mid))
  half.win <- 250
  n <- append(n,values=(time.mid))
  
  for (i in 1:length(time.mid)) {
    age.lo <- time.mid[i] - half.win
    age.hi <- time.mid[i] + half.win
    ageID <- which(x$CalYrBP >= age.lo & x$CalYrBP < age.hi)
    Abies.bin[i] = sum(x$Abies[ageID], na.rm=TRUE)
    Polsum.bin[i] = sum(x$Pollen.sum[ageID], na.rm=TRUE)
  }
  
  pollen.bin <- cbind(Abies.bin, Polsum.bin)
  pollen.bin <- as.data.frame(pollen.bin)
  colnames(pollen.bin) <- c("Abies", "Pollen.sum")
  pollen.bin$Age <- time.mid+250
  pollen.bin$Label <- rep(x[1,'Label'], length(time.mid))
  pollen.bin$Name <- rep(x[1,'Name'], length(time.mid))
  pollen.bin$LatDD <- rep(x[1,'LatDD'], length(time.mid))
  pollen.bin$LonDD <- rep(x[1,'LonDD'], length(time.mid))
  
  MAP_Abies[(length(n)-39):length(n),] <- pollen.bin[1:length(time.mid),]
}

colnames(MAP_Abies) <- c("Abies", "Pollen.sum", "CalYrBP", "Label", "Name", "LatDD", "LonDD")

MAP_Abies$Label = unlist(MAP_Abies$Label)
MAP_Abies$Name = unlist(MAP_Abies$Name)
MAP_Abies$LatDD = unlist(MAP_Abies$LatDD)
MAP_Abies$LonDD = unlist(MAP_Abies$LonDD)

# calculate percentages

MAP_Abies$Abies_perc <- (MAP_Abies$Abies / MAP_Abies$Pollen.sum)*100
hist(MAP_Abies$Abies_perc)

# make time slices
for (k in 1:length(time.mid)) {
  assign(paste("Abies_",time.mid[k]+250,"_calBP", sep=""), subset(MAP_Abies, CalYrBP==time.mid[k]+250, select=c("Abies", "Pollen.sum", "CalYrBP", "Label", "Name", "LatDD", "LonDD", "Abies_perc")))
}

# create maps
# first check all data points
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
is.list(MAP_Abies$LatDD)

Abies_8500_calBP= MAP_Abies[MAP_Abies$CalYrBP == 8500,]

world <- ne_coastline(scale = "medium", returnclass = "sf")
g =ggplot(data=world) +
  geom_sf() +
  geom_point(data=Abies_8500_calBP, aes(x=LonDD, y=LatDD, color = Name), size=1) +
  scale_color_discrete()+
  coord_sf(xlim=c(-11,45), ylim=c(35,72), expand=FALSE)+theme(legend.position = 'none')
g

library(plotly)
ggplotly(g)

#some cleaning before saving
#fix the coordinates of monticchio
MAP_Abies = MAP_Abies[!MAP_Abies$Label == 'MAILL',] #into the sea
MAP_Abies = MAP_Abies[!MAP_Abies$Label == 'ALGEN',] #on an island

MAP_Abies$LatDD[MAP_Abies$Label == 'MONTI'] = 40.931235
MAP_Abies$LonDD[MAP_Abies$Label == 'MONTI'] = 15.605185

MAP_Abies$LatDD = as.numeric(MAP_Abies$LatDD)
MAP_Abies$LonDD = as.numeric(MAP_Abies$LonDD)
class(MAP_Abies)

write.csv(MAP_Abies, "MAP_Abies_percentage_el_add_sites.csv")
