setwd("/Users/Yuki/Dropbox/Network/revised_data")
w_env = read.csv("w_mean_env.csv")
est = read.csv("est_d_tok1.csv")

head(w_env)
head(est)
est2 = merge(est, w_env, by = c("species", "year", "season", "knot_i"))
require(tidyr)
for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  assign(paste0(sakana),
         data = filter(est2, species == sakana)
         do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
           filter(env == "DO") %>% spread(key = layer, value = w_mean) 
         colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
         sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
           filter(env == "salinity") %>% spread(key = layer, value = w_mean)
         colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
         wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
           filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
         colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
         
         left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance")))
}


head(est2)
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[1]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
isigarei = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[2]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
konosiro = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[3]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
kouika = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[4]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
kurumaebi = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[5]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
maanago = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[6]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
makogarei = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[7]
data = filter(est2, species == sakana)
#%>% mutate(layer2 = paste(env,layer,sep = "_"))
do = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "DO") %>% spread(key = layer, value = w_mean) 
colnames(do)[7:11] = c("do_0","do_5","do_10","do_20","do_50")
sal = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "salinity") %>% spread(key = layer, value = w_mean)
colnames(sal)[7:11] = c("sal_0","sal_5","sal_10","sal_20","sal_50")
wt = data %>% select(species, year, season, knot_i, log_abundance, layer, env, w_mean) %>% 
  filter(env == "w_temperature") %>% spread(key = layer, value = w_mean)
colnames(wt)[7:11] = c("wt_0","wt_5","wt_10","wt_20","wt_50")
head(do)
suzuki = left_join(do, sal, wt, by = c("species", "year", "season", "knot_i", "log_abundance"))

write.csv(isigarei, "boost_isi.csv")
write.csv(konosiro, "boost_kono.csv")
write.csv(kouika, "boost_ika.csv")
write.csv(kurumaebi, "boost_ebi.csv")
write.csv(maanago, "boost_ana.csv")
write.csv(makogarei, "boost_mako.csv")
write.csv(suzuki, "boost_suzu.csv")
