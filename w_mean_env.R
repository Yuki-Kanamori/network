setwd("/Users/Yuki/Dropbox/Network/revised_data")
env = read.csv("env_data.csv", fileEncoding = "CP932")
site = read.csv("env_site.csv", fileEncoding = "CP932")
require(dplyr)
head(site)
site = mutate(site, lat = ifelse(site$N > 3500, 35+(site$N-3500)/60, 34+(site$N-3400)/60), 
              lon = ifelse(site$E > 14000, 140+(site$E-14000)/60, 139+(site$E-13900)/60)) %>% select(st, r_st, lon, lat)
head(env,3)
head(site,3)
env = merge(env, site, by = "st")
env = env[, -2]
write.csv(env, "env_data2.csv")

site = env %>% select(r_st, lon, lat) %>% distinct(r_st, .keep_all = TRUE) 

for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  assign(paste0(sakana),
         read.csv("est_d_tok1.csv") %>% 
           filter(species == sakana) %>% 
           select(knot_i, m_Lon, m_Lat) %>% 
           distinct(knot_i, .keep_all = TRUE))
}

dist = array(0, dim = c(50,20,7))
dist = matrix(0, ncol = 20, nrow = 50)
a_dist = matrix(0, ncol = 1, nrow = 7)
for(i in 1:7){
  for(j in 1:50){
    for(k in 1:20){
      sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
      data = get(sakana) %>% filter(knot_i == j)
      e_data = site %>% filter(r_st == k)
      
      dist[j,k,i] = sqrt((data$m_Lon - e_data$lon)^2 + (data$m_Lat - e_data$lat)^2)
    }
  }
}

require(tidyr)
require(stringr)
dist2 = data.frame(rbind(dist[,,1],dist[,,2],dist[,,3],dist[,,4],dist[,,5],dist[,,6],dist[,,7])) %>% 
  mutate(species = rep(c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki"), each = 50), knot_i = rep(seq(1,50,1), 7)) %>%
  gather(key = r_st, value = eu_dist, 1:20) %>% mutate(r_st = as.numeric(as.factor(str_sub(r_st, 2, 4))))
summary(dist2)

env = rbind(filter(env, between(month,1,3)) %>% mutate(season = "WS"),
            filter(env, between(month,4,6)) %>% mutate(season = "SS"),
            filter(env, between(month,7,9)) %>% mutate(season = "SA"),
            filter(env, between(month,10,12)) %>% mutate(season = "AW"))

#明白な外れ値と，余分な環境データ を削除
env2 = rbind(filter(env, env == "w_temperature") %>% filter(value < 100),
             filter(env, env == "salinity") %>% filter(value < 100),
             filter(env, env == "DO") %>% filter(value > 0))
require(plyr)
env3 = ddply(env2, .(year, season, r_st, layer, env), summarize, mean = mean(value))
summary(filter(env3, env == "w_temperature"))
summary(filter(env3, env == "DO"))
summary(filter(env3, env == "salinity"))

env4 = c()
for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  data = filter(dist2, species == sakana)
  
  t = merge(env3, data, by = "r_st")
  env4 = rbind(env4,t)
  
}

env4 = env4 %>% mutate(wa = (1/eu_dist)*mean)
head(env4)
haha = ddply(dist2, .(species, knot_i), summarize, total = sum((1/eu_dist)))
ko = ddply(env4, .(species, knot_i, year, season, layer, env), summarize, sum = sum(wa))
head(ko)
ko = merge(ko, haha, by = c("species", "knot_i"))
ko = ko %>% mutate(w_mean = sum/total)
t = ko %>% filter(env == "w_temperature" & season == "WS")
summary(t)
setwd("/Users/Yuki/Dropbox/Network/revised_data")
write.csv(ko, "w_mean_env.csv")
