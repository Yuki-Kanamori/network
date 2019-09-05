#install.packages("xlsx", dep=T)
require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
setwd("/Users/Yuki/Dropbox/Network/envdata")

do = c()
for(i in 1:5){
  data = read.xlsx("DO2_1987-2018.xlsx", i)
  do = rbind(do, data)
}
382*5
dep = c(0,5,10,20,50)
do = mutate(do, layer = rep(lay, each = 382), env = "DO")

sal = c()
for(i in 1:5){
  data = read.xlsx("塩分2_1987-2018.xlsx", i)
  sal = rbind(sal, data)
}
382*5
lay = c(0,5,10,20,50)
sal = mutate(sal, layer = rep(lay, each = 382), env = "salinity")


dep = read.xlsx("海深2_1987-2018.xlsx", 1)
dep = mutate(dep, layer = NA, env = "depth")

wt = c()
for(i in 1:5){
  data = read.xlsx("水温2_1987-2018.xlsx", i)
  wt = rbind(wt, data)
}
382*5
lay = c(0,5,10,20,50)
wt = mutate(wt, layer = rep(lay, each = 382), env = "w_temperature")


col = read.xlsx("透明度2_1987-2018.xlsx", 1)
col = mutate(col, layer = NA, env = "colour")

# colnames(do)
# colnames(sal)
# colnames(dep)
# colnames(wt)
# colnames(col)

summary(wt)
env = rbind(do, sal, dep, wt, col)
#env = env[-126004, ]

require(stringr)
mode(env$年月)
summary(env)
yr = rep(1987:2018, each = 12)
mo = rep(seq(1,12,1), 32)
time = data_frame(year = yr, month = formatC(mo, width=2, flag="0"))
time = time[-c(383,384), ] %>% mutate(time = paste(year, month, sep = "_"))
env = cbind(env, time)
env = gather(env, key = st, value = value, 2:21)
summary(env)
write.csv(env, "env_data.csv", fileEncoding = "CP932")

setwd("/Users/Yuki/Dropbox/Network/revised_data")
env = read.csv("env_data.csv", fileEncoding = "CP932")
head(env)
summary(env)
do = env %>% filter(env == "DO")
sal = env %>% filter(env == "salinity")
wt = env %>% filter(env == "w_temperature")
plot(do$value, wt$value, ylim = c(0, 30), xlim = c(0,20))
plot(sal$value, wt$value, ylim = c(0, 30), xlim = c(0,35))
plot(sal$value, do$value, ylim = c(0, 20), xlim = c(0,40))

require(plyr)
env2 = env
env2 = na.omit(env2)
summary(env2)
#env2 = ddply(env2, .(year, month, env, layer), summarize, mean = mean(value))
env3 = ddply(env2, .(year, env, layer), summarize, mean = mean(value))
n_sal = filter(env2, env == "salinity" & mean == max(mean))
env4 = ddply(env2, .(time, env, layer), summarize, mean = mean(value))
env5 = ddply(env2, .(year, month, env, layer), summarize, mean = mean(value))



require(ggplot2)
g = ggplot(env3, aes(x = year, y = mean, colour = factor(layer), group = factor(layer)))
p = geom_point()
l = geom_line()
f = facet_wrap(~ env, ncol = 2, scales = "free")
g+p+l+f+theme_bw()


env4_1 = env2 %>% filter(year > 2000 & env != "salinity")
env4_1 = ddply(env4_1, .(time, env, layer), summarize, mean = mean(value))
env5 = filter(env5, env != "salinity" & year > 2010)
g = ggplot(env5, aes(x = month, y = mean, colour = factor(layer), group = factor(layer)))
p = geom_point()
l = geom_line()
f = facet_grid(env ~ year, scales = "free")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1),
           #axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1),
           axis.text.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13))
g+p+l+f+th+theme_bw()



# depth ---------------------------------------------------------
require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
require(plyr)

setwd("/Users/Yuki/Dropbox/Network/envdata")
dep = read.xlsx("海深2_1987-2018.xlsx", 1)
yr = rep(1987:2018, each = 12)
mo = rep(seq(1,12,1), 32)
time = data_frame(year = yr, month = formatC(mo, width=2, flag="0"))
time = time[-c(383,384), ] %>% mutate(time = paste(year, month, sep = "_"))
dep = cbind(dep, time)
dep = dep %>% gather(key = st, value = value, 2:21) %>% filter(!is.na(value)) 
y_dep = ddply(dep, .(st, year), summarize, mean = mean(value))

g = ggplot(y_dep, aes(x = year, y = mean, group = st))
p = geom_point()
l = geom_line()
f = facet_wrap( ~ st, ncol = 2, scales = "free")
g+p+l+f

m_dep = ddply(y_dep, .(st), summarize, mean = mean(mean))
setwd("/Users/Yuki/Dropbox/Network/revised_data")
site = read.csv("env_site.csv", fileEncoding = "CP932")
require(dplyr)
head(site)
site = mutate(site, lat = ifelse(site$N > 3500, 35+(site$N-3500)/60, 34+(site$N-3400)/60), 
              lon = ifelse(site$E > 14000, 140+(site$E-14000)/60, 139+(site$E-13900)/60)) %>% select(st, r_st, lon, lat)
head(m_dep,3)
head(site,3)
m_dep = merge(m_dep, site, by = "st")
#m_dep = m_dep[, -2]


require(maps)
require(mapdata)
require(ggplot2)
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(34.9, 35.75))

p <- geom_point(data = m_dep, aes(x = lon, y = lat, colour = mean), shape = 16, size = 5)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Mean depth", title = "")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            #strip.text.y = element_text(size = 100),
            legend.title = element_text(size = 10))
t2+p+s+lb+th+theme_bw()

