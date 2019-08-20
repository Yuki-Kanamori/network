install.packages("xlsx", dep=T)
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

require(plyr)
env2 = env
env2 = na.omit(env2)
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

