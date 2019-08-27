require(tidyverse)
require(plyr)

setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
pa <- read.table("countOTUv2.fish.txt", header = T)
#各調査地点での調査回数を調べた
t <- pa[pa$Site=="27", ]
length(unique(t$Date))
head(pa,1)
pa <- pa %>% tidyr::gather(key = Species, value = Count, 4:max(ncol(pa)))
pa <- pa %>% mutate(Year = str_sub(pa$Date, 1, 2), Month = str_sub(pa$Date, 3, 4), Day = str_sub(pa$Date, 5, 6), Depth = ifelse(pa$Depth == "s", "Surface", "Bottom"))

name = read.csv("otu_name.csv", fileEncoding = "CP932")
pa = merge(pa, name, by = "Species", all = T)
#pa = left_join(pa, name, by = "Species", )
pa$Depth = factor(pa$Depth, levels = c("Surface", "Bottom"))

head(pa)
pa$pa = ifelse(pa$Count > 0, 1, 0)
n_sp = pa %>% distinct(species,.keep_all = T)

enc = ddply(pa, .(Year, Site, Depth, species, wamei), summarize, mean_logenc = log(mean(pa)+1))
summary(enc)

enc2 = ddply(pa, .(Depth, species, wamei), summarize, mean_logenc = log(mean(pa)+1))
enc2 = enc2 %>% arrange(Depth, desc(mean_logenc))

sp_mean = ddply(enc, .(Year, Depth, species, wamei), summarize, smean_logenc = mean(mean_logenc))
enc = merge(enc, sp_mean, by = c("species", "Depth", "Year", "wamei"))

enc = enc %>% mutate(si = (mean_logenc-smean_logenc)^2)
bd_t = ddply(enc, .(Depth), summarize, BD_total = sum(si)/(length(unique(enc$Site))-1))

enc = merge(enc, bd_t, by = "Depth")

site_ss = ddply(enc, .(Depth, Site), summarize, ss_i = sum(si))
site_ss = merge(site_ss, bd_t, by = "Depth")
site_ss$ss_i2 = site_ss$ss_i/site_ss$BD_total

site = read.table("sampling_points.txt", header = T)
site = site[, -1]
head(site, 1)
colnames(site) = c("Lon", "Lat", "Site")
site_ss = merge(site_ss, site, by = "Site")
site_ss = site_ss %>% arrange(Depth, ss_i2)

require(maps)
require(mapdata)
require(ggplot2)
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))

p <- geom_point(data = site_ss, aes(x = Lon, y = Lat, colour = ss_i2), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "LCBD", title = "")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            #strip.text.y = element_text(size = 100),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

sp_ss = ddply(enc, .(Depth, species,wamei), summarize, ss_j = sum(si))
sp_ss = merge(sp_ss, bd_t, by = "Depth")
sp_ss$ss_j2 = sp_ss$ss_j/sp_ss$BD_total

require(ggplot2)
g <- ggplot(data = sp_ss, aes(x = species, y = ss_j2))
f <- facet_wrap( ~ Depth, ncol = 2)
b = geom_bar(stat = "identity")
#p = geom_point()
#s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "SCBD", title = "")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            #strip.text.y = element_text(size = 100),
            legend.title = element_text(size = 10))
g+f+b+lb+theme_bw()+th

sp_ss05 = filter(sp_ss, ss_j2 > 0.5) %>% arrange(Depth, desc(ss_j2))


stsp_ss = ddply(enc, .(Depth, Site, species, wamei), summarize, ss_ij = sum(si))
stsp_ss = merge(stsp_ss, bd_t, by = "Depth")
stsp_ss$ss_ij2 = stsp_ss$ss_ij/stsp_ss$BD_total

site = read.table("sampling_points.txt", header = T)
site = site[, -1]
head(site, 1)
colnames(site) = c("Lon", "Lat", "Site")
stsp_ss = merge(stsp_ss, site, by = "Site")
stsp_ss = stsp_ss %>% arrange(Depth, Site, desc(ss_ij2))




# variation and env. relationship -------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
env = read.csv("env_data2.csv")
e_site = env %>% distinct(st, .keep_all = T)




