setwd(dir = "/Users/Yuki/Dropbox/Network")
chiba = read.csv("chiba3_revised.csv")
require(dplyr)
chiba2 = filter(chiba, between(Lon, 139.5, 140.5) & between(Lat, 35, 35.75))
summary(chiba2)
chiba2_1 = chiba2[chiba2$Lat != max(chiba2$Lat), ]
chiba2_1 = chiba2_1[chiba2_1$Lon != min(chiba2_1$Lon), ]
summary(chiba2_1)

d1_1 = filter(chiba2_1, between(Lon, 139.5, 139.725) & between(Lat, 35.25, 35.3))
summary(d1_1)
n_d1_1 = d1_1[d1_1$Lat == min(d1_1$Lat), ] #左下の部分

n_d1_2 = filter(chiba2_1, between(Lon, 139.5, 139.75) & between(Lat, 35.475, 35.75))
summary(d1_2)
g <- ggplot(data = n_d1_2, aes(x = Lon, y = Lat, label = X))
p2 <- geom_point(shape = 16, size = 1)
t <- geom_text()
g+p2+t+theme_bw()+scale_y_continuous(breaks=seq(35,36,0.025))+scale_x_continuous(breaks=seq(139.5,140.3,0.025))+theme(axis.text.x = element_text(angle = 90, hjust = 1))


n_d1_3 = filter(chiba2_1, between(Lon, 139.725, 139.75) & between(Lat, 35.475, 35.55))

n_d2_1 = filter(chiba2_1, between(Lon, 139.85, 140) & between(Lat, 35.2, 35.3))

n_d2_2 = filter(chiba2_1, between(Lon, 139.91, 140.05) & between(Lat, 35.325, 35.375))
min(d2_2$Lon)

d2_3 = filter(chiba2_1, between(Lon, 139.92, 140.05) & between(Lat, 35.376, 35.425))
summary(d2_3)
d2_3$t_lo = ifelse(d2_3$Lon == min(d2_3$Lon), 0, 1)
d2_3$t_la = ifelse(d2_3$Lat == max(d2_3$Lat), 0, 1)
d2_3$sum = d2_3$t_lo+d2_3$t_la
n_d2_3 = d2_3[d2_3$sum != 0, ]
n_d2_3 = n_d2_3[, 1:15]

d2_4 = filter(chiba2_1, between(Lon, 139.951, 140.05) & between(Lat, 35.425, 35.49))
n_d2_4 = d2_4[d2_4$Lat == min(d2_4$Lat), ]
d2_4 = d2_4[d2_4$Lat != min(d2_4$Lat), ]
n_d2_4 = rbind(n_d2_4, d2_4[d2_4$Lat == min(d2_4$Lat), ])
n_d2_4 = rbind(n_d2_4, d2_4[d2_4$Lat == max(d2_4$Lat) & d2_4$Lon == max(d2_4$Lon), ])


na_list = rbind(n_d1_1, n_d1_2, n_d2_1, n_d2_2, n_d2_3, n_d2_4)
na_list = na_list[, c("Lon", "Lat")]
na_list$na = 0
na_list$site = paste(na_list$Lon, na_list$Lat, sep = "_")
na_list2 = data.frame(unique(na_list$site))
colnames(na_list2) = "site"
na_list2$na = 0

chiba3 = chiba2_1
chiba3$site = paste(chiba3$Lon, chiba3$Lat, sep = "_")
chiba3 = merge(chiba3, na_list2, by = "site", all = T)
chiba3$na[is.na(chiba3$na)] <- 1
new_chiba3 = chiba3[chiba3$na == 1, ] #site = 328
new_chiba3 = new_chiba3[, -c(1,2,17)]
write.csv(new_chiba3, "new_chiba3.csv", fileEncoding = "CP932")


require(maps)
require(mapdata)
require(ggplot2)

# p = ggplot() + coord_fixed() +
#   xlab("longitude") + ylab("latitude")
# world_map = map_data("world")
# jap = subset(world_map, world_map$region == "Japan")
# jap2 = subset(jap, lat > 35 & lat < 37 & long > 139.5 & long < 141)
# t2 = p + geom_polygon(data = jap2, aes(x=long, y=lat, group=group), colour="gray 50", fill=NA) + theme_bw()
# t2 + geom_point(data = new_chiba3, aes(x = Lon, y = Lat), shape = 16, size = 1)

p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
t2 + geom_point(data = new_chiba3, aes(x = Lon, y = Lat), shape = 16, size = 1)

