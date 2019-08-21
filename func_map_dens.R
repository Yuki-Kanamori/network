require(dplyr)
require(maps)
require(mapdata)
require(ggplot2)


# function to map the local density -------------------------------------

map_density = function(data){
  for(i in 1:4){
    kisetu = c("AW", "SA", "SS", "WS")[i]
    
    p = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
    world_map = map_data("world")
    jap = subset(world_map, world_map$region == "Japan")
    jap2 = jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
    bay = p + geom_polygon(data = jap2, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))
    th = theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(size = rel(1), angle = 90),
               axis.text.y = element_text(size = rel(1)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.title = element_text(size = 13))
    p = filter(data, season == kisetu) %>% geom_point(data = ., aes(x = m_Lon, y = m_Lat, colour = log_abundance), shape = 16, size = 2)
    f = facet_wrap( ~ year, ncol = 8)
    c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
    lab = labs(title = paste(sakana, kisetu, sep = "_"))
    fig = bay+theme_bw()+th+p+f+c+lab
    
    ggsave(filename = paste0("map_", sakana, "_", kisetu, ".pdf"), plot = fig, units = "in", width = 11.69, height = 8.27)
  }
}

setwd("/Users/Yuki/Dropbox/Network/revised_data")
for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  data = read.csv("est_d_tok1.csv") %>% filter(species == sakana)
  map_density(data)
}