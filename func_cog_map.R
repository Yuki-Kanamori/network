require(VAST)
require(rgdal)
require(maps)
require(mapdata)
require(ggplot2)


# please change here --------------------------------------------
#set directory which contains the Save.RData estimated by VAST
for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  dirname = paste0("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50",sakana)
  
  year_set = seq(1990, 2018)
  category_name = c("AW","SA","SS","WS")
  # category_name = factor(category_name, levels = c("SS","SA","AW","WS"))
  # levels(category_name)
  fileEncoding = "CP932"
  package = c("SpatialDeltaGLMM", "FishStatsUtils")[2]
  zone = 54 #it was estimated when "Extrapolation_List" was made (zone range in Japan is 51:56)
  
  
  #figure setting
  labs = labs(title = sakana, x = "Longitude", y = "Latitude", colour = "Year")
  ncol = 4 #number of figures in line side by side (max is no. of "Category")
  shape = 16 #16 is closed dot
  size = 2 #size of shape
  
  # make a COG_Table and maps of COG ------------------------------
  cog_map = function(Save, package){
    #load Save.RData estimated by VAST
    setwd(dir = dirname)
    load("Save.RData")
    DateFile = dirname
    
    #make a COG_Table using VAST package
    #!! re-make default figures of COG and Effective Area !!
    if(package == "SpatialDeltaGLMM"){
      require(SpatialDeltaGLMM)
      COG = SpatialDeltaGLMM::Plot_range_shifts(Report = Save$Report, 
                                                TmbData = Save$TmbData, 
                                                Sdreport = Save$Opt$SD,
                                                Znames = colnames(Save$TmbData$Z_xm),
                                                PlotDir = DateFile,
                                                use_biascorr = TRUE,
                                                Year_Set = year_set,
                                                category_names = category_name)$COG_Table
    }else{
      COG = FishStatsUtils::plot_range_index(Report = Save$Report, 
                                             TmbData = Save$TmbData, 
                                             Sdreport = Save$Opt$SD,
                                             Znames = colnames(Save$TmbData$Z_xm),
                                             PlotDir = DateFile,
                                             use_biascorr = TRUE,
                                             Year_Set = year_set,
                                             category_names = category_name)$COG_Table
    }
    
    write.csv(COG, "COG_Table.csv", fileEncoding = fileEncoding)
    
    #from UTM to longitude and latitude
    cog = read.csv("COG_Table.csv", fileEncoding = fileEncoding)
    
    tag = data.frame(rep(year_set, each = length(category_name)))
    tag$Category = rep(category_name)
    colnames(tag) = c("Year", "Category")
    #tag$Category = factor(tag$Category, levels = category_name)
    #levels(tag$Category)
    cog <- merge(cog, tag, by = c("Category", "Year"))
    
    lat = cog[cog$m == 1, ]
    lon = cog[cog$m == 2, ]
    x = lat$COG_hat*1000
    y = lon$COG_hat*1000
    xy = cbind(x,y)
    lonlat = data.frame(project(xy, paste0("+proj=utm +zone=", zone, " ellps=WGS84"), inv = TRUE))
    colnames(lonlat) = c("lon", "lat")
    lonlat = cbind(lonlat, lat[, c("Year", "Category")])
    lonlat$Category = factor(lonlat$Category, levels = category_name)
    levels(lonlat$Category)
    lonlat$Category = factor(lonlat$Category, levels = c("SS","SA","AW","WS"))
    
    #make COG maps
    map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
    world_map = map_data("world")
    jap = subset(world_map, world_map$region == "Japan")
    jap_map = map + geom_polygon(data = jap, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + coord_map(xlim = c(min(lonlat$lon)-0.3, max(lonlat$lon)+0.2), ylim = c(min(lonlat$lat)-0.5, max(lonlat$lat)+0.2))
    th = theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_text(size = rel(1.5)),
               axis.text.y = element_text(size = rel(1.5)),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               legend.title = element_text(size = 13))
    p = geom_point(data = lonlat, aes(x = lon, y = lat, colour = Year), shape = shape, size = size)
    f = facet_wrap( ~ Category, ncol = ncol)
    c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
    fig = jap_map+theme_bw()+th+p+f+c+labs
    ggsave(filename = paste0("COG_map_",sakana,".pdf"), plot = fig, units = "in", width = 11.69, height = 8.27)
  }
  
  
  # run function --------------------------------------------------
  cog_map(Save, package)
}

