setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
###chiba
cb <- read.csv("kono_chiba.csv", fileEncoding = "CP932")
require(dplyr)
cb <- cb %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(cb)))
require(stringr)
cb$Date <- as.numeric(str_sub(cb$Date, start = 2, end = 9))
cb <- cb %>% mutate(Year = str_sub(cb$Date, 1, 4), Month = str_sub(cb$Date, 5, 6), Day = str_sub(cb$Date, 7, 8))
mode(cb$Site)
mode(cb$Reads)
mode(cb$Year)
summary(cb$Site) #FTのデータあり
cb$Site <- as.character(as.factor(cb$Site))
###kanagawa
kn <- read.csv("kono_kana.csv", fileEncoding = "CP932")
kn <- kn %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(kn)))
kn$Date <- as.numeric(str_sub(kn$Date, start = 2, end = 9))
kn <- kn %>% mutate(Year = str_sub(kn$Date, 1, 4), Month = str_sub(kn$Date, 5, 6), Day = str_sub(kn$Date, 7, 8))
mode(kn$Site)
mode(kn$Reads)
mode(kn$Year)
kn$Site <- as.character(as.factor(kn$Site))
edna <- rbind(cb, kn)
mode(edna$Year)
edna$Depth <- factor(edna$Depth, levels = c("Surface", "Bottom"))
#summary(edna$Site) #FTのデータが無い
#ft <- edna[edna$Site == "FT", ]
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
ft <- site[site$Site == "FT", ]
edna <- merge(edna, site, by = "Site")
#####yearly pattern#####
#edna <- transform(edna, Loc = paste(Lon, Lat, sep = "_"))
mode(edna$Copies)
year <- plyr::ddply(edna, .(Year, Summary, Depth, Site), summarize, mean = mean(log(Copies)))
mode(year$Year)
year$Year <- as.numeric(year$Year)
#####Map#####
require(maps)
require(mapdata)
require(ggplot2)
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))

year <- merge(year, site, by = "Site")
year_a <- subset(year, Year == 2018)
mode(year_a$Site)
year_a$Site <- as.factor(year_a$Site)

p <- geom_point(data = filter(year_a, Summary == "Average"), aes(x = Lon, y = Lat, colour = exp(mean)), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Log number of copies\n(yearly mean)", title = "eDNA(Konoshiro)")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1.5), angle = 90), #x軸メモリ
            axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
            axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
            axis.title.y = element_text(size = rel(1.5)),
            legend.title = element_text(size = 10))
k = t2+p+f+s+lb+theme_bw()+th
ggsave("kono_copy.pdf", k, width = 11.69, height = 8.27)

#####suzuki#####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
###chiba
cb <- read.csv("suzu_chiba.csv", fileEncoding = "CP932")
require(dplyr)
cb <- cb %>% tidyr::gather(key = Date, value = Reads, 5:max(ncol(cb)))
require(stringr)
cb$Date <- as.numeric(str_sub(cb$Date, start = 2, end = 9))
cb <- cb %>% mutate(Year = str_sub(cb$Date, 1, 4), Month = str_sub(cb$Date, 5, 6), Day = str_sub(cb$Date, 7, 8))
mode(cb$Site)
mode(cb$Reads)
mode(cb$Year)
summary(cb)
summary(cb$Site) #FTのデータあり
cb$Site <- as.character(as.factor(cb$Site))
cb2 = filter(cb, Summary == "Average") %>% na.omit()

#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
edna <- merge(cb2, site, by = "Site")
summary(edna)

#####yearly pattern#####
#edna <- transform(edna, Loc = paste(Lon, Lat, sep = "_"))
mode(edna$Reads)
require(plyr)
year <- ddply(edna, .(Year, Summary, Depth, Site), summarize, mean = mean(log(Reads)))
mode(year$Year)
year$Year <- as.numeric(year$Year)
#####Map#####
require(maps)
require(mapdata)
require(ggplot2)
p <- ggplot() + coord_fixed() +
  xlab("Longitude") + ylab("Latitude")
world_map <- map_data("world")
jap <- subset(world_map, world_map$region == "Japan")
jap_cog <- jap[jap$lat > 35 & jap$lat < 38 & jap$long > 139 & jap$long < 141, ]
t2 <- p + geom_polygon(data = jap_cog, aes(x=long, y=lat, group=group), colour="gray 50", fill="gray 50")+ coord_map(xlim = c(139.5, 140.3), ylim = c(35, 35.75))

year <- merge(year, site, by = "Site")
year_a <- subset(year, Year == 2018)
mode(year_a$Site)
year_a$Site <- as.factor(year_a$Site)

p <- geom_point(data = year_a, aes(x = Lon, y = Lat, colour = exp(mean)), shape = 16, size = 5)
f = facet_wrap(~ Depth)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Log number of copies\n(yearly mean)", title = "eDNA (Suzuki)")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1.5), angle = 90), #x軸メモリ
            axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
            axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
            axis.title.y = element_text(size = rel(1.5)),
            legend.title = element_text(size = 10))
s = t2+p+f+s+lb+th+theme_bw()
ggsave("suzu_copy.pdf", s, width = 11.69, height = 8.27)

#####pre-abs data#####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
pa <- read.table("countOTUv2.fish.txt", header = T)
#各調査地点での調査回数を調べた
t <- pa[pa$Site=="27", ]
length(unique(t$Date))

pa <- pa %>% mutate(Year = str_sub(pa$Date, 1, 2), Month = str_sub(pa$Date, 3, 4), Day = str_sub(pa$Date, 5, 6))
head(pa,1)
require(dplyr)
pa <- select(pa, "Year", "Month", "Day", "Site", "Depth", "OTU1", "OTU120", "OTU8", "OTU142")

pa <- pa %>% tidyr::gather(key = Species, value = Count, 6:max(ncol(pa)))

pa$Species <- ifelse(pa$Species == "OTU1", "Konoshiro", ifelse(pa$Species == "OTU8", "Suzuki", ifelse(pa$Species == "OTU120", "Makogarei", "Maanago")))
pa$Depth <- ifelse(pa$Depth == "s", "Surface", "Bottom")
pa$Depth <- factor(pa$Depth, levels = c("Surface", "Bottom"))
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
pa <- merge(pa, site, by = "Site")
summary(pa$Count)
pa$Count2 <- ifelse(pa$Count > 0, 1, 0)
sum <- ddply(pa, .(Year, Depth, Site, Species), summarize, sum = sum(Count2))
sum <- merge(sum, site, by = "Site")
sum$Freq <- sum$sum/sum$times

###マアナゴ
p <- geom_point(data = filter(sum, Species == "Maanago"), aes(x = Lon, y = Lat, colour = Freq), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Appearance ratio", title = "Maanago")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            #strip.text.y = element_text(size = 100),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###スズキ
p <- geom_point(data = filter(sum, Species == "Suzuki"), aes(x = Lon, y = Lat, colour = Freq), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Appearance ratio", title = "Suzuki")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###マコガレイ
p <- geom_point(data = filter(sum, Species == "Makogarei"), aes(x = Lon, y = Lat, colour = Freq), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Appearane ratio", title = "Makogarei")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###コノシロ
p <- geom_point(data = filter(sum, Species == "Konoshiro"), aes(x = Lon, y = Lat, colour = Freq), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Appearance ratio", title = "Konoshiro")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()


#####リード数#####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
pa <- read.table("countOTUv2.fish.txt", header = T)
#各調査地点での調査回数を調べた
t <- pa[pa$Site=="27", ]
length(unique(t$Date))

pa <- pa %>% mutate(Year = str_sub(pa$Date, 1, 2), Month = str_sub(pa$Date, 3, 4), Day = str_sub(pa$Date, 5, 6))
head(pa,1)
require(dplyr)
pa <- select(pa, "Year", "Month", "Day", "Site", "Depth", "OTU1", "OTU120", "OTU8", "OTU142")
pa <- pa %>% tidyr::gather(key = Species, value = Count, 6:max(ncol(pa)))
pa$Species <- ifelse(pa$Species == "OTU1", "Konoshiro", ifelse(pa$Species == "OTU8", "Suzuki", ifelse(pa$Species == "OTU120", "Makogarei", "Maanago")))
pa$Depth <- ifelse(pa$Depth == "s", "Surface", "Bottom")
pa$Depth <- factor(pa$Depth, levels = c("Surface", "Bottom"))
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
site$times <- c(16,16,16,16,16,16,16,16,16,16,10,10,10,10)
pa <- merge(pa, site, by = "Site")
summary(pa$Count)
mean <- ddply(pa, .(Year, Depth, Site, Species), summarize, Mean = exp(mean(log(Count+0.01))))
mean <- merge(mean, site, by = "Site")

###マアナゴ
p <- geom_point(data = filter(mean, Species == "Maanago"), aes(x = Lon, y = Lat, colour = Mean), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Number of reads\n(yearly mean)", title = "Maanago")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            #strip.text.y = element_text(size = 100),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###スズキ
p <- geom_point(data = filter(mean, Species == "Suzuki"), aes(x = Lon, y = Lat, colour = Mean), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Number of reads\n(yearly mean)", title = "Suzuki")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###マコガレイ
p <- geom_point(data = filter(mean, Species == "Makogarei"), aes(x = Lon, y = Lat, colour = Mean), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Number of reads\n(yearly mean)", title = "Makogarei")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

###コノシロ
p <- geom_point(data = filter(mean, Species == "Konoshiro"), aes(x = Lon, y = Lat, colour = Mean), shape = 16, size = 5)
f <- facet_wrap( ~ Depth, ncol = 2)
s <- scale_colour_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red"))
lb <- labs(color = "Number of reads\n(yearly mean)", title = "Konoshiro")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
t2+p+f+s+lb+th+theme_bw()

#####リード数とコピー数の関係(平均)#####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
pa <- read.table("countOTUv2.fish.txt", header = T)
#各調査地点での調査回数を調べた
t <- pa[pa$Site=="27", ]
length(unique(t$Date))

pa <- pa %>% mutate(Year = str_sub(pa$Date, 1, 2), Month = str_sub(pa$Date, 3, 4), Day = str_sub(pa$Date, 5, 6))
head(pa,1)
require(dplyr)
pa <- select(pa, "Year", "Month", "Day", "Site", "Depth", "OTU1", "OTU120", "OTU8", "OTU142")
pa <- pa %>% tidyr::gather(key = Species, value = Count, 6:max(ncol(pa)))
pa$Species <- ifelse(pa$Species == "OTU1", "Konoshiro", ifelse(pa$Species == "OTU8", "Suzuki", ifelse(pa$Species == "OTU120", "Makogarei", "Maanago")))
pa$Depth <- ifelse(pa$Depth == "s", "Surface", "Bottom")
pa$Depth <- factor(pa$Depth, levels = c("Surface", "Bottom"))
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
site$times <- c(16,16,16,16,16,16,16,16,16,16,10,10,10,10)
pa <- merge(pa, site, by = "Site")
summary(pa$Count)
read <- ddply(pa, .(Year, Depth, Site, Species), summarize, Mean = exp(mean(log(Count+0.01))))
read <- merge(read, site, by = "Site")

####コピー数####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
###chiba
cb <- read.csv("kono_chiba.csv", fileEncoding = "CP932")
require(dplyr)
cb <- cb %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(cb)))
require(stringr)
cb$Date <- as.numeric(str_sub(cb$Date, start = 2, end = 9))
cb <- cb %>% mutate(Year = str_sub(cb$Date, 1, 4), Month = str_sub(cb$Date, 5, 6), Day = str_sub(cb$Date, 7, 8))
mode(cb$Site)
mode(cb$Reads)
mode(cb$Year)
summary(cb$Site) #FTのデータあり
cb$Site <- as.character(as.factor(cb$Site))
###kanagawa
kn <- read.csv("kono_kana.csv", fileEncoding = "CP932")
kn <- kn %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(kn)))
kn$Date <- as.numeric(str_sub(kn$Date, start = 2, end = 9))
kn <- kn %>% mutate(Year = str_sub(kn$Date, 1, 4), Month = str_sub(kn$Date, 5, 6), Day = str_sub(kn$Date, 7, 8))
mode(kn$Site)
mode(kn$Reads)
mode(kn$Year)
kn$Site <- as.character(as.factor(kn$Site))
edna <- rbind(cb, kn)
mode(edna$Year)
edna$Depth <- factor(edna$Depth, levels = c("Surface", "Bottom"))
#summary(edna$Site) #FTのデータが無い
#ft <- edna[edna$Site == "FT", ]
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
ft <- site[site$Site == "FT", ]
edna <- merge(edna, site, by = "Site")
#####yearly pattern#####
#edna <- transform(edna, Loc = paste(Lon, Lat, sep = "_"))
mode(edna$Copies)
year <- plyr::ddply(edna, .(Year, Summary, Depth, Site), summarize, Mean = exp(mean(log(Copies))))
mode(year$Year)
year$Year <- as.numeric(year$Year)
year <- merge(year, site, by = "Site")
year_a <- subset(year, Year == 2018)
mode(year_a$Site)
year_a$Site <- as.factor(year_a$Site)

copy <- year_a[year_a$Summary == "Average", ]
read2 <- read[read$Species == "Konoshiro", ]
head(copy,1)
head(read2,1)
colnames(copy)[5] <- "Mean_copy"
colnames(read2)[5] <- "Mean_read"
rel <- merge(copy, read2, by = c("Site", "Depth"))

g <- ggplot(data = rel, aes(x = Mean_copy, y = Mean_read))
p <- geom_point()
f <- facet_wrap( ~ Depth, ncol = 2, scales = "free")
lb <- labs(x = "Number of copies\n(yearly mean)", y = "Number of reads \n(yearly mean)", title = "Konoshiro")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
g+p+f+lb+th+theme_bw()

#####リード数とコピー数の関係#####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
pa <- read.table("countOTUv2.fish.txt", header = T)
#各調査地点での調査回数を調べた
t <- pa[pa$Site=="27", ]
length(unique(t$Date))
require(dplyr)
require(stringr)
pa <- pa %>% mutate(Year = str_sub(pa$Date, 1, 2), Month = str_sub(pa$Date, 3, 4), Day = str_sub(pa$Date, 5, 6))
head(pa,1)
mode(pa$Date)
pa$Date <- as.numeric(pa$Date)
pa$Date <- pa$Date+20000000
pa <- select(pa, "Date","Year", "Month", "Day", "Site", "Depth", "OTU1", "OTU120", "OTU8", "OTU142")
pa <- pa %>% tidyr::gather(key = Species, value = Count, 7:max(ncol(pa)))
pa$Species <- ifelse(pa$Species == "OTU1", "Konoshiro", ifelse(pa$Species == "OTU8", "Suzuki", ifelse(pa$Species == "OTU120", "Makogarei", "Maanago")))
pa$Depth <- ifelse(pa$Depth == "s", "Surface", "Bottom")
pa$Depth <- factor(pa$Depth, levels = c("Surface", "Bottom"))
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
site$times <- c(16,16,16,16,16,16,16,16,16,16,10,10,10,10)
pa <- merge(pa, site, by = "Site")
summary(pa$Count)

####コピー数####
setwd(dir = "/Users/Yuki/Dropbox/Network/eDNA")
#####eDNA data#####
###chiba
cb <- read.csv("kono_chiba.csv", fileEncoding = "CP932")
require(dplyr)
cb <- cb %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(cb)))
require(stringr)
cb$Date <- as.numeric(str_sub(cb$Date, start = 2, end = 9))
cb <- cb %>% mutate(Year = str_sub(cb$Date, 1, 4), Month = str_sub(cb$Date, 5, 6), Day = str_sub(cb$Date, 7, 8))
mode(cb$Site)
mode(cb$Reads)
mode(cb$Year)
summary(cb$Site) #FTのデータあり
cb$Site <- as.character(as.factor(cb$Site))
###kanagawa
kn <- read.csv("kono_kana.csv", fileEncoding = "CP932")
kn <- kn %>% tidyr::gather(key = Date, value = Copies, 5:max(ncol(kn)))
kn$Date <- as.numeric(str_sub(kn$Date, start = 2, end = 9))
kn <- kn %>% mutate(Year = str_sub(kn$Date, 1, 4), Month = str_sub(kn$Date, 5, 6), Day = str_sub(kn$Date, 7, 8))
mode(kn$Site)
mode(kn$Reads)
mode(kn$Year)
kn$Site <- as.character(as.factor(kn$Site))
edna <- rbind(cb, kn)
mode(edna$Year)
edna$Depth <- factor(edna$Depth, levels = c("Surface", "Bottom"))
#summary(edna$Site) #FTのデータが無い
#ft <- edna[edna$Site == "FT", ]
#####sampling location#####
site <- read.table("sampling_points.txt", header = T)
site <- site[, -1]
head(site, 1)
colnames(site) <- c("Lon", "Lat", "Site")
ft <- site[site$Site == "FT", ]
edna <- merge(edna, site, by = "Site")
#####yearly pattern#####
mode(edna$Copies)
mode(edna$Year)
edna$Year <- as.numeric(edna$Year)
year <- merge(edna, site, by = "Site")
year_a <- subset(year, Year == 2018)
mode(year_a$Site)
year_a$Site <- as.factor(year_a$Site)

copy <- year_a[year_a$Summary == "Average", ]
read2 <- pa[pa$Species == "Konoshiro", ]

head(copy,1)
head(read2,1)
rel <- merge(copy, read2, by = c("Site", "Depth", "Date"))
mode(rel$Site)
rel$Site <- as.character(as.factor(rel$Site))
rel$Site <- factor(rel$Site, levels = c("FT", "56", "J5", "6", "54", "UY", "FB", "2", "AZ", "BB", "129", "134", "136", "27"))
rel_b <- rel %>% filter(Depth == "Bottom" & Copies < 7500)
summary(rel_b)
rel_s <- rel %>% filter(Depth == "Surface" & Copies < 10000)
rel <- rbind(rel_b, rel_s)

require(ggplot2)
g <- ggplot(data = rel, aes(x = Copies, y = Count, colour = Site))
p <- geom_point()
f <- facet_wrap( ~ Depth, ncol = 2, scales = "free")
lb <- labs(x = "Number of copies", y = "Number of reads", title = "Konoshiro")
th <- theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1), #x軸メモリ
            axis.text.y = element_text(size = rel(1)), #y軸メモリ
            axis.title.x = element_text(size = rel(1)), #x軸タイトル
            axis.title.y = element_text(size = rel(1)),
            legend.title = element_text(size = 10))
g+p+f+lb+th+theme_bw()

