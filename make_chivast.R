
# 0. Prepare the data -------------------------------------------
dirname = "/Users/Yuki/Dropbox/Network/revised_data"
setwd(dir = dirname)

# Packages
require(TMB)
require(VAST)
require(tidyverse)
require(plyr)

# read the data
df = read.csv("new_chiba3.csv", fileEncoding = "CP932")
summary(df)
df = df %>% 
  select(Lat, Lon, CPUE, GEAR, FISH, Y, M) %>%
  dplyr::rename(lon = Lon, lat = Lat, year = Y, month = M, gear = GEAR, fish = FISH)
summary(df)

dirname = "/Users/Yuki/Dropbox/Network"
setwd(dir = dirname)
df2 = read.csv("added_data.csv", fileEncoding = "CP932")
summary(df2)
df2 = df2 %>%
  select(year, month, lon, lat, CPUE, gear, species) %>%
  dplyr::rename(fish = species) %>%
  filter(between(lon, min(df$lon), max(df$lon)), between(lat, min(df$lat), max(df$lat)))
df2$fish = ifelse(df2$fish == "akakamasu", "kamasu spp.", ifelse(df2$fish == "kamasu spp.", "kamasu spp.", ifelse(df2$fish == "kurodai", "kurodai", ifelse(df2$fish == "siroguti", "siroguti", ifelse(df2$fish == "torafugu", "torafugu", NA)))))
summary(df2)
na_check = df2 %>% filter(fish == "torafugu") %>% arrange(year)


df2 = na.omit(df2)

df3 = rbind(df, df2)
summary(df3)
levels(df3$fish)



df4 = ddply(df3, .(year, fish), summarize, mean = mean(CPUE))
df5 = ddply(df3, .(year, fish), summarize, sum = sum(CPUE)) %>% arrange(fish, year)

summary(df4)
dirname = "/Users/Yuki/Dropbox/Network/revised_data"
setwd(dir = dirname)
write.csv(df3, "chivast.csv", fileEncoding = "CP932")
df4 = df4 %>% arrange(fish, year)

g = ggplot(df4, aes(x = year, y = mean), group = fish)
b = geom_bar(stat = "identity")
# p = geom_point()
# l = geom_line()
f = facet_wrap(~ fish, ncol = 4, scales = "free")
labs = labs(title = "Nominal", x = "Year", y = "Mean CPUE")
#g+p+l+f+labs+theme_bw()
g+b+f+labs+theme_bw()+scale_x_continuous(breaks=seq(1990,2019,5))

