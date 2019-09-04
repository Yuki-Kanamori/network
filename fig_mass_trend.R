
a_data = c()
for(i in 1:7){
  sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[i]
  dirname = paste0("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50",sakana)
  setwd(dir = dirname)
  data = read.csv("Table_for_SS3.csv")
  data =  data %>%  
    mutate(season = ifelse(data$Category == 1, "AW", ifelse(data$Category == 2, "SA", ifelse(data$Category == 3, "SS", "WS"))),
           species = sakana)
  data$season = factor(data$season, levels = c("SS", "SA","AW","WS"))
  
  a_data = rbind(a_data, data)
  data = NULL
}


# g = ggplot(a_data, aes(x = Year, y = Estimate_metric_tons, group = season, colour = season))
# p = geom_point()
# e = geom_ribbon(aes(ymax = Estimate_metric_tons+SD_log, ymin = Estimate_metric_tons-SD_log),alpha=0.2)
# l = geom_line()
# f = facet_wrap(~ species, ncol = 1)
# g+p+e+l+f+theme_bw()


a_data = a_data %>% mutate(season2 = ifelse(a_data$season == "WS", "Winter", ifelse(a_data$season == "SS", "Spring", ifelse(a_data$season == "SA", "Summer", "Autumn"))))
a_data$season2 = factor(a_data$season2, levels = c("Winter","Spring","Summer","Autumn"))

g <- ggplot(a_data, aes(x = Year, y =  Estimate_metric_tons, fill = season2))
p <- geom_bar(stat = "identity", colour = "black")
lb <- labs(x = "Year", y = "Index of abundance", fill = "Season")
#s <- scale_fill_discrete(value = c("#56B4E9", "#009E73", "#E69F00", "#999999"))
s = scale_fill_manual(values = c("gray50", "#ff8082", "#4dc4ff", "gold"))
f <- facet_wrap( ~ species, ncol = 2, scales = "free")
th <- theme(axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
            axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
            axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
            axis.title.y = element_text(size = rel(1.5)),
            legend.title = element_text(size = 13), #凡例
            strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
            plot.title = element_text(size = rel(2.2))) #タイトル
x <- scale_x_continuous(breaks=c(1978, 1988, 1998, 2008, 2018))
g+p+f+x+lb+theme_bw()+th+s

tumu = g+p+f+x+lb+s+theme_bw()+th
ggsave(filename = "tumutumu.pdf", plot = tumu, units = "in", width = 11.69, height = 8.27)
