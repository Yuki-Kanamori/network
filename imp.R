setwd("/Users/Yuki/Dropbox/Network/revised_data")

isi = read.csv("isi_imp.csv") %>% mutate(species = "Ishigarei")
kono = read.csv("kono_imp.csv") %>% mutate(species = "Konoshiro")
ika = read.csv("ika_imp.csv") %>% mutate(species = "Kouika")
ebi = read.csv("ebi_imp.csv") %>% mutate(species = "Kurumaebi")
ana = read.csv("ana_imp.csv") %>% mutate(species = "Maanago")
mako = read.csv("mako_imp.csv") %>% mutate(species = "Makogarei")
suzu = read.csv("suzu_imp.csv") %>% mutate(species = "Suzuki")

imp = rbind(isi,kono,ika,ebi,ana,mako,suzu)
write.csv(tag, "tag.csv")
imp = merge(imp, tag, by = "n_season")
# imp$season = factor(imp$season, levels = c("SS", "SA","AW","WS"))
imp$season2 = factor(imp$season2, levels = c("Winter", "Spring","Summer","Autumn"))
imp = imp %>% select(Feature, Gain, species, season2) %>% arrange(species, season2, desc(Gain))
write.csv(imp, "imp.csv")

mode(imp$Gain)
g = ggplot(imp, aes(x = Feature, y = Gain, fill = Gain))
b = geom_bar(stat = "identity")
f = facet_grid(species ~ season2)
lab = labs(y = "Importance", colour = "")
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2), angle = 90, hjust = 1),
  axis.text.y = element_text(size = rel(1.2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
c = scale_fill_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red"))
g+b+f+theme_bw()+th+lab+c
ggsave("imp.pdf", g+b+f+theme_bw()+th+lab+c, width = 11.96, height = 8.27)


