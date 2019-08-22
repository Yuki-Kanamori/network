setwd("/Users/Yuki/Dropbox/Network/revised_data")

isi = read.csv("isi_imp.csv")
kono = read.csv("kono_imp.csv")
ika = read.csv("ika_imp.csv")
ebi = read.csv("ebi_imp.csv")
ana = read.csv("ana_imp.csv")
mako = read.csv("mako_imp.csv")
suzu = read.csv("suzu_imp.csv")

imp = rbind(isi,kono,ika,ebi,ana,mako,suzu)
imp$season = factor(imp$season, levels = c("SS", "SA","AW","WS"))

g = ggplot(imp, aes(x = Feature, y = Gain))
b = geom_bar(stat = "identity", colour = "gray30")
f = facet_grid(species ~ season)
lab = labs(y = "Importance")
th = theme(axis.text.x = element_text(angle = 90, hjust = 1))
g+b+f+theme_bw()+th+lab
ggsave("imp.pdf", g+b+f+theme_bw()+th+lab, width = 11.96, height = 8.27)
