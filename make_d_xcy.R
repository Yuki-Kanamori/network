require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)

# function to get the local density in each knot from Save.RData -------------------------------
est_index = function(){
  load("Save.RData")
  n_c = Save$TmbData$n_c #category (month or season)
  n_t = Save$TmbData$n_t #year
  n_x = Save$TmbData$n_x #knot
  est_d = c()
  for(i in 1:n_c){
    m = log(Save$Report$D_xcy[,i,])
    #m = log(Save$Report$Index_xcyl[,i,,]) #if abundance index needed
    #m = log(Save$Report$Index_xcyl[,i,,]*1000) #if not tons
    est_d = rbind(est_d, m)
  }
  est_d = data.frame(est_d)
  
  est_d = est_d %>% mutate(knot_i = rep(1:n_x, n_c), n_season = rep(1:n_c, each = n_x))
  est_d = est_d %>% tidyr::gather(key = x_year, value = log_abundance, 1:n_t)
  tag = data_frame(x_year = paste0("X", rep(1:n_t)), year = rep(1990:2018))
  est_d = merge(est_d, tag, by = "x_year")
  
  if(sakana == "isigarei"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50isigarei")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "konosiro"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50konosiro")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "kouika"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50kouika")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "kurumaebi"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50kurumaebi")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "maanago"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50maanago")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "makogarei"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50makogarei")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  if(sakana == "suzuki"){
    #setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50suzuki")
    space_data = read.csv("Data_Geostat.csv")
    space_data = space_data[, c("Lat", "Lon", "knot_i")]
    space_data = unique(space_data)
    space_data = ddply(space_data, .(knot_i), summarize, m_Lon = mean(Lon), m_Lat = mean(Lat))
  }
  
  est_d = merge(est_d, space_data, by = "knot_i")
  est_d = mutate(est_d, season = ifelse(est_d$n_season == 1, "AW", ifelse(est_d$n_season == 2, "SA", ifelse(est_d$n_season == 3, "SS", "WS"))),
                 species = sakana)
}


# change here ---------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50isigarei")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[1]
est_d_isi = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50konosiro")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[2]
est_d_kono = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50kouika")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[3]
est_d_ika = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50kurumaebi")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[4]
est_d_ebi = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50maanago")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[5]
est_d_ana = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50makogarei")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[6]
est_d_mako = est_index()

setwd("/Users/Yuki/Dropbox/Network/revised_data/vast2019-08-21_lognorm_log50suzuki")
sakana = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[7]
est_d_suzu = est_index()

est_d_tok1 = rbind(est_d_isi, est_d_kono, est_d_ika, est_d_ebi, est_d_ana, est_d_mako, est_d_suzu)
setwd("/Users/Yuki/Dropbox/Network/revised_data")
write.csv(est_d_tok1, "est_d_tok1.csv")


# check for definition of season name by comparing Save$Report$Index_xcyl[,i,,] with VAST output figure---------------------------
# change m = ... in the function
test = ddply(est_d_isi, .(year, season), summarize, Index = sum(exp(abundance)))

g = ggplot(test, aes(x = year, y = Index))
p = geom_point()
l = geom_line()
f = facet_wrap(~ season, ncol = 1, scales = "free")
lab = labs(x = "year", y = "Index", title = "isigarei")
g+p+l+f+lab+theme_bw()



