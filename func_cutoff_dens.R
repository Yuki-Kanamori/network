# the Tokyo Bay -------------------------------------------------
dirname = "/Users/Yuki/Dropbox/Network"
setwd(dir = dirname)
# df = read.csv("new_chiba3.csv")
# require(plyr)
# head(df)
# min = ddply(df, .(M, FISH), summarize, min = mean(CATCH))
# head(min)
# colnames(min) = c("Month", "Sp", "min")
# write.csv(min, "min.csv", fileEncoding = "CP932")



cutoff_dens = function(data, Sp){
  require(plyr)
  require(dplyr)
  
  raw = data[["raw"]]
  posit_enc = raw[raw$sum_enc > 0, ]
  zero_enc = raw[raw$sum_enc == 0, ]
  
  min = read.csv("min.csv")
  min = min[, -1]
  
  if(Sp == "isigarei"){
    min = filter(min, Sp == "isigarei")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  
  if(Sp == "konosiro"){
    #zero_enc$rlnorm = ifelse(zero_enc$rlnorm < 0.10507174, 0, zero_enc$rlnorm)
    min = filter(min, Sp == "konosiro")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  if(Sp == "kouika"){
    #zero_enc$rlnorm = ifelse(zero_enc$rlnorm < 0.15947467, 0, zero_enc$rlnorm)
    min = filter(min, Sp == "kouika")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  if(Sp == "kurumaebi"){
    #zero_enc$rlnorm = ifelse(zero_enc$rlnorm < 0.06801183, 0, zero_enc$rlnorm)
    min = filter(min, Sp == "kurumaebi")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  if(Sp == "maanago"){
    #zero_enc$rlnorm = ifelse(zero_enc$rlnorm < 0.06801183, 0, zero_enc$rlnorm)
    min = filter(min, Sp == "maanago")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  if(Sp == "suzuki"){
    #zero_enc$rlnorm = ifelse(zero_enc$rlnorm < 0.06801183, 0, zero_enc$rlnorm)
    min = filter(min, Sp == "suzuki")
    zero_enc = merge(zero_enc, min, by = c("Month", "Sp"))
    
    zero_enc$rlnorm = ifelse(zero_enc$rlnorm <= zero_enc$min, 0, zero_enc$rlnorm)
    zero_enc = zero_enc[, colnames(zero_enc) != "min"]
    
    df = rbind(posit_enc, zero_enc)
    arithmean_dens = ddply(df, .(Year, Month, Sp, knot_i), summarize, mean_dens = mean(rlnorm))
    
  }
  return(arithmean_dens)
}

load("isi3_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[1]
isi3 = cutoff_dens(isi3_glmm, Sp)
summary(isi1)

load("kono3_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[2]
kono3 = cutoff_dens(kono3_glmm, Sp)

load("ika3_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[3]
ika3 = cutoff_dens(ika3_glmm, Sp)

load("ebi3_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[4]
ebi3 = cutoff_dens(ebi3_glmm, Sp)

load("ana3_glmm.Rdata")
ana3_glmm = sar3_glmm
Sp = c("isigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[5]
ana3 = cutoff_dens(ana3_glmm, Sp)

tok3 = rbind(isi3, kono3, ika3, ebi3, ana3)

# データ補完まだやってない種 --------------------------------------------------
load("mako1_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "ebi", "maanago", "makogarei", "suzuki")[6]
mako1 = cutoff_dens(mako1_glmm, Sp)

load("suzu1_glmm.Rdata")
Sp = c("isigarei", "konosiro", "kouika", "ebi", "maanago", "makogarei", "suzuki")[7]
suzu1 = cutoff_dens(suzu1_glmm, Sp)


tok1 = rbind(isi1, kono1, ika1, ebi1, ana1, mako1, suzu1)
summary(tok1)

# glmmデータに緯度経度を再度与える --------------------------------------------
setwd(dir = "/Users/Yuki/Dropbox/Network")
DG = read.csv("Data_Geostat_na.csv")
head(DG)
require(plyr)
knot = ddply(DG, .(knot_i), summarize, m_lon = mean(Lon), m_lat = mean(Lat))
summary(knot)
tok3 = merge(tok3, knot, by = "knot_i")
summary(tok1)
write.csv(tok3, "tok_glmm3.csv", fileEncoding = "CP932")
