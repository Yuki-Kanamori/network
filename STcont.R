rm(list = ls())

require(plyr)
require(dplyr)
require(xgboost)
require(Matrix)
require(mlr)
require(caret)
require(EIX)
require(car)
require(doParallel)
detectCores() # 4-cores
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

# isigarei -------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
isi = read.csv("boost_isi.csv", fileEncoding = "CP932")
isi = isi[, -1]
summary(isi)
isi = isi %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_isi = isi %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_isi = na.omit(df_mat_isi)
summary(df_isi)

set.seed(0)

load("tuned_params_isi.RData")

best_isi = train_isi$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_isi$eta,
  gamma             = best_isi$gamma,
  max_depth         = best_isi$max_depth,
  min_child_weight  = best_isi$min_child_weight,
  subsample         = best_isi$subsample,
  colsample_bytree  = best_isi$colsample_bytree
)


# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("isi", i),
         isi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("isi", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_isi", i),
         data[sprit, ])
  assign(paste0("te_isi", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_isi", i))
  te_data = get(paste0("te_isi", i))
  
  assign(paste0("tr_isi", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_isi", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_isi1)

for(i in 1:4){
  data = get(paste0("isi", i))
  
  assign(paste0("isi_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_isi", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_isi$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_isi", i, ".RData"))
  assign(paste0("model_isi", i),
         model)
}

pred_isi = NULL
for(j in 1:4){
    model = get(paste0("model_isi", j))
    data = get(paste0("isi_norm", j))
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_full", "obs")
    pred = cbind(pred, isi %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
    pred_isi = rbind(pred_isi, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
    for(i in 2:7){
    data = get(paste0("tr_isi", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_isi$nrounds
    
    assign(paste0("model_isi_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_isi_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_isi_x", i, "_", j))
    data = get(paste0("isi_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, isi %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_isi_x = rbind(pred_isi_x, pred)
  }
}


# decomposition -------------------------------------------------
f_t_isi = ddply(pred_isi, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_isi = ddply(pred_isi, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_isi = ddply(pred_isi_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_isi = ddply(pred_isi_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_isi = merge(pred_isi, f_t_isi, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_isi)
head(f_s_isi)
f_st_isi = merge(f_st_isi, f_s_isi, by = c("year", "season", "n_season"), all = T)
f_st_isi$migi = f_st_isi$pred_full - f_st_isi$fs_mean - f_st_isi$ft_mean

head(pred_isi_x)
head(t_isi)
x_st_isi = merge(pred_isi_x, t_isi, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_isi)
head(s_isi)
x_st_isi = merge(x_st_isi, s_isi, by = c("year", "season", "n_season", "model"), all = T)
x_st_isi$migi = x_st_isi$pred_x - x_st_isi$s_mean - x_st_isi$t_mean

head(f_st_isi)
head(x_st_isi)
st_isi = merge(x_st_isi, f_st_isi, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_isi)
st_isi$migi = (st_isi$migi.x - st_isi$migi.y)^2

f_t_isi = merge(f_t_isi, t_isi, by = c("season", "n_season", "knot_i"), all = T)
#f_s_isi = merge(f_s_isi, pred_isi, by = c("season", "n_season", "knot_i"), all = T)
f_s_isi = merge(f_s_isi, s_isi, by = c("year", "season", "n_season"), all = T)
#f_t_isi = merge(f_t_isi, pred_isi, by = c("year", "season", "n_season"), all = T)

head(f_t_isi)
f_t_isi$migi = ((f_t_isi$ft_mean - f_t_isi$t_mean)^2)
f_s_isi$migi = ((f_s_isi$fs_mean - f_s_isi$s_mean)^2)

summary(f_t_isi)
summary(f_s_isi)
summary(st_isi)

length(unique(f_s_isi$year)) #29
length(unique(f_t_isi$knot_i)) #50
t_isi = ddply(f_t_isi, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_isi = ddply(f_s_isi, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_isi2 = ddply(st_isi, .(model, season, n_season), summarize, Est = mean(migi))

cont_isi = merge(s_isi, t_isi, by = c("model", "season", "n_season"))
cont_isi = merge(cont_isi, st_isi2, by = c("model", "season", "n_season"))

cont_isi$cont_s = cont_isi$Es/(cont_isi$Es+cont_isi$Et+cont_isi$Est)
cont_isi$cont_t = cont_isi$Et/(cont_isi$Es+cont_isi$Et+cont_isi$Est)
cont_isi$cont_st =  cont_isi$Est/(cont_isi$Es+cont_isi$Et+cont_isi$Est)
cont_isi$feature = ifelse(cont_isi$model == 2, "do_0", ifelse(cont_isi$model == 3, "do_50", ifelse(cont_isi$model == 4, "sal_0", ifelse(cont_isi$model == 5, "sal_50", ifelse(cont_isi$model == 6, "wt_0", "wt_50")))))
cont_isi = arrange(cont_isi, n_season)
cont_isi$species = "isigarei"

write.csv(cont_isi, "cont_isi.csv")

# konosiro ------------------------------------------------------
kono = read.csv("boost_kono.csv", fileEncoding = "CP932")
kono = kono[, -1]
summary(kono)
kono = kono %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_kono = kono %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_kono = na.omit(df_mat_kono)
summary(df_kono)

set.seed(0)
load("tuned_params_kono.RData")

best_kono = train_kono$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_kono$eta,
  gamma             = best_kono$gamma,
  max_depth         = best_kono$max_depth,
  min_child_weight  = best_kono$min_child_weight,
  subsample         = best_kono$subsample,
  colsample_bytree  = best_kono$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("kono", i),
         kono %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("kono", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_kono", i),
         data[sprit, ])
  assign(paste0("te_kono", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_kono", i))
  te_data = get(paste0("te_kono", i))
  
  assign(paste0("tr_kono", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_kono", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_kono1)

for(i in 1:4){
  data = get(paste0("kono", i))
  
  assign(paste0("kono_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_kono", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_kono$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_kono", i, ".RData"))
  assign(paste0("model_kono", i),
         model)
}

pred_kono = NULL
for(j in 1:4){
  model = get(paste0("model_kono", j))
  data = get(paste0("kono_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, kono %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_kono = rbind(pred_kono, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_kono", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_kono$nrounds
    
    assign(paste0("model_kono_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_kono_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_kono_x", i, "_", j))
    data = get(paste0("kono_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, kono %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_kono_x = rbind(pred_kono_x, pred)
  }
}

# decomposition -------------------------------------------------
f_t_kono = ddply(pred_kono, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_kono = ddply(pred_kono, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_kono = ddply(pred_kono_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_kono = ddply(pred_kono_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_kono = merge(pred_kono, f_t_kono, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_kono)
head(f_s_kono)
f_st_kono = merge(f_st_kono, f_s_kono, by = c("year", "season", "n_season"), all = T)
f_st_kono$migi = f_st_kono$pred_full - f_st_kono$fs_mean - f_st_kono$ft_mean

head(pred_kono_x)
head(t_kono)
x_st_kono = merge(pred_kono_x, t_kono, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_kono)
head(s_kono)
x_st_kono = merge(x_st_kono, s_kono, by = c("year", "season", "n_season", "model"), all = T)
x_st_kono$migi = x_st_kono$pred_x - x_st_kono$s_mean - x_st_kono$t_mean

head(f_st_kono)
head(x_st_kono)
st_kono = merge(x_st_kono, f_st_kono, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_kono)
st_kono$migi = (st_kono$migi.x - st_kono$migi.y)^2

f_t_kono = merge(f_t_kono, t_kono, by = c("season", "n_season", "knot_i"), all = T)
#f_s_kono = merge(f_s_kono, pred_kono, by = c("season", "n_season", "knot_i"), all = T)
f_s_kono = merge(f_s_kono, s_kono, by = c("year", "season", "n_season"), all = T)
#f_t_kono = merge(f_t_kono, pred_kono, by = c("year", "season", "n_season"), all = T)

head(f_t_kono)
f_t_kono$migi = ((f_t_kono$ft_mean - f_t_kono$t_mean)^2)
f_s_kono$migi = ((f_s_kono$fs_mean - f_s_kono$s_mean)^2)

summary(f_t_kono)
summary(f_s_kono)
summary(st_kono)

length(unique(f_s_kono$year)) #29
length(unique(f_t_kono$knot_i)) #50
t_kono = ddply(f_t_kono, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_kono = ddply(f_s_kono, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_kono2 = ddply(st_kono, .(model, season, n_season), summarize, Est = mean(migi))

cont_kono = merge(s_kono, t_kono, by = c("model", "season", "n_season"))
cont_kono = merge(cont_kono, st_kono2, by = c("model", "season", "n_season"))

cont_kono$cont_s = cont_kono$Es/(cont_kono$Es+cont_kono$Et+cont_kono$Est)
cont_kono$cont_t = cont_kono$Et/(cont_kono$Es+cont_kono$Et+cont_kono$Est)
cont_kono$cont_st =  cont_kono$Est/(cont_kono$Es+cont_kono$Et+cont_kono$Est)
cont_kono$feature = ifelse(cont_kono$model == 2, "do_0", ifelse(cont_kono$model == 3, "do_50", ifelse(cont_kono$model == 4, "sal_0", ifelse(cont_kono$model == 5, "sal_50", ifelse(cont_kono$model == 6, "wt_0", "wt_50")))))
cont_kono = arrange(cont_kono, n_season)
cont_kono$species = "konosiro"

write.csv(cont_kono, "cont_kono.csv")


# kouika --------------------------------------------------------
ika = read.csv("boost_ika.csv", fileEncoding = "CP932")
ika = ika[, -1]
summary(ika)
ika = ika %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ika = ika %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_ika = na.omit(df_mat_ika)
summary(df_ika)

set.seed(0)
load("tuned_params_ika.RData")

best_ika = train_ika$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_ika$eta,
  gamma             = best_ika$gamma,
  max_depth         = best_ika$max_depth,
  min_child_weight  = best_ika$min_child_weight,
  subsample         = best_ika$subsample,
  colsample_bytree  = best_ika$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("ika", i),
         ika %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ika", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_ika", i),
         data[sprit, ])
  assign(paste0("te_ika", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_ika", i))
  te_data = get(paste0("te_ika", i))
  
  assign(paste0("tr_ika", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_ika", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_ika1)

for(i in 1:4){
  data = get(paste0("ika", i))
  
  assign(paste0("ika_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_ika", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ika$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_ika", i, ".RData"))
  assign(paste0("model_ika", i),
         model)
}

pred_ika = NULL
for(j in 1:4){
  model = get(paste0("model_ika", j))
  data = get(paste0("ika_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, ika %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_ika = rbind(pred_ika, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_ika", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_ika$nrounds
    
    assign(paste0("model_ika_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_ika_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_ika_x", i, "_", j))
    data = get(paste0("ika_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, ika %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_ika_x = rbind(pred_ika_x, pred)
  }
}


# decomposition -------------------------------------------------
f_t_ika = ddply(pred_ika, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_ika = ddply(pred_ika, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_ika = ddply(pred_ika_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_ika = ddply(pred_ika_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_ika = merge(pred_ika, f_t_ika, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_ika)
head(f_s_ika)
f_st_ika = merge(f_st_ika, f_s_ika, by = c("year", "season", "n_season"), all = T)
f_st_ika$migi = f_st_ika$pred_full - f_st_ika$fs_mean - f_st_ika$ft_mean

head(pred_ika_x)
head(t_ika)
x_st_ika = merge(pred_ika_x, t_ika, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_ika)
head(s_ika)
x_st_ika = merge(x_st_ika, s_ika, by = c("year", "season", "n_season", "model"), all = T)
x_st_ika$migi = x_st_ika$pred_x - x_st_ika$s_mean - x_st_ika$t_mean

head(f_st_ika)
head(x_st_ika)
st_ika = merge(x_st_ika, f_st_ika, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_ika)
st_ika$migi = (st_ika$migi.x - st_ika$migi.y)^2

f_t_ika = merge(f_t_ika, t_ika, by = c("season", "n_season", "knot_i"), all = T)
#f_s_ika = merge(f_s_ika, pred_ika, by = c("season", "n_season", "knot_i"), all = T)
f_s_ika = merge(f_s_ika, s_ika, by = c("year", "season", "n_season"), all = T)
#f_t_ika = merge(f_t_ika, pred_ika, by = c("year", "season", "n_season"), all = T)

head(f_t_ika)
f_t_ika$migi = ((f_t_ika$ft_mean - f_t_ika$t_mean)^2)
f_s_ika$migi = ((f_s_ika$fs_mean - f_s_ika$s_mean)^2)

summary(f_t_ika)
summary(f_s_ika)
summary(st_ika)

length(unique(f_s_ika$year)) #29
length(unique(f_t_ika$knot_i)) #50
t_ika = ddply(f_t_ika, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_ika = ddply(f_s_ika, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_ika2 = ddply(st_ika, .(model, season, n_season), summarize, Est = mean(migi))

cont_ika = merge(s_ika, t_ika, by = c("model", "season", "n_season"))
cont_ika = merge(cont_ika, st_ika2, by = c("model", "season", "n_season"))

cont_ika$cont_s = cont_ika$Es/(cont_ika$Es+cont_ika$Et+cont_ika$Est)
cont_ika$cont_t = cont_ika$Et/(cont_ika$Es+cont_ika$Et+cont_ika$Est)
cont_ika$cont_st =  cont_ika$Est/(cont_ika$Es+cont_ika$Et+cont_ika$Est)
cont_ika$feature = ifelse(cont_ika$model == 2, "do_0", ifelse(cont_ika$model == 3, "do_50", ifelse(cont_ika$model == 4, "sal_0", ifelse(cont_ika$model == 5, "sal_50", ifelse(cont_ika$model == 6, "wt_0", "wt_50")))))
cont_ika = arrange(cont_ika, n_season)
cont_ika$species = "kouika"

write.csv(cont_ika, "cont_ika.csv")


# kurumaebi -----------------------------------------------------
ebi = read.csv("boost_ebi.csv", fileEncoding = "CP932")
ebi = ebi[, -1]
summary(ebi)
ebi = ebi %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ebi = ebi %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_ebi = na.omit(df_mat_ebi)
summary(df_ebi)

set.seed(0)
load("tuned_params_ebi.RData")

best_ebi = train_ebi$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_ebi$eta,
  gamma             = best_ebi$gamma,
  max_depth         = best_ebi$max_depth,
  min_child_weight  = best_ebi$min_child_weight,
  subsample         = best_ebi$subsample,
  colsample_bytree  = best_ebi$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("ebi", i),
         ebi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ebi", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_ebi", i),
         data[sprit, ])
  assign(paste0("te_ebi", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_ebi", i))
  te_data = get(paste0("te_ebi", i))
  
  assign(paste0("tr_ebi", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_ebi", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_ebi1)

for(i in 1:4){
  data = get(paste0("ebi", i))
  
  assign(paste0("ebi_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_ebi", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ebi$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_ebi", i, ".RData"))
  assign(paste0("model_ebi", i),
         model)
}

pred_ebi = NULL
for(j in 1:4){
  model = get(paste0("model_ebi", j))
  data = get(paste0("ebi_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, ebi %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_ebi = rbind(pred_ebi, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_ebi", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_ebi$nrounds
    
    assign(paste0("model_ebi_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_ebi_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_ebi_x", i, "_", j))
    data = get(paste0("ebi_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, ebi %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_ebi_x = rbind(pred_ebi_x, pred)
  }
}


# decomposition -------------------------------------------------
f_t_ebi = ddply(pred_ebi, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_ebi = ddply(pred_ebi, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_ebi = ddply(pred_ebi_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_ebi = ddply(pred_ebi_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_ebi = merge(pred_ebi, f_t_ebi, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_ebi)
head(f_s_ebi)
f_st_ebi = merge(f_st_ebi, f_s_ebi, by = c("year", "season", "n_season"), all = T)
f_st_ebi$migi = f_st_ebi$pred_full - f_st_ebi$fs_mean - f_st_ebi$ft_mean

head(pred_ebi_x)
head(t_ebi)
x_st_ebi = merge(pred_ebi_x, t_ebi, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_ebi)
head(s_ebi)
x_st_ebi = merge(x_st_ebi, s_ebi, by = c("year", "season", "n_season", "model"), all = T)
x_st_ebi$migi = x_st_ebi$pred_x - x_st_ebi$s_mean - x_st_ebi$t_mean

head(f_st_ebi)
head(x_st_ebi)
st_ebi = merge(x_st_ebi, f_st_ebi, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_ebi)
st_ebi$migi = (st_ebi$migi.x - st_ebi$migi.y)^2

f_t_ebi = merge(f_t_ebi, t_ebi, by = c("season", "n_season", "knot_i"), all = T)
#f_s_ebi = merge(f_s_ebi, pred_ebi, by = c("season", "n_season", "knot_i"), all = T)
f_s_ebi = merge(f_s_ebi, s_ebi, by = c("year", "season", "n_season"), all = T)
#f_t_ebi = merge(f_t_ebi, pred_ebi, by = c("year", "season", "n_season"), all = T)

head(f_t_ebi)
f_t_ebi$migi = ((f_t_ebi$ft_mean - f_t_ebi$t_mean)^2)
f_s_ebi$migi = ((f_s_ebi$fs_mean - f_s_ebi$s_mean)^2)

summary(f_t_ebi)
summary(f_s_ebi)
summary(st_ebi)

length(unique(f_s_ebi$year)) #29
length(unique(f_t_ebi$knot_i)) #50
t_ebi = ddply(f_t_ebi, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_ebi = ddply(f_s_ebi, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_ebi2 = ddply(st_ebi, .(model, season, n_season), summarize, Est = mean(migi))

cont_ebi = merge(s_ebi, t_ebi, by = c("model", "season", "n_season"))
cont_ebi = merge(cont_ebi, st_ebi2, by = c("model", "season", "n_season"))

cont_ebi$cont_s = cont_ebi$Es/(cont_ebi$Es+cont_ebi$Et+cont_ebi$Est)
cont_ebi$cont_t = cont_ebi$Et/(cont_ebi$Es+cont_ebi$Et+cont_ebi$Est)
cont_ebi$cont_st =  cont_ebi$Est/(cont_ebi$Es+cont_ebi$Et+cont_ebi$Est)
cont_ebi$feature = ifelse(cont_ebi$model == 2, "do_0", ifelse(cont_ebi$model == 3, "do_50", ifelse(cont_ebi$model == 4, "sal_0", ifelse(cont_ebi$model == 5, "sal_50", ifelse(cont_ebi$model == 6, "wt_0", "wt_50")))))
cont_ebi = arrange(cont_ebi, n_season)
cont_ebi$species = "kurumaebi"

write.csv(cont_ebi, "cont_ebi.csv")


# maanago -------------------------------------------------------
ana = read.csv("boost_ana.csv", fileEncoding = "CP932")
ana = ana[, -1]
summary(ana)
ana = ana %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ana = ana %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_ana = na.omit(df_mat_ana)
summary(df_ana)

set.seed(0)
load("tuned_params_ana.RData")

best_ana = train_ana$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_ana$eta,
  gamma             = best_ana$gamma,
  max_depth         = best_ana$max_depth,
  min_child_weight  = best_ana$min_child_weight,
  subsample         = best_ana$subsample,
  colsample_bytree  = best_ana$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("ana", i),
         ana %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ana", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_ana", i),
         data[sprit, ])
  assign(paste0("te_ana", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_ana", i))
  te_data = get(paste0("te_ana", i))
  
  assign(paste0("tr_ana", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_ana", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_ana1)

for(i in 1:4){
  data = get(paste0("ana", i))
  
  assign(paste0("ana_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_ana", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ana$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_ana", i, ".RData"))
  assign(paste0("model_ana", i),
         model)
}

pred_ana = NULL
for(j in 1:4){
  model = get(paste0("model_ana", j))
  data = get(paste0("ana_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, ana %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_ana = rbind(pred_ana, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_ana", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_ana$nrounds
    
    assign(paste0("model_ana_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_ana_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_ana_x", i, "_", j))
    data = get(paste0("ana_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, ana %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_ana_x = rbind(pred_ana_x, pred)
  }
}

# decomposition -------------------------------------------------
f_t_ana = ddply(pred_ana, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_ana = ddply(pred_ana, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_ana = ddply(pred_ana_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_ana = ddply(pred_ana_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_ana = merge(pred_ana, f_t_ana, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_ana)
head(f_s_ana)
f_st_ana = merge(f_st_ana, f_s_ana, by = c("year", "season", "n_season"), all = T)
f_st_ana$migi = f_st_ana$pred_full - f_st_ana$fs_mean - f_st_ana$ft_mean

head(pred_ana_x)
head(t_ana)
x_st_ana = merge(pred_ana_x, t_ana, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_ana)
head(s_ana)
x_st_ana = merge(x_st_ana, s_ana, by = c("year", "season", "n_season", "model"), all = T)
x_st_ana$migi = x_st_ana$pred_x - x_st_ana$s_mean - x_st_ana$t_mean

head(f_st_ana)
head(x_st_ana)
st_ana = merge(x_st_ana, f_st_ana, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_ana)
st_ana$migi = (st_ana$migi.x - st_ana$migi.y)^2

f_t_ana = merge(f_t_ana, t_ana, by = c("season", "n_season", "knot_i"), all = T)
#f_s_ana = merge(f_s_ana, pred_ana, by = c("season", "n_season", "knot_i"), all = T)
f_s_ana = merge(f_s_ana, s_ana, by = c("year", "season", "n_season"), all = T)
#f_t_ana = merge(f_t_ana, pred_ana, by = c("year", "season", "n_season"), all = T)

head(f_t_ana)
f_t_ana$migi = ((f_t_ana$ft_mean - f_t_ana$t_mean)^2)
f_s_ana$migi = ((f_s_ana$fs_mean - f_s_ana$s_mean)^2)

summary(f_t_ana)
summary(f_s_ana)
summary(st_ana)

length(unique(f_s_ana$year)) #29
length(unique(f_t_ana$knot_i)) #50
t_ana = ddply(f_t_ana, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_ana = ddply(f_s_ana, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_ana2 = ddply(st_ana, .(model, season, n_season), summarize, Est = mean(migi))

cont_ana = merge(s_ana, t_ana, by = c("model", "season", "n_season"))
cont_ana = merge(cont_ana, st_ana2, by = c("model", "season", "n_season"))

cont_ana$cont_s = cont_ana$Es/(cont_ana$Es+cont_ana$Et+cont_ana$Est)
cont_ana$cont_t = cont_ana$Et/(cont_ana$Es+cont_ana$Et+cont_ana$Est)
cont_ana$cont_st =  cont_ana$Est/(cont_ana$Es+cont_ana$Et+cont_ana$Est)
cont_ana$feature = ifelse(cont_ana$model == 2, "do_0", ifelse(cont_ana$model == 3, "do_50", ifelse(cont_ana$model == 4, "sal_0", ifelse(cont_ana$model == 5, "sal_50", ifelse(cont_ana$model == 6, "wt_0", "wt_50")))))
cont_ana = arrange(cont_ana, n_season)
cont_ana$species = "maanago"

write.csv(cont_ana, "cont_ana.csv")


# makogarei -----------------------------------------------------
mako = read.csv("boost_mako.csv", fileEncoding = "CP932")
mako = mako[, -1]
summary(mako)
mako = mako %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_mako = mako %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_mako = na.omit(df_mat_mako)
summary(df_mako)

set.seed(0)
load("tuned_params_mako.RData")

best_mako = train_mako$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_mako$eta,
  gamma             = best_mako$gamma,
  max_depth         = best_mako$max_depth,
  min_child_weight  = best_mako$min_child_weight,
  subsample         = best_mako$subsample,
  colsample_bytree  = best_mako$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("mako", i),
         mako %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("mako", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_mako", i),
         data[sprit, ])
  assign(paste0("te_mako", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_mako", i))
  te_data = get(paste0("te_mako", i))
  
  assign(paste0("tr_mako", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_mako", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_mako1)

for(i in 1:4){
  data = get(paste0("mako", i))
  
  assign(paste0("mako_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_mako", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_mako$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_mako", i, ".RData"))
  assign(paste0("model_mako", i),
         model)
}

pred_mako = NULL
for(j in 1:4){
  model = get(paste0("model_mako", j))
  data = get(paste0("mako_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, mako %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_mako = rbind(pred_mako, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_mako", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_mako$nrounds
    
    assign(paste0("model_mako_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_mako_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_mako_x", i, "_", j))
    data = get(paste0("mako_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, mako %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_mako_x = rbind(pred_mako_x, pred)
  }
}

# decomposition -------------------------------------------------
f_t_mako = ddply(pred_mako, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_mako = ddply(pred_mako, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_mako = ddply(pred_mako_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_mako = ddply(pred_mako_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_mako = merge(pred_mako, f_t_mako, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_mako)
head(f_s_mako)
f_st_mako = merge(f_st_mako, f_s_mako, by = c("year", "season", "n_season"), all = T)
f_st_mako$migi = f_st_mako$pred_full - f_st_mako$fs_mean - f_st_mako$ft_mean

head(pred_mako_x)
head(t_mako)
x_st_mako = merge(pred_mako_x, t_mako, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_mako)
head(s_mako)
x_st_mako = merge(x_st_mako, s_mako, by = c("year", "season", "n_season", "model"), all = T)
x_st_mako$migi = x_st_mako$pred_x - x_st_mako$s_mean - x_st_mako$t_mean

head(f_st_mako)
head(x_st_mako)
st_mako = merge(x_st_mako, f_st_mako, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_mako)
st_mako$migi = (st_mako$migi.x - st_mako$migi.y)^2

f_t_mako = merge(f_t_mako, t_mako, by = c("season", "n_season", "knot_i"), all = T)
#f_s_mako = merge(f_s_mako, pred_mako, by = c("season", "n_season", "knot_i"), all = T)
f_s_mako = merge(f_s_mako, s_mako, by = c("year", "season", "n_season"), all = T)
#f_t_mako = merge(f_t_mako, pred_mako, by = c("year", "season", "n_season"), all = T)

head(f_t_mako)
f_t_mako$migi = ((f_t_mako$ft_mean - f_t_mako$t_mean)^2)
f_s_mako$migi = ((f_s_mako$fs_mean - f_s_mako$s_mean)^2)

summary(f_t_mako)
summary(f_s_mako)
summary(st_mako)

length(unique(f_s_mako$year)) #29
length(unique(f_t_mako$knot_i)) #50
t_mako = ddply(f_t_mako, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_mako = ddply(f_s_mako, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_mako2 = ddply(st_mako, .(model, season, n_season), summarize, Est = mean(migi))

cont_mako = merge(s_mako, t_mako, by = c("model", "season", "n_season"))
cont_mako = merge(cont_mako, st_mako2, by = c("model", "season", "n_season"))

cont_mako$cont_s = cont_mako$Es/(cont_mako$Es+cont_mako$Et+cont_mako$Est)
cont_mako$cont_t = cont_mako$Et/(cont_mako$Es+cont_mako$Et+cont_mako$Est)
cont_mako$cont_st =  cont_mako$Est/(cont_mako$Es+cont_mako$Et+cont_mako$Est)
cont_mako$feature = ifelse(cont_mako$model == 2, "do_0", ifelse(cont_mako$model == 3, "do_50", ifelse(cont_mako$model == 4, "sal_0", ifelse(cont_mako$model == 5, "sal_50", ifelse(cont_mako$model == 6, "wt_0", "wt_50")))))
cont_mako = arrange(cont_mako, n_season)
cont_mako$species = "makogarei"

write.csv(cont_mako, "cont_mako.csv")


# suzuki --------------------------------------------------------
suzu = read.csv("boost_suzu.csv", fileEncoding = "CP932")
suzu = suzu[, -1]
summary(suzu)
suzu = suzu %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_suzu = suzu %>% 
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
df_suzu = na.omit(df_mat_suzu)
summary(df_suzu)

set.seed(0)
load("tuned_params_suzu.RData")

best_suzu = train_suzu$bestTune
params = list(
  booster           = 'gbtree',
  objective         = 'reg:linear',
  eval_metric       = 'mae',
  eta               = best_suzu$eta,
  gamma             = best_suzu$gamma,
  max_depth         = best_suzu$max_depth,
  min_child_weight  = best_suzu$min_child_weight,
  subsample         = best_suzu$subsample,
  colsample_bytree  = best_suzu$colsample_bytree
)

# make full model -----------------------------------------------
for(i in 1:4){
  assign(paste0("suzu", i),
         suzu %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("suzu", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.8)
  
  assign(paste0("tr_suzu", i),
         data[sprit, ])
  assign(paste0("te_suzu", i),
         data[-sprit, ])
}

for(i in 1:4){
  tr_data = get(paste0("tr_suzu", i))
  te_data = get(paste0("te_suzu", i))
  
  assign(paste0("tr_suzu", i),
         normalizeFeatures(tr_data, target = "log_abundance"))
  assign(paste0("te_suzu", i),
         normalizeFeatures(te_data, target = "log_abundance"))
}
summary(tr_suzu1)

for(i in 1:4){
  data = get(paste0("suzu", i))
  
  assign(paste0("suzu_norm", i),
         normalizeFeatures(data, target = "log_abundance"))
}

for(i in 1:4){
  data = get(paste0("tr_suzu", i))
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_suzu$nrounds
  model = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  save(model, file = paste0("model_suzu", i, ".RData"))
  assign(paste0("model_suzu", i),
         model)
}

pred_suzu = NULL
for(j in 1:4){
  model = get(paste0("model_suzu", j))
  data = get(paste0("suzu_norm", j))
  
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
  
  colnames(pred) = c("pred_full", "obs")
  pred = cbind(pred, suzu %>% filter(n_season == j) %>% select(year,season,n_season,knot_i))
  pred_suzu = rbind(pred_suzu, pred)
}


# make models without features -----------------------------------
for(j in 1:4){
  for(i in 2:7){
    data = get(paste0("tr_suzu", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    X = as.matrix(data %>% mutate(log_abundance = NULL))
    nrounds =best_suzu$nrounds
    
    assign(paste0("model_suzu_x", i, "_", j),
           xgboost(
             data = X,
             label = data$log_abundance,
             nrounds = nrounds,
             params = params)
    )
  }
}

# 特徴量Xを抜いたモデルの推定値 ------------------------------------------
pred_suzu_x = NULL
for(j in 1:4){
  for(i in 2:7){
    model = get(paste0("model_suzu_x", i, "_", j))
    data = get(paste0("suzu_norm", j))
    col = colnames(data)
    rm_col = col[i]
    data = data %>% select(-rm_col)
    
    pred = data.frame(predict(model, as.matrix(data[, -1])), data[,1])
    
    colnames(pred) = c("pred_x", "obs")
    pred = cbind(pred, suzu %>% filter(n_season == j) %>% select(year,season,n_season,knot_i)) %>% mutate(model = paste0(i))
    pred_suzu_x = rbind(pred_suzu_x, pred)
  }
}

# decomposition -------------------------------------------------
f_t_suzu = ddply(pred_suzu, .(season, n_season, knot_i), summarize, ft_mean = mean(pred_full))
f_s_suzu = ddply(pred_suzu, .(year, season, n_season), summarize, fs_mean = mean(pred_full))
t_suzu = ddply(pred_suzu_x, .(season, n_season, knot_i, model), summarize, t_mean = mean(pred_x))
s_suzu = ddply(pred_suzu_x, .(year, season, n_season, model), summarize, s_mean = mean(pred_x))

f_st_suzu = merge(pred_suzu, f_t_suzu, by = c("season", "n_season", "knot_i"), all = T)
head(f_st_suzu)
head(f_s_suzu)
f_st_suzu = merge(f_st_suzu, f_s_suzu, by = c("year", "season", "n_season"), all = T)
f_st_suzu$migi = f_st_suzu$pred_full - f_st_suzu$fs_mean - f_st_suzu$ft_mean

head(pred_suzu_x)
head(t_suzu)
x_st_suzu = merge(pred_suzu_x, t_suzu, by = c("season", "n_season", "knot_i", "model"), all = T)
head(x_st_suzu)
head(s_suzu)
x_st_suzu = merge(x_st_suzu, s_suzu, by = c("year", "season", "n_season", "model"), all = T)
x_st_suzu$migi = x_st_suzu$pred_x - x_st_suzu$s_mean - x_st_suzu$t_mean

head(f_st_suzu)
head(x_st_suzu)
st_suzu = merge(x_st_suzu, f_st_suzu, by = c("year", "season", "n_season", "knot_i"), all = T)
head(st_suzu)
st_suzu$migi = (st_suzu$migi.x - st_suzu$migi.y)^2

f_t_suzu = merge(f_t_suzu, t_suzu, by = c("season", "n_season", "knot_i"), all = T)
#f_s_suzu = merge(f_s_suzu, pred_suzu, by = c("season", "n_season", "knot_i"), all = T)
f_s_suzu = merge(f_s_suzu, s_suzu, by = c("year", "season", "n_season"), all = T)
#f_t_suzu = merge(f_t_suzu, pred_suzu, by = c("year", "season", "n_season"), all = T)

head(f_t_suzu)
f_t_suzu$migi = ((f_t_suzu$ft_mean - f_t_suzu$t_mean)^2)
f_s_suzu$migi = ((f_s_suzu$fs_mean - f_s_suzu$s_mean)^2)

summary(f_t_suzu)
summary(f_s_suzu)
summary(st_suzu)

length(unique(f_s_suzu$year)) #29
length(unique(f_t_suzu$knot_i)) #50
t_suzu = ddply(f_t_suzu, .(model, season, n_season), summarize, Et = sum(migi)/50)
s_suzu = ddply(f_s_suzu, .(model, season, n_season), summarize, Es = sum(migi)/29)
st_suzu2 = ddply(st_suzu, .(model, season, n_season), summarize, Est = mean(migi))

cont_suzu = merge(s_suzu, t_suzu, by = c("model", "season", "n_season"))
cont_suzu = merge(cont_suzu, st_suzu2, by = c("model", "season", "n_season"))

cont_suzu$cont_s = cont_suzu$Es/(cont_suzu$Es+cont_suzu$Et+cont_suzu$Est)
cont_suzu$cont_t = cont_suzu$Et/(cont_suzu$Es+cont_suzu$Et+cont_suzu$Est)
cont_suzu$cont_st =  cont_suzu$Est/(cont_suzu$Es+cont_suzu$Et+cont_suzu$Est)
cont_suzu$feature = ifelse(cont_suzu$model == 2, "do_0", ifelse(cont_suzu$model == 3, "do_50", ifelse(cont_suzu$model == 4, "sal_0", ifelse(cont_suzu$model == 5, "sal_50", ifelse(cont_suzu$model == 6, "wt_0", "wt_50")))))
cont_suzu = arrange(cont_suzu, n_season)
cont_suzu$species = "suzuki"

write.csv(cont_suzu, "cont_suzu.csv")


# all contST ----------------------------------------------------
contST = rbind(cont_isi, cont_kono, cont_ika, cont_ebi, cont_ana, cont_mako, cont_suzu)
colnames(contST)[1] = "n_feature"
write.csv(contST, "ST.csv")
