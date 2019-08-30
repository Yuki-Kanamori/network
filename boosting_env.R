# ---------------------------------------------------------------
# boosting ------------------------------------------------------
# 
# step1-1 tune params by train() of the package caret 
#        (caret not allow containing NA in data)
# step1-2 check RMSE along iteration by plot()
# step2   performance check using test-data by predict()
#        (test-data = DMatrix form?)
# step3   construct the best model by xgboost() using the best params
#         estimated in step1-1,
#         then, calculate the importance in each variable
# 
# ---------------------------------------------------------------
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
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_isi = na.omit(df_mat_isi)
summary(df_isi)

set.seed(0)
train_isi = train(
  #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
  log_abundance ~ .,
  data = df_isi,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_isi, file = paste0("tuned_params_isi", ".RData"))

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

for(i in 1:4){
  assign(paste0("isi", i),
         isi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
         )
}

for(i in 1:4){
  data = get(paste0("isi", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

isi_imp = c()
for(i in 1:4){
  data = get(paste0("tr_isi", i))
  model = get(paste0("model_isi", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  isi_imp = rbind(isi_imp, imp)
}

isi_pred = c()
for(i in 1:4){
  data = get(paste0("te_isi", i))
  model = get(paste0("model_isi", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  isi_pred = rbind(isi_pred, pred)
}

isi_cor = c()
for(i in 1:4){
  data = isi_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)
  cor$n_season = paste0(i)
  isi_cor = rbind(isi_cor, cor)
}

isi_mae = c()
for(i in 1:4){
  pred = isi_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  isi_mae = rbind(isi_mae, m)
}

write.csv(isi_imp, "isi_imp.csv")
write.csv(isi_pred, "isi_pred.csv")
write.csv(isi_mae, "isi_mae.csv")


# konosiro ------------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
kono = read.csv("boost_kono.csv", fileEncoding = "CP932")
kono = kono[, -1]
summary(kono)

kono_pred = c()
kono_mae = c()
kono_imp = c()

kono = kono %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)

df_mat_kono =  
  filter(kono, n_season == i) %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
#select(log_abundance, do_0, do_
  
  df_mat_kono = na.omit(df_mat_kono)
  nr = nrow(df_mat_kono)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_kono = df_mat_kono[sprit, ]
  test_data_kono = df_mat_kono[-sprit, ]
  summary(train_data_kono)
  
  set.seed(0)
  train_kono = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_kono,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_kono, file = paste0("tuned_params_kono", i, ".RData"))
  
  #best params
  best_kono = train_kono$bestTune
  
  #標準化
  data = df_mat_kono
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_kono = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_kono$nrounds
  model_kono = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_kono, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  kono_pred = rbind(kono_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  kono_mae = rbind(kono_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_kono)
  kono_imp = rbind(kono_imp, imp)
  
  

kono_imp = mutate(kono_imp, n_season = rep(1:4, each = 15))
head(kono,3)
t = kono %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
kono_imp = left_join(kono_imp, t, by = "n_season")
write.csv(kono_imp, "kono_imp.csv")

nrow(kono_pred)
nrow(pred)
kono_pred = mutate(kono_pred, n_season == rep(1:4, each = 1400))
kono_pred = left_join(kono_pred, t, by = "n_season")
write.csv(kono_pred, "kono_pred.csv")

kono_mae = data.frame(kono_mae) %>% mutate(kono_mae, n_season = rep(1:4))
kono_mae = left_join(kono_mae, t, by = "n_season")
write.csv(kono_mae, "kono_mae.csv")


# kouika --------------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
ika = read.csv("boost_ika.csv", fileEncoding = "CP932")
ika = ika[, -1]
summary(ika)
ika = ika %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)

df_mat_ika =  
  filter(ika, n_season == i) %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
#select(log_abundance, do_0, do_

df_mat_kono = na.omit(df_mat_kono)
nr = nrow(df_mat_kono)
sprit = sample(nr, replace = F, nr*0.7)
nr*0.7
train_data_kono = df_mat_kono[sprit, ]
test_data_kono = df_mat_kono[-sprit, ]
summary(train_data_kono)

ika_pred = c()
ika_mae = c()
ika_imp = c()

for(i in 1:4){
  df_mat_ika = filter(ika, n_season == i) %>% 
    select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  
  df_mat_ika = na.omit(df_mat_ika)
  nr = nrow(df_mat_ika)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_ika = df_mat_ika[sprit, ]
  test_data_ika = df_mat_ika[-sprit, ]
  summary(train_data_ika)
  
  set.seed(0)
  train_ika = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_ika,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_ika, file = paste0("train_ika", i, ".RData"))
  
  #best params
  best_ika = train_ika$bestTune
  
  #標準化
  data = df_mat_ika
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_ika = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ika$nrounds
  model_ika = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_ika, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  ika_pred = rbind(ika_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  ika_mae = rbind(ika_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_ika)
  ika_imp = rbind(ika_imp, imp)
  
  
}
ika_imp = mutate(ika_imp, n_season = rep(1:4, each = 15))
head(ika,3)
t = ika %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
ika_imp = left_join(ika_imp, t, by = "n_season")
write.csv(ika_imp, "ika_imp.csv")

nrow(ika_pred)
nrow(pred)
ika_pred = mutate(ika_pred, n_season == rep(1:4, each = 1400))
ika_pred = left_join(ika_pred, t, by = "n_season")
write.csv(ika_pred, "ika_pred.csv")

ika_mae = data.frame(ika_mae) %>% mutate(ika_mae, n_season = rep(1:4))
ika_mae = left_join(ika_mae, t, by = "n_season")
write.csv(ika_mae, "ika_mae.csv")


# kurumaebi -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
ebi = read.csv("boost_ebi.csv", fileEncoding = "CP932")
ebi = ebi[, -1]
summary(ebi)

ebi_pred = c()
ebi_mae = c()
ebi_imp = c()

for(i in 1:4){
  df_mat_ebi = filter(ebi, n_season == i) %>% 
    select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  
  df_mat_ebi = na.omit(df_mat_ebi)
  nr = nrow(df_mat_ebi)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_ebi = df_mat_ebi[sprit, ]
  test_data_ebi = df_mat_ebi[-sprit, ]
  summary(train_data_ebi)
  
  set.seed(0)
  train_ebi = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_ebi,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_ebi, file = paste0("train_ebi", i, ".RData"))
  
  #best params
  best_ebi = train_ebi$bestTune
  
  #標準化
  data = df_mat_ebi
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_ebi = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ebi$nrounds
  model_ebi = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_ebi, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  ebi_pred = rbind(ebi_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  ebi_mae = rbind(ebi_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_ebi)
  ebi_imp = rbind(ebi_imp, imp)
  
  
}
ebi_imp = mutate(ebi_imp, n_season = rep(1:4, each = 15))
head(ebi,3)
t = ebi %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
ebi_imp = left_join(ebi_imp, t, by = "n_season")
write.csv(ebi_imp, "ebi_imp.csv")

nrow(ebi_pred)
nrow(pred)
ebi_pred = mutate(ebi_pred, n_season == rep(1:4, each = 1400))
ebi_pred = left_join(ebi_pred, t, by = "n_season")
write.csv(ebi_pred, "ebi_pred.csv")

ebi_mae = data.frame(ebi_mae) %>% mutate(ebi_mae, n_season = rep(1:4))
ebi_mae = left_join(ebi_mae, t, by = "n_season")
write.csv(ebi_mae, "ebi_mae.csv")


# maanago -----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
ana = read.csv("boost_ana.csv", fileEncoding = "CP932")
ana = ana[, -1]
summary(ana)

ana_pred = c()
ana_mae = c()
ana_imp = c()

for(i in 1:4){
  df_mat_ana = filter(ana, n_season == i) %>% 
    select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  
  df_mat_ana = na.omit(df_mat_ana)
  nr = nrow(df_mat_ana)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_ana = df_mat_ana[sprit, ]
  test_data_ana = df_mat_ana[-sprit, ]
  summary(train_data_ana)
  
  set.seed(0)
  train_ana = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_ana,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_ana, file = paste0("train_ana", i, ".RData"))
  
  #best params
  best_ana = train_ana$bestTune
  
  #標準化
  data = df_mat_ana
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_ana = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_ana$nrounds
  model_ana = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_ana, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  ana_pred = rbind(ana_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  ana_mae = rbind(ana_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_ana)
  ana_imp = rbind(ana_imp, imp)
  
  
}
ana_imp = mutate(ana_imp, n_season = rep(1:4, each = 15))
head(ana,3)
t = ana %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
ana_imp = left_join(ana_imp, t, by = "n_season")
write.csv(ana_imp, "ana_imp.csv")

nrow(ana_pred)
nrow(pred)
ana_pred = mutate(ana_pred, n_season == rep(1:4, each = 1400))
ana_pred = left_join(ana_pred, t, by = "n_season")
write.csv(ana_pred, "ana_pred.csv")

ana_mae = data.frame(ana_mae) %>% mutate(ana_mae, n_season = rep(1:4))
ana_mae = left_join(ana_mae, t, by = "n_season")
write.csv(ana_mae, "ana_mae.csv")


# makogarei -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
mako = read.csv("boost_mako.csv", fileEncoding = "CP932")
mako = mako[, -1]
summary(mako)

mako_pred = c()
mako_mae = c()
mako_imp = c()

for(i in 1:4){
  df_mat_mako = filter(mako, n_season == i) %>% 
    select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  
  df_mat_mako = na.omit(df_mat_mako)
  nr = nrow(df_mat_mako)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_mako = df_mat_mako[sprit, ]
  test_data_mako = df_mat_mako[-sprit, ]
  summary(train_data_mako)
  
  set.seed(0)
  train_mako = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_mako,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_mako, file = paste0("train_mako", i, ".RData"))
  
  #best params
  best_mako = train_mako$bestTune
  
  #標準化
  data = df_mat_mako
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_mako = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_mako$nrounds
  model_mako = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_mako, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  mako_pred = rbind(mako_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  mako_mae = rbind(mako_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_mako)
  mako_imp = rbind(mako_imp, imp)
  
  
}
mako_imp = mutate(mako_imp, n_season = rep(1:4, each = 15))
head(mako,3)
t = mako %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
mako_imp = left_join(mako_imp, t, by = "n_season")
write.csv(mako_imp, "mako_imp.csv")

nrow(mako_pred)
nrow(pred)
mako_pred = mutate(mako_pred, n_season == rep(1:4, each = 1400))
mako_pred = left_join(mako_pred, t, by = "n_season")
write.csv(mako_pred, "mako_pred.csv")

mako_mae = data.frame(mako_mae) %>% mutate(mako_mae, n_season = rep(1:4))
mako_mae = left_join(mako_mae, t, by = "n_season")
write.csv(mako_mae, "mako_mae.csv")


# suzuki --------------------------------------------------------
setwd("/Users/Yuki/Dropbox/Network/revised_data")
suzu = read.csv("boost_suzu.csv", fileEncoding = "CP932")
suzu = suzu[, -1]
summary(suzu)

suzu_pred = c()
suzu_mae = c()
suzu_imp = c()

for(i in 1:4){
  df_mat_suzu = filter(suzu, n_season == i) %>% 
    select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  
  df_mat_suzu = na.omit(df_mat_suzu)
  nr = nrow(df_mat_suzu)
  sprit = sample(nr, replace = F, nr*0.7)
  nr*0.7
  train_data_suzu = df_mat_suzu[sprit, ]
  test_data_suzu = df_mat_suzu[-sprit, ]
  summary(train_data_suzu)
  
  set.seed(0)
  train_suzu = train(
    #dens_sar ~ adult_sar+dens_anc+dens_uru+volume+m_SST+m_PDO+m_IPO+knot_i2+days+int,
    log_abundance ~ .,
    data = train_data_suzu,
    method = "xgbTree",
    preProcess = c("center", "scale"),
    trControl = trainControl(method = "cv"),
    tuneLength = 5
  )
  save(train_suzu, file = paste0("train_suzu", i, ".RData"))
  
  #best params
  best_suzu = train_suzu$bestTune
  
  #標準化
  data = df_mat_suzu
  
  data =   normalizeFeatures(data, target = "log_abundance")
  
  #make DMatrix
  d_suzu = xgb.DMatrix(sparse.model.matrix(log_abundance ~ ., data = data), label = data[, 1])
  
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
  
  
  X = as.matrix(data %>% mutate(log_abundance = NULL))
  nrounds = best_suzu$nrounds
  model_suzu = xgboost(
    data = X,
    label = data$log_abundance,
    nrounds = nrounds,
    params = params)
  
  pred = data.frame(predict(model_suzu, as.matrix(data[, -1])), data[,1])
  colnames(pred) = c("pred", "obs")
  suzu_pred = rbind(suzu_pred, pred)
  
  m = mean(sum(abs(pred$pred-pred$obs)))
  suzu_mae = rbind(suzu_mae, m)
  
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model_suzu)
  suzu_imp = rbind(suzu_imp, imp)
  
  
}
suzu_imp = mutate(suzu_imp, n_season = rep(1:4, each = 15))
head(suzu,3)
t = suzu %>% select(season, n_season, species) %>%
  distinct(n_season, .keep_all = T)
suzu_imp = left_join(suzu_imp, t, by = "n_season")
write.csv(suzu_imp, "suzu_imp.csv")

nrow(suzu_pred)
nrow(pred)
suzu_pred = mutate(suzu_pred, n_season == rep(1:4, each = 1400))
suzu_pred = left_join(suzu_pred, t, by = "n_season")
write.csv(suzu_pred, "suzu_pred.csv")

suzu_mae = data.frame(suzu_mae) %>% mutate(suzu_mae, n_season = rep(1:4))
suzu_mae = left_join(suzu_mae, t, by = "n_season")
write.csv(suzu_mae, "suzu_mae.csv")



