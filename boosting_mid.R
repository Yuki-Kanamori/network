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
setwd("C:/kana/Dropbox/Network/revised_data")
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
  log_abundance ~ .,
  data = df_isi,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_isi, file = paste0("tuned_params_isi_mid", ".RData"))

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
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  isi_cor = rbind(isi_cor, cor)
}

isi_mae = c()
for(i in 1:4){
  pred = isi_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  isi_mae = rbind(isi_mae, m)
}

write.csv(isi_imp, "isi_imp_mid.csv")
write.csv(isi_pred, "isi_pred_mid.csv")
write.csv(isi_cor, "isi_cor_mid.csv")
write.csv(isi_mae, "isi_mae_mid.csv")



# konosiro ------------------------
rm(list = ls())
kono = read.csv("boost_kono.csv", fileEncoding = "CP932")
kono = kono[, -1]
summary(kono)
kono = kono %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_kono = kono %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_kono = na.omit(df_mat_kono)
summary(df_kono)

set.seed(0)
train_kono = train(
  log_abundance ~ .,
  data = df_kono,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_kono, file = paste0("tuned_params_kono_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("kono", i),
         kono %>% filter(n_season == i) %>% select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("kono", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

kono_imp = c()
for(i in 1:4){
  data = get(paste0("tr_kono", i))
  model = get(paste0("model_kono", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  kono_imp = rbind(kono_imp, imp)
}

kono_pred = c()
for(i in 1:4){
  data = get(paste0("te_kono", i))
  model = get(paste0("model_kono", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  kono_pred = rbind(kono_pred, pred)
}

kono_cor = c()
for(i in 1:4){
  data = kono_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  kono_cor = rbind(kono_cor, cor)
}

kono_mae = c()
for(i in 1:4){
  pred = kono_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  kono_mae = rbind(kono_mae, m)
}

write.csv(kono_imp, "kono_imp_mid.csv")
write.csv(kono_pred, "kono_pred_mid.csv")
write.csv(kono_cor, "kono_cor_mid.csv")
write.csv(kono_mae, "kono_mae_mid.csv")



# ika -----------------------------
ika = read.csv("boost_ika.csv", fileEncoding = "CP932")
ika = ika[, -1]
summary(ika)
ika = ika %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ika = ika %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_ika = na.omit(df_mat_ika)
summary(df_ika)

set.seed(0)
train_ika = train(
  log_abundance ~ .,
  data = df_ika,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ika, file = paste0("tuned_params_ika_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("ika", i),
         ika %>% filter(n_season == i) %>% select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ika", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

ika_imp = c()
for(i in 1:4){
  data = get(paste0("tr_ika", i))
  model = get(paste0("model_ika", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  ika_imp = rbind(ika_imp, imp)
}

ika_pred = c()
for(i in 1:4){
  data = get(paste0("te_ika", i))
  model = get(paste0("model_ika", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  ika_pred = rbind(ika_pred, pred)
}

ika_cor = c()
for(i in 1:4){
  data = ika_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  ika_cor = rbind(ika_cor, cor)
}

ika_mae = c()
for(i in 1:4){
  pred = ika_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  ika_mae = rbind(ika_mae, m)
}

write.csv(ika_imp, "ika_imp_mid.csv")
write.csv(ika_pred, "ika_pred_mid.csv")
write.csv(ika_cor, "ika_cor_mid.csv")
write.csv(ika_mae, "ika_mae_mid.csv")



# kurumaebi -----------------------
ebi = read.csv("boost_ebi.csv", fileEncoding = "CP932")
ebi = ebi[, -1]
summary(ebi)
ebi = ebi %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ebi = ebi %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_ebi = na.omit(df_mat_ebi)
summary(df_ebi)

set.seed(0)
train_ebi = train(
  log_abundance ~ .,
  data = df_ebi,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ebi, file = paste0("tuned_params_ebi_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("ebi", i),
         ebi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ebi", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

ebi_imp = c()
for(i in 1:4){
  data = get(paste0("tr_ebi", i))
  model = get(paste0("model_ebi", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  ebi_imp = rbind(ebi_imp, imp)
}

ebi_pred = c()
for(i in 1:4){
  data = get(paste0("te_ebi", i))
  model = get(paste0("model_ebi", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  ebi_pred = rbind(ebi_pred, pred)
}

ebi_cor = c()
for(i in 1:4){
  data = ebi_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  ebi_cor = rbind(ebi_cor, cor)
}

ebi_mae = c()
for(i in 1:4){
  pred = ebi_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  ebi_mae = rbind(ebi_mae, m)
}

write.csv(ebi_imp, "ebi_imp_mid.csv")
write.csv(ebi_pred, "ebi_pred_mid.csv")
write.csv(ebi_cor, "ebi_cor_mid.csv")
write.csv(ebi_mae, "ebi_mae_mid.csv")


# maanago -------------------------
ana = read.csv("boost_ana.csv", fileEncoding = "CP932")
ana = ana[, -1]
summary(ana)
ana = ana %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_ana = ana %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_ana = na.omit(df_mat_ana)
summary(df_ana)

set.seed(0)
train_ana = train(
  log_abundance ~ .,
  data = df_ana,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ana, file = paste0("tuned_params_ana_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("ana", i),
         ana %>% filter(n_season == i) %>% select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("ana", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

ana_imp = c()
for(i in 1:4){
  data = get(paste0("tr_ana", i))
  model = get(paste0("model_ana", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  ana_imp = rbind(ana_imp, imp)
}

ana_pred = c()
for(i in 1:4){
  data = get(paste0("te_ana", i))
  model = get(paste0("model_ana", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  ana_pred = rbind(ana_pred, pred)
}

ana_cor = c()
for(i in 1:4){
  data = ana_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  ana_cor = rbind(ana_cor, cor)
}

ana_mae = c()
for(i in 1:4){
  pred = ana_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  ana_mae = rbind(ana_mae, m)
}

write.csv(ana_imp, "ana_imp_mid.csv")
write.csv(ana_pred, "ana_pred_mid.csv")
write.csv(ana_cor, "ana_cor_mid.csv")
write.csv(ana_mae, "ana_mae_mid.csv")



# makogarei -----------------------
mako = read.csv("boost_mako.csv", fileEncoding = "CP932")
mako = mako[, -1]
summary(mako)
mako = mako %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_mako = mako %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_mako = na.omit(df_mat_mako)
summary(df_mako)

set.seed(0)
train_mako = train(
  log_abundance ~ .,
  data = df_mako,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_mako, file = paste0("tuned_params_mako_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("mako", i),
         mako %>% filter(n_season == i) %>% select(log_abundance, do_0, do_5, do_10, do_20, do_50, sal_0, sal_5, sal_10, sal_20, sal_50, wt_0, wt_5, wt_10, wt_20, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("mako", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

mako_imp = c()
for(i in 1:4){
  data = get(paste0("tr_mako", i))
  model = get(paste0("model_mako", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  mako_imp = rbind(mako_imp, imp)
}

mako_pred = c()
for(i in 1:4){
  data = get(paste0("te_mako", i))
  model = get(paste0("model_mako", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  mako_pred = rbind(mako_pred, pred)
}

mako_cor = c()
for(i in 1:4){
  data = mako_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  mako_cor = rbind(mako_cor, cor)
}

mako_mae = c()
for(i in 1:4){
  pred = mako_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  mako_mae = rbind(mako_mae, m)
}

write.csv(mako_imp, "mako_imp_mid.csv")
write.csv(mako_pred, "mako_pred_mid.csv")
write.csv(mako_cor, "mako_cor_mid.csv")
write.csv(mako_mae, "mako_mae_mid.csv")



# suzuki --------------------------
suzu = read.csv("boost_suzu.csv", fileEncoding = "CP932")
suzu = suzu[, -1]
summary(suzu)
suzu = suzu %>% mutate(do_mid = (do_5+do_10+do_20)/3, sal_mid = (sal_5+sal_10+sal_20)/3, wt_mid = (wt_5+wt_10+wt_20)/3)
df_mat_suzu = suzu %>% 
  select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
df_suzu = na.omit(df_mat_suzu)
summary(df_suzu)

set.seed(0)
train_suzu = train(
  log_abundance ~ .,
  data = df_suzu,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_suzu, file = paste0("tuned_params_suzu_mid", ".RData"))

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

for(i in 1:4){
  assign(paste0("suzu", i),
         suzu %>% filter(n_season == i) %>% select(log_abundance, do_0, do_mid, do_50, sal_0, sal_mid, sal_50, wt_0, wt_mid, wt_50)
  )
}

for(i in 1:4){
  data = get(paste0("suzu", i))
  data = na.omit(data)
  nr = nrow(data)
  sprit = sample(nr, replace = F, nr*0.7)
  
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

suzu_imp = c()
for(i in 1:4){
  data = get(paste0("tr_suzu", i))
  model = get(paste0("model_suzu", i))
  imp = xgb.importance(colnames(as.matrix(data %>% mutate(log_abundance = NULL))), model = model)
  imp$n_season = paste0(i)
  suzu_imp = rbind(suzu_imp, imp)
}

suzu_pred = c()
for(i in 1:4){
  data = get(paste0("te_suzu", i))
  model = get(paste0("model_suzu", i))
  pred = data.frame(predict(model, as.matrix(data[, -1])), data[, 1])
  colnames(pred) = c("pred", "obs")
  pred$n_season = paste0(i)
  suzu_pred = rbind(suzu_pred, pred)
}

suzu_cor = c()
for(i in 1:4){
  data = suzu_pred %>% filter(n_season == 1)
  cor = cor(data$obs, data$pred)[[1]]
  cor$n_season = paste0(i)
  suzu_cor = rbind(suzu_cor, cor)
}

suzu_mae = c()
for(i in 1:4){
  pred = suzu_pred %>% filter(n_season == i)
  
  m = mean(sum(abs(pred$pred - pred$obs)))
  suzu_mae = rbind(suzu_mae, m)
}

write.csv(suzu_imp, "suzu_imp_mid.csv")
write.csv(suzu_pred, "suzu_pred_mid.csv")
write.csv(suzu_cor, "suzu_cor_mid.csv")
write.csv(suzu_mae, "suzu_mae_mid.csv")