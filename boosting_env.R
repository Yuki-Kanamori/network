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
  select(log_abundance, do_0,do_50, sal_0, sal_50, wt_0, wt_50)
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
save(train_isi, file = paste0("tuned_params_isi", ".RData"))
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

for(i in 1:4){
  assign(paste0("isi", i),
         isi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = isi_pred %>% filter(n_season == i)
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

write.csv(isi_imp, "isi_imp.csv")
write.csv(isi_pred, "isi_pred.csv")
write.csv(isi_cor, "isi_cor.csv")
write.csv(isi_mae, "isi_mae.csv")



require(lme4)
require(glmmTMB)
#require(car)

 for(i in 1:4){
   assign(paste0("isi", i),
          isi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
   )
 }
# summary(isi1)
# 
 for(i in 1:4){
   data = get(paste0("isi",i))
   data = na.omit(data)
   assign(paste0("isi", i),
          normalizeFeatures(data, target = "log_abundance"))
 }

pre_glm_isi = c()
for(i in 1:4){
  data = get(paste0("tr_isi", i))
  p_data = get(paste0("te_isi", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_isi = rbind(pre_glm_isi, pre_glm)
}

# pre_boo_isi = c()
# for(i in 1:4){
#   load(paste0("model_isi",i,".RData"))
#   model = get(paste0("model_isi", i))
#   data = get(paste0("isi", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_isi = rbind(pre_boo_isi, boo)
# }

isi_pred$model = "Boosting"
comp_glm_isi = rbind(pre_glm_isi, isi_pred)
head(isi)
tag = distinct(isi[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_isi = merge(comp_glm_isi, tag, by = "n_season")
write.csv(comp_glm_isi, "comp_glm_isi.csv")

comp_glm_isi$model = factor(comp_glm_isi$model, levels = c("GLM", "Boosting"))
comp_glm_isi$season2 = factor(comp_glm_isi$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_isi, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Ishigarei")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
fig = g+p+f+lab+theme_bw()+l+th
ggsave("pred_obs_isi.pdf", g+p+f+lab+theme_bw()+l+th, width = 11.69, height = 8.27)

###response curve###
require(DALEX)
require(ALEPlot)
for(j in 1:4){
  setwd("/Users/Yuki/Dropbox/Network/revised_data")
    model = load(paste0("model_isi", j, ".RData"))
    data = get(paste0("isi",j))
    exp = DALEX::explain(model, data = as.matrix(data[,-1]), y = data[,1])
    
    list = c()
    for(i in 1:6){
      env = c("do_0", "do_50", "sal_0", "sal_50", "wt_0", "wt_50")[i]
      ale = DALEX::variable_response(exp_isi1, variable = paste0(env), type = "ale")
      ale = ale %>% mutate(n_season = paste0(i))
      list = rbind(list, ale)
    }
    
    assign(paste0("ale_isi", j),
           list)
}



exp_isi1 = DALEX::explain(model_isi1, data = as.matrix(isi1[,-1]), y = isi1[,1])
# ale_isi1 = DALEX::variable_response(exp_isi1, variable = "wt_50", type = "ale")
# ale_isi1 %>% plot()
ale_isi1 = c()
for(i in 1:6){
  env = c("do_0", "do_50", "sal_0", "sal_50", "wt_0", "wt_50")[i]
  ale = DALEX::variable_response(exp_isi1, variable = paste0(env), type = "ale")
  ale = ale %>% mutate(env = paste0(env), n_season = paste0(i))
  ale_isi1 = rbind(ael_isi1, ael)
}


exp_isi2 = DALEX::explain(model_isi2, data = as.matrix(isi2[,-1]), y = isi2[,1])
ale_isi2 = DALEX::variable_response(exp_isi2, variable = "do_50", type = "ale")
ale_isi2 %>% plot()

exp_xgb_isi3 = DALEX::explain(model_isi3, data = as.matrix(isi3[,-1]), y = isi3[,1])
ale_isi3 = DALEX::variable_response(exp_xgb_isi3, variable = "sal_50", type = "ale")
ale_isi3 %>% plot()

exp_xgb_isi4 = DALEX::explain(model_isi4, data = as.matrix(isi4[,-1]), y = isi4[,1])
ale_xgb = DALEX::variable_response(exp_xgb_isi4, variable = "sal_50", type = "ale")
ale_xgb %>% plot()



require(ingredients)
cp_isi4 = ceteris_paribus(exp_xgb_isi4, new_observation = as.matrix(te_isi4[,-1]), y = te_isi4[,1])
plot(cp_isi4, variables = c("do_0", "do_50")) + show_observations(cp_isi4, variables = c("do_0", "do_50"))


#ICE+PDP
require(pdp)
pdp::partial(model_isi1, pred.var = "do_50", train = as.matrix(isi1[, -1]), plot = T, ice = T, alpha = 0.1, plot.engine = "ggplot2")
pdp::partial(model_isi2, pred.var = "do_50", train = as.matrix(isi2[, -1]), plot = T, ice = T, alpha = 0.1, plot.engine = "ggplot2")


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
train_kono = train(
  log_abundance ~ .,
  data = df_kono,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_kono, file = paste0("tuned_params_kono", ".RData"))
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

for(i in 1:4){
  assign(paste0("kono", i),
         kono %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = kono_pred %>% filter(n_season == i)
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

write.csv(kono_imp, "kono_imp.csv")
write.csv(kono_pred, "kono_pred.csv")
write.csv(kono_cor, "kono_cor.csv")
write.csv(kono_mae, "kono_mae.csv")



require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("kono", i),
#          kono %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(kono1)
# 
# for(i in 1:4){
#   data = get(paste0("kono",i))
#   data = na.omit(data)
#   assign(paste0("kono", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_kono = c()
for(i in 1:4){
  data = get(paste0("tr_kono", i))
  p_data = get(paste0("te_kono", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_kono = rbind(pre_glm_kono, pre_glm)
}

# pre_boo_kono = c()
# for(i in 1:4){
#   load(paste0("model_kono",i,".RData"))
#   model = get(paste0("model_kono", i))
#   data = get(paste0("kono", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_kono = rbind(pre_boo_kono, boo)
# }

kono_pred$model = "Boosting"
comp_glm_kono = rbind(pre_glm_kono, kono_pred)
head(kono)
tag = distinct(kono[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_kono = merge(comp_glm_kono, tag, by = "n_season")
write.csv(comp_glm_kono, "comp_glm_kono.csv")

comp_glm_kono$model = factor(comp_glm_kono$model, levels = c("GLM", "Boosting"))
comp_glm_kono$season2 = factor(comp_glm_kono$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_kono, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Konoshiro")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
g+p+f+lab+theme_bw()+l+th




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
train_ika = train(
  log_abundance ~ .,
  data = df_ika,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ika, file = paste0("tuned_params_ika", ".RData"))
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

for(i in 1:4){
  assign(paste0("ika", i),
         ika %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = ika_pred %>% filter(n_season == i)
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

write.csv(ika_imp, "ika_imp.csv")
write.csv(ika_pred, "ika_pred.csv")
write.csv(ika_cor, "ika_cor.csv")
write.csv(ika_mae, "ika_mae.csv")



require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("ika", i),
#          ika %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(ika1)
# 
# for(i in 1:4){
#   data = get(paste0("ika",i))
#   data = na.omit(data)
#   assign(paste0("ika", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_ika = c()
for(i in 1:4){
  data = get(paste0("tr_ika", i))
  p_data = get(paste0("te_ika", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_ika = rbind(pre_glm_ika, pre_glm)
}

# pre_boo_ika = c()
# for(i in 1:4){
#   load(paste0("model_ika",i,".RData"))
#   model = get(paste0("model_ika", i))
#   data = get(paste0("ika", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_ika = rbind(pre_boo_ika, boo)
# }

ika_pred$model = "Boosting"
comp_glm_ika = rbind(pre_glm_ika, ika_pred)
head(ika)
tag = distinct(ika[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_ika = merge(comp_glm_ika, tag, by = "n_season")
write.csv(comp_glm_ika, "comp_glm_ika.csv")

comp_glm_ika$model = factor(comp_glm_ika$model, levels = c("GLM", "Boosting"))
comp_glm_ika$season2 = factor(comp_glm_ika$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_ika, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Kouika")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
g+p+f+lab+theme_bw()+l+th



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
train_ebi = train(
  log_abundance ~ .,
  data = df_ebi,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ebi, file = paste0("tuned_params_ebi", ".RData"))
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

for(i in 1:4){
  assign(paste0("ebi", i),
         ebi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = ebi_pred %>% filter(n_season == i)
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

write.csv(ebi_imp, "ebi_imp.csv")
write.csv(ebi_pred, "ebi_pred.csv")
write.csv(ebi_cor, "ebi_cor.csv")
write.csv(ebi_mae, "ebi_mae.csv")



require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("ebi", i),
#          ebi %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(ebi1)
# 
# for(i in 1:4){
#   data = get(paste0("ebi",i))
#   data = na.omit(data)
#   assign(paste0("ebi", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_ebi = c()
for(i in 1:4){
  data = get(paste0("tr_ebi", i))
  p_data = get(paste0("te_ebi", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_ebi = rbind(pre_glm_ebi, pre_glm)
}

# pre_boo_ebi = c()
# for(i in 1:4){
#   load(paste0("model_ebi",i,".RData"))
#   model = get(paste0("model_ebi", i))
#   data = get(paste0("ebi", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_ebi = rbind(pre_boo_ebi, boo)
# }

ebi_pred$model = "Boosting"
comp_glm_ebi = rbind(pre_glm_ebi, ebi_pred)
head(ebi)
tag = distinct(ebi[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_ebi = merge(comp_glm_ebi, tag, by = "n_season")
write.csv(comp_glm_ebi, "comp_glm_ebi.csv")

comp_glm_ebi$model = factor(comp_glm_ebi$model, levels = c("GLM", "Boosting"))
comp_glm_ebi$season2 = factor(comp_glm_ebi$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_ebi, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Kurumaebi")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
g+p+f+lab+theme_bw()+l+th


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
train_ana = train(
  log_abundance ~ .,
  data = df_ana,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_ana, file = paste0("tuned_params_ana", ".RData"))
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

for(i in 1:4){
  assign(paste0("ana", i),
         ana %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = ana_pred %>% filter(n_season == i)
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

write.csv(ana_imp, "ana_imp.csv")
write.csv(ana_pred, "ana_pred.csv")
write.csv(ana_cor, "ana_cor.csv")
write.csv(ana_mae, "ana_mae.csv")




require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("ana", i),
#          ana %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(ana1)
# 
# for(i in 1:4){
#   data = get(paste0("ana",i))
#   data = na.omit(data)
#   assign(paste0("ana", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_ana = c()
for(i in 1:4){
  data = get(paste0("tr_ana", i))
  p_data = get(paste0("te_ana", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_ana = rbind(pre_glm_ana, pre_glm)
}

# pre_boo_ana = c()
# for(i in 1:4){
#   load(paste0("model_ana",i,".RData"))
#   model = get(paste0("model_ana", i))
#   data = get(paste0("ana", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_ana = rbind(pre_boo_ana, boo)
# }

ana_pred$model = "Boosting"
comp_glm_ana = rbind(pre_glm_ana, ana_pred)
head(ana)
tag = distinct(ana[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_ana = merge(comp_glm_ana, tag, by = "n_season")
write.csv(comp_glm_ana, "comp_glm_ana.csv")

comp_glm_ana$model = factor(comp_glm_ana$model, levels = c("GLM", "Boosting"))
comp_glm_ana$season2 = factor(comp_glm_ana$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_ana, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Maanago")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
g+p+f+lab+theme_bw()+l+th


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
train_mako = train(
  log_abundance ~ .,
  data = df_mako,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_mako, file = paste0("tuned_params_mako", ".RData"))
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

for(i in 1:4){
  assign(paste0("mako", i),
         mako %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = mako_pred %>% filter(n_season == i)
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

write.csv(mako_imp, "mako_imp.csv")
write.csv(mako_pred, "mako_pred.csv")
write.csv(mako_cor, "mako_cor.csv")
write.csv(mako_mae, "mako_mae.csv")




require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("mako", i),
#          mako %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(mako1)
# 
# for(i in 1:4){
#   data = get(paste0("mako",i))
#   data = na.omit(data)
#   assign(paste0("mako", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_mako = c()
for(i in 1:4){
  data = get(paste0("tr_mako", i))
  p_data = get(paste0("te_mako", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_mako = rbind(pre_glm_mako, pre_glm)
}

# pre_boo_mako = c()
# for(i in 1:4){
#   load(paste0("model_mako",i,".RData"))
#   model = get(paste0("model_mako", i))
#   data = get(paste0("mako", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_mako = rbind(pre_boo_mako, boo)
# }

mako_pred$model = "Boosting"
comp_glm_mako = rbind(pre_glm_mako, mako_pred)
head(mako)
tag = distinct(mako[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_mako = merge(comp_glm_mako, tag, by = "n_season")
write.csv(comp_glm_mako, "comp_glm_mako.csv")

comp_glm_mako$model = factor(comp_glm_mako$model, levels = c("GLM", "Boosting"))
comp_glm_mako$season2 = factor(comp_glm_mako$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_mako, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Makogarei")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(panel.grid.major = element_blank(),
           #panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(2)),
           axis.text.y = element_text(size = rel(2)),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_text(size = 15),
           strip.text = element_text(size = rel(1)))
g+p+f+lab+theme_bw()+l+th



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
train_suzu = train(
  log_abundance ~ .,
  data = df_suzu,
  method = "xgbTree",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv"),
  tuneLength = 5
)
save(train_suzu, file = paste0("tuned_params_suzu", ".RData"))
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

for(i in 1:4){
  assign(paste0("suzu", i),
         suzu %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
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
  data = suzu_pred %>% filter(n_season == i)
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

write.csv(suzu_imp, "suzu_imp.csv")
write.csv(suzu_pred, "suzu_pred.csv")
write.csv(suzu_cor, "suzu_cor.csv")
write.csv(suzu_mae, "suzu_mae.csv")



require(lme4)
require(glmmTMB)
#require(car)
# for(i in 1:4){
#   assign(paste0("mako", i),
#          mako %>% filter(n_season == i) %>% select(log_abundance, do_0, do_50, sal_0, sal_50, wt_0, wt_50)
#   )
# }
# summary(mako1)
# 
# for(i in 1:4){
#   data = get(paste0("mako",i))
#   data = na.omit(data)
#   assign(paste0("mako", i),
#          normalizeFeatures(data, target = "log_abundance"))
# }

pre_glm_mako = c()
for(i in 1:4){
  data = get(paste0("tr_mako", i))
  p_data = get(paste0("te_mako", i))
  
  glm = glm(log_abundance ~ ., data = data, family = gaussian)
  pre_glm = data.frame(pred = predict(glm, newdata = p_data[,-1]), obs = p_data[,1])
  pre_glm = pre_glm %>% mutate(n_season = paste0(i), model = "GLM")
  pre_glm_mako = rbind(pre_glm_mako, pre_glm)
}

# pre_boo_mako = c()
# for(i in 1:4){
#   load(paste0("model_mako",i,".RData"))
#   model = get(paste0("model_mako", i))
#   data = get(paste0("mako", i))
#   
#   boo = data.frame(pred = predict(model, as.matrix(data[, -1])), obs = data[, 1])
#   boo = boo %>% mutate(n_season = paste0(i), model = "Boosting")
#   
#   pre_boo_mako = rbind(pre_boo_mako, boo)
# }

mako_pred$model = "Boosting"
comp_glm_mako = rbind(pre_glm_mako, mako_pred)
head(mako)
tag = distinct(mako[, 3:4], .keep_all = F)
tag$season2 = c("Winter", "Summer", "Spring", "Autumn")
comp_glm_mako = merge(comp_glm_mako, tag, by = "n_season")
write.csv(comp_glm_mako, "comp_glm_mako.csv")

comp_glm_mako$model = factor(comp_glm_mako$model, levels = c("GLM", "Boosting"))
comp_glm_mako$season2 = factor(comp_glm_mako$season2, levels = c("Winter", "Spring", "Summer", "Autumn"))

require(ggplot2)
g = ggplot(data = comp_glm_mako, aes(x = obs, y = pred))
p = geom_point()
f = facet_wrap(season2 ~ model, scales = "free", ncol = 2)
lab = labs(x = "Abundance index from the VAST", y = "Prediction", title = "Suzuki")
l = geom_abline(intercept = 0, slope = 1, colour = "red") 
th = theme(#panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = rel(2)),
  axis.text.y = element_text(size = rel(2)),
  axis.title.x = element_text(size = rel(2)),
  axis.title.y = element_text(size = rel(2)),
  legend.title = element_text(size = 15),
  strip.text = element_text(size = rel(1.2)))
g+p+f+lab+theme_bw()+l+th