# --------------------------------------------------------------
# 95-845 AAMLP: Final Project
# Nathan Deron, David Contreras, Laura Goyeneche
# Last update: December 01, 2019
# --------------------------------------------------------------

rm(list = ls())

# Define libraries
# --------------------------------------------------------------
libs = c('dplyr',
         'lubridate',
         'rgdal',
         'fastDummies',
         'glmnet','caret', 
         'tree','rpart','rpart.plot',
         'randomForest','e1071',
         'pROC','ROCR')

# Attach libraries
invisible(suppressMessages(lapply(libs, library, character.only = T)))

# Working directory
# --------------------------------------------------------------
cd = 'C:/Users/lgoye/OneDrive/Documents/Github/aamlp_chicago_rides'
setwd(cd)

# Import data
data = readRDS(paste0(cd, '/data_rds/df_final.rds'))

# Preliminary
# --------------------------------------------------------------

table(data$id_ride)
  # 12.6% taxi
  # 87.3% ride sharing 
  # unbalanced sample

# Variable selection
df = 
  data %>%
  
  # Exclude variables that aren't suppose to add predictive accuracy to our model
  select(-trip_id,
         -pickup_centroid_latitude, 
         -pickup_centroid_longitude, 
         -dropoff_centroid_latitude, 
         -dropoff_centroid_longitude,
         -h_start, 
         -pickup_total_crime,
         -pickup_tract,
         -pickup_chgoca
         
         # Exclude variables that are perfectly correlated with other 
         # We kept rate variables instead of total population
         -grep("pop", colnames(data)),
         -grep("_total", colnames(data)))

# Find variables with variance equal to zero
# Exclude them from the sample
zeroVar = nearZeroVar(df)
df = df[,-zeroVar]

rm(data)

# Rename variables 
names(df) = gsub(" ", "", names(df), fixed = T)
names(df) = gsub("-", "", names(df), fixed = T)
names(df) = gsub(".", "_", names(df), fixed = T)

# Train and Test data
set.seed(123)

  temp     = df %>% filter(day == 1)
  idx0     = sample(1:nrow(temp), round(0.003*nrow(temp))) 
  temp     = temp[idx0,]
  temp     = temp %>% select(-hm_start, -date_start, -date_end) 

idx      = sample(1:nrow(temp), round(0.7*nrow(temp))) 
df_train = temp[idx,]  %>% as_tibble()
df_test  = temp[-idx,] %>% as_tibble()

rm(df, idx0, idx)

# CV Classification models
# --------------------------------------------------------------

# Models 
  
  set.seed(123)

  m    = "cv"
  k    = 5
  metr = "Accuracy"
  tLen = 20
  pPro = c("center","scale")

  logitCV = train(as.factor(id_ride) ~ .,
                  data       = df_train, 
                  method     = "glm", 
                  metric     = metr,
                  family     = binomial("logit"),
                  trControl  = trainControl(method = m, number = k),
                  preProcess = pPro)
  
  # Given that mtry (# of variables randomly sampled as candidates at each split) and
    # ntree (# of trees to grow) are the most likely to have high effect on final accuracy
  # Random search: One search strategy that we can use is to try random values within a range.
    # This can be good if we are unsure of what the value might be and we want to overcome any 
    # biases we may have for setting the parameter (like the suggested equation above)
  
  rfCV    = train(as.factor(id_ride) ~ ., 
                  data       = df_train, 
                  method     = "rf", 
                  metric     = metr,
                  trControl  = trainControl(method = m, number = k, search = "random"),
                  tuneLength = tLen, 
                  preProcess = pPro)
  
  # kNN requires variables to be normalized or scaled
  
  knnCV   = train(as.factor(id_ride) ~ ., 
                  data       = df_train, 
                  method     = "knn",
                  metric     = metr,
                  trControl  = trainControl(method = m, number = k),
                  tuneLength = tLen,
                  preProcess = pPro)
  
  treeCV  = train(factor(id_ride, labels = c("yes","no")) ~ .,
                  data       = df_train,
                  method     = "rpart",
                  metric     = metr,
                  trControl  = trainControl(method = m, number = k),
                  tuneLength = tLen,
                  preProcess = pPro)
  
# Plots evaluation 
  
  plot(rfCV, 
       xlab = "# of Randomly Selected Predictors",
       ylab = "Accuracy CV", 
       lwd  = 2)
  
  plot(knnCV,
       xlab = "# of Neighbors",
       ylab = "Accuracy CV",
       lwd  = 2)
  
  plot(treeCV,
       xlab = "Complexity Parameter",
       ylab = "Accuracy CV",
       lwd  = 2) 
 
# F1 score
  
  glmCM   = table(predict(logitCV, df_test, type = "raw"), as.factor(df_test$id_ride))  
    glm_p = round(glmCM[1,1]/(glmCM[1,1] + glmCM[1,2]),2)
    glm_r = round(glmCM[1,1]/(glmCM[1,1] + glmCM[2,2]),2)
    glmF1 = round(2*(glm_p*glm_r)/(glm_p + glm_r),2)
    
  rfCM    = table(predict(rfCV, df_test, type = "raw"), as.factor(df_test$id_ride))  
    rf_p  = round(rfCM[1,1]/(rfCM[1,1] + rfCM[1,2]),2)
    rf_r  = round(rfCM[1,1]/(rfCM[1,1] + rfCM[2,2]),2)
    rfF1  = round(2*(rf_p*rf_r)/(rf_p + rf_r),2)
    
  treeCM   = table(predict(treeCV, df_test, type = "raw"), as.factor(df_test$id_ride))  
    tree_p = round(treeCM[1,1]/(treeCM[1,1] + treeCM[1,2]),2)
    tree_r = round(treeCM[1,1]/(treeCM[1,1] + treeCM[2,2]),2)
    treeF1 = round(2*(tree_p*tree_r)/(tree_p + tree_r),2)
    
  knnCM   = table(predict(knnCV, df_test, type = "raw"), as.factor(df_test$id_ride))  
    knn_p = round(knnCM[1,1]/(knnCM[1,1] + knnCM[1,2]),2)
    knn_r = round(knnCM[1,1]/(knnCM[1,1] + knnCM[2,2]),2)
    knnF1 = round(2*(knn_p*knn_r)/(knn_p + knn_r),2)
    
# Accuracy
  
  glmtest  = predict(logitCV, df_test, type = "prob") %>% as_tibble()  
  glmtestP = prediction(glmtest[,2], df_test$id_ride %>% as.factor() %>% as_tibble())
  glmROC   = performance(glmtestP, "tpr", "fpr")
    
  rftest   = predict(rfCV, df_test, type = "prob") %>% as_tibble()
  rftestP  = prediction(rftest[,2], df_test$id_ride %>% as.factor() %>% as_tibble())
  rfROC    = performance(rftestP, "tpr","fpr")
  
  tretest   = predict(treeCV, df_test, type = "prob") %>% as_tibble()
  tretestP  = prediction(tretest[,2], df_test$id_ride %>% as.factor() %>% as_tibble())
  treeROC   = performance(tretestP, "tpr","fpr")
  
  knntest   = predict(knnCV, df_test, type = "prob") %>% as_tibble()
  knntestP  = prediction(knntest[,2], df_test$id_ride %>% as.factor() %>% as_tibble())
  knnROC    = performance(knntestP, "tpr","fpr")
  
  ROC = data.frame(TPR = rfROC@y.values[[1]], FPR = rfROC@x.values[[1]], Model = "Random Forest")
  ROC = rbind(ROC, data.frame(TPR = glmROC@y.values[[1]]  , FPR = glmROC@x.values[[1]], Model = "Logistic Regression"))
  ROC = rbind(ROC, data.frame(TPR = treeROC@y.values[[1]] , FPR = treeROC@x.values[[1]], Model = "Regression Tree"))
  ROC = rbind(ROC, data.frame(TPR = knnROC@y.values[[1]]  , FPR = knnROC@x.values[[1]], Model = "KNN"))
  
# Table
  
  AUC = c(performance(rftestP , "tpr","fpr", measure = "auc")@y.values[[1]] %>% round(., digits = 2),
          performance(glmtestP, "tpr","fpr", measure = "auc")@y.values[[1]] %>% round(., digits = 2),
          performance(tretestP, "tpr","fpr", measure = "auc")@y.values[[1]] %>% round(., digits = 2),
          performance(knntestP, "tpr","fpr", measure = "auc")@y.values[[1]] %>% round(., digits = 2))
  
  F1  = c(rfF1, glmF1, treeF1, knnF1)
  P   = c(rf_p, glm_p, tree_p, knn_p)
  R   = c(rf_r, glm_r, tree_r, knn_r)
  
  Model = c("Random Forest", "Logistic Regression", "Regression Tree", "Knn")
  table = data.frame(model, AUC, F1, P, R)
  xtable(table, type = "latex", file = "Table.tex")
  
# ROC plot
  
  ggplot(data = ROC) + 
    geom_line(aes(x = FPR, y = TPR, color = Model), size = 0.8) + 
    geom_abline(slope = 1) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
    scale_color_discrete(name   = "Model", 
                         labels = c("Random Forest",
                                    "Logistic Regression",
                                    "Regression Tree",
                                    "Knn")) +
    theme(legend.position = c(0.8, 0.2))

# Variable importance
  
  varImp(rfCV)$importance %>%
    as.data.frame() %>%
    mutate(vars = row.names(.),
           Overall = round(Overall, 2)) %>%
    arrange(-Overall) %>%
    top_n(15, wt = Overall) %>%
    ggplot(aes(x = reorder(vars, Overall), y = Overall)) +
    geom_bar(stat = 'identity', fill = "steelblue") +
    #scale_x_discrete(labels = c("Census Tr")) +
    coord_flip() + 
    labs(title = "Variable Importance", 
         x     = "Variable", 
         y     = " ")
   
# --------------------------------------------------------------