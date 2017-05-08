# SETUP
install.packages("plyr")
library("plyr") 
install.packages("gdata")
library("gdata") 
install.packages("ROCR")
library(ROCR)
install.packages("leaps")
library(leaps)
install.packages("stat")
library("stat")

install.packages("glmnet")
install.packages("pROC")
library("glmnet")
library("pROC")
library(ggplot2)
library(dplyr)
library(plyr)
install.packages("xlsx")
library(xlsx)

# ---------------------------------------------------------------------------------------

# READ IN DATA
table_by_user <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_ROLLUP_v1.csv")

###########################################################################################
###########################################################################################

# PREPARATION: SIMPLIFY TABLE AND PUT INTO MATRIX FORMAT
table_by_user_v1 <- table_by_user
colnames(table_by_user_v1)[19] <- "NULL_touches"
table_by_user_v1$converted_to_VISIT <- ifelse(table_by_user_v1$converted_to_VISIT == "FALSE", 0, 1)

tbu_cleaned <- subset(table_by_user_v1, select = -c(X, ak_user_id, 
                                          total_conversions_VISIT, total_conversions_SALES, total_conversions_NULL, 
                                          converted_to_SALES, 
                                          timeframe_secs, timeframe_mins, timeframe_days, 
                                          channel_display_perc, channel_highimpact_perc, channel_search_perc, 
                                          channel_video_perc, channel_NULL_perc, 
                                          platform_NULL_abs, platform_desktop_perc, platform_mobile_perc, 
                                          platform_social_perc, platform_NULL_perc, 
                                          typeofbuy_NULL_abs, typeofbuy_contentamplification_perc, typeofbuy_endemic_perc,
                                          typeofbuy_programmatic_perc, typeofbuy_publisherdirect_perc, typeofbuy_search_perc,
                                          typeofbuy_network_perc, typeofbuy_NULL_perc, 
                                          sitetype_NULL_abs, sitetype_broadcast_perc, sitetype_contentamplification_perc,
                                          sitetype_discoveryengine_perc, sitetype_endemic_perc, sitetype_internetradio_perc,
                                          sitetype_lifestyle_perc, sitetype_majormedia_perc, sitetype_network_perc, 
                                          sitetype_portal_perc, sitetype_programmatic_perc, sitetype_sportsleague_perc,
                                          sitetype_videoportal_perc, sitetype_NULL_perc, 
                                          overalltargeting_NULL_abs, overalltargeting_behavioral_perc,
                                          overalltargeting_contextual_perc, overalltargeting_geo_perc, 
                                          overalltargeting_lookalike_perc, overalltargeting_predictive_perc, 
                                          overalltargeting_prospecting_perc, overalltargeting_remarketing_perc, 
                                          overalltargeting_retargeting_perc, overalltargeting_NULL_perc, 
                                          funnel_NULL_abs, funnel_lower_perc, funnel_middle_perc, 
                                          funnel_upper_perc, funnel_NULL_perc))
tbu_cleaned <- subset(tbu_cleaned, select=c(converted_to_VISIT,total_impressions, 
                              first_touchpoint:channel_video_abs,
                              platform_desktop_abs:funnel_upper_abs,
                              NULL_touches))

tbu_final <- tbu_cleaned
tbu_final$first_touchpoint <- as.POSIXct(tbu_final$first_touchpoint)
tbu_final$last_touchpoint <- as.POSIXct(tbu_final$last_touchpoint)
tbu_final$first_touchpoint <- as.numeric(tbu_final$first_touchpoint)
tbu_final$last_touchpoint <- as.numeric(tbu_final$last_touchpoint)


# CREATE TEST AND TRAINING SET
set.seed(123)
sample_size <- floor(0.5 * nrow(tbu_final))
train_ind <- sample(seq_len(nrow(tbu_final)), size = sample_size)
train <- tbu_final[train_ind, ]
test <- tbu_final[-train_ind, ]


####################################################################################################
####################################################################################################
####################################################################################################
###### VERSION 1: LINEAR REGRESSION ################################################################
####################################################################################################


# RUN REGRESSION
fit.logit <- glm(converted_to_VISIT ~.,family=binomial(link = "logit"), data = train)
fit.logit <- glm(converted_to_VISIT ~ total_impressions + last_touchpoint + 
                   platform_desktop_abs + overalltargeting_behavioral_abs + 
                   overalltargeting_contextual_abs + funnel_lower_abs,
                 family=binomial(link = "logit"), data = train)

# GET MODEL SUMMARY
fit.logit.summary <- fit.logit %>% summary
fit.logit
fit.logit.summary
# get model AIC
fit.logit$aic

write.xlsx(fit.logit.summary$coefficients, file = "imp1_script_logreg_RESULTS.xlsx", sheetName = "fit.logit.summary")


#total_impressions + last_touchpoint + platform_desktop_abs + overalltargeting_behavioral_abs + 
#  overalltargeting_contextual_abs + overalltargeting_geo_abs + overalltargeting_predictive_abs + 
#  funnel_lower_abs


# get variables as text
fit.logit_coeff <- as.matrix(fit.logit$coefficients)
p0lm <- function(X){paste0(X, collapse = " + ")}
var_logit <- p0lm(rownames(fit.logit_coeff)) 

# GET PREDICTON [TRAIN]
train.logreg <- train
train.logreg$p_garbage <- predict(fit.logit, type = "response")
# calculate accuracy (~RMSE)
train.logreg$predicted_conversion <- ifelse(train.logreg$p_garbage > .5, 1, 0)
act_vs_pred.logreg.train <- table(train.logreg$converted_to_VISIT, train.logreg$predicted_conversion)
true_p.logreg.train <- mean(train.logreg$converted_to_VISIT==train.logreg$predicted_conversion)
true_pos_p.logreg.train <- act_vs_pred.logreg.train[2,2]/sum(act_vs_pred.logreg.train[,2])
act_vs_pred.logreg.train
true_p.logreg.train
true_pos_p.logreg.train
# calculate and plot ROC
ROC.garbage.train <- roc(converted_to_VISIT ~ p_garbage, data = train.logreg)
plot(ROC.garbage.train)
ROC.garbage.train

# GET PREDICTION [TEST]
test.logreg <- test
test.logreg$p_garbage <- predict(fit.logit, newdata = test.logreg, type = "response")
# calculate accuracy (~RMSE)
test.logreg$predicted_conversion <- ifelse(test.logreg$p_garbage > .5, 1, 0)
act_vs_pred.logreg.test <- table(test.logreg$converted_to_VISIT, test.logreg$predicted_conversion)
true_p.logreg.test <- mean(test.logreg$converted_to_VISIT==test.logreg$predicted_conversion)
true_pos_p.logreg.test <- act_vs_pred.logreg.test[2,2]/sum(act_vs_pred.logreg.test[,2])
act_vs_pred.logreg.test
true_p.logreg.test
true_pos_p.logreg.test
# calculate and plot ROC
ROC.garbage.test <- roc(converted_to_VISIT ~ p_garbage, data = test.logreg)
plot(ROC.garbage.test)
ROC.garbage.test

# how to plot predicted against actual
plot(test.logreg$converted_to_VISIT, test.logreg$predicted_conversion)



####################################################################################################
####################################################################################################
###### VERSION 1: LASSO VS. RIDGE ##################################################################
####################################################################################################


# SETUP FINAL RESULTS TABLES
# selected variable table - TEXT
var_store <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(var_store)[1] <- "alpha"
conames(var_store)[3] <- "no_variables"
colnames(var_store)[2] <- "variables"
var_store[,1] <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

# final results table: accuracy
fin_results <- data.frame(matrix(ncol = 8, nrow = 6))
colnames(fin_results)[1] <- "alpha"
colnames(fin_results)[2] <- "aic"
colnames(fin_results)[3] <- "true_p.rl.train"
colnames(fin_results)[4] <- "true_pos_p.rl.train"
colnames(fin_results)[5] <- "ROC.elastic.train$auc"
colnames(fin_results)[6] <- "true_p.rl.test"
colnames(fin_results)[7] <- "true_pos_p.rl.test"
colnames(fin_results)[8] <- "ROC.elastic.test$auc"
fin_results[,1] <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

# final results table: elastic net logit plot 
net_logit_val <- data.frame(matrix(ncol = 5, nrow = 6))
colnames(net_logit_val)[1] <- "alpha"
colnames(net_logit_val)[2] <- "X=0.01"
colnames(net_logit_val)[3] <- "X=0.02"
colnames(net_logit_val)[4] <- "X=0.03"
colnames(net_logit_val)[5] <- "X=0.10"
net_logit_val[,1] <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)


# DEFINE X AND Ys
X.val <- as.matrix(model.matrix(converted_to_VISIT ~., train)[,-1])
Y.val <- as.matrix(train[,1])

# DEFINE ALPHA
alp = 0.0
alp_char <- as.character(alp)

# DEFINE LAMBDA 
fit.elastic.cv <- cv.glmnet(X.val, Y.val, alpha=alp, family="binomial", 
                            nfolds = 10, type.measure = "deviance") 
fit.elastic.1se <- glmnet(X.val, Y.val, alpha=alp, family="binomial", 
                          lambda=fit.elastic.cv$lambda.1se)
fit.elastic.1se.beta <- coef(fit.elastic.1se)
# test and see values
plot(fit.elastic.cv)
#plot(fit.elastic.1se)
#fit.elastic.cv
#fit.elastic.1se

# FIND VARIABLES ACCORDING TO RIDGE/LASSO ELASTIC SELECTION
beta.elastic <- fit.elastic.1se.beta[which(fit.elastic.1se.beta !=0),]
beta.elastic <- as.matrix(beta.elastic)

# SAVE THE CHOSEN VALUES IN DF
# clean up text
rownames_list <- rownames(beta.elastic)
p0lm <- function(X){paste0(X, collapse = " + ")}
raw_text <- p0lm(rownames(beta.elastic)) 
clean_text <- gsub("\\(Intercept\\) \\+ ", "", raw_text)
# enter into table
var_store[1,2] <- ifelse(alp == 0.0, length(rownames_list))
var_store[1,3] <- ifelse(alp == 0.0, clean_text)
var_store[2,2] <- ifelse(alp == 0.2, length(rownames_list))
var_store[2,3] <- ifelse(alp == 0.2, clean_text)
var_store[3,2] <- ifelse(alp == 0.4, length(rownames_list))
var_store[3,3] <- ifelse(alp == 0.4, clean_text)
var_store[4,2] <- ifelse(alp == 0.6, length(rownames_list))
var_store[4,3] <- ifelse(alp == 0.6, clean_text)
var_store[5,2] <- ifelse(alp == 0.8, length(rownames_list))
var_store[5,3] <- ifelse(alp == 0.8, clean_text)
var_store[6,2] <- ifelse(alp == 1.0, length(rownames_list))
var_store[6,23] <- ifelse(alp == 1.0, clean_text)


# DO LOGISTIC REGRESSION WITH VARIABLES CHOSEN ABOVE
# pull variables as string
picked_var <- as.character(switch(alp_char, "0" = {var_store[1,3]},
                                               "0.2" = {var_store[2,3]},
                                               "0.4" = {var_store[3,3]},
                                               "0.6" = {var_store[4,3]},
                                               "0.8" = {var_store[5,3]},
                                               "1" = {var_store[6,3]}))
# run regression
formula_text <- paste("converted_to_VISIT ~ ", picked_var)
fit.elastic <- glm(as.formula(formula_text), family=binomial(link = 'logit'), 
                   data = train)


# GET MODEL SUMMARY [TRAIN DATA]
fit.elastic.summary <- fit.elastic %>% summary
fit.elastic
fit.elastic.summary
# get model AIC
fit.elastic$aic

# GET PREDICTON [TRAIN]
train.rl <- train
train.rl$p_elastic <- predict(fit.elastic, type = "response")
# calculate accuracy (~RMSE)
train.rl$predicted_conversion <- ifelse(train.rl$p_elastic > .5, 1, 0)
act_vs_pred.rl.train <- table(train.rl$converted_to_VISIT, train.rl$predicted_conversion)
true_p.rl.train <- mean(train.rl$converted_to_VISIT==train.rl$predicted_conversion)
true_pos_p.rl.train <- act_vs_pred.rl.train[2,2]/sum(act_vs_pred.rl.train[,2])
act_vs_pred.rl.train
true_p.rl.train
true_pos_p.rl.train
# calculate and plot ROC
ROC.elastic.train <- roc(converted_to_VISIT ~ p_elastic, data = train.rl)
plot(ROC.elastic.train)
ROC.elastic.train


# GET PREDICTON [TEST]
test.rl <- test
test.rl$p_elastic <- predict(fit.elastic, newdata = test.rl, type = "response")
# calculate accuracy rate
test.rl$predicted_conversion <- ifelse(test.rl$p_elastic > .5, 1, 0)
act_vs_pred.rl.test <- table(test.rl$converted_to_VISIT, test.rl$predicted_conversion)
true_p.rl.test <- mean(test.rl$converted_to_VISIT==test.rl$predicted_conversion)
true_pos_p.rl.test <- act_vs_pred.rl.test[2,2]/sum(act_vs_pred.rl.test[,2])
act_vs_pred.rl.test
true_p.rl.test
true_pos_p.logreg.test
# calculate and plot ROC
ROC.elastic.test <- roc(converted_to_VISIT ~ p_elastic, data = test.rl)
plot(ROC.elastic.test)
ROC.elastic.test
ROC.elastic.test$auc


# CUSTOM FUNCTION TO PLOT ELAST NET LOGIT PLOT
lapply.uvec <- function(X,fx){lapply(X,fx) %>% unlist %>% as.vector}
perc.f.75 <- function(i){sum(as.numeric(as.numeric(test.rl$p_elastic > i ) 
                                        == (test.rl$converted_to_VISIT %>% unique)[2]) )/nrow(test.rl)}
pY.75 <- lapply.uvec(((1:1000)/1000),perc.f.75) 


# PLOT ELASTIC NET LOGIT PLOT 
results.elasticlogit <- data.frame(X=c(((1:1000)/1000)),p.75=pY.75)
results.elasticlogit.plot <- ggplot(results.elasticlogit , aes(X)) +
  geom_line(aes(y = p.75, colour = "p.75")) +
  ggtitle("Elastic Net Logit") + ylab("Accuracy Rate at p=X decision boundary")
results.elasticlogit.plot


# INSERT VALUES INTO FINAL RESULTS TABLES
# final results table: accuracy
fin_results[1,2] <- ifelse(alp == 0.0, fit.elastic$aic)
fin_results[1,3] <- ifelse(alp == 0.0, true_p.rl.train)
fin_results[1,4] <- ifelse(alp == 0.0, true_pos_p.rl.train)
fin_results[1,5] <- ifelse(alp == 0.0, ROC.elastic.train$auc)
fin_results[1,6] <- ifelse(alp == 0.0, true_p.rl.test)
fin_results[1,7] <- ifelse(alp == 0.0, true_pos_p.rl.test)
fin_results[1,8] <- ifelse(alp == 0.0, ROC.elastic.test$auc)
fin_results[2,2] <- ifelse(alp == 0.2, fit.elastic$aic)
fin_results[2,3] <- ifelse(alp == 0.2, true_p.rl.train)
fin_results[2,4] <- ifelse(alp == 0.2, true_pos_p.rl.train)
fin_results[2,5] <- ifelse(alp == 0.2, ROC.elastic.train$auc)
fin_results[2,6] <- ifelse(alp == 0.2, true_p.rl.test)
fin_results[2,7] <- ifelse(alp == 0.2, true_pos_p.rl.test)
fin_results[2,8] <- ifelse(alp == 0.2, ROC.elastic.test$auc)
fin_results[3,2] <- ifelse(alp == 0.4, fit.elastic$aic)
fin_results[3,3] <- ifelse(alp == 0.4, true_p.rl.train)
fin_results[3,4] <- ifelse(alp == 0.4, true_pos_p.rl.train)
fin_results[3,5] <- ifelse(alp == 0.4, ROC.elastic.train$auc)
fin_results[3,6] <- ifelse(alp == 0.4, true_p.rl.test)
fin_results[3,7] <- ifelse(alp == 0.4, true_pos_p.rl.test)
fin_results[3,8] <- ifelse(alp == 0.4, ROC.elastic.test$auc)
fin_results[4,2] <- ifelse(alp == 0.6, fit.elastic$aic)
fin_results[4,3] <- ifelse(alp == 0.6, true_p.rl.train)
fin_results[4,4] <- ifelse(alp == 0.6, true_pos_p.rl.train)
fin_results[4,5] <- ifelse(alp == 0.6, ROC.elastic.train$auc)
fin_results[4,6] <- ifelse(alp == 0.6, true_p.rl.test)
fin_results[4,7] <- ifelse(alp == 0.6, true_pos_p.rl.test)
fin_results[4,8] <- ifelse(alp == 0.6, ROC.elastic.test$auc)
fin_results[5,2] <- ifelse(alp == 0.8, fit.elastic$aic)
fin_results[5,3] <- ifelse(alp == 0.8, true_p.rl.train)
fin_results[5,4] <- ifelse(alp == 0.8, true_pos_p.rl.train)
fin_results[5,5] <- ifelse(alp == 0.8, ROC.elastic.train$auc)
fin_results[5,6] <- ifelse(alp == 0.8, true_p.rl.test)
fin_results[5,7] <- ifelse(alp == 0.8, true_pos_p.rl.test)
fin_results[5,8] <- ifelse(alp == 0.8, ROC.elastic.test$auc)
fin_results[6,2] <- ifelse(alp == 1.0, fit.elastic$aic)
fin_results[6,3] <- ifelse(alp == 1.0, true_p.rl.train)
fin_results[6,4] <- ifelse(alp == 1.0, true_pos_p.rl.train)
fin_results[6,5] <- ifelse(alp == 1.0, ROC.elastic.train$auc)
fin_results[6,6] <- ifelse(alp == 1.0, true_p.rl.test)
fin_results[6,7] <- ifelse(alp == 1.0, true_pos_p.rl.test)
fin_results[6,8] <- ifelse(alp == 1.0, ROC.elastic.test$auc)
fin_results

# final results table: elastic net logit plot
# final results table: elastic net logit plot
net_logit_val[1,2] <- ifelse(alp == 0.0, perc.f.75(0.01))
net_logit_val[1,3] <- ifelse(alp == 0.0, perc.f.75(0.02))
net_logit_val[1,4] <- ifelse(alp == 0.0, perc.f.75(0.03))
net_logit_val[1,5] <- ifelse(alp == 0.0, perc.f.75(0.10))
net_logit_val[1,2] <- ifelse(alp == 0.0, perc.f.75(0.01))
net_logit_val[1,3] <- ifelse(alp == 0.0, perc.f.75(0.02))
net_logit_val[1,4] <- ifelse(alp == 0.0, perc.f.75(0.03))
net_logit_val[1,5] <- ifelse(alp == 0.0, perc.f.75(0.10))
net_logit_val[2,2] <- ifelse(alp == 0.2, perc.f.75(0.01))
net_logit_val[2,3] <- ifelse(alp == 0.2, perc.f.75(0.02))
net_logit_val[2,4] <- ifelse(alp == 0.2, perc.f.75(0.03))
net_logit_val[2,5] <- ifelse(alp == 0.2, perc.f.75(0.10))
net_logit_val[3,2] <- ifelse(alp == 0.4, perc.f.75(0.01))
net_logit_val[3,3] <- ifelse(alp == 0.4, perc.f.75(0.02))
net_logit_val[3,4] <- ifelse(alp == 0.4, perc.f.75(0.03))
net_logit_val[3,5] <- ifelse(alp == 0.4, perc.f.75(0.10))
net_logit_val[4,2] <- ifelse(alp == 0.6, perc.f.75(0.01))
net_logit_val[4,3] <- ifelse(alp == 0.6, perc.f.75(0.02))
net_logit_val[4,4] <- ifelse(alp == 0.6, perc.f.75(0.03))
net_logit_val[4,5] <- ifelse(alp == 0.6, perc.f.75(0.10))
net_logit_val[5,2] <- ifelse(alp == 0.8, perc.f.75(0.01))
net_logit_val[5,3] <- ifelse(alp == 0.8, perc.f.75(0.02))
net_logit_val[5,4] <- ifelse(alp == 0.8, perc.f.75(0.03))
net_logit_val[5,5] <- ifelse(alp == 0.8, perc.f.75(0.10))
net_logit_val[6,2] <- ifelse(alp == 1.0, perc.f.75(0.01))
net_logit_val[6,3] <- ifelse(alp == 1.0, perc.f.75(0.02))
net_logit_val[6,4] <- ifelse(alp == 1.0, perc.f.75(0.03))
net_logit_val[6,5] <- ifelse(alp == 1.0, perc.f.75(0.10))
net_logit_val


var_store
fin_results
net_logit_val



write.xlsx(fin_results, file="imp1_script_logreg_RESULTS.xlsx", sheetName="1 - fin_results")
write.xlsx(net_logit_val, file="imp1_script_logreg_RESULTS.xlsx", sheetName="2 - net_logit_val", append=TRUE)
write.xlsx(var_store, file="imp1_script_logreg_RESULTS.xlsx", sheetName="3 - var_store", append=TRUE)


# CUSTOM FUNCTION: GET ROWNAMES TO PASTE INTO FUNCTION (after define alpha part)
# rownames(beta.elastic)
# p0lm <- function(X){paste0(X, collapse = " + ")}
# p0lm(rownames(beta.elastic)) 



##################################################################################################
##################################################################################################
##################################################################################################

# ALTERNATIVE FUNCTIONS... didn't work

# net_logit_insert_vector <- c(alp, perc.f.75(0.01), perc.f.75(0.02), perc.f.75(0.03), perc.f.75(0.10))
# net_logit_val[1,] <- ifelse(alp == 0.0, net_logit_insert_vector)
# net_logit_val[2,] <- ifelse(alp == 0.2, net_logit_insert_vector)
# net_logit_val[3,] <- ifelse(alp == 0.4, net_logit_insert_vector)
# net_logit_val[4,] <- ifelse(alp == 0.6, net_logit_insert_vector)
# net_logit_val[5,] <- ifelse(alp == 0.8, net_logit_insert_vector)
# net_logit_val[6,] <- ifelse(alp == 1.0, net_logit_insert_vector)
# net_logit_val

# fin_results_insert_vector <- c(alp, test.rl$aic, true_p.rl.train, true_pos_p.rl.train, 
#                                ROC.elastic.train$auc, true_p.rl.test, true_pos_p.rl.test, 
#                                ROC.elastic.test$auc)
# fin_results[1,] <- ifelse(alp == 0.0, fin_results_insert_vector)
# fin_results[2,] <- ifelse(alp == 0.2, fin_results_insert_vector)
# fin_results[3,] <- ifelse(alp == 0.4, fin_results_insert_vector)
# fin_results[4,] <- ifelse(alp == 0.6, fin_results_insert_vector)
# fin_results[5,] <- ifelse(alp == 0.8, fin_results_insert_vector)
# fin_results[6,] <- ifelse(alp == 1.0, fin_results_insert_vector)
# fin_results


