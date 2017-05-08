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
library("glmnet")
install.packages("pROC")
library("pROC")
library(ggplot2)
library(dplyr)

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
sample_size <- floor(0.75 * nrow(tbu_final))
train_ind <- sample(seq_len(nrow(tbu_final)), size = sample_size)
train <- tbu_final[train_ind, ]
test <- tbu_final[-train_ind, ]


###### VERSION 1: LINEAR REGRESSION ################################################################

# RUN REGRESSION
fit.logit <- glm(converted_to_VISIT ~ .,family=binomial(link = "logit"), data = train)

# GET MODEL SUMMARY
fit.logit.summary <- fit.logit %>% summary
fit.logit
fit.logit.summary

# GET ROC VALUE AND PLOT
train.logreg <- train
train.logreg$p_garbage <- predict(fit.logit, type = "response")
ROC.garbage <- roc(converted_to_VISIT ~ p_garbage, data = train.logreg)
plot(ROC.garbage)
ROC.garbage

###### VERSION 1: LASSO VS. RIDGE ##################################################################

# DEFINE X AND Ys
X.val <- as.matrix(model.matrix(converted_to_VISIT ~., train)[,-1])
Y.val <- as.matrix(train[,1])


# DEFINE ALPHA
fit.elastic.cv <- cv.glmnet(X.val, Y.val, alpha=0.01, family="binomial", nfolds = 10, type.measure = "deviance") 
#### THE TWO ALPHAS RELATED???????????????
fit.elastic.1se <- glmnet(X.val, Y.val, alpha=0.01, family="binomial", lambda=fit.elastic.cv$lambda.1se)
fit.elastic.1se.beta <- coef(fit.elastic.1se)
beta.elastic <- fit.elastic.1se.beta[which(fit.elastic.1se.beta !=0),]
beta.elastic <- as.matrix(beta.elastic)

# DO REGRESSION WITH PARAMS CHOSEN ABOVE
fit.elastic <- glm(converted_to_VISIT ~ total_impressions + first_touchpoint + 
                     last_touchpoint + timeframe_hours + channel_display_abs + 
                     channel_highimpact_abs + channel_video_abs + platform_desktop_abs + 
                     platform_mobile_abs + typeofbuy_contentamplification_abs + 
                     typeofbuy_network_abs + typeofbuy_programmatic_abs + 
                     typeofbuy_publisherdirect_abs + sitetype_contentamplification_abs + 
                     sitetype_network_abs + sitetype_portal_abs + sitetype_programmatic_abs + 
                     sitetype_videoportal_abs + overalltargeting_behavioral_abs + 
                     overalltargeting_contextual_abs + overalltargeting_geo_abs + 
                     overalltargeting_predictive_abs + overalltargeting_prospecting_abs + 
                     funnel_lower_abs + funnel_middle_abs + funnel_upper_abs + 
                     NULL_touches,family=binomial(link = "logit"), data = train)

# GET MODEL SUMMARY [TRAIN DATA]
fit.elastic.summary <- fit.elastic %>% summary
fit.elastic
fit.elastic.summary

# GET ROC VALUE AND PLOT [TRAIN DATA]
train$p_elastic <- predict(fit.elastic, type = "response")
ROC.elastic <- roc(converted_to_VISIT ~ p_elastic, data = train)
plot(ROC.elastic)
ROC.elastic


# RUN MODEL WITH TEST DATA!
test$p_elastic <- predict(fit.elastic, newdata = test, type = "response")

# CUSTOM FUNCTION
lapply.uvec <- function(X,fx){lapply(X,fx) %>% unlist %>% as.vector}
perc.f.75 <- function(i){sum(as.numeric(as.numeric(test$p_elastic > i ) 
                                        == (test$converted_to_VISIT %>% unique)[2]) )/nrow(test)}
pY.75 <- lapply.uvec(((1:1000)/1000),perc.f.75) 

# PLOT ELASTIC NET LOGIT PLOT 
results.elasticlogit <- data.frame(X=c(((1:1000)/1000)),p.75=pY.75)
results.elasticlogit.plot <- ggplot(results.elasticlogit , aes(X)) +
  geom_line(aes(y = p.75, colour = "p.75")) +
  ggtitle("Elastic Net Logit") + ylab("Accuracy Rate at p=X decision boundary")
results.elasticlogit.plot

# test$p_elastic 
# table(test$converted_to_VISIT)


# VALUE WITH ALPHA = 0
alpha0.p01 <- perc.f.75(0.01)
alpha0.p02 <- perc.f.75(0.02)
alpha0.p03 <- perc.f.75(0.03)
alpha0.p10 <- perc.f.75(0.10)

# VALUE WITH ALPHA = 0.2
alpha02.p01 <- perc.f.75(0.01)
alpha02.p02 <- perc.f.75(0.02)
alpha02.p03 <- perc.f.75(0.03)
alpha02.p10 <- perc.f.75(0.10)

# VALUE WITH ALPHA = 0.4
alpha04.p01 <- perc.f.75(0.01)
alpha04.p02 <- perc.f.75(0.02)
alpha04.p03 <- perc.f.75(0.03)
alpha04.p10 <- perc.f.75(0.10)

# VALUE WITH ALPHA = 0.6
alpha06.p01 <- perc.f.75(0.01)
alpha06.p02 <- perc.f.75(0.02)
alpha06.p03 <- perc.f.75(0.03)
alpha06.p10 <- perc.f.75(0.10)

# VALUE WITH ALPHA = 0.8
alpha08.p01 <- perc.f.75(0.01)
alpha08.p02 <- perc.f.75(0.02)
alpha08.p03 <- perc.f.75(0.03)
alpha08.p10 <- perc.f.75(0.10)

# VALUE WITH ALPHA = 0.99
alpha099.p01 <- perc.f.75(0.01)
alpha099.p02 <- perc.f.75(0.02)
alpha099.p03 <- perc.f.75(0.03)
alpha099.p10 <- perc.f.75(0.10)



# CUSTOM FUNCTION: GET ROWNAMES TO PASTE INTO FUNCTION (after define alpha part)
# rownames(beta.elastic)
# p0lm <- function(X){paste0(X, collapse = " + ")}
# p0lm(rownames(beta.elastic)) 
