install.packages("tree")
library(tree)
library(ISLR)
library(ggplot2)

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

count(tbu_final$converted_to_VISIT)

################################################################################


tree.conv <- tree(train$converted_to_VISIT~., train)
summary(tree.conv)

plot(tree.conv)
text(tree.conv, pretty=0)
tree.conv

tree.pred.train <- predict(tree.conv, train)
tree.pred.train <- as.data.frame(tree.pred.train)
tree.pred.train$pred_conv <- ifelse(tree.pred.train[,1] < .5, 0, 1)
result_table <- as.data.frame(table(tree.pred.train$pred_conv, train$converted_to_VISIT))
acc_rate <- (result_table[1,3]+result_table[4,3])/sum(result_table$Freq)
tp_acc_rate <- (result_table[4,3])/(sum(result_table[3,3])+sum(result_table[3,4]))
acc_rate
tp_acc_rate
  
tree.pred.test <- predict(tree.conv, test)
tree.pred.test <- as.data.frame(tree.pred.test)
tree.pred.test$pred_conv <- ifelse(tree.pred.test[,1] < .5, 0, 1)
result_table <- as.data.frame(table(tree.pred.test$pred_conv, test$converted_to_VISIT))
acc_rate <- (result_table[1,3]+result_table[4,3])/sum(result_table$Freq)
tp_acc_rate <- (result_table[4,3])/(sum(result_table[3,3])+sum(result_table[3,4]))
acc_rate
tp_acc_rate

cv.conv <- cv.tree(tree.conv, FUN = prune.misclass)
  

