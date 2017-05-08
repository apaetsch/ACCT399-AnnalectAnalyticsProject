# SETUP
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

# DATA EXPLORATION
conversion_counts <- count(tbu_final$converted_to_VISIT)
channel_counts <- data.frame(matrix(ncol = 2, nrow = 4))
channel_counts[,1] <- c("Display", "High Impact", "Search", "Video")
channel_counts[,2] <- c(sum(tbu_final$channel_display_abs), 
                    sum(tbu_final$channel_highimpact_abs),
                    sum(tbu_final$channel_search_abs),
                    sum(tbu_final$channel_video_abs))

platform_counts <- data.frame(matrix(ncol = 2, nrow = 3))
platform_counts[,1] <- c("Display", "High Impact", "Search")
platform_counts[,2] <- c(sum(tbu_final$platform_desktop_abs), 
                        sum(tbu_final$platform_mobile_abs),
                        sum(tbu_final$platform_social_abs))

tob_counts <- data.frame(matrix(ncol = 2, nrow = 6))
tob_counts[,1] <- c("Content Amplification", "Endemic", "Network", "Programmatic", "Publisher Direct", "Search")
tob_counts[,2] <- c(sum(tbu_final$typeofbuy_contentamplification_abs), 
                         sum(tbu_final$typeofbuy_endemic_abs),
                         sum(tbu_final$typeofbuy_network_abs),
                         sum(tbu_final$typeofbuy_programmatic_abs),
                         sum(tbu_final$typeofbuy_publisherdirect_abs),
                         sum(tbu_final$typeofbuy_search_abs))

ot_counts <- data.frame(matrix(ncol = 2, nrow = 8))
ot_counts[,1] <- c("Behavioral", "Contextual", "Geo", "Lookalike", "Predictive", 
                    "Prospecting", "Remarketing", "Retargeting")
ot_counts[,2] <- c(sum(tbu_final$overalltargeting_behavioral_abs), 
                    sum(tbu_final$overalltargeting_contextual_abs),
                    sum(tbu_final$overalltargeting_geo_abs),
                    sum(tbu_final$overalltargeting_lookalike_abs),
                    sum(tbu_final$overalltargeting_predictive_abs),
                    sum(tbu_final$overalltargeting_prospecting_abs),
                    sum(tbu_final$overalltargeting_remarketing_abs),
                    sum(tbu_final$overalltargeting_retargeting_abs))

funnel_counts <- data.frame(matrix(ncol = 2, nrow = 3))
funnel_counts[,1] <- c("Lower", "Middle", "Upper")
funnel_counts[,2] <- c(sum(tbu_final$funnel_lower_abs), 
                         sum(tbu_final$funnel_middle_abs),
                         sum(tbu_final$funnel_upper_abs))

channel_counts
platform_counts
tob_counts
ot_counts
funnel_counts
