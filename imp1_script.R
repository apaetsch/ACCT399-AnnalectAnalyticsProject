# SETUP
install.packages("plyr")
library("plyr") 

# ---------------------------------------------------------------------------------------

# table <- subset(table, select=-impression_no)

# ---------------------------------------------------------------------------------------

# READ IN DATA
table <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users.csv")
table$imp_record_date <- as.POSIXct(table$imp_record_date, format = "%Y-%m-%d %H:%M:%S") 
table$eng_record_date <- as.POSIXct(table$eng_record_date, format = "%Y-%m-%d %H:%M:%S") 
table$engcon_record_date <- as.POSIXct(table$engcon_record_date, format = "%Y-%m-%d %H:%M:%S") 
table$campaign_start_date <- as.POSIXct(table$campaign_start_date, format = "%Y-%m-%d %H:%M:%S") 
table$campaign_end_date <- as.POSIXct(table$campaign_end_date, format = "%Y-%m-%d %H:%M:%S") 
head(table)

# GET TOUCH_NO BY PERSON

# GET TOTAL IMPRESSIONS & APPEND
total_impressions <- count(table, c("ak_user_id"))
colnames(total_impressions)[2] <- "total_impressions"
table_temp <- merge(table, total_impressions, by.x='ak_user_id', by.y='ak_user_id')

# GET NO OF VISITS AND SALES
total_conversions <- count(table, c("ak_user_id", "conversion_class"))
colnames(total_conversions)[2] <- "total_conversions"
total_conversions["total_conversions_VISIT"] <- ifelse(total_conversions$total_conversions == 'VISIT', total_conversions$freq, 0)
total_conversions["total_conversions_SALES"] <- ifelse(total_conversions$total_conversions == 'SALES', total_conversions$freq, 0)
total_conversions["total_conversions_NULL"] <- ifelse(total_conversions$total_conversions == 'NULL', total_conversions$freq, 0)
total_conversions <- subset(total_conversions, select=-total_conversions)
total_conversions <- subset(total_conversions, select=-freq)
total_conversions <- aggregate(total_conversions, by=list(total_conversions$ak_user_id), FUN=sum)
total_conversions <- subset(total_conversions, select=-ak_user_id)
colnames(total_conversions)[1] <- "ak_user_id"
total_conversions["converted_to_VISIT"] <- ifelse(total_conversions$total_conversions_VISIT > 0, TRUE, FALSE)
total_conversions["converted_to_SALES"] <- ifelse(total_conversions$total_conversions_SALES > 0, TRUE, FALSE)
table_temp <- merge(table_temp, total_conversions, by.x='ak_user_id', by.y='ak_user_id')


#GET FIRST TP PER USER
first_touchpoint <- aggregate(imp_record_date ~ ak_user_id, table, min)
colnames(first_touchpoint)[2] <- "first_touchpoint"
table_temp <- merge(table_temp, first_touchpoint, by.x='ak_user_id', by.y='ak_user_id')

#GET LAST TP PER USER
last_touchpoint <- aggregate(imp_record_date ~ ak_user_id, table, max)
colnames(last_touchpoint)[2] <- "last_touchpoint"
table_temp <- merge(table_temp, last_touchpoint, by.x='ak_user_id', by.y='ak_user_id')

#GET TIMEFRAME PER USER
# Seconds
table_temp["timeframe_secs"] <- 0
table_temp$timeframe_secs <- difftime(table_temp$last_touchpoint, table_temp$first_touchpoint, units = "secs")
# Minutes
table_temp["timeframe_mins"] <- 0
table_temp$timeframe_mins <- difftime(table_temp$last_touchpoint, table_temp$first_touchpoint, units = "mins")
# Hours
table_temp["timeframe_hours"] <- 0
table_temp$timeframe_hours <- difftime(table_temp$last_touchpoint, table_temp$first_touchpoint, units = "hours")
# Days
table_temp["timeframe_days"] <- 0
table_temp$timeframe_days <- difftime(table_temp$last_touchpoint, table_temp$first_touchpoint, units = "days")


########################################################################################################################
###### GET CHANNELS PER USER
channel_per_user <- count(table, c("ak_user_id", "channel"))
colnames(channel_per_user)[2] <- "channel_type"
# Channel: Display
channel_per_user["channel_display_abs"] <- 0
channel_per_user$channel_display_abs <- ifelse(channel_per_user$channel_type == 'Display', channel_per_user$freq, 0)
# Channel: High Impact
channel_per_user["channel_highimpact_abs"] <- 0
channel_per_user$channel_highimpact_abs <- ifelse(channel_per_user$channel_type == 'High Impact', channel_per_user$freq, 0)
# Channel: Search
channel_per_user["channel_search_abs"] <- 0
channel_per_user$channel_search_abs <- ifelse(channel_per_user$channel_type == 'Search', channel_per_user$freq, 0)
# Channel: Video
channel_per_user["channel_video_abs"] <- 0
channel_per_user$channel_video_abs <- ifelse(channel_per_user$channel_type == 'Video', channel_per_user$freq, 0)
# Channel: NULL
channel_per_user["channel_NULL_abs"] <- 0
channel_per_user$channel_NULL_abs <- ifelse(channel_per_user$channel_type == 'NULL', channel_per_user$freq, 0)

# FORMAT CHANNEL PER USER TABLE
channel_per_user <-aggregate(subset(channel_per_user, select = -channel_type), by=list(channel_per_user$ak_user_id), FUN=sum)
channel_per_user <- subset(channel_per_user, select =-ak_user_id)
channel_per_user["channel_display_perc"] <- (channel_per_user$channel_display_abs/channel_per_user$freq)
channel_per_user["channel_highimpact_perc"] <- (channel_per_user$channel_highimpact_abs/channel_per_user$freq)
channel_per_user["channel_search_perc"] <- (channel_per_user$channel_search_abs/channel_per_user$freq)
channel_per_user["channel_video_perc"] <- (channel_per_user$channel_video_abs/channel_per_user$freq)
channel_per_user["channel_NULL_perc"] <- (channel_per_user$channel_NULL_abs/channel_per_user$freq)
channel_per_user <- subset(channel_per_user, select =-freq)
colnames(channel_per_user)[1] <- "ak_user_id"


########################################################################################################################
######  GET PLATFORM PER USER
platform_per_user <- count(table, c("ak_user_id", "platform"))
colnames(platform_per_user)[2] <- "platform_type"
# Platform: Desktop
platform_per_user["platform_desktop_abs"] <- 0
platform_per_user$platform_desktop_abs <- ifelse(platform_per_user$platform_type == 'Desktop', platform_per_user$freq, 0)
# Platform: Mobile
platform_per_user["platform_mobile_abs"] <- 0
platform_per_user$platform_mobile_abs <- ifelse(platform_per_user$platform_type == 'Mobile', platform_per_user$freq, 0)
# Platform: Social
platform_per_user["platform_social_abs"] <- 0
platform_per_user$platform_social_abs <- ifelse(platform_per_user$platform_type == 'Social', platform_per_user$freq, 0)
# Platform: NULL
platform_per_user["platform_NULL_abs"] <- 0
platform_per_user$platform_NULL_abs <- ifelse(platform_per_user$platform_type == 'NULL', platform_per_user$freq, 0)

# FORMAT PLATFORM PER USER TABLE
platform_per_user <-aggregate(subset(platform_per_user, select = -platform_type), by=list(platform_per_user$ak_user_id), FUN=sum)
platform_per_user <- subset(platform_per_user, select =-ak_user_id)
platform_per_user["platform_desktop_perc"] <- (platform_per_user$platform_desktop_abs/platform_per_user$freq)
platform_per_user["platform_mobile_perc"] <- (platform_per_user$platform_mobile_abs/platform_per_user$freq)
platform_per_user["platform_social_perc"] <- (platform_per_user$platform_social_abs/platform_per_user$freq)
platform_per_user["platform_NULL_perc"] <- (platform_per_user$platform_NULL_abs/platform_per_user$freq)
platform_per_user <- subset(platform_per_user, select =-freq)
colnames(platform_per_user)[1] <- "ak_user_id"


########################################################################################################################
###### GET TYPE OF BUY PER USER
typeofbuy_per_user <- count(table, c("ak_user_id", "type_of_buy"))
colnames(typeofbuy_per_user)[2] <- "typeofbuy_type"
# Type Of Buy: Content Amplification
typeofbuy_per_user["typeofbuy_contentamplification_abs"] <- 0
typeofbuy_per_user$typeofbuy_contentamplification_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Content Amplification', typeofbuy_per_user$freq, 0)
# Type Of Buy: Endemic
typeofbuy_per_user["typeofbuy_endemic_abs"] <- 0
typeofbuy_per_user$typeofbuy_endemic_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Endemic', typeofbuy_per_user$freq, 0)
# Type Of Buy: Network
typeofbuy_per_user["typeofbuy_network_abs"] <- 0
typeofbuy_per_user$typeofbuy_network_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Network', typeofbuy_per_user$freq, 0)
# Type Of Buy: Programmatic
typeofbuy_per_user["typeofbuy_programmatic_abs"] <- 0
typeofbuy_per_user$typeofbuy_programmatic_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Programmatic', typeofbuy_per_user$freq, 0)
# Type Of Buy: Publisher Direct
typeofbuy_per_user["typeofbuy_publisherdirect_abs"] <- 0
typeofbuy_per_user$typeofbuy_publisherdirect_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Publisher Direct', typeofbuy_per_user$freq, 0)
# Type Of Buy: Search
typeofbuy_per_user["typeofbuy_search_abs"] <- 0
typeofbuy_per_user$typeofbuy_search_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'Search', typeofbuy_per_user$freq, 0)
# Type Of Buy: NULL
typeofbuy_per_user["typeofbuy_NULL_abs"] <- 0
typeofbuy_per_user$typeofbuy_NULL_abs <- ifelse(typeofbuy_per_user$typeofbuy_type == 'NULL', typeofbuy_per_user$freq, 0)

# FORMAT TYPE OF BUY PER USER TABLE
typeofbuy_per_user <-aggregate(subset(typeofbuy_per_user, select = -typeofbuy_type), by=list(typeofbuy_per_user$ak_user_id), FUN=sum)
typeofbuy_per_user <- subset(typeofbuy_per_user, select =-ak_user_id)
typeofbuy_per_user["typeofbuy_contentamplification_perc"] <- (typeofbuy_per_user$typeofbuy_contentamplification_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_endemic_perc"] <- (typeofbuy_per_user$typeofbuy_endemic_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_network_perc"] <- (typeofbuy_per_user$typeofbuy_network_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_programmatic_perc"] <- (typeofbuy_per_user$typeofbuy_programmatic_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_publisherdirect_perc"] <- (typeofbuy_per_user$typeofbuy_publisherdirect_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_search_perc"] <- (typeofbuy_per_user$typeofbuy_search_abs/typeofbuy_per_user$freq)
typeofbuy_per_user["typeofbuy_NULL_perc"] <- (typeofbuy_per_user$typeofbuy_NULL_abs/typeofbuy_per_user$freq)
typeofbuy_per_user <- subset(typeofbuy_per_user, select =-freq)
colnames(typeofbuy_per_user)[1] <- "ak_user_id"


########################################################################################################################
###### GET SITE TYPE PER USER
sitetype_per_user <- count(table, c("ak_user_id", "site_type"))
colnames(sitetype_per_user)[2] <- "sitetype_type"
# Site Type: Broadcast
sitetype_per_user["sitetype_broadcast_abs"] <- 0
sitetype_per_user$sitetype_broadcast_abs <- ifelse(sitetype_per_user$sitetype_type == 'Broadcast', sitetype_per_user$freq, 0)
# Site Type: Content Amplification
sitetype_per_user["sitetype_contentamplification_abs"] <- 0
sitetype_per_user$sitetype_contentamplification_abs <- ifelse(sitetype_per_user$sitetype_type == 'Content Amplification', sitetype_per_user$freq, 0)
# Site Type: Discovery Engine
sitetype_per_user["sitetype_discoveryengine_abs"] <- 0
sitetype_per_user$sitetype_discoveryengine_abs <- ifelse(sitetype_per_user$sitetype_type == 'Discovery Engine', sitetype_per_user$freq, 0)
# Site Type: Endemic
sitetype_per_user["sitetype_endemic_abs"] <- 0
sitetype_per_user$sitetype_endemic_abs <- ifelse(sitetype_per_user$sitetype_type == 'Endemic', sitetype_per_user$freq, 0)
# Site Type: Internet Radio
sitetype_per_user["sitetype_internetradio_abs"] <- 0
sitetype_per_user$sitetype_internetradio_abs <- ifelse(sitetype_per_user$sitetype_type == 'Internet Radio', sitetype_per_user$freq, 0)
# Site Type: Lifestyle
sitetype_per_user["sitetype_lifestyle_abs"] <- 0
sitetype_per_user$sitetype_lifestyle_abs <- ifelse(sitetype_per_user$sitetype_type == 'Lifestyle', sitetype_per_user$freq, 0)
# Site Type: Major Media
sitetype_per_user["sitetype_majormedia_abs"] <- 0
sitetype_per_user$sitetype_majormedia_abs <- ifelse(sitetype_per_user$sitetype_type == 'Major Media', sitetype_per_user$freq, 0)
# Site Type: Network
sitetype_per_user["sitetype_network_abs"] <- 0
sitetype_per_user$sitetype_network_abs <- ifelse(sitetype_per_user$sitetype_type == 'Network', sitetype_per_user$freq, 0)
# Site Type: Portal
sitetype_per_user["sitetype_portal_abs"] <- 0
sitetype_per_user$sitetype_portal_abs <- ifelse(sitetype_per_user$sitetype_type == 'Portal', sitetype_per_user$freq, 0)
# Site Type: Programmatic
sitetype_per_user["sitetype_programmatic_abs"] <- 0
sitetype_per_user$sitetype_programmatic_abs <- ifelse(sitetype_per_user$sitetype_type == 'Programmatic', sitetype_per_user$freq, 0)
# Site Type: Sports League
sitetype_per_user["sitetype_sportsleague_abs"] <- 0
sitetype_per_user$sitetype_sportsleague_abs <- ifelse(sitetype_per_user$sitetype_type == 'Sports League', sitetype_per_user$freq, 0)
# Site Type: Video Portal
sitetype_per_user["sitetype_videoportal_abs"] <- 0
sitetype_per_user$sitetype_videoportal_abs <- ifelse(sitetype_per_user$sitetype_type == 'Video Portal', sitetype_per_user$freq, 0)
# Site Type: NULL
sitetype_per_user["sitetype_NULL_abs"] <- 0
sitetype_per_user$sitetype_NULL_abs <- ifelse(sitetype_per_user$sitetype_type == 'NULL', sitetype_per_user$freq, 0)

# FORMAT SITE TYPE PER USER TABLE
sitetype_per_user <-aggregate(subset(sitetype_per_user, select = -sitetype_type), by=list(sitetype_per_user$ak_user_id), FUN=sum)
sitetype_per_user <- subset(sitetype_per_user, select =-ak_user_id)
sitetype_per_user["sitetype_broadcast_perc"] <- (sitetype_per_user$sitetype_broadcast_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_contentamplification_perc"] <- (sitetype_per_user$sitetype_contentamplification_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_discoveryengine_perc"] <- (sitetype_per_user$sitetype_discoveryengine_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_endemic_perc"] <- (sitetype_per_user$sitetype_endemic_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_internetradio_perc"] <- (sitetype_per_user$sitetype_internetradio_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_lifestyle_perc"] <- (sitetype_per_user$sitetype_lifestyle_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_majormedia_perc"] <- (sitetype_per_user$sitetype_majormedia_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_network_perc"] <- (sitetype_per_user$sitetype_network_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_portal_perc"] <- (sitetype_per_user$sitetype_portal_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_programmatic_perc"] <- (sitetype_per_user$sitetype_programmatic_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_sportsleague_perc"] <- (sitetype_per_user$sitetype_sportsleague_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_videoportal_perc"] <- (sitetype_per_user$sitetype_videoportal_abs/sitetype_per_user$freq)
sitetype_per_user["sitetype_NULL_perc"] <- (sitetype_per_user$sitetype_NULL_abs/sitetype_per_user$freq)
sitetype_per_user <- subset(sitetype_per_user, select =-freq)
colnames(sitetype_per_user)[1] <- "ak_user_id"


########################################################################################################################
###### GET OVERALL TARGETING PER USER
overalltargeting_per_user <- count(table, c("ak_user_id", "overall_targeting"))
colnames(overalltargeting_per_user)[2] <- "overalltargeting_type"
# Overall Targeting: Behavioral
overalltargeting_per_user["overalltargeting_behavioral_abs"] <- 0
overalltargeting_per_user$overalltargeting_behavioral_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Behavioral ', overalltargeting_per_user$freq, 0)
# Overall Targeting: Contextual
overalltargeting_per_user["overalltargeting_contextual_abs"] <- 0
overalltargeting_per_user$overalltargeting_contextual_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Contextual', overalltargeting_per_user$freq, 0)
# Overall Targeting: Geo
overalltargeting_per_user["overalltargeting_geo_abs"] <- 0
overalltargeting_per_user$overalltargeting_geo_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Geo', overalltargeting_per_user$freq, 0)
# Overall Targeting: Lookalike
overalltargeting_per_user["overalltargeting_lookalike_abs"] <- 0
overalltargeting_per_user$overalltargeting_lookalike_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Lookalike', overalltargeting_per_user$freq, 0)
# Overall Targeting: Predictive
overalltargeting_per_user["overalltargeting_predictive_abs"] <- 0
overalltargeting_per_user$overalltargeting_predictive_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Predictive', overalltargeting_per_user$freq, 0)
# Overall Targeting: Prospecting
overalltargeting_per_user["overalltargeting_prospecting_abs"] <- 0
overalltargeting_per_user$overalltargeting_prospecting_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Prospecting', overalltargeting_per_user$freq, 0)
# Overall Targeting: Remarketing    
overalltargeting_per_user["overalltargeting_remarketing_abs"] <- 0
overalltargeting_per_user$overalltargeting_remarketing_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Remarketing', overalltargeting_per_user$freq, 0)
# Overall Targeting: Retargeting
overalltargeting_per_user["overalltargeting_retargeting_abs"] <- 0
overalltargeting_per_user$overalltargeting_retargeting_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'Retargeting', overalltargeting_per_user$freq, 0)
# Overall Targeting: NULL
overalltargeting_per_user["overalltargeting_NULL_abs"] <- 0
overalltargeting_per_user$overalltargeting_NULL_abs <- ifelse(overalltargeting_per_user$overalltargeting_type == 'NULL', overalltargeting_per_user$freq, 0)

# FORMAT OVERALL TARGETING PER USER TABLE
overalltargeting_per_user <-aggregate(subset(overalltargeting_per_user, select = -overalltargeting_type), by=list(overalltargeting_per_user$ak_user_id), FUN=sum)
overalltargeting_per_user <- subset(overalltargeting_per_user, select =-ak_user_id)
overalltargeting_per_user["overalltargeting_behavioral_perc"] <- (overalltargeting_per_user$overalltargeting_behavioral_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_contextual_perc"] <- (overalltargeting_per_user$overalltargeting_contextual_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_geo_perc"] <- (overalltargeting_per_user$overalltargeting_geo_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_lookalike_perc"] <- (overalltargeting_per_user$overalltargeting_lookalike_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_predictive_perc"] <- (overalltargeting_per_user$overalltargeting_predictive_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_prospecting_perc"] <- (overalltargeting_per_user$overalltargeting_prospecting_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_remarketing_perc"] <- (overalltargeting_per_user$overalltargeting_remarketing_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_retargeting_perc"] <- (overalltargeting_per_user$overalltargeting_retargeting_abs/overalltargeting_per_user$freq)
overalltargeting_per_user["overalltargeting_NULL_perc"] <- (overalltargeting_per_user$overalltargeting_NULL_abs/overalltargeting_per_user$freq)
overalltargeting_per_user <- subset(overalltargeting_per_user, select =-freq)
colnames(overalltargeting_per_user)[1] <- "ak_user_id"


########################################################################################################################
###### GET FUNNEL PER USER
funnel_per_user <- count(table, c("ak_user_id", "funnel"))
colnames(funnel_per_user)[2] <- "funnel_type"
# Funnel: Lower
funnel_per_user["funnel_lower_abs"] <- 0
funnel_per_user$funnel_lower_abs <- ifelse(funnel_per_user$funnel_type == 'Lower', funnel_per_user$freq, 0)
# Funnel: Middle
funnel_per_user["funnel_middle_abs"] <- 0
funnel_per_user$funnel_middle_abs <- ifelse(funnel_per_user$funnel_type == 'Middle', funnel_per_user$freq, 0)
# Funnel: Upper
funnel_per_user["funnel_upper_abs"] <- 0
funnel_per_user$funnel_upper_abs <- ifelse(funnel_per_user$funnel_type == 'Upper', funnel_per_user$freq, 0)
# Funnel: NULL
funnel_per_user["funnel_NULL_abs"] <- 0
funnel_per_user$funnel_NULL_abs <- ifelse(funnel_per_user$funnel_type == 'NULL', funnel_per_user$freq, 0)

# FORMAT FUNNEL PER USER TABLE
funnel_per_user <-aggregate(subset(funnel_per_user, select = -funnel_type), by=list(funnel_per_user$ak_user_id), FUN=sum)
funnel_per_user <- subset(funnel_per_user, select =-ak_user_id)
funnel_per_user["funnel_lower_perc"] <- (funnel_per_user$funnel_lower_abs/funnel_per_user$freq)
funnel_per_user["funnel_middle_perc"] <- (funnel_per_user$funnel_middle_abs/funnel_per_user$freq)
funnel_per_user["funnel_upper_perc"] <- (funnel_per_user$funnel_upper_abs/funnel_per_user$freq)
funnel_per_user["funnel_NULL_perc"] <- (funnel_per_user$funnel_NULL_abs/funnel_per_user$freq)
funnel_per_user <- subset(funnel_per_user, select =-freq)
colnames(funnel_per_user)[1] <- "ak_user_id"


########################################################################################################################
## SUPERMERGER
table_fin <- merge(table_temp, channel_per_user, by.x='ak_user_id', by.y='ak_user_id')
table_fin <- merge(table_fin, platform_per_user, by.x='ak_user_id', by.y='ak_user_id')
table_fin <- merge(table_fin, typeofbuy_per_user, by.x='ak_user_id', by.y='ak_user_id')
table_fin <- merge(table_fin, sitetype_per_user, by.x='ak_user_id', by.y='ak_user_id')
table_fin <- merge(table_fin, overalltargeting_per_user, by.x='ak_user_id', by.y='ak_user_id')
table_fin <- merge(table_fin, funnel_per_user, by.x='ak_user_id', by.y='ak_user_id')

########################################################################################################################
## REDUCE TO RELEVANT COLUMNS & RELEVANT ROWS
table_fin <- table_fin[-c(2:26)]
table_fin <- unique(table_fin[ 1:97 ])


# OUTPUT FINAL DATA TABLE
write.table(table_fin, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_ROLLUP_v1.xls", sep="\t")

###### BEFORE USING FILE DON'T FORGET TO MOVE FIRST ROW OVER BY ONE COLUMN!!!!!!!!!!!!!!!


write.csv(table_fin, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_ROLLUP_v1.csv")
