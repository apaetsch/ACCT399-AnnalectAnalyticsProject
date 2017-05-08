# SETUP
install.packages("plyr")
library("plyr") 

install.packages("dplyr")
library("dplyr") 
install.packages("reshape2")
library("reshape2") 
install.packages("ggplot2")
library(ggplot2)
install.packages("ChannelAttribution")
library(ChannelAttribution)
install.packages("markovchain")
library(markovchain)

# ---------------------------------------------------------------------------------------

###########################################################################################
################################## TEST EXAMPLE ###########################################

# creating a data sample
df1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'), conv = c(1, 0, 0), conv_null = c(0, 1, 1))

# calculating the models
mod1 <- markov_model(df1,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

# extracting the results of attribution
df_res1 <- mod1$result

# extracting a transition matrix
df_trans1 <- mod1$transition_matrix
df_trans1 <- dcast(df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

### plotting the Markov graph ###
df_trans <- mod1$transition_matrix

# adding dummies in order to plot the graph
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
df_trans <- rbind(df_trans, df_dummy)

# ordering channels
df_trans$channel_from <- factor(df_trans$channel_from,
                                levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans$channel_to <- factor(df_trans$channel_to,
                              levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans <- dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))
trans_matrix[is.na(trans_matrix)] <- 0
trans_matrix1 <- new("markovchain", transitionMatrix = trans_matrix)

# plotting the graph
plot(trans_matrix1, edge.arrow.size = 0.35)


###########################################################################################
###########################################################################################


# READ IN DATA
table_by_activity <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users.csv")

total_conversions <- count(table_by_activity, c("ak_user_id", "conversion_class"))
colnames(total_conversions)[2] <- "total_conversions"
total_conversions["total_conversions_VISIT"] <- ifelse(total_conversions$total_conversions == 'VISIT', total_conversions$freq, 0)
total_conversions["total_conversions_SALE"] <- ifelse(total_conversions$total_conversions == 'SALES', total_conversions$freq, 0)
total_conversions["total_conversions_NULL"] <- ifelse(total_conversions$total_conversions == 'NULL', total_conversions$freq, 0)
total_conversions <- subset(total_conversions, select=-total_conversions)
total_conversions <- subset(total_conversions, select=-freq)
total_conversions <- aggregate(total_conversions, by=list(total_conversions$ak_user_id), FUN=sum)
total_conversions <- subset(total_conversions, select=-ak_user_id)
colnames(total_conversions)[1] <- "ak_user_id"
total_conversions["converted"] <- ifelse(total_conversions$total_conversions_VISIT > 0 , 1, 0)
total_conversions["not_converted"] <- ifelse(total_conversions$converted == 1 , 0, 1)
total_conversions <- subset(total_conversions, select=-total_conversions_VISIT)
total_conversions <- subset(total_conversions, select=-total_conversions_SALE)
total_conversions <- subset(total_conversions, select=-total_conversions_NULL)
table_intermed <- merge(table_by_activity, total_conversions, by.x='ak_user_id', by.y='ak_user_id')

table_intermed <- table_intermed[c(1, 27, 28, 2, 22)]
table_intermed <- table_intermed[order( table_intermed[,1], table_intermed[,2] ),]
table_intermed <- table_intermed %>% group_by(ak_user_id) %>% mutate(counter = row_number(imp_record_date))
table_intermed <- subset(table_intermed, select=-imp_record_date)

### V1
table_intermed_2 <- unique(table_intermed[c(1,2)])
table_intermed_2 <- subset(table_intermed_2, select=-ak_user_id)
table_intermed_3 <- unique(table_intermed[c(1,3)])
table_intermed_3 <- subset(table_intermed_3, select=-ak_user_id)
table_intermed_4 <- dcast(table_intermed, ak_user_id ~ counter, value.var="channel")
table_intermed_4 <- subset(table_intermed_4, select=-ak_user_id)

# NOT A MUST!!!: REPLACE HIGH IMPACT VARIABLE AND NULL
table_intermed_4[table_intermed_4=="High Impact"] <- "HighImpact"
table_intermed_4[table_intermed_4=="NULL"] <- "(null)"

# WRITE TABLES INTO CSV
write.table(table_intermed_2, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_conv.csv", sep = ", ", row.names = FALSE)
write.table(table_intermed_3, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_notconv.csv", sep = ", ", row.names = FALSE)
write.table(table_intermed_4, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_channels.csv", sep=" > ", na = "FILLER", row.names = FALSE)


write.table(table_intermed_4, file = "/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_channels.csv", sep=" > ", na = "", row.names = FALSE)

