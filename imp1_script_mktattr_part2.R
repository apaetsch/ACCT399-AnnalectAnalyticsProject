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
####################### VERSION 1: WITHOUT NULL/(null) ####################################

# READ IN DATA
converted_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_conv.csv", col.names = "conv")
not_converted_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_notconv.csv", col.names = "conv_null")
channel_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_channels.csv", col.names = "path")


# creating a data sample
x_df1 <- data.frame(path = channel_data, conv = converted_data, conv_null = not_converted_data)                    


# calculating the models
x_mod1 <- markov_model(x_df1,
                       var_path = 'path',
                       var_conv = 'conv',
                       var_null = 'conv_null',
                       out_more = TRUE)

# extracting the results of attribution
x_df_res1 <- x_mod1$result

# extracting a transition matrix
x_df_trans1 <- x_mod1$transition_matrix
x_df_trans1 <- dcast(x_df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

### plotting the Markov graph ###
x_df_trans <- x_mod1$transition_matrix

# adding dummies in order to plot the graph
x_df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                         channel_to = c('(start)', '(conversion)', '(null)'),
                         transition_probability = c(0, 1, 0))
x_df_trans <- rbind(x_df_trans, x_df_dummy)

# ordering channels
x_df_trans$channel_from <- factor(x_df_trans$channel_from,
                                  levels = c('(start)', '(conversion)', '(null)', 'Display', 'FILLER', 'Video', 'HighImpact'))
x_df_trans$channel_to <- factor(x_df_trans$channel_to,
                                levels = c('(start)', '(conversion)', '(null)', 'Display', 'FILLER', 'Video', 'HighImpact'))
x_df_trans <- dcast(x_df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
x_trans_matrix <- matrix(data = as.matrix(x_df_trans[, -1]),
                         nrow = nrow(x_df_trans[, -1]), ncol = ncol(x_df_trans[, -1]),
                         dimnames = list(c(as.character(x_df_trans[, 1])), c(colnames(x_df_trans[, -1]))))
x_trans_matrix[is.na(x_trans_matrix)] <- 0
x_trans_matrix1 <- new("markovchain", transitionMatrix = x_trans_matrix)

# plotting the graph
plot(x_trans_matrix1, edge.arrow.size = 0.25)


###########################################################################################
###########################################################################################


# ---------------------------------------------------------------------------------------


###########################################################################################
####################### VERSION DUMMY: WITH NULL/(null) ###################################

# READ IN DATA
converted_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_conv.csv", col.names = "conv")
not_converted_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_notconv.csv", col.names = "conv_null")
channel_data <- read.csv("/Users/apaetsch/Desktop/imp_eng_conv_v1_10k_users_MKTGATTR_channels.csv", col.names = "path")
      

# creating a data sample
x_df1 <- data.frame(path = channel_data, conv = converted_data, conv_null = not_converted_data)                    


# calculating the models
x_mod1 <- markov_model(x_df1,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

# extracting the results of attribution
x_df_res1 <- x_mod1$result

# extracting a transition matrix
x_df_trans1 <- x_mod1$transition_matrix
x_df_trans1 <- dcast(x_df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

### plotting the Markov graph ###
x_df_trans <- x_mod1$transition_matrix

# adding dummies in order to plot the graph
x_df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
x_df_trans <- rbind(x_df_trans, x_df_dummy)

# ordering channels
x_df_trans$channel_from <- factor(x_df_trans$channel_from,
                                levels = c('(start)', '(conversion)', '(null)', 'NULL', 'Display', 'FILLER', 'Video', 'HighImpact'))
x_df_trans$channel_to <- factor(x_df_trans$channel_to,
                              levels = c('(start)', '(conversion)', '(null)', 'NULL', 'Display', 'FILLER', 'Video', 'HighImpact'))
x_df_trans <- dcast(x_df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
x_trans_matrix <- matrix(data = as.matrix(x_df_trans[, -1]),
                       nrow = nrow(x_df_trans[, -1]), ncol = ncol(x_df_trans[, -1]),
                       dimnames = list(c(as.character(x_df_trans[, 1])), c(colnames(x_df_trans[, -1]))))
x_trans_matrix[is.na(x_trans_matrix)] <- 0
x_trans_matrix1 <- new("markovchain", transitionMatrix = x_trans_matrix)

# plotting the graph
plot(x_trans_matrix1, edge.arrow.size = 0.25)


###########################################################################################
###########################################################################################


