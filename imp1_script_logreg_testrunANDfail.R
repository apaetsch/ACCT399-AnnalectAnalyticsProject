


###########################################################################################
### TEST 1 #############################################################################

train <- table_by_user[1:5000,]
test <- table_by_user[5001:10000,]

model <- glm(converted_to_VISIT ~ total_impressions + timeframe_hours, family = binomial(link = "logit"), data=train)
summary(model)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=subset(test,select=c(total_impressions, timeframe_hours)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$converted_to_VISIT)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, newdata=subset(test,select=c(total_impressions, timeframe_hours)), type="response")
pr <- prediction(p, test$converted_to_VISIT)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###########################################################################################
### TEST 2 #############################################################################

train <- table_by_user[1:5000,]
test <- table_by_user[5001:10000,]

leaps = regsubsets(converted_to_VISIT ~ total_impressions 
                   + timeframe_hours 
                   #+ first_touchpoint 
                   #+ last_touchpoint 
                   #+ timeframe_secs 
                   + channel_display_perc, 
                   #+ channel_highimpact_perc
                   #+ channel_search_perc,
                   #+ channel_NULL_perc,
                   #                   + platform_desktop_perc
                   #                   + platform_mobile_perc
                   #                   + platform_social_perc
                   #                   + typeofbuy_contentamplification_perc
                   #                   + typeofbuy_network_perc
                   #                   + typeofbuy_programmatic_perc
                   #                   + typeofbuy_publisherdirect_perc
                   #                   + typeofbuy_search_perc, 
                   data=train, nbest = 10)

plot(leaps, scale ="adjr2")


###########################################################################################
###########################################################################################


