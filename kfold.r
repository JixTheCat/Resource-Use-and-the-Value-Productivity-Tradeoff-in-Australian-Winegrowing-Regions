library(caret)

# data_prep.py has to be executed in order for df.csv to exist
set.seed(100)
df <- read.csv("df.csv")

#specify the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=100)
#fit a regression model and use k-fold CV to evaluate performance
model <- train(tonnes_grapes_harvested ~
    data_year_id + giregion + area_harvested + water_used + scope1
    , data = df
    , method = "lm"
    , trControl = ctrl
    , na.action = na.omit)


sink("model1.txt")
print(model)
sink()


#specify the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=100)
#fit a regression model and use k-fold CV to evaluate performance
model <- train(ha_tonnes_grapes_harvested ~
    area_harvested * ((scope1) + water_used) + data_year_id:giregion + data_year_id + giregion
    , data = df
    , method = "lm"
    , trControl = ctrl
    , na.action = na.omit)

sink("model2.txt")
print(model)
sink()

#specify the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=100)
#fit a regression model and use k-fold CV to evaluate performance
model <- train(value ~
    data_year_id + giregion + area_harvested + water_used + scope1
    , data = df
    , method = "lm"
    , trControl = ctrl
    , na.action = na.omit)

sink("model3.txt")
print(model)
sink()

#specify the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=100)
#fit a regression model and use k-fold CV to evaluate performance
model <- train(average_per_tonne ~
    area_harvested * ((scope1) + water_used) + data_year_id:giregion + data_year_id + giregion
    , data = df
    , method = "lm"
    , trControl = ctrl
    , na.action = na.omit)


sink("model4.txt")
print(model)
sink()

#specify the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=100)
#fit a regression model and use k-fold CV to evaluate performance
model <- train(average_per_tonne ~
    scope1 + water_used + data_year_id:giregion + data_year_id + giregion
    , data = df
    , method = "lm"
    , trControl = ctrl
    , na.action = na.omit)

sink("model5.txt")
print(model)
sink()
