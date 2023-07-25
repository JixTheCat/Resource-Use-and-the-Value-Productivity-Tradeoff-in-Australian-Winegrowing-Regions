library(ggplot2)

# data_prep.py has to be executed in order for df.csv to exist
set.seed(100)
df <- read.csv("df.csv")

model1 <- lm(
    tonnes_grapes_harvested ~
    data_year_id + giregion + area_harvested + water_used + scope1
    , data = df)
model2 <- lm(
    ha_tonnes_grapes_harvested ~
    area_harvested * ((scope1) + water_used) + data_year_id:giregion
    , data = df)
model3 <- lm(
    value ~
    data_year_id + giregion + area_harvested + water_used + scope1
    , data = df)
model4 <- lm(
    ha_value ~
    area_harvested * ((scope1) + water_used) + data_year_id:giregion
    , data = df)

# We review our first model

# MODEL 1
anova(model1)
summary(model1)
coefs1 <- coefs(model1)
coefs1_giregion <- coefs1[grep("gi*", row.names(ok)), ]
coefs1_year <- coefs1[grep("data*", row.names(ok)), ]
model1_residuals <- residuals(model1)
model1_fitted <- fitted(model1)
ggplot(
    data.frame(
        y = model1_residuals)
        , aes(sample = y)
       ) +
       stat_qq() +
       stat_qq_line()

ggplot(
    data.frame(
        y = model1_residuals
        , x = model1_fitted)
    , aes(x = x, y = y)
    ) +
    geom_point() +
    geom_hline(yintercept = 0)
#Linear Regression
#
#6068 samples
#   5 predictor
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 100 times) 
#Summary of sample sizes: 4768, 4767, 4768, 4769, 4768, 4769, ... 
#Resampling results:
#
#Tuning parameter 'intercept' was held constant at a value of TRUE

# MODEL 2
# Lets look at ratios
anova(model2)
summary(model2)
coefs2 <- coefs(model2)
coefs2_giregion <- coefs2[grep("gi*", row.names(ok)), ]
coefs2_year <- coefs2[grep("data*", row.names(ok)), ]
model2_residuals <- residuals(model2)
model2_fitted <- fitted(model2)
ggplot(
    data.frame(
        y = model2_residuals)
        , aes(sample = y)
       ) +
       stat_qq() +
       stat_qq_line()
ggplot(
    data.frame(
        y = model2_residuals
        , x = model2_fitted)
    , aes(x = x, y = y)
    ) +
    geom_point() +
    geom_hline(yintercept = 0)
#Linear Regression 
#
#6068 samples
#   5 predictor
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 100 times) 
#Summary of sample sizes: 4768, 4768, 4768, 4769, 4769, 4768, ... 
#Resampling results:
#
#  RMSE       Rsquared   MAE      
#  0.5104218  0.7408576  0.3492901
#
#Tuning parameter 'intercept' was held constant at a value of TRUE

# MODEL 3
# We add Value to the model :P
anova(model3)
summary(model3)
coefs3 <- coefs(model3)

coefs3_giregion <- coefs3[grep("gi*", row.names(ok)), ]
coefs3_year <- coefs3[grep("data*", row.names(ok)), ]
model3_residuals <- residuals(model3)
model3_fitted <- fitted(model3)
ggplot(
    data.frame(
        y = model3_residuals)
        , aes(sample = y)
       ) +
       stat_qq() +
       stat_qq_line()
ggplot(
    data.frame(
        y = model3_residuals
        , x = model3_fitted)
        , aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = 0)

ggplot(df4, aes(class, Estimate)) + ggplot(df2, aes(class, Estimate)) + geom_violin() + geom_boxplot(width=0.1, fill="white") + theme_classic()

df3[names3[grepl("^da", names3)], "class"] <- "Year"
df3[names3[grepl("^gi", names3)], "class"] <- "GI Region"
df3 <- df3[complete.cases(df3),]
ggplot(df3, aes(class, Estimate)) + geom_violin() + scale_fill_brewer(palette="Dark2") + theme_minimal()

# Residual standard error: 0.1688 on 1962 degrees of freedom
#   (2877 observations deleted due to missingness)
# Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9715 
# F-statistic:  1274 on 54 and 1962 DF,  p-value: < 2.2e-16
#Linear Regression 
#
#6068 samples
#   5 predictor
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 100 times) 
#Summary of sample sizes: 2590, 2590, 2591, 2590, 2590, 2591, ... 
#Resampling results:
#
#Tuning parameter 'intercept' was held constant at a value of TRUE

# MODEL 4
anova(model4)
summary(model4)
coefs4 <- coefs(model4)
coefs4_giregion <- coefs4[grep("gi*", row.names(ok)), ]
coefs4_year <- coefs4[grep("data*", row.names(ok)), ]
model4_residuals <- residuals(model4)
model4_fitted <- fitted(model4)
ggplot(
    data.frame(
        y = model4_residuals)
        , aes(sample = y)
       ) +
       stat_qq() +
       stat_qq_line()
ggplot(
    data.frame(
        y = model4_residuals
        , x = model4_fitted)
    , aes(x = x, y = y)
    ) +
    geom_point() +
    geom_hline(yintercept = 0)

# Residual standard error: 0.1949 on 1791 degrees of freedom
#   (2877 observations deleted due to missingness)
# Multiple R-squared:  0.9662,	Adjusted R-squared:  0.962 
# F-statistic: 227.9 on 225 and 1791 DF,  p-value: < 2.2e-16
#Linear Regression 
#
#6068 samples
#   5 predictor
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 100 times) 
#Summary of sample sizes: 2590, 2590, 2590, 2590, 2590, 2591, ... 
#Resampling results:
#
#  RMSE     Rsquared   MAE      
#  0.22349  0.9500051  0.1279485
#

############################ #
# We graph the coefficients! #
##############################

df1 <- data.frame(summary(model1)$coefficients)
names1 <- rownames(df1) 
df1[names1[grepl("^da", names1)], "class"] <- "Model 1: Year"
df1[names1[grepl("^gi", names1)], "class"] <- "model 1: GI Region"
df1 <- df1[complete.cases(df1),]

df2 <- data.frame(summary(model2)$coefficients)
names2 <- rownames(df2)
df2[names2[grepl("^da", names2)], "class"] <- "Model 2: Year * GI Region"
df2 <- df2[complete.cases(df2),]

df3 <- data.frame(summary(model3)$coefficients)
names3 <- rownames(df3) 
df3[names3[grepl("^da", names3)], "class"] <- "Model 3: Year"
df3[names3[grepl("^gi", names3)], "class"] <- "model 3: GI Region"
df3 <- df3[complete.cases(df3),]

df4 <- data.frame(summary(model4)$coefficients)
names4 <- rownames(df4)
df4[names4[grepl("^da", names4)], "class"] <- "Model 4: Year * GI Region"
df4 <- df4[complete.cases(df4),]

violinplot <- ggplot(rbind(df1, df2, df3, df4), aes(class, Estimate, fill=class)) + geom_violin() + geom_boxplot(width=0.1, fill="white") + scale_color_brewer(palette="Dark2") + theme_minimal()
pdf("violinplot.pdf")
print(violinplot)
dev.off()

############################
# We look at the t-values: #
############################

df1 <- data.frame(summary(model1)$coefficients)
names1 <- rownames(df1) 
df1[names1[grepl("^da", names1)], "class"] <- "Model 1: Year"
df1[names1[grepl("^gi", names1)], "class"] <- "model 1: GI Region"
df1 <- df1[complete.cases(df1),]

df2 <- data.frame(summary(model2)$coefficients)
names2 <- rownames(df2)
df2[names2[grepl("^da", names2)], "class"] <- "Model 2: Year * GI Region"
df2 <- df2[complete.cases(df2),]

df3 <- data.frame(summary(model3)$coefficients)
names3 <- rownames(df3) 
df3[names3[grepl("^da", names3)], "class"] <- "Model 3: Year"
df3[names3[grepl("^gi", names3)], "class"] <- "model 3: GI Region"
df3 <- df3[complete.cases(df3),]

df4 <- data.frame(summary(model4)$coefficients)
names4 <- rownames(df4)
df4[names4[grepl("^da", names4)], "class"] <- "Model 4: Year * GI Region"
df4 <- df4[complete.cases(df4),]

violinplot <- ggplot(rbind(df1, df2, df3, df4), aes(class, Estimate, fill=class)) + geom_violin() + geom_boxplot(width=0.1, fill="white") + scale_color_brewer(palette="Dark2") + theme_minimal()
pdf("violinplot.pdf")
print(violinplot)
dev.off()

####################################
# We look at productivity vs value #
####################################

#Tuning parameter 'intercept' was held constant at a value of TRUE

# note that vtha~tha is not a great predictor of one another.
# but it is a significant variable!
summary(lm(df$ha_value ~ df$ha_tonnes_grapes_harvested))
anova(lm(df$ha_value ~ df$ha_tonnes_grapes_harvested))

# note that scope1 is only good for predicting the amount of grapes
# not the quality.

# here are some plots
yield.vs.value <- ggplot(df
    , aes(
        x = tonnes_grapes_harvested
        , y = tonnes_grapes_harvested * average_per_tonne
        , col = climate)
    ) +
    xlim(-2.5, 2.5) +
    ylim(-2.5, 2.5) +
    geom_point() +
    xlab("Yield") +
    ylab("Yield * Average Price") +
    theme_light()
pdf("yvv.pdf")
print(yield.vs.value)
dev.off()

yield.vs.value.per.ha <- ggplot(
    df
    , aes(
        x = ha_tonnes_grapes_harvested
        , y = ha_tonnes_grapes_harvested * average_per_tonne
        , col = climate)
        ) +
        xlim(-2.5, 2.5) +
        ylim(-2.5, 2.5) +
        geom_point() +
        xlab("Yield / Area Harvested") +
        ylab("Yield * Average Price / Area Harvested") +
        theme_light()
pdf("yvvph.pdf")
print(yield.vs.value.per.ha)
dev.off()

#######################
# Lets make some maps #
library(terra)
nt <- read.csv("no_trans.csv")
v <- vect("data/Wine_Geographical_Indications_Australia_2022/Wine_Geographical_Indications_Australia_2022.shp")
aus <- vect("data/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")

v$ha_tonnes_grapes_harvested <- 0
v$ha_value <- 0

mean.t <- function(x) {mean(x, na.rm=TRUE)}
aggs.ha_tonnes_grapes_harvested <- aggregate(nt$ha_tonnes_grapes_harvested*1000, list(nt$giregion), FUN=mean.t)
aggs.ha_value <- aggregate(nt$ha_value, list(nt$giregion), FUN=mean.t)

for (region in unique(df$giregion))
{
v[v$GI_NAME==region , ]$ha_tonnes_grapes_harvested <-
 aggs.ha_tonnes_grapes_harvested[aggs.ha_tonnes_grapes_harvested$Group.1==region , ]$x
v[v$GI_NAME==region , ]$ha_value <- aggs.ha_value[aggs.ha_value$Group.1==region , ]$x
}

# note these are the averages.
plot(v, "ha_value")
lines(aus)
sbar(1000, lonlat=TRUE, label="1000km")
north(xy="bottomleft", type=2)

# below is just a way to grab the coefs from a linear model.
#summary(model2)$coefficients
