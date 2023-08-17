library(ggplot2)
library(gridExtra)
library(viridis)
library(sf)
library(cowplot)
library(stringi)

mean.t <- function(x) {
    mean(x, na.rm = TRUE)
}

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
    average_per_tonne ~
    area_harvested * ((scope1) + water_used) + data_year_id:giregion
    , data = df)
model5 <- lm(
    average_per_tonne ~
    scope1 + water_used + data_year_id:giregion
    , data = df)
# We review our first model

# MODEL 1
anova(model1)
summary(model1)
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
#Summary of sample sizes: 4768, 4767, 4768, 4769, 4768, 4769,
#Resampling results:
#
#Tuning parameter 'intercept' was held constant at a value of TRUE

# MODEL 2
# Lets look at ratios
anova(model2)
summary(model2)
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
#Summary of sample sizes: 4768, 4768, 4768, 4769, 4769, 4768,
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

# MODEL 1
anova(model5)
summary(model5)
model5_residuals <- residuals(model5)
model5_fitted <- fitted(model5)
ggplot(
    data.frame(
        y = model5_residuals)
        , aes(sample = y)
       ) +
       stat_qq() +
       stat_qq_line()

ggplot(
    data.frame(
        y = model5_residuals
        , x = model5_fitted)
    , aes(x = x, y = y)
    ) +
    geom_point() +
    geom_hline(yintercept = 0)

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

# #D81B60
# #1E88E5
# #FFC107
# #004D40

temp_colors <- c(
    # Cool
    #"#1E88E5"
    "#1E88E5"
    # Hot
    #, "#D81B60"
    , "#D81B60"
    # Mild
    #, "#004D40"
    , "#004d40"
    # Warm
    #, "#FFC107"
    , "#FFC107"
)
rain_colors <- c(
    # Damp
    #"#1E88E5"
    "#1E88E5"
    # Dry
    #, "#D81B60"
    , "#FFC107"# Very Dry
    #, "#FFC107"
    , "#D81B60"
)
#scale_color_manual(values = my_colors)

# here are some plots
temp_yield_vs_value <- ggplot(
    df[df$temp!="", ]
    , aes(
        x = tonnes_grapes_harvested
        , y = value
        , col = temp)
    ) +
    xlim(-2.5, 2.5) +
    ylim(-2.5, 2.5) +
    geom_point(size = .5) +
    xlab("Yield") +
    ylab("Yield * Average Price") +
    ggtitle("Temperature") +
    theme_light() +
    theme(panel.grid.minor = element_blank()
        , legend.title = element_blank()
        , legend.position = "top", legend.direction = "horizontal") +
    scale_color_manual(values = temp_colors)

rain_yield_vs_value <- ggplot(
    df[df$rain!="", ]
    , aes(
        x = tonnes_grapes_harvested
        , y = value
        , col = rain)
        ) +
        xlim(-2.5, 2.5) +
        ylim(-2.5, 2.5) +
        geom_point(size = .5) +
        xlab("Yield") +
        ylab("Yield * Average Price") +
        ggtitle("Rainfall") +
        theme_light() +
        theme(panel.grid.minor = element_blank()
            , legend.title = element_blank()
            , legend.position = "top", legend.direction = "horizontal") +
        scale_color_manual(values = rain_colors)

pdf("yield_verse_value.pdf")
grid.arrange(temp_yield_vs_value, rain_yield_vs_value, ncol=2)
dev.off()


# here are some plots
temp_yield_vs_value <- ggplot(
    df[df$temp!="", ]
    , aes(
        x = ha_tonnes_grapes_harvested
        , y = ha_value
        , col = temp)
    ) +
    xlim(-2.5, 2.5) +
    ylim(-2.5, 2.5) +
    geom_point(size = .5) +
    xlab("Yield / Area Harvested") +
    ylab("Yield * Average Price / Area Harvested") +
    ggtitle("Temperature") +
    theme_light() +
    theme(panel.grid.minor = element_blank()
        , legend.title = element_blank()
        , legend.position = "top", legend.direction = "horizontal") +
    scale_color_manual(values = temp_colors)

rain_yield_vs_value <- ggplot(
    df[df$rain!="", ]
    , aes(
        x = ha_tonnes_grapes_harvested
        , y = ha_value
        , col = rain)
        ) +
        xlim(-2.5, 2.5) +
        ylim(-2.5, 2.5) +
        geom_point(size = .5) +
        xlab("Yield / Area Harvested") +
        ylab("Yield * Average Price / Area Harvested") +
        ggtitle("Rainfall") +
        theme_light() +
        theme(panel.grid.minor = element_blank()
            , legend.title = element_blank()
            , legend.position = "top", legend.direction = "horizontal") +
        scale_color_manual(values = rain_colors)

pdf("yield_verse_value_by_area.pdf")
grid.arrange(temp_yield_vs_value, rain_yield_vs_value, ncol=2)
dev.off()

#####%%%%%%%%%%%%%%%%%%%%
# Yearly variation plots
########################

# We construct a data frame for the coefficients
# We start with the first model
temp_df <- data.frame(summary(model1)$coefficients)
temp_df <- temp_df[grep("year", rownames(temp_df)), ]
estimates <- data.frame(temp_df[, c("Estimate", "Std..Error")])
estimates$model <- rep("model1", length(rownames(estimates)))

# The second model has coefficients averaged as there are so many per year.
temp_df <- data.frame(summary(model2)$coefficients)
temp_df <- temp_df[grep("year", rownames(temp_df)), ]
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])

temp_df$year <- stri_extract_first_regex(rownames(temp_df), "[0-9]+")
temp_df <- aggregate(temp_df, list(temp_df$year), FUN=mean.t)
rownames(temp_df) <- temp_df$Group.1
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])
temp_df$model <- rep("model2", length(rownames(temp_df)))

estimates <- rbind(estimates, temp_df)

# The third model is integrated somewhat the same as the first
temp_df <- data.frame(summary(model3)$coefficients)
temp_df <- temp_df[grep("year", rownames(temp_df)), ]
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])
temp_df$model <- rep("model3", length(rownames(temp_df)))

estimates <- rbind(estimates, temp_df)

# The fourth model is similar to the second and requires averaging
temp_df <- data.frame(summary(model4)$coefficients)
temp_df <- temp_df[grep("year", rownames(temp_df)), ]
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])

temp_df$year <- stri_extract_first_regex(rownames(temp_df), "[0-9]+")
temp_df <- aggregate(temp_df, list(temp_df$year), FUN=mean.t)
rownames(temp_df) <- temp_df$Group.1
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])
temp_df$model <- rep("model4", length(rownames(temp_df)))
rownames(temp_df) <- paste0(rownames(temp_df), "a")

estimates <- rbind(estimates, temp_df)

# The fith model is similar to the second and requires averaging
temp_df <- data.frame(summary(model5)$coefficients)
temp_df <- temp_df[grep("year", rownames(temp_df)), ]
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])

temp_df$year <- stri_extract_first_regex(rownames(temp_df), "[0-9]+")
temp_df <- aggregate(temp_df, list(temp_df$year), FUN=mean.t)
rownames(temp_df) <- temp_df$Group.1
temp_df <- data.frame(temp_df[, c("Estimate", "Std..Error")])
temp_df$model <- rep("model4", length(rownames(temp_df)))
rownames(temp_df) <- paste0(rownames(temp_df), "b")

estimates <- rbind(estimates, temp_df)

estimates$year <- stri_extract_first_regex(rownames(estimates), "[0-9]+")
estimates$year <- as.integer(estimates$year)

# Default line plot
p<- ggplot(estimates, aes(x = year, y = Estimate, group = model, color=model)) + 
#  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=Estimate-Std..Error, ymax=Estimate+Std..Error), width=.2,
                 position=position_dodge(0.05)) +
    theme_classic()

pdf("yearly_plots.pdf")
print(p)
dev.off()
