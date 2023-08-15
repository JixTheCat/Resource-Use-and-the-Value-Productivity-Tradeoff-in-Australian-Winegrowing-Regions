# Library
library(fmsb)

set.seed(100)
df <- read.csv("df.csv")

mean.t <- function(x) {
    mean(x, na.rm = TRUE)
}

selected_columns <- c("water_used", "scope1", "area_harvested")

dfrescale <- df[, c("water_used", "scope1", "area_harvested")]
dfrescale$rain <- df$rain
means <- aggregate(dfrescale, list(df$rain), FUN = mean.t)

maxes <- c()
mins <- c()
for (i in selected_columns) {
    current_min <- min(means[, i], na.rm=TRUE)
  mins <- append(mins, current_min)
  maxes <- append(maxes, max(means[, i], na.rm=TRUE) + current_min**2)
}
df_radar <- data.frame(rbind(maxes, mins))
colnames(df_radar) <- selected_columns

radarchart(rbind(df_radar, means[, selected_columns]))
