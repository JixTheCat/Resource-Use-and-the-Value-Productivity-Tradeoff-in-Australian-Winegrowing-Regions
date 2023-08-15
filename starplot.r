# Library
library(fmsb)

# read in the data
set.seed(100)
df <- read.csv("df.csv")

# This is to pass to the aggregation function with the
# preset argument to remove NaNs.
mean.t <- function(x) {
    mean(x, na.rm = TRUE)
}

#  We grab the columns we want to display:
selected_columns <- c("water_used"
    , "scope1"
    , "area_harvested"
    , "average_per_tonne"
    , "tonnes_grapes_harvested"
)

dfrescale <- df[, selected_columns]
dfrescale$temp <- df$temp
means <- aggregate(dfrescale, list(df$temp), FUN = mean.t)

# The first two rows in the dataframe determine the scaling of the graph.
# They are set to be the maximum and minimum values
# So we go and grab them by iterating through the columns:
maxes <- c()
mins <- c()
for (i in selected_columns) {
    current_min <- min(means[, i], na.rm=TRUE)
    mins <- append(mins, current_min)
    # We add the squrae of the minimum to the maximum
    # this is to give it a bit of white space.
    maxes <- append(maxes, max(means[, i], na.rm=TRUE) + current_min**2)
}

# We stick the two together!
df_radar <- data.frame(rbind(maxes, mins))
colnames(df_radar) <- selected_columns

# And plot!
radarchart(rbind(df_radar, means[-1, selected_columns])
    , cglwd = 1.2
    , plwd = 2
    , plty = 1)
