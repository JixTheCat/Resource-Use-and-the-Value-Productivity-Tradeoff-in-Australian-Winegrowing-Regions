# Library
library(fmsb)

# This is to pass to the aggregation function with the
# preset argument to remove NaNs.
mean.t <- function(x) {
    mean(x, na.rm = TRUE)
}

starplot <- function(selected_columns, df, target_col, colors_in) {
    dfrescale <- df[, selected_columns]
    dfrescale[, target_col] <- df[, target_col]
    means <- aggregate(dfrescale[, selected_columns]
        , list(df[, target_col]), FUN = mean.t)

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

    # To help with the legend we name the rows
    rownames(means) <- means$Group.1
    # The -1 is to removes the unknown regions.
    df_radar <- rbind(df_radar, means[-1, selected_columns])

    # And plot!
    radarchart(df_radar
        , pcol = colors_in
        , cglwd = 1.2
        , plwd = 2
        , plty = 1)
    legend(x = 0.7
        , y = 1
        , legend = rownames(df_radar[-(1:2), ])
        , bty = "n"
        , pch = 20
        , col = colors_in
        , text.col = "grey"
        , cex = 1.2
        , pt.cex = 3)
}

# read in the data
set.seed(100)
df <- read.csv("df.csv")

#  We grab the columns we want to display:
selected_columns <- c("water_used"
    , "scope1"
    , "area_harvested"
    , "average_per_tonne"
    , "tonnes_grapes_harvested"
)

# We rename to make the output prettier
new_columns <- c("Water Used"
    , "Scope One"
    , "Area Harvested"
    , "Average Price Per Tonne"
    , "Yield"
)

for (x in seq_len(length(selected_columns))) {
    names(df)[names(df) == selected_columns[x]] <- new_columns[x]
}

# We set the colours we will use:bty
temp_colors <- c(
    # Cool
    "#1E88E5"
    # Hot
    , "#D81B60"
    # Mild
    , "#004d40"
    # Warm
    , "#FFC107"
)

rain_colors <- c(
    # Damp
    "#1E88E5"
    # Dry
    , "#FFC107"
    # Very Dry
    , "#D81B60"
)

# Two example starplots
pdf("starplots.pdf")
starplot(new_columns, df, "rain", rain_colors)
starplot(new_columns, df, "temp", temp_colors)
dev.off()