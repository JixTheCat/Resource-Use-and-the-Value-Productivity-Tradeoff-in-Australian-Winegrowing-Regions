#######################
# Lets make some maps #

library(ggplot2)
library(gridExtra)
library(viridis)
library(sf)
library(cowplot)
library(stringi)

mean.t <- function(x) {
    mean(x, na.rm = TRUE)
}

nt <- read.csv("no_trans.csv")
v <- st_read("data/Wine_Geographical_Indications_Australia_2022/Wine_Geographical_Indications_Australia_2022.shp")
aus <- st_read("data/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")

v$value <- 0

agg_value <- aggregate(
    nt$value
    , list(nt$giregion), FUN = mean.t)

agg_value$x <- agg_value$x / 1000000

for (region in unique(nt$giregion)){
    v[v$GI_NAME == region
    , "value"] <- agg_value[
    agg_value$Group.1 == region
    , ]$x
}

# note these are the averages.
value_map <- ggplot() +
    ggtitle("Average Vineyard Income by Region") +
    geom_sf(data = v, aes(fill = value)) +
    geom_sf(data=aus, fill = NA) +
    xlim(113, 154) +
    ylim(-45, -25) +
    scale_fill_gradientn(colours = rev(viridis(3))
                         , name = ""
                         , na.value = "grey100") +
    theme_void()

pdf("my_map.pdf")
print(value_map)
dev.off()

# below is just a way to grab the coef from a linear model.
#summary(model2)$coefficients
