#######################
# Lets make some maps #
nt <- read.csv("no_trans.csv")
v <- st_read("data/Wine_Geographical_Indications_Australia_2022/Wine_Geographical_Indications_Australia_2022.shp")
aus <- st_read("data/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")

v$ha_tonnes_grapes_harvested <- 0
v$ha_value <- 0

aggs.ha_tonnes_grapes_harvested <- aggregate(
    nt$ha_tonnes_grapes_harvested * 10000
    , list(nt$giregion), FUN = mean.t)
aggs.ha_value <- aggregate(
    nt$ha_value
    , list(nt$giregion)
    , FUN=mean.t)

for (region in unique(df$giregion)){
    v[v$GI_NAME == region
    , "ha_tonnes_grapes_harvested"] <- aggs.ha_tonnes_grapes_harvested[
    aggs.ha_tonnes_grapes_harvested$Group.1 == region
    , ]$x
    v[
        v$GI_NAME==region
        , "ha_value"] <- aggs.ha_value[
            aggs.ha_value$Group.1 == region
            , ]$x
}

# note these are the averages.
#ggdraw() + 
value_map <- ggplot() +
    ggtitle("Average Price Per Ha ") +
    geom_sf(data = v, aes(fill = ha_value)) +
    geom_sf(data=aus, fill = NA) +
    xlim(113, 154) +
    ylim(-45, -25) +
    scale_fill_gradientn(colours=rev(viridis(3))
                         , name=""
                         , na.value = "grey100") +
    theme_void()

yield_map <-  ggplot() +
    xlim(113, 154) +
    ylim(-45, -25) +
    theme_void() +
        ggtitle("Average Yield (tonnes) Per Ha") +
    geom_sf(data = v
        , aes(fill = ha_tonnes_grapes_harvested)) +
    geom_sf(data=aus, fill = NA) +
    scale_fill_gradientn(colours=rev(viridis(3))
                         , name=""
                         , na.value = "grey100")

pdf("my_map.pdf")
grid.arrange(value_map, yield_map, ncol=2, nrow=1)
dev.off()

# below is just a way to grab the coef from a linear model.
#summary(model2)$coefficients
