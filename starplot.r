coord_radar <- function (theta = "x", start = 0, direction = 1)

 {
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x") 
            "y"
        else "x"
        ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
    }

RadarTheme<-theme(panel.background=element_blank(),
                      plot.title= element_text(size = 25,face=c("bold","italic")),
                      plot.margin = unit(c(2, 2, 2, 2), "cm"),
                      text=element_text(family="Open Sans"), aspect.ratio = 1,
                      legend.position="bottom",legend.title=element_blank(),legend.direction="vertical",
                      strip.text.x = element_text(size = rel(0.8)),
                      axis.text.x = element_text(size = 15,face ="bold"),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.line.x=element_line(size=0.5),
                      panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"))

set.seed(100)
df <- read.csv("df.csv")

dfrescale <- df[, c("water_used", "scope1", "area_harvested")]
dfrescale <- as.data.frame(lapply(dfrescale, ggplot2:::rescale01))
# mtcarsscaled <- as.data.frame(lapply(mtcars, ggplot2:::rescale01))
# mtcarsscaled$model <- rownames(mtcars)
dfrescale$rain <- df$rain

dfdry <- subset(dfrescale, rain=='Very_Dry')
# tcarsscaled1<-subset(mtcarsscaled,model=="Lotus Europa")
dfwet <- subset(dfrescale, rain=='Damp')
# mtcarsscaled2<-subset(mtcarsscaled,model=="Volvo 142E")

dfscaled <- rbind(dfwet, dfdry)
# mtcarsscaled<-rbind(mtcarsscaled1,mtcarsscaled2)

dfmelted <- reshape2::melt(dfrescale)
dfmelted$value<-dfmelted$value*100
# mtcarsmelted <- reshape2::melt(mtcarsscaled)
# mtcarsmelted$value<-mtcarsmelted$value*100
c<-ggplot(dfmelted, aes(x = variable, y = value)) +
    geom_polygon(aes(group = rain, color = rain,fill = rain), alpha=0.4, size = 1) +
    RadarTheme+
    xlab("") + ylab("") +scale_y_continuous(limits = c(-5, 100), breaks = seq(0, 100, 25))+
    coord_radar()+
    guides(fill = guide_legend(keywidth = rel(1.3), keyheight = rel(1.3)))
print(c)