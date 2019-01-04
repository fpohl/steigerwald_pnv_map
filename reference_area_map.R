#This code will generate a map of the Steigerwald, 
#which shows the potential natural vegetation 
#and the location of the Steigerwald in Germany.

#TODO:
#fix "-0.1" in line 135
#add a better code for scalebar and north arrow
#add the predone changes of the pnv-file from QGIS to R

library(rgdal)
library(ggplot2)
library(grid)
library(ggmap)
library(dplyr)
library(sf)
library(gridExtra)
library(tidyverse)
library(plyr)
library(OpenStreetMap)
library(RColorBrewer)
library(colorspace)
library(prettymapr)

type <- c("osm", 
          "osm-bw",
          "maptoolkit-topo", 
          "waze", "bing", 
          "stamen-toner", 
          "stamen-terrain",
          "stamen-watercolor",
          "osm-german", 
          "osm-wanderreitkarte", 
          "mapbox", 
          "esri",
          "esri-topo", 
          "nps", 
          "apple-iphoto", 
          "skobbler",
          "hillshade",
          "opencyclemap",
          "osm-transport", 
          "osm-public-transport", 
          "osm-bbike",
          "osm-bbike-german")

#####################################################################
#####CREATE BASEMAP
lon <- c(9.92,11.032) #found coordinates by trial and error
lat <- c(49.4,50.1)
map <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=9,type=type[13]) #better don't use 16
autoplot(map)
map <- openproj(map, projection="+proj=longlat +datum=WGS84")
steigi_map <- autoplot(map) 

#get long & lat max/min
xlim <- ggplot_build(steigi_map)$layout$panel_scales_x[[1]]$range$range 
ylim <- ggplot_build(steigi_map)$layout$panel_scales_y[[1]]$range$range
df <- data.frame(xlim,ylim)

#####CREATE SECOND MAP
#source: https://gadm.org/data.html
#import vectorfile of Germany 
germany <- readOGR("data/gadm36_DEU_1.shp") 
proj4string(germany)
germany <- fortify(germany)
#create map of Germany 
ger_map <- ggplot(germany,aes(x=long,y=lat,group=group)) +
                  geom_polygon(fill="gray",col="white") +
                  coord_equal()+
                  geom_rect(data = df, inherit.aes = FALSE , fill=NA, colour="black",
                  aes(xmin = df$xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]))+
                  labs(x = NULL, y = NULL)
plot(ger_map)


#####IMPORT STEIGERWALD DATA AND ADD TO MAP
#import vectorfile of steigerwald
steigi <- readOGR("data/steigerwald.gpkg")
steigi <- spTransform(steigi,CRS=CRS("+proj=longlat +datum=WGS84"))
steigi <- fortify(steigi) #convert to df
#import pnv data
#source: https://www.lfu.bayern.de/natur/potentielle_natuerliche_vegetation/download_pnv/index.htm
# I already edited the data in QGIS -> reduced the spatial extent and the amount of categories
# In future this could be done in R as well
steigi_pnv <- readOGR("data/steigi_pnv.gpkg")
plot(steigi_pnv)
head(steigi_pnv)
pnv <- spTransform(steigi_pnv,CRS=CRS("+proj=longlat +datum=WGS84")) #making sure everything has the same CRS
pnv@data 
pnv@data$id<-rownames(pnv@data) #create an ID column in the vectorfile
pnv@data 
pnv.df <- fortify(pnv)
pnv.df <- join(pnv.df,pnv@data,by="id") #join the attribute table from the vectorfile to the new data frame
head(pnv.df)

rhg_cols <- c("#3498DB",  "#4d407a", "#D4AC0D", "#F39C12", "#E67E22", "#D35400", "#8e44ad",
              "#1abc9c", "#16a085", "#27ae60") #predefining some colours in order to be able to distinguish the different pnv-groups visually

#plot everything
steigi_map <- autoplot(map) +
  geom_polygon(data=pnv.df,
               aes(x=long,y=lat,group=group,fill=NEU),
               alpha=0.575) +
  scale_fill_manual("Potential Natural Vegetation",
                    labels=c("Open water surface",
                             "Fluttering elm/Common oak/Riparian forest",
                             "Alpine grass/Common oak/Hornbeam forest",
                             "Whitespot/Ash/Hornbeam forest",
                             "Fluttering elm/Hornbeam forest",
                             "Wood bedstraw/Ash/Hornbeam forest",
                             "Moor grass/Beech/Common oak forest",
                             "Wood rush/Beech forest",
                             "Wood millet/Beech forest",
                             "Sweetscented bedstraw/Beech forest"
                    ),
                    values=rhg_cols) +
  geom_polygon(data=steigi, 
               aes(x=long, y=lat, group=group), 
               color="black",alpha=0) 

print(steigi_map) #looks nice, but something is missing

#creating a blank theme
maptheme <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank(),
  plot.margin=grid::unit(c(0,0,0,0), "mm")
)

#reducing xlim/ylim in order to plot Germany-map in the left upper corner of the basemap 
grid.newpage() 
xmin <- xlim[1] - 0.1 #I don't understand why I need -0.1 
xmax <- xlim[1] + ((xlim[2] - xlim[1])*0.33)
ymax <- ylim[2]
ymin <- ylim[2] - ((ylim[2] - ylim[1])*0.33)

#finally it's time to inset!
ins <- steigi_map + 
  inset(
    ggplotGrob(ger_map + maptheme), 
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
  )
plot(ins,xaxs="i", yaxs="i",asp=1)
#ggsave(filename="reference_area.png")

#I was not able to plot a north arrow and scalebar with ggsn or prettymapr
#Until someone got this fixed we will use this unhandsome code:

scalebar = function(x,y,w,n,d, units="km"){
  # x,y = lower left coordinate of bar
  # w = width of bar
  # n = number of divisions on bar
  # d = distance along each division
  
  bar = data.frame( 
    xmin = seq(0.0, n*d, by=d) + x,
    xmax = seq(0.0, n*d, by=d) + x + d,
    ymin = y,
    ymax = y+w,
    z = rep(c(1,0),n)[1:(n+1)],
    fill.col = rep(c("black","white"),n)[1:(n+1)])
  
  labs = data.frame(
    xlab = c(seq(0.0, (n+1)*d, by=d) + x, x), 
    ylab = c(rep(y-w*1.5, n+2), y-3*w),
    text = c(as.character(seq(0.0, ((n+1)*d)*100, by=d*100)), units)
  )
  list(bar, labs)
}

sb = scalebar(10.58, 49.45, 0.015, 2, 0.1, "km" )

x <- steigi_map +
  geom_segment(arrow=arrow(length=unit(4,"mm"), type="closed", angle=40), 
                             aes(x=10.95,xend=10.95,y=50.04,yend=50.08), colour=hcl(250,50,80)) +
  geom_label(aes(x=10.95, y=50.04, label="N"),
             size=4, label.padding=unit(1,"mm"), label.r=unit(0.4,"lines"))  +
  geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
            show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
  geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 
plot(x)

ins <- x + 
  inset(
    ggplotGrob(ger_map + maptheme), 
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
  )
plot(ins,xaxs="i", yaxs="i",asp=1)
ggsave(filename="reference_area.png")

