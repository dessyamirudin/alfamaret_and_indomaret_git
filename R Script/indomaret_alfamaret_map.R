library(readxl)
library(ggmap)
library(ggplot2)
library(RColorBrewer)

map_key <- "XXXXXXXX"
register_google(key = map_key)

# read data
tangsel = read_excel("Alfamart and Indomart/South Tangerang MT.xlsx",sheet = "Data")

# select longitude and latitude
d <- tangsel %>% dplyr::select("cid","location/lat","location/lng")
d = data.frame(d)
colnames(d) = c("ID","lat","lon")

# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  
  meanLat <- mean(centers$lat)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$lon, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# create circle with radius 500 m
myCircles <- make_circles(d, 0.5)

# load South Tangerang Map
ts_map <- get_map("South Tangerang", zoom=12)

# create circle map 500 meters from the minimarket
p <- ggmap(ts_map)
p <- p + geom_point(data=d, aes(x=lon, y=lat),
                    color="red", alpha=0.5)
p <- p + geom_polygon(data = myCircles, aes(lon, lat, group = ID),fill="blue",alpha = 0.4)

p

ggsave(p,filename = "radius 500m.png")

# create heatmap from location
m <- ggmap(ts_map)
m <- m + stat_density2d(data=d, aes(x=lon, y=lat,fill=after_stat(level), alpha=after_stat(level)),
                  geom = "polygon")
m <- m + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
m <- m + geom_point(data=d, aes(x=lon, y=lat),
                    color="red", alpha=0.5)
m

ggsave(m,filename = "heatmap location minimarket.png")
