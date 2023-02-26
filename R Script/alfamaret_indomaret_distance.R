library(readxl)
library(ggplot2)
library(tidyverse)
library(geosphere)
library(scales)
library(gtable)
library(ggrepel)
library(ggmap)
library(RColorBrewer)

# read data
tangsel = read_excel("South Tangerang MT.xlsx",sheet = "Data")

# proportion alfamaret and indomaret
data.frame(table(tangsel$searchString))

# plot barchart alfamaret and indomaret
# plot bubble chart alfamaret and indomaret

# measuring distance
alfamaret = tangsel %>% filter(searchString =="Alfamaret") %>% select("cid","location/lat","location/lng","searchString")
colnames(alfamaret) = c("ID_alfa","lat_alfa","lon_alfa","alfa_brand")

indomaret = tangsel %>% filter(searchString =="Indomaret") %>% select("cid","location/lat","location/lng","searchString")
colnames(indomaret) = c("ID_indo","lat_indo","lon_indo","indo_brand")


# ------------------ DISTANCE ALFA INDO -----------------------

# cross join
dist_alfa_indo = merge(x=alfamaret,y=indomaret,right_by="ID_alfa",left_by="ID_indo",all.x = TRUE, all.y = TRUE)
dist_alfa_indo$alfa_lon_lat = cbind(dist_alfa_indo$lon_alfa,dist_alfa_indo$lat_alfa)
dist_alfa_indo$indo_lon_lat = cbind(dist_alfa_indo$lon_indo,dist_alfa_indo$lat_indo)

# distance
dist_alfa_indo$distance = distHaversine(dist_alfa_indo$alfa_lon_lat, dist_alfa_indo$indo_lon_lat, r=6378137)

# sort by alfa id and distance for ease of observation and rank based on alfa_ID
dist_alfa_indo = dist_alfa_indo %>% arrange(ID_alfa,distance) %>% group_by(ID_alfa) %>% mutate(rank=row_number())

# filter minimum distance
min_dist_alfa_indo = dist_alfa_indo %>% filter(rank == 1)

# binning based on 50 meter distance
min_dist_alfa_indo = min_dist_alfa_indo %>% 
  mutate(dist50m =
           case_when (distance <= 50 ~ "<= 50m",
                      distance > 50 & distance <= 100 ~ "50m - 100m",
                      distance > 100 & distance <= 150 ~ "100m - 150m",
                      distance > 150 & distance <= 200 ~ "150m - 200m",
                      distance > 200 & distance <= 250 ~ "200m - 250m",
                      distance > 250 & distance <= 300 ~ "250m - 300m",
                      distance > 300 & distance <= 350 ~ "300m - 350m",
                      distance > 350 & distance <= 400 ~ "350m - 400m",
                      distance > 400 & distance <= 450 ~ "400m - 450m",
                      distance > 450 & distance <= 500 ~ "450m - 500m",
                      TRUE ~ ">500m"
         )
  ) %>% 
  mutate(dis10m =
           case_when(distance <= 10 ~ "<= 10m",
                     distance > 10 & distance <=20 ~ "10m - 20m",
                     distance >20 & distance <=30 ~ "20m - 30m",
                     distance >30 & distance <=40 ~ "30m - 40m",
                     distance >40 & distance <=50 ~ "40m - 50m",
                     TRUE ~ ">50m")
         
  )

# ordering factor of dist50m
min_dist_alfa_indo$dist50m = factor(min_dist_alfa_indo$dist50m,levels = c("<= 50m","50m - 100m","100m - 150m","150m - 200m","200m - 250m","250m - 300m","300m - 350m",
                            "350m - 400m","400m - 450m","450m - 500m",">500m"))

min_dist_alfa_indo$dis10m = factor(min_dist_alfa_indo$dis10m,levels = c("<= 10m","10m - 20m","20m - 30m","30m - 40m","40m - 50m",">50m"))

hist(min_dist_alfa_indo$distance)
prop_dist_alfa = data.frame(prop.table(table(min_dist_alfa_indo$dist50m)))

prop_dist_alfa$color_fill = case_when(
  prop_dist_alfa$Var1 == "<= 50m" ~ "1",
  TRUE ~ "0"
)

prop_dist_alfa$label_fill = case_when(
  prop_dist_alfa$Var1 == "<= 50m" | prop_dist_alfa$Var1 == "50m - 100m" | prop_dist_alfa$Var1 == "100m - 150m" ~ paste0(as.character(round(prop_dist_alfa$Freq*100,1)),"%"),
  TRUE ~ " ")

# plotting
group.colors <- c("1" = "dark blue", "0" = "light blue")

p_alfa = prop_dist_alfa %>% ggplot(aes(x=Var1,y=Freq,fill=color_fill)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels=scales::percent) +
      scale_fill_manual(values=group.colors) +
      geom_text(aes(label=label_fill),
                  nudge_y = +0.01)

# title
p_alfa = p_alfa + 
  ggtitle("Number of Alfamart Store Based on Distance with Indomaret") +
  ylab("Number of Store")+
  xlab("Closest Distance to Indomaret")

# label for 

# nudge and text position
p_alfa = p_alfa + 
  theme(
  axis.title.y = element_text(vjust = +3),
  axis.text.x = element_text(angle = 45, vjust =+1, hjust=1),
  axis.title.x = element_text(vjust = 0),
  plot.title = element_text(vjust = +2, size = 17),
  legend.position = "none"
)

# deleting grid
p_alfa = p_alfa + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "white"))

p_alfa


# ------------------ DISTANCE INDO INDO ------------------------------
# cross join
dist_indo_indo = crossing(x=indomaret,y=indomaret)

# hacking my way to get intended data format
dist_indo_indo_x = tibble(dist_indo_indo$x)
colnames(dist_indo_indo_x) = c("ID_indo_x","lat_indo_x","lon_indo_x","indo_brand_x")
dist_indo_indo_y = tibble(dist_indo_indo$y)
colnames(dist_indo_indo_y) = c("ID_indo_y","lat_indo_y","lon_indo_y","indo_brand_y")
dist_indo_indo = cbind(dist_indo_indo_x,dist_indo_indo_y)

dist_indo_indo$indo_lon_lat_x = cbind(dist_indo_indo$lon_indo_x,dist_indo_indo$lat_indo_x)
dist_indo_indo$indo_lon_lat_y = cbind(dist_indo_indo$lon_indo_y,dist_indo_indo$lat_indo_y)

# distance
dist_indo_indo$distance = distHaversine(dist_indo_indo$indo_lon_lat_x, dist_indo_indo$indo_lon_lat_y, r=6378137)

# sort by alfa id and distance for ease of observation and rank based on alfa_ID
dist_indo_indo = dist_indo_indo %>% arrange(ID_indo_x,distance) %>% group_by(ID_indo_x) %>% mutate(rank=row_number())

# filter minimum distance
min_dist_indo_indo = dist_indo_indo %>% filter(rank == 2)

# binning based on 50 meter distance
min_dist_indo_indo = min_dist_indo_indo %>% 
  mutate(dist50m =
           case_when (distance <= 50 ~ "<= 50m",
                      distance > 50 & distance <= 100 ~ "50m - 100m",
                      distance > 100 & distance <= 150 ~ "100m - 150m",
                      distance > 150 & distance <= 200 ~ "150m - 200m",
                      distance > 200 & distance <= 250 ~ "200m - 250m",
                      distance > 250 & distance <= 300 ~ "250m - 300m",
                      distance > 300 & distance <= 350 ~ "300m - 350m",
                      distance > 350 & distance <= 400 ~ "350m - 400m",
                      distance > 400 & distance <= 450 ~ "400m - 450m",
                      distance > 450 & distance <= 500 ~ "450m - 500m",
                      TRUE ~ ">500m"
           )
  ) %>% 
  mutate(dis10m =
           case_when(distance <= 10 ~ "<= 10m",
                     distance > 10 & distance <=20 ~ "10m - 20m",
                     distance >20 & distance <=30 ~ "20m - 30m",
                     distance >30 & distance <=40 ~ "30m - 40m",
                     distance >40 & distance <=50 ~ "40m - 50m",
                     TRUE ~ ">50m")
         
  )
# ordering factor of dist50m
min_dist_indo_indo$dist50m = factor(min_dist_indo_indo$dist50m,levels = c("<= 50m","50m - 100m","100m - 150m","150m - 200m","200m - 250m","250m - 300m","300m - 350m",
                                                                          "350m - 400m","400m - 450m","450m - 500m",">500m"))

min_dist_indo_indo$dis10m = factor(min_dist_indo_indo$dis10m,levels = c("<= 10m","10m - 20m","20m - 30m","30m - 40m","40m - 50m",">50m"))

hist(min_dist_indo_indo$distance)
prop.table(table(min_dist_indo_indo$dist50m))
prop_dist_indo = data.frame(prop.table(table(min_dist_indo_indo$dist50m)))

# preparation of plotting
prop_dist_indo$color_fill = case_when(
  prop_dist_indo$Var1 == "<= 50m" ~ "1",
  TRUE ~ "0"
)

prop_dist_indo$label_fill = case_when(
  prop_dist_indo$Var1 == "<= 50m" | prop_dist_indo$Var1 == "50m - 100m" | prop_dist_indo$Var1 == "100m - 150m" ~ paste0(as.character(round(prop_dist_indo$Freq*100,1)),"%"),
  TRUE ~ " ")

# plotting
group.colors <- c("1" = "dark blue", "0" = "light blue")

p_indo = prop_dist_indo %>% ggplot(aes(x=Var1,y=Freq,fill=color_fill)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=group.colors) +
  geom_text(aes(label=label_fill),
            nudge_y = +0.01)

# title
p_indo = p_indo + 
  ggtitle("Number of Indomaret Store Based on The Closest Distance with Other Indomaret") +
  ylab("Number of Store")+
  xlab("Closest Distance to Other Indomaret")

# label for 

# nudge and text position
p_indo = p_indo + 
  theme(
    axis.title.y = element_text(vjust = +3),
    axis.text.x = element_text(angle = 45, vjust =+1, hjust=1),
    axis.title.x = element_text(vjust = 0),
    plot.title = element_text(vjust = +2, size = 15),
    legend.position = "none"
  )

# deleting grid
p_indo = p_indo + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "white"))

p_indo

source_string <- paste0("Source: Amir Harjo Analysis (amirharjo.medium.com)")

# Turn plot into a gtable for adding text grobs
indo_gtable   <- ggplot_gtable(ggplot_build(p_indo))

# Make the source and note text grobs
source_text <- grid::textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =grid::gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
indo_gtable   <- arrangeGrob(indo_gtable, bottom = source_text)

ggsave("Indomaret Distance to Indomaret.jpg", indo_gtable, width = 20, height = 12, units = "cm")

# ----------------- plotting Alfamaret and Indomaret location Red Ocean --------------------
map_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx"
register_google(key = map_key)

# alfa and indomaret
select_alfa_indo = min_dist_alfa_indo %>% filter (distance <=150)

select_alfa_indo_alfa = select_alfa_indo %>% select (ID_alfa, lat_alfa, lon_alfa, alfa_brand)
colnames(select_alfa_indo_alfa) = c("ID","lat","lon","brand")
select_alfa_indo_alfa = unique(select_alfa_indo_alfa)

select_alfa_indo_indo = select_alfa_indo %>% data.frame() %>% select (ID_indo, lat_indo, lon_indo, indo_brand)
colnames(select_alfa_indo_indo) = c("ID","lat","lon","brand")
select_alfa_indo_indo = unique(select_alfa_indo_indo)

alfa_indo_map = rbind(select_alfa_indo_alfa, select_alfa_indo_indo)

# load South Tangerang Map
ts_map <- get_map("South Tangerang", zoom=12)

# create heatmap from location
m_alfa <- ggmap(ts_map)
m_alfa <- m_alfa + stat_density2d(data=alfa_indo_map, aes(x=lon, y=lat,fill=after_stat(level), alpha=after_stat(level)),
                        geom = "polygon")
m_alfa <- m_alfa + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
m_alfa <- m_alfa + geom_point(data=alfa_indo_map, aes(x=lon, y=lat,color=brand),
                    # color="red", 
                    alpha=0.5) +
          ggtitle("Red Ocean Alfamart and Indomaret")+
          theme(
            plot.title = element_text(vjust = +2, size = 15)
          )
          
          
m_alfa

ggsave(m_alfa,filename = "heatmap alfa and indo.png")

# indomaret and indomaret

select_indo_indo = min_dist_indo_indo %>% filter (distance <=150)

select_indo_indo = select_indo_indo %>% select (ID_indo_x, lat_indo_x, lon_indo_x, indo_brand_x)
colnames(select_indo_indo) = c("ID","lat","lon","brand")
select_indo_indo = unique(select_indo_indo)

# create heatmap from location
m_indo <- ggmap(ts_map)
m_indo <- m_indo + stat_density2d(data=select_indo_indo, aes(x=lon, y=lat,fill=after_stat(level), alpha=after_stat(level)),
                                  geom = "polygon")
m_indo <- m_indo + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
m_indo <- m_indo + geom_point(data=select_indo_indo, aes(x=lon, y=lat,color=brand),
                              # color="red", 
                              alpha=0.5) +
  ggtitle("Red Ocean Indomaret vs Indomaret")+
  theme(
    plot.title = element_text(vjust = +2, size = 15)
  )

m_indo

ggsave(m_indo,filename = "heatmap indo and indo.png")

# source
# https://www.programmingr.com/tutorial/cross-join-in-r/
# https://www.rdocumentation.org/packages/geosphere/versions/1.5-18/topics/distHaversine
# https://stackoverflow.com/questions/40364066/find-distance-between-locations-in-r-data-frame
# https://statisticsglobe.com/numbering-rows-within-groups-of-data-frame-in-r
# https://www.rdocumentation.org/packages/dplyr/versions/1.0.10/topics/case_when
# 