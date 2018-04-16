suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(viridis))


#get the data
accidents <- as_tibble(read_csv("mvr-data.csv"))

#keep the traffic accidents
traffacc <- accidents %>%
  #separate Date of event; having months as observations might be useful.
  separate(DateOfEvent, c("Day", "Month", "Year"), sep = "\\.", remove = FALSE) %>%
  #separate hours:mins, make numeric
  separate(Hour, c("H", "M"), sep = "\\:", remove = FALSE) %>%
  mutate(Hours = as.numeric(H)) %>%
  #filter so we keep just the traffic accidents
  filter(Event == "сообраќајна незгода") %>% 
  mutate(Outcome = ordered(Outcome, levels = c("непознато", "повреда", "тешка повреда", "животна опасност", "смрт")))

# time as time!
traffacc <- traffacc %>% 
  mutate(DateTime= parse_date_time(paste(DateOfEvent, Hour, sep=" "), orders="dmy HM")) %>% 
  mutate(TimeHM=parse_time(x = Hour, format="%H:%M")) %>%
  mutate(TimeH=hour(x = DateTime)) %>% 
  mutate(TypeOfRoadMK=case_when(
    TypeOfRoad == 'Open'  ~ 'Отворен',
    TypeOfRoad == 'Rural' ~ 'Селски',
    TypeOfRoad == 'Urban' ~ 'Градски'
  ))

# Note the mising gdal-config error
# is solved by installing libgdal and libgdal development package
# on ubuntu 17 these were libgdal20 and libgdal-dev
# these have lots od dependencies 

# rgeos is needed for converting the rgdal object into a dataframe (fortify call)

# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("raster")
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
#get spatial data for Macedonia from GADM (but don't download every time)
MKD.adm1.spdf <- 
  getData(
    "GADM"
    , country = "Macedonia"
    , level = 1
  )

# map accidents to regions/municipalities
# every town is a municipality 
# just add municipalities for villages (start with 'с.')


# Read in administrative area level data
#https://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2
mkop <- get(load("MKD.adm1.spdf.RData"))
mkop.df <- fortify(mkop, region = "NAME_1")
head(mkop.df)
traffacc$Region %>% table

# Sample dataframe of unemployment info
unemployment.df <- data.frame(id= unique(mkop.df[,'id']),
                              unemployment = runif(n = length(unique(mkop.df[,'id'])), min = 0, max = 25))

mkop.df2 <- merge(mkop.df, unemployment.df, by.y = 'id', all.x = TRUE)

#Extracting names and centoids of administrative areas for plotting
# Get centroids of spatialPolygonDataFrame and convert to dataframe
# for use in plotting  area names. 

mkop.centroids.df <- data.frame(long = coordinates(mkop)[, 1],
                                lat = coordinates(mkop)[, 2]) 

# Get names and id numbers corresponding to administrative areas
mkop.centroids.df[, 'ID_2'] <- mkop@data[,'ID_1']
mkop.centroids.df[, 'NAME_2'] <- mkop@data[,'NAME_1']

#Create ggplot with labels for administrative areas

p <- ggplot(mkop.df2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = cut(unemployment,5))) +
  geom_text(data = mkop.centroids.df, aes(label = NAME_2, x = long, y = lat, group = NAME_2), size = 3) + 
  labs(x=" ", y=" ") + 
  theme_bw() + scale_fill_brewer('Unemployment Rate (Jan 2011)', palette  = 'PuRd') + 
  coord_map() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())

print(p)
