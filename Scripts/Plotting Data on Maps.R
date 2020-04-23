library("ggplot2")
theme_set(theme_bw())
library("sf")


library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
head(world)

accidents_df <- read.csv('C:/Users/Soham More/Documents/Github/dmml1/Datasets/Iowa_Road_Accidents/v1_Data_Processing/Vehicle_Accidents_in_Iowa_by_Location__Last_Ten_Years_.csv', header = TRUE)

accident_location <- accidents_df$Crash.Location
accident_location
accident_location <- sub("\\).*", "", sub(".*\\(", "", accident_location)) 
accident_location
accident_location_df <- data.frame(do.call('rbind', strsplit(as.character(accident_location),' ',fixed=TRUE)))
accident_location_df
colnames(accident_location_df) <- c('longitude', 'latitude')

(accident_location_df <- st_as_sf(accident_location_df, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))

library("tools")
states$ID <- as.character(states$ID)
states$ID <- toTitleCase(states$ID)
head(states)

str(states)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("iowa", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

ggplot(data = world) +
  geom_sf() +
#  geom_sf(data = states, fill = NA) + 
#  geom_sf(data = counties, aes(fill = area)) +
#  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  geom_sf(data = accident_location_df, size = 3, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-96.724, -90.141), ylim = c(40.376, 43.549), expand = TRUE)
