library("ggplot2")
theme_set(theme_bw())
library("sf")


library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
head(world)

accidents_df_for_plotting <- read.csv('C:/Users/Soham More/Documents/Github/dmml1/Datasets/Iowa_Road_Accidents/v1_Data_Processing/Road_Accidents_2015_2016.csv', header = TRUE)

fatality_locations <- accidents_df_for_plotting$Crash.Location[accidents_df$Fatalities>0]
fatality_locations <- sub("\\).*", "", sub(".*\\(", "", fatality_locations)) 
fatality_locations <- data.frame(do.call('rbind', strsplit(as.character(fatality_locations),' ',fixed=TRUE)))
tail(fatality_locations)
colnames(fatality_locations) <- c('longitude', 'latitude')
str(fatality_locations)
fatality_locations$longitude <- as.numeric(as.character(fatality_locations$longitude))
fatality_locations$latitude <- as.numeric(as.character(fatality_locations$latitude))
(fatality_locations_sf <- st_as_sf(fatality_locations, coords = c("longitude", "latitude"), 
                                  crs = 4326, agr = "constant"))


#accident_location <- accidents_df$Crash.Location
#accident_location
#accident_location <- sub("\\).*", "", sub(".*\\(", "", accident_location)) 
#accident_location
#accident_location_df <- data.frame(do.call('rbind', strsplit(as.character(accident_location),' ',fixed=TRUE)))
#tail(accident_location_df)
#colnames(accident_location_df) <- c('longitude', 'latitude')

#str(accident_location_df)

#head(accident_location_df$lonogitude)
#accident_location_df$longitude <- as.numeric(as.character(accident_location_df$longitude))
#accident_location_df$latitude <- as.numeric(as.character(accident_location_df$latitude))
#head(accident_location_df)

#(accident_location_sf <- st_as_sf(accident_location_df, coords = c("longitude", "latitude"), 
#                   crs = 4326, agr = "constant"))

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

str(accident_location_df)
accident_location_df$longitude <- as.numeric(accident_location_df$longitude)
accident_location_df$latitude <- as.numeric(accident_location_df$latitude)

(accident_location_sf <- st_as_sf(accident_location_df, coords = c("longitude", "latitude"), 
                                  crs = 4326, agr = "constant"))

View(counties)
library(ggspatial)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = counties, fill = NA) +
  #geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  geom_sf(data = accident_location_sf, size = 1, shape = 23, fill = "red") +
  geom_sf(data = fatality_locations_sf, size = 2, shape = 21, fill = "yellow") +
#  geom_point(data = accident_location_df, aes(x = longitude, y = latitude), size = 0.25, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-96.724, -90.141), ylim = c(40.410, 43.549), expand = TRUE)



# Plotting alcohol stores and weather stations

liquor_store_df <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/Iowa_Liquor_Sales_Final_2015_2016_v2.csv', header = TRUE)
weather_station_df <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Weather_Data/NewDatasets/Weather Station Location.csv', header = TRUE)
head(liquor_store_df)
head(weather_station_df)

(weather_station_sf <- st_as_sf(weather_station_df, coords = c("Longitude", "Latitude"), 
                                  crs = 4326, agr = "constant"))


liquor_store_location <- liquor_store_df$Store.Location
head(liquor_store_location)
liquor_store_location <- sub("\\).*", "", sub(".*\\(", "", liquor_store_location)) 
head(liquor_store_location)
liquor_store_location <- unique(liquor_store_location)
write.csv(liquor_store_location,'C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/unique_liquor_store_locations.csv', row.names = FALSE)

sum(grepl("^\\s*$", liquor_store_location))
str(liquor_store_location)
liquor_store_location <- liquor_store_location[!liquor_store_location == '']
liquor_store_location <- trimws(liquor_store_location)
liquor_store_location <- gsub("\\s+"," ",liquor_store_location)  # Replace consecutive spaces with a single space
liquor_store_location <- data.frame(do.call('rbind', strsplit(as.character(liquor_store_location),' ',fixed=TRUE)))
tail(liquor_store_location)
colnames(liquor_store_location) <- c('longitude', 'latitude')


str(liquor_store_location)
liquor_store_location$longitude <- as.numeric(as.character(liquor_store_location$longitude))
liquor_store_location$latitude <- as.numeric(as.character(liquor_store_location$latitude))
head(liquor_store_location)

(liquor_store_sf <- st_as_sf(liquor_store_location,  coords = c("longitude", "latitude"),
                                crs = 4326, agr = "constant"))


library(ggspatial)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = counties, fill = NA) +
  geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  geom_sf(data = weather_station_sf, size = 2, shape = 23, fill = "red") +
  geom_sf(data = liquor_store_sf, size = 2, shape = 21, fill = "yellow") +
  #geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", nudge_y = states$nudge_y) +
  #annotation_scale(location = "bl", width_hint = 0.2) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
  #                       style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-96.724, -90.141), ylim = c(40.376, 43.549), expand = TRUE)





#####
## Plot seasonality in alcohol sales
#####

alcohol_df_for_plotting <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales/v1_Data_Processing/alcohol_df_aggregate.csv", header = TRUE)
str(alcohol_df_for_plotting)
alcohol_df_for_plotting <- aggregate(. ~ Date,data=alcohol_df_for_plotting, FUN=sum, na.rm=TRUE, na.action=NULL)

library(ggplot2)

# Basic line plot
ggplot(data = alcohol_df_for_plotting, aes(x = Date, y = Sale..Dollars., group = 1))+
  geom_line(color = "#00AFBB", size = 1)

ggplot(data = alcohol_df, aes(x = Date, y = TMIN, group = 1))+
  geom_line(color = "#00AFBB", size = 1)


### Plotting Histogram of temperatures

hist(alcohol_df$TMAX)
hist(alcohol_df$TMIN)

ggplot(alcohol_df, aes(alcohol_df)) + 
  geom_histogram(data=subset(alcohol_df,y == 'TMAX'), fill = "red", alpha = 0.2) + 
  geom_histogram(data=subset(alcohol_df,y == 'TMIN'), fill = "blue", alpha = 0.2) 
p <- alcohol_df %>%
ggplot( aes(x=TMAX, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

ggplot(data = alcohol_df) +
  geom_histogram(aes(x = TMAX, y=(..count..)), 
                 alpha=0.2, fill ="red",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF") +
  geom_histogram(aes(x = TMIN, y=(..count..)), 
                 alpha=0.2,, fill ="blue",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF")


# PLotting ROC

library(pROC)
var1 <- as.numeric(test1$Fatality)
summary(test1$Fatality)
table(fitted.results)
var2 <- as.numeric(fitted.results)
var3 <- as.numeric(weighted_fitted.results)
var4 <- as.numeric(naive_pred)
var5 <- as.numeric(y_pred_svm_linear)
rocobj_weighted_logit <- roc(var1, var3)
rocobj_logit <- roc(var1, var2)
rocobj_naive <- roc(var1, var4)
rocobj_svm <- roc(var1, var5)
#rocobj2 <- roc(aSAH$outcome, aSAH$wfns)

library(ggplot2)
g <- ggroc(rocobj, alpha = 0.5, colour = "red", linetype = 2, size = 2)
g

g + theme_minimal() + ggtitle("My ROC curve") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")


gl <- ggroc(rocobj, legacy.axes = TRUE)
gl
gl + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")


g2 <- ggroc(list(NaiveBayes=rocobj_naive, LogisticRegresssion=rocobj_logit, WeightedLogisticRegression=rocobj_weighted_logit, SVM = rocobj_svm),  size = 1)
g2  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Comparison") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="black", linetype="dashed")
