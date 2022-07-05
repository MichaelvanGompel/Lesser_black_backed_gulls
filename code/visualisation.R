# REQUIRES main.R successful run
library(gridExtra)
library(rnaturalearth) 
library(cowplot) 
library(ggplot2) 
library(dplyr) 
# create map canvas for ggplot
world <- ne_countries(scale = "medium", returnclass = "sf")
return_map_timeframe <- c(1, 6:12)
departure_map_timeframe <- c(1:6, 12)


# function for selecting a timeframe from df for future map generation
SelectionDf <- function(df, map_timeframe) {
  df <- df %>% filter(year == 0)
  df <- df[!as.integer(format(df$date_time, "%m")) %in% map_timeframe,]
  df
}

# creates a map plot from df, my_size optional argument 
TrajectGroupPlot <- function (df, my_size=1.5) {
  temp <- df

  max_long <- max(temp$longitude)
  min_long <- min(temp$longitude)
  max_lat <- max(temp$latitude)
  min_lat <- min(temp$latitude)
  
  # generate bounderies with latitude and longitude for map generation
  upperleftmap <- c(min_lat - 5 ,max_long + 5)
  lowerrightmap <- c(max_lat + 5, min_long - 5)
  
  # all birds different colour lines, no gradient
  my_plot <- ggplot(data = world) +
    geom_sf() +
    #geom_point(data = first_bird, aes(x = longitude, y = latitude), size = 1, 
    #   shape = 23, fill = "darkred") +
    geom_path(data=temp, aes(x=longitude, y=latitude, group=device_info_serial, color=device_info_serial)  ) +
    geom_point(data=temp, aes(x=longitude, y=latitude, group=device_info_serial,color=device_info_serial), size=my_size)+
    coord_sf(xlim = c(min_long - 5, max_long + 5), ylim = c(min_lat - 5, max_lat + 5), expand = FALSE) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5), panel.background = element_rect(fill = "white"))+
    labs(colour = "Bird ID") 
  my_plot
  
  
}


# make a lat over time and map plot for every bird past 
for (i in 1:length(raw_split_data) ){
  next_bird <- raw_split_data[[i]]


  max_long <- max(next_bird$longitude)
  min_long <- min(next_bird$longitude)
  max_lat <- max(next_bird$latitude)
  min_lat <- min(next_bird$latitude)

  #generate map plot using ggplot and rnaturalearth
  map_plot <- ggplot(data = world) +
    geom_sf() +
    geom_path(aes(x=longitude, y=latitude, color= date_time), data=next_bird ) +
    coord_sf(xlim = c(min_long - 5, max_long + 5), ylim = c(min_lat - 5, max_lat + 5), expand = FALSE) +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
                                          size = 0.5), panel.background = element_rect(fill = "white"))+
    labs(colour = "Date") +
    ggtitle(next_bird$device_info_serial)



  # generate plot for single bird with latitude over time
  lat_time_plot <- next_bird %>%
    ggplot(aes(date_time, latitude, color=date_time))+
    geom_path() +
    scale_color_datetime() +
    ggtitle(next_bird$device_info_serial)


  # plotting two
  print(plot_grid(map_plot, lat_time_plot, labels = "AUTO", ncol = 1))

  g <- grid.arrange(map_plot, lat_time_plot, ncol=1)
  # for saving plots
  #ggsave(g, path = "figures", filename=paste("bird_",next_bird$device_info_serial[[1]],".png",sep=""))

}

# section for creating specific plots for individuals
select_bird <- raw_split_data$`5952`
select_bird <- select_bird %>% 
  filter(year==0)

max_long <- max(select_bird$longitude)
min_long <- min(select_bird$longitude)
max_lat <- max(select_bird$latitude)
min_lat <- min(select_bird$latitude)

# generate bounderies with latitude and longitude for map generation
upperleftmap <- c(min_lat - 5 ,max_long + 5)
lowerrightmap <- c(max_lat + 5, min_long - 5)

#mean_long = mean(select_bird$longitude)
#mean_lat = mean(select_bird$latitude)


#generate map plot for a single bird using ggplot and rnaturalearth
map_plot <- ggplot(data = world) +
  geom_sf() +
  #geom_point(data = first_bird, aes(x = longitude, y = latitude), size = 1, 
          #   shape = 23, fill = "darkred") +
  geom_path(aes(x=longitude, y=latitude, color= date_time), data=select_bird ) +
  coord_sf(xlim = c(min_long - 5, max_long + 5), ylim = c(min_lat - 5, max_lat + 5), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                      size = 0.5), panel.background = element_rect(fill = "white"))+
  labs(colour = "Date") 

# generate plot for single bird with latitude over time
lat_time_plot <-  ggplot(select_bird, aes(date_time, latitude, color=date_time))+
  geom_path() +
  scale_color_datetime() +
  theme_minimal()+
  ylim(min_lat - 5, max_lat + 5)+
  theme(legend.position = "none")


# plotting the map plot and the latplot
plot_grid(map_plot, lat_time_plot, labels = "AUTO", ncol = 2, align = "h")

# plot all birds latitude in one graph
ggplot(bind_rows(departure_data, .id="df"), aes(date_time, latitude, colour=df)) +
  geom_path()





# transpose data into dataframe for visualisation
juv_depart <- data.frame(group="Juvenile", value=juv_depart)
adult_depart  <- data.frame(group="Adult", value=adult_depart)

juv_return <- data.frame(group="Juvenile", value=juv_return)
adult_return <- data.frame(group="Adult", value=adult_return)

# group data frames based on return or depart
depart_plot_data <- rbind(juv_depart, adult_depart)
return_plot_data <- rbind(juv_return, adult_return)

# create boxplot for both groups
autumn <- ggplot(depart_plot_data, aes(x=group, y=value)) +
  geom_boxplot(fill="gray") +
  labs(y= "Day in Year", x = "") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position = "none")

# create boxplot for both groups
spring <- ggplot(return_plot_data, aes(x=group, y=value)) +
  geom_boxplot(fill="darkgray") +
  labs(y= "Day in Year", x = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size = 20), legend.position = "none")

plot_grid(autumn,spring, labels = "auto")

departure_data_filtered <- lapply(departure_data, SelectionDf, map_timeframe=departure_map_timeframe)
return_data_filtered <- lapply(return_data, SelectionDf, map_timeframe=return_map_timeframe)

# create plot for autumn migration
departure_plot <- TrajectGroupPlot(bind_rows(departure_data_filtered), my_size=1)

# identify which birds are not returning
non_migrating <- which(is.na(return_df$onset_migration))
non_migrating_ids <- rownames(return_df)[non_migrating]
# segregate non migrating birds from migrating birds 
temp <- bind_rows(return_data_filtered)
non_returning <- subset(temp, (device_info_serial %in% non_migrating_ids))
returning <- subset(temp, !(device_info_serial %in% non_migrating_ids))


# create plots for 
return_plot <- TrajectGroupPlot(returning)
non_return_plot <- TrajectGroupPlot(non_returning)

departure_plot

plot_grid(return_plot, non_return_plot, labels="auto")


#NOTE these values were set using two iterations of main.R using two different critical values
juv_dep_250 <- juv_depart
juv_dep_100 <- juv_depart

# transpose data into dataframe for visualisation
juv_dep_100 <- data.frame(group="Threshold 100km", value=juv_dep_100)
juv_dep_250 <- data.frame(group="Threshold 250km", value=juv_dep_250)
threshold_plot_data <- rbind(juv_dep_100, juv_dep_250)

# create boxplot for both groups
threshold_plot <- ggplot(threshold_plot_data, aes(x=group, y=value)) +
  geom_boxplot(fill="gray") +
  labs(y= "Day in Year", x = "") +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position = "none")
threshold_plot
