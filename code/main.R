library(dplyr)
library(geosphere)
library(lubridate)
library(modelr)

setwd("~/Biologie/Bachelor Project LBBG/R_studio_files")


# times in minutes 
ONEWEEK <- 10080
TWOWEEKS <- 20160
FOURWEEKS <- 40320
YEARINSECONDS <- 31556926
# conversion rate for latitude to km
LATTOKM <- 110.574
# location of Forteiland
FORTENEILAND <- c( 4.575903, 52.464984)
CRITICALDISTANCE <- 250
# time frame for analysing spring migration, not adjustable for now
time_frame <- c(3:6)

GetTheSpeed <- function(df){
  df$difftime <-  as.numeric(difftime(df$date_time, lag(df$date_time)))/60
  df$difflat <- (df$latitude - lag(df$latitude)) * LATTOKM
  
  df$speed <- df$difflat / df$difftime
  df
}
GetTheYear <- function(df){
  df$year <-  floor((df$date_time - min(df$date_time)) / YEARINSECONDS)
  df$year <- as.integer(df$year)
  df
  
}
GetBaseDist <- function(df){
  df %>% rowwise() %>%
    mutate(distance = distHaversine(FORTENEILAND, c(longitude, latitude))/1000)

}

GetTheWeek <- function(df) {
  df %>% rowwise() %>%
    mutate(weeknr = week(date_time))
}
GetTheWeeklyAvg <- function(df) {
  df %>% group_by(weeknr, year) %>%
    mutate(weekly_lat = weekly_lat <- mean(latitude),
           weekly_lon = weekly_lon <- mean(longitude))
    
}

GetSpringDist <- function(df, point_origin, i) {

  df <- df %>% filter(weeknr == i) %>% rowwise() %>%
    mutate(spring_distance = distHaversine(point_origin, c(longitude, latitude))/1000)
  df$spring_distance
  
}


# function for checking if a period fits in the returning subset 
CheckPeriodReturn <- function (df, min_month, max_month, temp_year=0) {
  
  # if given year is unavailable for this data return NA
  if (!(temp_year %in% df$year )){
    return(NA)
  } else {
    cdf <- df %>% filter(year == temp_year)
  }
  # return date is always in the next calender year
  return_year <- year(min(cdf$date_time)) +1
  
  cdf$date_time <- as.Date.POSIXct(as.numeric(cdf$date_time))
  
  # generate a starting and ending date for the period
  start_date <- as.Date(paste(return_year, min_month, 1, sep="-"), "%Y-%m-%d")
  # go to max month + 1 to eventually get to the end of max month
  end_date <- as.Date(paste(return_year, max_month +1, 1, sep="-"), "%Y-%m-%d")
  # if the start date and end date are within the bounds, return original dataframe
  if ((start_date >= min(df$date_time)) & (end_date < max(df$date_time))){
    return(df)
    
  } else {
    return(NA)
  }
}
GetSpringMigration <- function (df, timeframe, year_selected){
  
  # create vector to store haversine distances in
  dist_vect <- c()
  #initialize variables to NA 
  migrating <- NA
  onset_migration <- NA
  subset_df <- df %>%
    filter(year == year_selected & as.integer(month(date_time)) %in% timeframe)
   
  # problematic line creates error if weeks absent in dataset
  subset_range <- seq_range(subset_df$weeknr, by = 1)
  # better but does not work in line 109 and 110
  # subset_range <- unique(subset_df$weeknr)
  
  # exclude first week from range because we need average weekly lat/lon from week before
  # NOTE because it is impossible for me to create a spring distance vector from within for loop in dataframe i work around it
  for (i in subset_range[-1]){
    j <- subset_range[which(subset_range == i)-1]
    # get weekly averages from week before
    temp_lat <- filter(subset_df, weeknr == i-1)['weekly_lat'][[1]][1]
    temp_lon <- filter(subset_df, weeknr == i-1)['weekly_lon'][[1]][1]
    
    point_origin <- c(temp_lon, temp_lat)
    # calculate the distances from previous weekly average for every measurement
    temp_dist_vect <- GetSpringDist(subset_df, point_origin, i)
    # append this week to the list
    dist_vect <- c(dist_vect, temp_dist_vect)
    
  }
  # remove the first week from the subset
  length_first_Week <- length(which(subset_df$weeknr==min(subset_df$weeknr)))
  
  #subset_df <- subset_df[-c(1:length_first_Week),]
  potential_onset <- min(which(dist_vect> CRITICALDISTANCE)) + length_first_Week
  cat("potential_onset: ", potential_onset,'\n')
  
  # return data based on how the potential spring migration is categorised
  if (is.infinite(potential_onset)){
    migrating <- FALSE
  } else if (subset_df$difftime[[potential_onset + 1]] > 36) {
    migrating <- NA
    cat("dist_vect", dist_vect, "\n")
    cat("time hole thus not available: ", subset_df$difftime[(potential_onset-1):potential_onset +1], "subset_df: ", subset_df$datetime[potential_onset], "\n")
  } else {
    migrating <- TRUE
    onset_migration <- subset_df$date_time[[potential_onset]]  
  }
  # format return data as list
  return_data <- list("id"=subset_df$device_info_serial[[1]],"migrating"= migrating, "onset_migration"=onset_migration) 
  return_data
}    






juv_data <- read.csv('IJ_juv_data_extracted.csv')
klaassen <- read.csv("Klaassen_suppl.csv", sep = ";")

# assigning the right data types to the dataframe vectors
juv_data$date_time <-as.POSIXct(juv_data$date_time ,fomat="%Y-%m-%d %H:%M:%S", tz="UTC")
juv_data$device_info_serial <- as.factor(juv_data$device_info_serial)

#remove faulty measurement
juv_data <- juv_data[-13305,]



# create data for adult individuals from Klaasen 2013
klaassen <- klaassen[1:14,] 

klaassen <- klaassen %>%
  mutate_at(vars(c(2,3,5,6)), as.Date, format="%d-%m")%>%
  mutate_at(c(4,7), as.numeric) %>%
  mutate_at("ID", as.factor)

# create seperate data frame for each bird
raw_split_data = split(juv_data, juv_data$device_info_serial)

# create new data columns for every individual based on location and datetime
raw_split_data <- lapply(raw_split_data, GetTheSpeed)
raw_split_data <- lapply(raw_split_data, GetTheYear)
raw_split_data <- lapply(raw_split_data, GetBaseDist)
raw_split_data <- lapply(raw_split_data, GetTheWeek)
raw_split_data <- lapply(raw_split_data, GetTheWeeklyAvg)



# start check for return migration, individuals should have data from febuari until end of may
return_data <- lapply(raw_split_data, CheckPeriodReturn, min_month=1, max_month=6)
return_data <- return_data[!is.na(return_data)]


# list with every individual spring migration information
return_dates <- lapply(return_data, GetSpringMigration, timeframe=time_frame, year_selected=0)

# generate spring migration dataframe
return_df <- as.data.frame(do.call(rbind, return_dates))
return_df$onset_migration <- as.numeric(return_df$onset_migration)
return_df$onset_migration <- as.Date.POSIXct(return_df$onset_migration)





# initialize for autumn migration analysis
suspect_birds <- c()
departure_dates <- c()
departure_rows <- c("device_info_number","datetime")
departure_df <- data.frame(row.names = departure_rows)
j <- 1
# old but functionial, made before I understood how to work with lapply
# iterate over list of dataframes
for (i in 1:length(raw_split_data)){

  next_bird <- raw_split_data[[i]]
  first_min <- min(which(next_bird$distance>CRITICALDISTANCE))

  # if bird does not travel over further than CRITICALDISTANCE first_min is infinite
  if (is.infinite(first_min)){
    suspect_birds <- c(suspect_birds, as.character(next_bird$device_info_serial[[1]]))
    cat("excluded", as.character(next_bird$device_info_serial[[1]]), "based on no distance above", CRITICALDISTANCE,"km", "\n")
    next

  }
  #check for big datagap at location of threshold
  temp_max_time_diff <- next_bird$difftime[first_min]
  
  #exclude data if timegap exceeeds 36 hours
  if (temp_max_time_diff> 36){
    suspect_birds <- c(suspect_birds, as.character(next_bird$device_info_serial[[1]]))
    cat("excluded", as.character(next_bird$device_info_serial[[1]]), "based on time diff of:", temp_max_time_diff, "\n")
    next
  }
  cat(as.character(next_bird$device_info_serial[[1]]), "with min ", next_bird$distance[[first_min]], "\n")
  
  # fill dataframe with Autumn migration onsets
  departure_df[j] <- c(as.character(next_bird$device_info_serial[[1]]),
                         next_bird$date_time[[first_min-1]])
  # departure_df$date_time[j] <- next_bird$date_time[[first_min-1]]
  departure_dates <- c(departure_dates, next_bird$date_time[first_min-1])
  j <- j+1

}
print("infinate values are caught and excluded.")

# transpose and assign right data type to departure dataframe
departure_df <- as.data.frame(t(departure_df))
departure_df$datetime <- as.Date.POSIXct(as.numeric(departure_df$datetime))


# create list of dataframes with birds that migrate atleast in autumn
subset_non_suspect <- subset(juv_data, !(device_info_serial %in% suspect_birds))
departure_data <-  split(subset_non_suspect, subset_non_suspect$device_info_serial)
#remove empty dataframes
departure_data <- departure_data[sapply(departure_data, function(x) dim(x)[1]) > 0]
departure_data <- lapply(departure_data, GetTheSpeed)
departure_data <- lapply(departure_data, GetTheYear)
departure_data <- lapply(departure_data, GetBaseDist)
departure_data <- lapply(departure_data, GetTheWeek)
departure_data <- lapply(departure_data, GetTheWeeklyAvg)


# dont know why but faulty datapoint for 5953 is back, so remove it
departure_data$`5953` <- departure_data$`5953`[-205,]




# transfer departure and return dates to day of year
adult_return <- yday(klaassen$spring_migration_start)
juv_return <- yday(return_df$onset_migration)
adult_depart <- yday(klaassen$autumn_migration_start)
juv_depart <- yday(departure_df$datetime)


# t test comparsons 
t.test(juv_return, adult_return, conf.level = 0.9)
t.test(juv_depart, adult_depart)





