library(haven)
library(dplyr)
library(tidyverse)


# 1 Data Clean  -----------------------------------------------------------------

#### 1.1.1 Pre data (040724) ####

PRE <- read_dta("/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Data/Pre_Data/Original_Pre.dta") # Import data

# filter APCODE = 0
PRE_sum_ele <- subset(PRE, APCODE == '0') 

# sum daily electricy
PRE_sum_ele <- PRE_sum_ele %>% mutate(across(8:31, as.numeric))
PRE_sum_ele$elesum = rowSums(PRE_sum_ele[, 8:31], na.rm = TRUE)

PRE_sum_ele <- PRE_sum_ele %>% select(LOCATN_K,BILACCT_K,MTR_NB,APCODE,CHNM,DATE,rate,elesum)


# For accounts with only APCODE “0”, there should be no “kWh-Received”
PRE_sum_ele <- subset(PRE_sum_ele, CHNM != "kWh - Received")


# length(unique(PRE_sum_ele$BILACCT_K)) 
#check ID number 8688


#### 1.1.2 Add dummy variables (040824) ####

# whether Holiday from ULR https://publicholidays.com/us/arizona/2020-dates/
PRE_sum_ele$holiday <- ifelse(PRE_sum_ele$DATE == "2020-05-25"|
                                PRE_sum_ele$DATE == "2020-07-04"|PRE_sum_ele$DATE == "2020-09-07"|
                                PRE_sum_ele$DATE == "2020-10-12"|PRE_sum_ele$DATE == "2020-11-11"|
                                PRE_sum_ele$DATE == "2020-11-26"|PRE_sum_ele$DATE == "2020-12-25"|
                                PRE_sum_ele$DATE == "2020-01-01"|PRE_sum_ele$DATE == "2020-01-20"|
                                PRE_sum_ele$DATE == "2020-02-17"|PRE_sum_ele$DATE == "2019-01-01"|
                                PRE_sum_ele$DATE == "2019-01-21"|PRE_sum_ele$DATE == "2019-02-18"|
                                PRE_sum_ele$DATE == "2019-05-27"|PRE_sum_ele$DATE == "2019-07-04"|
                                PRE_sum_ele$DATE == "2019-09-02"|PRE_sum_ele$DATE == "2019-10-14"|
                                PRE_sum_ele$DATE == "2019-11-11"|PRE_sum_ele$DATE == "2019-11-28"|
                                PRE_sum_ele$DATE == "2019-12-25",1,0)

#Get the data set up where each day of the week has its own column.  
#That means one of the columns will be a "1" and the rest will be "0s" for each date
PRE_sum_ele$DATE <- as.Date(PRE_sum_ele$DATE)
PRE_sum_ele$Day_of_Week <- strftime(PRE_sum_ele$DATE, '%A')

# Day <- PRE_sum_ele %>% select(DATE,Day_of_Week)
# Day <- Day %>% distinct()
# Day_1 <- Day %>% 
#   mutate(var = 1) %>%                                              # Put a 1 in all reows of a column
#   spread(key = Day_of_Week, value = var, fill = 0) %>%             # Create the dummy variables
#   dplyr::select(DATE, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday)  # Re-order and drop "SAT"


PRE_sum_ele <- left_join(PRE_sum_ele,Day_1,by='DATE')

PRE_sum_ele <- PRE_sum_ele[ , -10]


save(PRE_sum_ele,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_data.RData')



#### 1.2.1 Post data (040824) ####

Post <- read_dta("/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Data/Post_Data/Original_Post.dta") # Import data


# filter APCODE = 0
Post_sum_ele <- subset(Post, APCODE == '0')

# sum daily electricy
Post_sum_ele <- Post_sum_ele %>% mutate(across(13:36, as.numeric))
Post_sum_ele$elesum = rowSums(Post_sum_ele[, 13:36], na.rm = TRUE)


# For accounts with only APCODE “0”, there should be no “kWh-Received”
Post_sum_ele <- subset(Post_sum_ele, CHNM != "kWh - Received")

Post_sum_ele <- Post_sum_ele %>% select(LOCATN_K,BILACCT_K,MTR_NB,APCODE,CHNM,DATE,RATE,elesum)

# length(unique(Post_sum_ele$BILACCT_K)) 
# check ID number 7591

#### 1.2.2 Add dummy variables (040824) ####

# whether Holiday from ULR https://publicholidays.com/us/arizona/2020-dates/
Post_sum_ele$holiday <- ifelse(Post_sum_ele$DATE == "2020-05-25"|
                                 Post_sum_ele$DATE == "2020-07-04"|Post_sum_ele$DATE == "2020-09-07"|
                                 Post_sum_ele$DATE == "2020-10-12"|Post_sum_ele$DATE == "2020-11-11"|
                                 Post_sum_ele$DATE == "2020-11-26"|Post_sum_ele$DATE == "2020-12-25"|
                                 Post_sum_ele$DATE == "2021-01-01"|Post_sum_ele$DATE == "2021-01-18"|
                                 Post_sum_ele$DATE == "2021-02-15"|Post_sum_ele$DATE == "2021-05-31"|
                                 Post_sum_ele$DATE == "2021-07-04"|Post_sum_ele$DATE == "2021-07-05"|
                                 Post_sum_ele$DATE == "2021-09-06"|Post_sum_ele$DATE == "2021-10-11"|
                                 Post_sum_ele$DATE == "2021-11-11"|Post_sum_ele$DATE == "2021-11-25"|
                                 Post_sum_ele$DATE == "2021-12-24"|Post_sum_ele$DATE == "2021-12-25"|
                                 Post_sum_ele$DATE == "2021-12-31"|Post_sum_ele$DATE == "2022-01-01"|
                                 Post_sum_ele$DATE == "2022-01-17"|Post_sum_ele$DATE == "2022-02-21"|
                                 Post_sum_ele$DATE == "2022-05-30"|Post_sum_ele$DATE == "2022-07-04"|
                                 Post_sum_ele$DATE == "2022-09-05"|Post_sum_ele$DATE == "2022-10-10"|
                                 Post_sum_ele$DATE == "2022-11-11"|Post_sum_ele$DATE == "2022-11-24"|
                                 Post_sum_ele$DATE == "2022-12-25"|Post_sum_ele$DATE == "2022-12-26",
                               1,0)


Post_sum_ele$DATE <- as.Date(Post_sum_ele$DATE)
Post_sum_ele$Day_of_Week <- strftime(Post_sum_ele$DATE, '%A')

Day <- Post_sum_ele %>% select(DATE,Day_of_Week)
Day <- Day %>% distinct()
Day_1 <- Day %>% 
  mutate(var = 1) %>%                                              # Put a 1 in all reows of a column
  spread(key = Day_of_Week, value = var, fill = 0) %>%             # Create the dummy variables
  dplyr::select(DATE, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday)  # Re-order and drop "SAT"


Post_sum_ele <- left_join(Post_sum_ele,Day_1,by='DATE')
Post_sum_ele <- Post_sum_ele[,-10]

save(Post_sum_ele,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_data.RData')



# 2 Temp Data -------------------------------------------------------------

#### 2.1 Geo ZIP CODE ####

library(tigris)
library(sf)
library(tidycensus)

account <- read_dta('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Data/account_zipcode.dta')
Zip <- unique(account$ZIP) # Total in 96 zipcode 



# Import polygon dataset
US <- st_read('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/tl_2019_us_zcta510/tl_2019_us_zcta510.shp')
ResearchArea <- US %>% filter(ZCTA5CE10 %in% Zip)
st_crs(ResearchArea)    
# Transfer 
ResearchArea <- st_transform(ResearchArea, 4326)



# Ensure the GSODR package is loaded for historical weather data
library(GSODR)
# Load historical weather station data included with GSODR
load(system.file("extdata", "isd_history.rda", package = "GSODR"))
# Transforming the historical weather data into an 'sf' object with WGS84 coordinates
isd_history_az_sf <- isd_history %>%
  as_tibble() %>% 
  filter(CTRY == "US" & STATE == "AZ") %>%  # Filtering the spatial dataframe for records from Pennsylvania, US
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

# Filter stations available within a specific date range
stations <- filter(isd_history_az_sf, BEGIN <= 20190101 & END >= 20221231)
stations


# Calculate centroids of polygons (counties)
az_co_centroids <- st_centroid(ResearchArea)

plot(ResearchArea$geometry)
plot(az_co_centroids$geometry, col="green", add=TRUE)
plot(stations$geometry, col="red", add = TRUE)

ggplot() +
  geom_sf(data = ResearchArea, fill = "#edeae8", color = "gray", alpha = 0.4, size = 0.7) +
  # Adding az_co_centroids with green color
  geom_sf(data = az_co_centroids, color = "#C0632D", size = 0.7) +
  # Adding stations with red color
  geom_sf(data = stations, shape = 2, color = "#0B2560", size = 1.2) +
  labs(title = "Map of Research Area, Centroids, and Stations") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 14),  # Adjusting font and size
        legend.position = "none") +  # Remove legend if not needed
  coord_sf(xlim = c(-113, -110.5), ylim = c(32, 34.5), expand = FALSE)   # Ensuring that the plot uses the spatial data's coordinate system

ggsave("0603_AZ_Station.png", width = 6, height = 6, dpi = 900, units = "in", type = "cairo-png")




library(ggplot2)
p <- ggplot() +
  geom_sf(data = ResearchArea, fill = "white", color = "black") +  
  geom_sf(data = az_co_centroids, color = "green") +  #
  # geom_sf(data = stations, color = "red") +  
  theme_minimal() + 
  labs(title = "Map of Research Area with Centroids and Stations",
       subtitle = "Green: Centroids, Red: Stations") +  
  theme(legend.position = "none")  

ggsave("research_area_map_0414.tiff", plot = p, width = 6, height = 4, units = "in", dpi = 300, device = "tiff")


# Identifying Stations Close to Centroids
# Join stations to closest centroids within 20,000 meters
stations_close_centroids <- st_join(az_co_centroids,stations, join = st_is_within_distance, dist = 200000, left = FALSE)
length(unique(stations_close_centroids$ZCTA5CE10))
length(unique(stations_close_centroids$NAME))


#  station 
nearest_station_indices <- st_nearest_feature(az_co_centroids, stations)

# stations nearest 
nearest_stations <- stations[nearest_station_indices, ]

length(unique(nearest_stations$NAME))


StaName <- unique(nearest_stations$STNID)

for (i in 1:length(StaName)) {
  INT <- get_GSOD(years = 2019:2022, station = StaName[i]) %>% select(STNID, NAME, YEARMODA, TEMP)
  if (i == 1) {Weather <- INT} else (Weather <- rbind(Weather, INT))
}

StationName <- cbind(az_co_centroids,nearest_stations) %>% select(ZCTA5CE10,STNID,NAME)

save(Weather,StationName, ResearchArea, account, 
     file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Weather_Station0416.RData' )

#### 2.2 Temp ####
# setwd('C:\\UMD_Research\\2022Sloan\\Originaldata\\TEMP')
# 
# Temp <- data.frame()
# 
# i <- 1:10
# Temp <- NULL
# for(i in 1:10){
#   path <- paste0(i, ".csv")
#   Temp <- rbind(Temp, read.csv(file = path, header = TRUE)[,c('STATION','DATE','HourlyDryBulbTemperature')])
# }
# 
# Temp$DATE <- as.POSIXct(Temp$DATE,format="%Y-%m-%dT%H")
# 
# Temp$HourlyDryBulbTemperature <- as.numeric(Temp$HourlyDryBulbTemperature)
# 
# Temp_Hour <- 
#   Temp %>% drop_na() %>% 
#   group_by(STATION,DATE) %>% 
#   summarise(meanbyhour=mean(HourlyDryBulbTemperature))

Temp <- read_csv('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2019-2022监测站点温度数据/HourlyTemp2019-2022.csv')

# Calculate the daily average temperature for each station
daily_avg_temp <- Temp %>%
  group_by(STATION, TIME) %>%
  summarise(daily_mean_temp = mean(TEMP, na.rm = TRUE))

#### 0603 Figure ####

AZ <- st_read('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/tl_2016_04_cousub/tl_2016_04_cousub.shp')
st_crs(AZ)    
# Transfer 
AZ <- st_transform(AZ, 4326)

library(ggplot2)

p <- ggplot() +
  geom_sf(data = ResearchArea, fill = "white", color = "black") +  
  geom_sf(data = az_co_centroids, color = "green") +  
  # geom_sf(data = stations, color = "red") +  
  theme_minimal() +  
  labs(title = "Map of Research Area with Centroids and Stations",
       subtitle = "Green: Centroids, Red: Stations") +  
  theme(legend.position = "none")  


# ggsave("research_area_map_0414.tiff", plot = p, width = 6, height = 4, units = "in", dpi = 300, device = "tiff")

# Enhanced plot setup
ggplot() +
  # Plot AZ with blue fill and black borders
  geom_sf(data = AZ, fill = "#edeae8", color = "gray", alpha = 0.4, size = 0.7) +
  # Plot ResearchArea with the same fill as AZ but different border
  geom_sf(data = ResearchArea, fill = "#C0632D", color = "black",  size = 0.7) +
  labs(title = "Map of Arizona and Research Area") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 14),  # Adjusting font and size
        legend.position = "none") +  # Remove legend if not needed
  coord_sf()  # Ensure the plot uses the spatial data's coordinate system

ggsave("0603_AZ.png", width = 6, height = 6, dpi = 900, units = "in", type = "cairo-png")



#### 2.3 Match ZIP TEMP (041424)####

#### PRE DATA 
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_data.RData')
account <- account %>% distinct()

Pre <- left_join(PRE_sum_ele,account,by='BILACCT_K')

Loc <- read.csv('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2019-2022监测站点温度数据/ZipcodeWithnearestWeatherStation.csv')
Loc1 <- Loc %>% select(ZIP,STATION)
Loc1$ZIP <- as.character(Loc1$ZIP)

Pre <- left_join(Pre,Loc1,by='ZIP')


Pre1 <- Pre %>% 
  left_join(daily_avg_temp, by = c('STATION', "DATE" = "TIME"))

# Pre1 <- Pre1[ , -16]

save(Pre1,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_data_withtemp.RData')

#### POST DATA 
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_data.RData')

Post <- left_join(Post_sum_ele,account,by='BILACCT_K')
Post <- left_join(Post,Loc1,by='ZIP')
Post <- Post %>% 
  left_join(daily_avg_temp, by = c('STATION', "DATE" = "TIME"))
# Post <- Post[ , -16]
save(Post,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_data_withtemp.RData')



# 3 Segmented (041524) -----------------------------------------------------
library(segmented)

#### POST ####
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_data_withtemp.RData')

# Clean RATE = NU 
Post <- Post %>%
  filter(RATE != "NU") %>% distinct()

# 2020-04-01 2022-09-27
Post %>% summarise(start_date = min(DATE), end_date = max(DATE))


##### POST 1 #####
Post_1 <- Post %>%
  filter(DATE >= as.Date("2020-04-01") & DATE <= as.Date("2021-03-31"))

Post_1$RATE <- as.numeric(Post_1$RATE)

ID <- unique(Post_1$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Post_1, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ daily_mean_temp + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ daily_mean_temp, psi = c(65,70))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]

  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2020}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = ' Post2020.csv')



##### POST 2 #####
Post_2 <- Post %>%
  filter(DATE >= as.Date("2021-04-01") & DATE <= as.Date("2022-12-31"))

Post_2$RATE <- as.numeric(Post_2$RATE)

ID <- unique(Post_2$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Post_2, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ daily_mean_temp + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ daily_mean_temp, psi = c(65,70))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]
  
  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2021}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = ' Post2021.csv')


##### Sample #####
DF <- Post_1 %>% filter(BILACCT_K =='18903002')


DF$RATE <- as.numeric(DF$RATE)


Datasubset_lm <- lm(elesum ~ daily_mean_temp + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                    , data = DF)
Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ daily_mean_temp, psi = c(55,60))
summary(Datasubset_seg)  





















#### PRE ####

load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_data_withtemp.RData')

# Clean RATE = NU 
Pre1 <- Pre1 %>%
  filter(rate != "NU") %>% distinct()

Pre1 %>% summarise(start_date = min(DATE), end_date = max(DATE))
# 2019-01-01 2020-04-30

Pre1$rate <- as.numeric(Pre1$rate)

ID <- unique(Pre1$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Pre1, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ daily_mean_temp + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ daily_mean_temp, psi = c(65,70))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]
  
  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2019}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = ' Pre2019.csv')









# 4 Result Analysis (041524) ----------------------------------------------

#Combine three year data
A <- read_csv('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Code/ Pre2019.csv')
B <- read_csv('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Code/ Post2021.csv')
C <- read_csv('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Code/ Post2020.csv')


IT0415 <- rbind(A,B,C) %>% arrange(V1, V8)
IT0415 <- IT0415[,-1]
IT0415 <- IT0415 %>% rename('BILACCT_K'='V1',
                            'HBP'='V2',
                            'CBP'='V3',
                            'S1'='V4',
                            'S2'='V5',
                            'S3'='V6',
                            'R2'='V7')

save(IT0415,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result0415.RData')


#### Heat Pump figure####

load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result0415.RData')
# Load survey 2017
library(readxl)
Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
                         sheet = 'Appended')

heatpump <- Survey2017 %>% select(BILACCT_K,VACTYPE,VINCOME) %>% drop_na()

HP <- IT0415 %>% left_join(heatpump, by='BILACCT_K') %>% drop_na()
# length(unique(HP$BILACCT_K)) # 6426
HP_1 <- HP %>% select(BILACCT_K,VACTYPE,VINCOME) %>% distinct()

HP_1$VACTYPE[which(HP_1$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
HP_1$VACTYPE[which(HP_1$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
HP_1$VACTYPE[which(HP_1$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'

HP_1$VINCOME[which(HP_1$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
HP_1$VINCOME[which(HP_1$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
HP_1$VINCOME[which(HP_1$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
HP_1$VINCOME[which(HP_1$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
HP_1$VINCOME[which(HP_1$VINCOME == "$150,000 or more")] <- '$150K or more'
HP_1$VINCOME[which(HP_1$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
HP_1$VINCOME[which(HP_1$VINCOME == "Less than $15,000")] <- 'Less than $15K'
HP_1$VINCOME[which(HP_1$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

HP_1$VACTYPE <- factor(HP_1$VACTYPE,levels = c("Heat pump",
                                               "AC unit packaged with gas heating",
                                               "Separate AC system",
                                               "Don't know"))
HP_1$VINCOME <- factor(HP_1$VINCOME,levels = c('Less than $15K',
                                               '$15K to 25K',
                                               '$25K to 35K',
                                               '$35K to 50K',
                                               '$50K to 75K',
                                               '$75K to 100K',
                                               '$100K to 150K',
                                               '$150K or more'))


library(viridis)
# Load the scales package to use label_number()
library(scales)

HP_1 %>% group_by(VACTYPE,VINCOME) %>% tally() %>% 
  ggplot(aes(x = VINCOME, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Income Group", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Income Group") +
  scale_fill_manual(breaks = HP_1$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(4))+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

setwd('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Fig')
ggsave("0415_AC_Types_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



#### PV figure ####
PV <- Survey2017 %>% select(BILACCT_K,VINCOME,VSOLARPNL) %>% drop_na()

Solar_1 <- IT0415 %>% left_join(PV, by='BILACCT_K') %>% drop_na()
length(unique(Solar_1$BILACCT_K)) # 6688

Solar_2 <- Solar_1 %>% select(BILACCT_K,VSOLARPNL,VINCOME) %>% distinct()

Solar_2$VINCOME[which(Solar_2$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$150,000 or more")] <- '$150K or more'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "Less than $15,000")] <- 'Less than $15K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

Solar_3 <- Solar_2 %>% 
  filter(VSOLARPNL == "Yes") %>% 
  select(BILACCT_K,VSOLARPNL,VINCOME)


frequency_data <- Solar_3 %>%
  group_by(VINCOME) %>%
  summarise(Frequency = n())

frequency_data$VINCOME <- factor(frequency_data$VINCOME,levels = c('Less than $15K',
                                                                   '$15K to 25K',
                                                                   '$25K to 35K',
                                                                   '$35K to 50K',
                                                                   '$50K to 75K',
                                                                   '$75K to 100K',
                                                                   '$100K to 150K',
                                                                   '$150K or more'))

# Now, create the plot
ggplot(frequency_data, aes(x = VINCOME, y = Frequency)) +
  geom_bar(stat = "identity", fill = "#38AAACCC", alpha = 0.8) + # Set the bar color to light yellow
  theme_minimal() +
  labs(x = "Income Group", y = "Frequency", title = "Distribution of Rooftop Solar by Income Group") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("0415_Solar_Types_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

















#### HP INCOME IT####

HP$VINCOME[which(HP$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
HP$VINCOME[which(HP$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
HP$VINCOME[which(HP$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
HP$VINCOME[which(HP$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
HP$VINCOME[which(HP$VINCOME == "$150,000 or more")] <- '$150K or more'
HP$VINCOME[which(HP$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
HP$VINCOME[which(HP$VINCOME == "Less than $15,000")] <- 'Less than $15K'
HP$VINCOME[which(HP$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

HP$VINCOME <- factor(HP$VINCOME,levels = c('Less than $15K',
                                           '$15K to 25K',
                                           '$25K to 35K',
                                           '$35K to 50K',
                                           '$50K to 75K',
                                           '$75K to 100K',
                                           '$100K to 150K',
                                           '$150K or more'))



HP_2019 <- HP %>% filter(V8 == 2019)
HP_2019 <- HP_2019 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))


ggplot(HP_2019, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9) )+
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0415_HPCBP2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2020 <- HP %>% filter(V8 == 2020)
HP_2020 <- HP_2020 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))


ggplot(HP_2020, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9) )+
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0415_HPCBP2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2021 <- HP %>% filter(V8 == 2021)
HP_2021 <- HP_2021 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))


ggplot(HP_2021, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9) )+
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0415_HPCBP2021_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")











# (2) Match Temp Data (041724) ------------------------------------------------

load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Weather_Station0416.RData')
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_data.RData')


account <- account[account$ZIP != "NULL", ]

Loc1 <- StationName %>% select(ZCTA5CE10,STNID) %>% rename('ZIP'='ZCTA5CE10')


zip_account <- unique(account$ZIP)
zip_Loc1 <- unique(Loc1$ZIP)
unique_account = setdiff(zip_account, zip_Loc1) ## Find ZIP codes that are unique to Loc1
# [1] "85236" "85244" "85269" "85068"

# Remove rows from account that have ZIP codes unique to Loc1
account1 <- account[!account$ZIP %in% unique_account, ]

Pre <- left_join(PRE_sum_ele,account1,by='BILACCT_K')

Pre <- left_join(Pre,Loc1,by='ZIP')

Pre1 <- Pre %>% 
  left_join(Weather, by = c('STNID', "DATE" = "YEARMODA"))


Pre1 <- Pre1[,-c(16:19)]
Pre1 <- Pre1 %>%
  filter(rate != "NU") %>% distinct()
save(Pre1,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_withGSODRtemp0417.RData')


#POST 
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_data.RData')

Post <- left_join(Post_sum_ele,account1,by='BILACCT_K')

Post <- left_join(Post,Loc1,by='ZIP')

Post1 <- Post %>% 
  left_join(Weather, by = c('STNID', "DATE" = "YEARMODA"))
Post1 <- Post1[,-c(16:19)]
Post1 <- Post1 %>%
  filter(RATE != "NU") %>% distinct()

save(Post1,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_withGSODRtemp0417.RData')

# (3) Segmented (041724) --------------------------------------------------
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Pre_withGSODRtemp0417.RData')
#### Pre ####
Pre1 %>% summarise(start_date = min(DATE), end_date = max(DATE))
# 2019-01-01 2020-04-30

Pre1$rate <- as.numeric(Pre1$rate)

ID <- unique(Pre1$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Pre1, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ TEMP + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ TEMP, psi = c(10,25))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]
  
  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2019}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = '0417Pre2019.csv')



# Post 
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Post_withGSODRtemp0417.RData')
#### POST 1 ####
Post_1 <- Post1 %>%
  filter(DATE >= as.Date("2020-04-01") & DATE <= as.Date("2021-03-31"))

Post_1$RATE <- as.numeric(Post_1$RATE)

ID <- unique(Post_1$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Post_1, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ TEMP + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ TEMP, psi = c(10,25))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]
  
  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2020}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = '0418Post2020.csv')

Post_2 <- Post1 %>%
  filter(DATE >= as.Date("2021-04-01") & DATE <= as.Date("2022-12-31"))

Post_2$RATE <- as.numeric(Post_2$RATE)

ID <- unique(Post_2$BILACCT_K)
REGSUMMARY <- array(NA,dim = c(length(ID),8))

for(i in 1:length(ID)){tryCatch({
  Datasubset <- subset(Post_2, BILACCT_K == ID[i] )
  Datasubset_lm <- lm(elesum ~ TEMP + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                      , data = Datasubset)
  Datasubset_seg <- segmented(Datasubset_lm,seg.Z = ~ TEMP, psi = c(10,25))
  
  REGSUMMARY[i,1] <- ID[i]
  #Balanced Point Est.
  REGSUMMARY[i,2] <- as.numeric(Datasubset_seg$psi[3])
  REGSUMMARY[i,3] <- as.numeric(Datasubset_seg$psi[4])
  
  #Slope
  S <- slope(Datasubset_seg)[[1]]
  #slope1
  REGSUMMARY[i,4] <- S[1,1]
  
  #slope2
  REGSUMMARY[i,5] <- S[2,1]
  
  #slope2
  REGSUMMARY[i,6] <- S[3,1]
  
  REGSUMMARY[i,7] <- as.numeric(summary(Datasubset_seg)[9])
  REGSUMMARY[i,8] <- 2021}
  
  ,error = function(e) {print(i)})
}

write.csv(REGSUMMARY,file = '0418Post2021.csv')





# (4) Analysis (041824) ---------------------------------------------------
#Combine three year data
A <- read_csv('/Users/yexiaofeng/0417Pre2019.csv')
B <- read_csv('/Users/yexiaofeng/0418Post2020.csv')
C <- read_csv('/Users/yexiaofeng/0418Post2021.csv')

IT0418 <- rbind(A,B,C) %>% arrange(V1, V8)
IT0418 <- IT0418[,-1]
IT0418 <- IT0418 %>% rename('BILACCT_K'='V1',
                            'HBP'='V2',
                            'CBP'='V3',
                            'S1'='V4',
                            'S2'='V5',
                            'S3'='V6',
                            'R2'='V7')

save(IT0418,file = '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result0418.RData')



#### Overall look and clean 0418 ####
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result0418.RData')
length(unique(IT0418$BILACCT_K)) #[1] 8412

# filter 1 R2 > 0 and S3 >0
DF <- subset(IT0418, R2 > 0 & S3 > 0)
length(unique(DF$BILACCT_K)) # [1] 8199


#### Heat Pump (041824) ####
Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
                         sheet = 'Appended')

HeatPump <- Survey2017 %>% select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)


DF <- DF %>% left_join(HeatPump, by='BILACCT_K') %>% drop_na()
length(unique(DF$BILACCT_K)) # 4690

DF <- DF %>% distinct()


DF$VACTYPE[which(DF$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
DF$VACTYPE[which(DF$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
DF$VACTYPE[which(DF$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'

DF$VINCOME[which(DF$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
DF$VINCOME[which(DF$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
DF$VINCOME[which(DF$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
DF$VINCOME[which(DF$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
DF$VINCOME[which(DF$VINCOME == "$150,000 or more")] <- '$150K or more'
DF$VINCOME[which(DF$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
DF$VINCOME[which(DF$VINCOME == "Less than $15,000")] <- 'Less than $15K'
DF$VINCOME[which(DF$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

DF$VACTYPE <- factor(DF$VACTYPE,levels = c("Heat pump",
                                               "AC unit packaged with gas heating",
                                               "Separate AC system",
                                               "Don't know"))
DF$VINCOME <- factor(DF$VINCOME,levels = c('Less than $15K',
                                               '$15K to 25K',
                                               '$25K to 35K',
                                               '$35K to 50K',
                                               '$50K to 75K',
                                               '$75K to 100K',
                                               '$100K to 150K',
                                               '$150K or more'))


library(viridis)
# Load the scales package to use label_number()
library(scales)

DF %>% select(BILACCT_K,VACTYPE,VINCOME) %>% distinct() %>% 
  group_by(VACTYPE,VINCOME) %>% tally() %>% 
  ggplot(aes(x = VINCOME, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Income Group", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Income Group") +
  scale_fill_manual(breaks = DF$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(4))+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

setwd('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724')
ggsave("0419_AC_Types_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



HP_2019 <- HP %>% filter(V8 == 2019)
HP_2019 <- HP_2019 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))


ggplot(HP_2019, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9) )+
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0415_HPCBP2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2020 <- HP %>% filter(V8 == 2020)
HP_2020 <- HP_2020 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))


ggplot(HP_2020, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9) )+
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0415_HPCBP2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2021 <- DF %>% filter(V8 == 2021)
HP_2021 <- HP_2021 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))


# ggplot(HP_2021, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
#   geom_boxplot(alpha = 0.8) +
#   scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
#   stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), vjust = -0.5, color = "black") +
#   theme_minimal() +
#   labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2021") +
#   theme_bw(base_size = 14, base_family = "Arial") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(0.2,0.1),
#         legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.title = element_text(size = 11),
#         legend.text = element_text(size = 9) )+
#   guides(fill = guide_legend(title = "Heat Pump"))


ggplot(HP_2021, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPCBP2021_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HP_2021, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPS2021_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2020 <- DF %>% filter(V8 == 2020)
HP_2020 <- HP_2020 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))
ggplot(HP_2020, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPCBP2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


ggplot(HP_2020, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPS2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

HP_2019 <- DF %>% filter(V8 == 2019)
HP_2019 <- HP_2019 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))
ggplot(HP_2019, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPCBP2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HP_2019, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave("0419_HPS2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")







# 5 Stata Prepare 0421  ---------------------------------------------------

load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Result0418.RData')
length(unique(IT0418$BILACCT_K)) #[1] 8412

# filter 1 R2 > 0 and S3 >0
DF <- subset(IT0418, R2 > 0 & S3 > 0)
length(unique(DF$BILACCT_K)) # [1] 8199


# Match Survey 
Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
                         sheet = 'Appended')
HeatPump <- Survey2017 %>% select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)


DF <- DF %>% left_join(HeatPump, by='BILACCT_K') %>% drop_na()
length(unique(DF$BILACCT_K)) # 4690


### RESAGE 2017data 
DF$VRESAGE <- ifelse(DF$V8 == 2019, DF$VRESAGE + 2, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2020, DF$VRESAGE + 3, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2021, DF$VRESAGE + 4, DF$VRESAGE)


DF$HP <- ifelse(DF$VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0)

write_dta(DF, '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_HP_0421.dta')











# 6 Stata Prepare 0607 ----------------------------------------------------
load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Result0418.RData')
length(unique(IT0418$BILACCT_K)) #[1] 8412

# filter 1 R2 > 0 and S3 >0
DF <- subset(IT0418, R2 > 0 & S3 > 0)
length(unique(DF$BILACCT_K)) # [1] 8199


# Match Survey 
Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
                         sheet = 'Appended')
PV <- Survey2017 %>% select(BILACCT_K,VSOLARPNL,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)


DF <- DF %>% left_join(PV, by='BILACCT_K') %>% drop_na()
length(unique(DF$BILACCT_K)) # 4846


### RESAGE 2017data 
DF$VRESAGE <- ifelse(DF$V8 == 2019, DF$VRESAGE + 2, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2020, DF$VRESAGE + 3, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2021, DF$VRESAGE + 4, DF$VRESAGE)


DF$PV <- ifelse(DF$VSOLARPNL == "Yes", 1, 0)

write_dta(DF, '/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_PV_0607.dta')






# 7 Heat Pump (060924) ------------------------------------------------------

load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Result0418.RData')


length(unique(IT0418$BILACCT_K)) #[1] 8412

# filter 1 R2 > 0 and S3 >0
DF <- subset(IT0418, R2 > 0 & S3 > 0)
length(unique(DF$BILACCT_K)) # [1] 8199

library(readxl)
# Match Survey 
Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
                         sheet = 'Appended')
HeatPump <- Survey2017 %>% select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)


DF <- DF %>% left_join(HeatPump, by='BILACCT_K') %>% drop_na()
length(unique(DF$BILACCT_K)) # 4690


### RESAGE 2017data 
DF$VRESAGE <- ifelse(DF$V8 == 2019, DF$VRESAGE + 2, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2020, DF$VRESAGE + 3, DF$VRESAGE)
DF$VRESAGE <- ifelse(DF$V8 == 2021, DF$VRESAGE + 4, DF$VRESAGE)


### data summary 

DF_0609 <- DF %>% 
  select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VHH_AGECODE) %>% distinct()


DF_0609$VACTYPE[which(DF_0609$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
DF_0609$VACTYPE[which(DF_0609$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
DF_0609$VACTYPE[which(DF_0609$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'

DF_0609$VINCOME[which(DF_0609$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$150,000 or more")] <- '$150K or more'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "Less than $15,000")] <- 'Less than $15K'
DF_0609$VINCOME[which(DF_0609$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

DF_0609$VACTYPE <- factor(DF_0609$VACTYPE,levels = c("Heat pump",
                                               "AC unit packaged with gas heating",
                                               "Separate AC system",
                                               "Don't know"))
DF_0609$VINCOME <- factor(DF_0609$VINCOME,levels = c('Less than $15K',
                                               '$15K to 25K',
                                               '$25K to 35K',
                                               '$35K to 50K',
                                               '$50K to 75K',
                                               '$75K to 100K',
                                               '$100K to 150K',
                                               '$150K or more'))


# Summarize data
summary <- DF_0609 %>%
  group_by(VACTYPE) %>%
  summarise(Count = n_distinct(BILACCT_K))  # Count distinct IDs for each AC type

# # Print the summary
# print(summary)

library(viridis)
# Load the scales package to use label_number()
library(scales)

DF_0609 %>% group_by(VACTYPE,VINCOME) %>% tally() %>% 
  ggplot(aes(x = VINCOME, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Income Group", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Income Group") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

setwd('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Fig')
ggsave("0609_AC_Types_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


DF_0609 <- DF_0609 %>%
  mutate(Ethnic = if_else(VETHNIC == "White/Caucasian", "White/Caucasian", "Others"))

DF_0609 %>% group_by(VACTYPE,Ethnic) %>% tally() %>% 
  ggplot(aes(x = Ethnic, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Ethnic", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Ethnic") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))

setwd('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Fig')
ggsave("0609_AC_Types_by_ethnic.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

DF_0609$VSQFEET <- factor(DF_0609$VSQFEET,levels = c('Under 1,000 sq. ft.',
                                                     '1,000 - 1,499 sq. ft.',
                                                     '1,500 - 1,999 sq. ft.',
                                                     '2,000 - 2,999 sq. ft.',
                                                     '3,000 - 3,999 sq. ft.',
                                                     '4,000 or more sq.'))
DF_0609 %>% group_by(VACTYPE,VSQFEET) %>% tally() %>% 
  ggplot(aes(x = VSQFEET, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Size", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Size") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.83,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9))
ggsave("0609_AC_Types_by_size.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



DF$VACTYPE[which(DF$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
DF$VACTYPE[which(DF$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
DF$VACTYPE[which(DF$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'

DF$VINCOME[which(DF$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
DF$VINCOME[which(DF$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
DF$VINCOME[which(DF$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
DF$VINCOME[which(DF$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
DF$VINCOME[which(DF$VINCOME == "$150,000 or more")] <- '$150K or more'
DF$VINCOME[which(DF$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
DF$VINCOME[which(DF$VINCOME == "Less than $15,000")] <- 'Less than $15K'
DF$VINCOME[which(DF$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

DF$VACTYPE <- factor(DF$VACTYPE,levels = c("Heat pump",
                                                     "AC unit packaged with gas heating",
                                                     "Separate AC system",
                                                     "Don't know"))
DF$VINCOME <- factor(DF$VINCOME,levels = c('Less than $15K',
                                                     '$15K to 25K',
                                                     '$25K to 35K',
                                                     '$35K to 50K',
                                                     '$50K to 75K',
                                                     '$75K to 100K',
                                                     '$100K to 150K',
                                                     '$150K or more'))

################### HP2019 ###################
HP_2019 <- DF %>% filter(V8 == 2019)
HP_2019 <- HP_2019 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))

CBP2019 <- ggplot(HP_2019, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(a) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CBP2019, file = "0619_HPCBP2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


CS2019 <- ggplot(HP_2019, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(b) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2019, file = "0619_HPCS2019_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


################### HP2020 ###################
HP_2020 <- DF %>% filter(V8 == 2020)
HP_2020 <- HP_2020 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))

CBP2020 <- ggplot(HP_2020, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(c) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CBP2020, file = "0619_HPCBP2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

CS2020 <- ggplot(HP_2020, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(d) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2020, file = "0619_HPCS2020_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

################### HP2021 ###################
HP_2021 <- DF %>% filter(V8 == 2021)
HP_2021 <- HP_2021 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0))

CBP2021 <- ggplot(HP_2021, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(e) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CBP2021, file = "0619_HPCBP2021_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

CS2021 <- ggplot(HP_2021, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(f) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2021, file = "0619_HPCS2021_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

# library(patchwork)
# # Arrange the plots
# plot_grid <- (CBP2019 + CS2019 + 
#                 CBP2020 + CS2020 +
#                 CBP2021 + CS2021)





################### 0615 Ethnic ###########
DF_0611 <- DF %>%
  mutate(Ethnic = if_else(VETHNIC == "White/Caucasian", "White/Caucasian", "Others"))


HP_2019 <- DF_0611 %>% filter(V8 == 2019)
HP_2019 <- HP_2019 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))

CBP2019 <- ggplot(HP_2019, aes(x = factor(Ethnic), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Balance Point", title = "(a) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
        legend.position = c(0.1,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

ggsave(CBP2019, file = "0615_HPCBP2019_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


CS2019 <- ggplot(HP_2019, aes(x = factor(Ethnic), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Slope", title = "(b) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
        legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2019, file = "0615_HPCS2019_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



HP_2020 <- DF_0611 %>% filter(V8 == 2020)
HP_2020 <- HP_2020 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))

CBP2020 <- ggplot(HP_2020, aes(x = factor(Ethnic), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Balance Point", title = "(c) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = c(0.1,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CBP2020, file = "0615_HPCBP2020_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

CS2020 <- ggplot(HP_2020, aes(x = factor(Ethnic), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Slope", title = "(d) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
        legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2020, file = "0615_HPCS2020_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2021 <- DF_0611 %>% filter(V8 == 2021)
HP_2021 <- HP_2021 %>%
  mutate(HP = if_else(VACTYPE == "Heat pump (same system heats and cools using electricity onl", 1, 0))

CBP2021 <- ggplot(HP_2021, aes(x = factor(Ethnic), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Balance Point", title = "(e) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
        legend.position = c(0.1,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CBP2021, file = "0615_HPCBP2021_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

CS2021 <- ggplot(HP_2021, aes(x = factor(Ethnic), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Ethnic", y = "Cooling Slope", title = "(f) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
        legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave(CS2021, file = "0615_HPCS2021_by_race.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



# 7 PV (061624） -----------------------------------------------------------
PV <- Survey2017 %>% select(BILACCT_K,VSOLARPNL,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)
# PV <- Survey2017 %>% select(BILACCT_K,VINCOME,VSOLARPNL) %>% drop_na()
# filter 1 R2 > 0 and S3 >0
DF <- subset(IT0418, R2 > 0 & S3 > 0)
length(unique(DF$BILACCT_K)) # [1] 8199
DF <- DF %>% left_join(PV, by='BILACCT_K') %>% drop_na()
length(unique(DF$BILACCT_K)) # 6457

################### frequency ###################

Solar_2 <- DF %>% select(BILACCT_K,VSOLARPNL,VINCOME) %>% distinct()

Solar_2$VINCOME[which(Solar_2$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$150,000 or more")] <- '$150K or more'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "Less than $15,000")] <- 'Less than $15K'
Solar_2$VINCOME[which(Solar_2$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

Solar_3 <- Solar_2 %>% 
  filter(VSOLARPNL == "Yes") %>% 
  select(BILACCT_K,VSOLARPNL,VINCOME)

frequency_data_Income <- Solar_2 %>%
  group_by(VINCOME) %>%
  summarise(Frequency = n())

frequency_data <- Solar_3 %>%
  group_by(VINCOME) %>%
  summarise(Frequency = n())

frequency_data$VINCOME <- factor(frequency_data$VINCOME,levels = c('Less than $15K',
                                                                   '$15K to 25K',
                                                                   '$25K to 35K',
                                                                   '$35K to 50K',
                                                                   '$50K to 75K',
                                                                   '$75K to 100K',
                                                                   '$100K to 150K',
                                                                   '$150K or more'))

# Now, create the plot
ggplot(frequency_data, aes(x = VINCOME, y = Frequency)) +
  geom_bar(stat = "identity", fill = "#38AAACCC", alpha = 0.8) + # Set the bar color to light yellow
  theme_minimal() +
  labs(x = "Income Group", y = "Frequency", title = "Distribution of Rooftop Solar by Income Group") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("0616_Solar_Types_by_Income_Group.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


################### CBP/CS ###################

DF_PV <- DF %>%
  mutate(HP = if_else(VSOLARPNL == "Yes", 1, 0))

ggplot(DF_PV, aes(x = factor(V8), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#FF7518", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Year", y = "Cooling Balance Point", title = "(a) Cooling Balance Point") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "PV"))

ggsave("0616_HPCBP.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(DF_PV, aes(x = factor(V8), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#FF7518", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Year", y = "Cooling Slope", title = "(b) Cooling Slope") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "PV"))

ggsave("0616_HPCS.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



# 0617 Energy Burden ------------------------------------------------------

DF <- read_dta('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_HP_0421.dta')

HPDF <- DF %>%  left_join(sorted_energy, by = c("BILACCT_K" = "BILACCT_K", "V8" = "YEAR"))


HPDF <- HPDF %>%
  mutate(Income = case_when(
    VINCOME == "Less than $15,000" ~ 7500,
    VINCOME == "$15,000 to $24,999" ~ 20000,
    VINCOME == "$25,000 to $34,999" ~ 30000,
    VINCOME == "$35,000 to $49,999" ~ 42500,
    VINCOME == "$50,000 to $74,999" ~ 62500,
    VINCOME == "$75,000 to $99,999" ~ 87500,
    VINCOME == "$100,000 to $149,999" ~ 125000,
    VINCOME == "$150,000 or more" ~ 300000,
    TRUE ~ NA_real_  # This handles any cases not specified
  ))

HPDF$EB <- HPDF$Cost/ HPDF$Income

HPDF <- HPDF %>%
  mutate(EB = round((Cost / Income) * 100, 2))


HPDF$VACTYPE[which(HPDF$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
HPDF$VACTYPE[which(HPDF$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
HPDF$VACTYPE[which(HPDF$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'

HPDF$VINCOME[which(HPDF$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
HPDF$VINCOME[which(HPDF$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
HPDF$VINCOME[which(HPDF$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
HPDF$VINCOME[which(HPDF$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
HPDF$VINCOME[which(HPDF$VINCOME == "$150,000 or more")] <- '$150K or more'
HPDF$VINCOME[which(HPDF$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
HPDF$VINCOME[which(HPDF$VINCOME == "Less than $15,000")] <- 'Less than $15K'
HPDF$VINCOME[which(HPDF$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'

HPDF$VACTYPE <- factor(HPDF$VACTYPE,levels = c("Heat pump",
                                           "AC unit packaged with gas heating",
                                           "Separate AC system",
                                           "Don't know"))
HPDF$VINCOME <- factor(HPDF$VINCOME,levels = c('Less than $15K',
                                           '$15K to 25K',
                                           '$25K to 35K',
                                           '$35K to 50K',
                                           '$50K to 75K',
                                           '$75K to 100K',
                                           '$100K to 150K',
                                           '$150K or more'))

HPDF_2019 <- HPDF %>% filter(V8 == 2021)

HPDF_2019_EI <- HPDF_2019 %>% filter(EB >= 6) #126
HPDF_2019_EI <- HPDF_2019_EI %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump

ggplot(HPDF_2019_EI, aes(x = factor(VINCOME), y = EB, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Burden %", title = "(c) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8,0.6),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EB_21.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



ggplot(HPDF_2019_EI, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(a) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICBP2019_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2019_EI, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(b) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICS2019_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



HPDF_2020 <- HPDF %>% filter(V8 == 2020)

HPDF_2020_EI <- HPDF_2020 %>% filter(EB >= 10) #126
HPDF_2020_EI <- HPDF_2020_EI %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump
ggplot(HPDF_2020_EI, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(c) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICBP2020_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2020_EI, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(d) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICS2020_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HPDF_2021 <- HPDF %>% filter(V8 == 2021)

HPDF_2021_EI <- HPDF_2021 %>% filter(EB >= 10) #126
HPDF_2021_EI <- HPDF_2021_EI %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump
ggplot(HPDF_2021_EI, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(e) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICBP2021_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2021_EI, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(f) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EICS2021_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")









HPDF_2019_EP <- HPDF_2019 %>% filter(EB >= 6 &EB <= 10) #295
HPDF_2019_EP <- HPDF_2019_EP %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump
ggplot(HPDF_2019_EP, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(a) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCBP2019_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2019_EP, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(b) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCS2019_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



HPDF_2020_EP <- HPDF_2020 %>% filter(EB >= 6 &EB <= 10) #295

HPDF_2020_EP <- HPDF_2020_EP %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump
ggplot(HPDF_2020_EP, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(c) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCBP2020_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2020_EP, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(d) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCS2020_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



HPDF_2021_EP <- HPDF_2021 %>% filter(EB >= 6 &EB <= 10) #295

HPDF_2021_EI <- HPDF_2021_EI %>% ##79
  mutate(HP = if_else(VACTYPE == "Heat pump", 1, 0)) #did not find heat pump
ggplot(HPDF_2021_EP, aes(x = factor(VINCOME), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Balance Point", title = "(e) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCBP2021_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

ggplot(HPDF_2021_EP, aes(x = factor(VINCOME), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Cooling Slope", title = "(f) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
ggsave( file = "0617_EPCS2021_by_Income.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

EI <- rbind(HPDF_2021_EI,HPDF_2020_EI,HPDF_2019_EI)
EP <- rbind(HPDF_2021_EP,HPDF_2020_EP,HPDF_2019_EP)

write_dta(EI,'/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_EI_0617.dta')
write_dta(EP,'/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_EP_0617.dta')




# __________________PV 

PV <- read_dta('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_PV_0607.dta')

PV <- PV %>%  left_join(sorted_energy, by = c("BILACCT_K" = "BILACCT_K", "V8" = "YEAR"))


PV <- PV %>%
  mutate(Income = case_when(
    VINCOME == "Less than $15,000" ~ 7500,
    VINCOME == "$15,000 to $24,999" ~ 20000,
    VINCOME == "$25,000 to $34,999" ~ 30000,
    VINCOME == "$35,000 to $49,999" ~ 42500,
    VINCOME == "$50,000 to $74,999" ~ 62500,
    VINCOME == "$75,000 to $99,999" ~ 87500,
    VINCOME == "$100,000 to $149,999" ~ 125000,
    VINCOME == "$150,000 or more" ~ 300000,
    TRUE ~ NA_real_  # This handles any cases not specified
  ))

PV$EB <- PV$Cost/ PV$Income

PV <- PV %>%
  mutate(EB = round((Cost / Income) * 100, 2))

DF_PV <- PV %>%
  mutate(HP = if_else(VSOLARPNL == "Yes", 1, 0))
DF_PV <- DF_PV %>% filter(EB <= 10) #295
ggplot(DF_PV, aes(x = factor(V8), y = EB, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#FF7518", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Year", y = "Cooling Balance Point", title = "(a) Cooling Balance Point") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "PV"))

ggsave("0617_PVEB.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


