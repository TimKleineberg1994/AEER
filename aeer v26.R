############ preparation ############

remove(list=ls())
rm()

# needed packages 

packages <- c("plm",
              "MatchIt",
              "phonTools" ,
              "MASS",
              "cobalt", 
              "lfe", 
              "sf", 
              "dplyr", 
              "sandwich", 
              "lmtest", 
              "miceadds",
              "misty", 
              "plyr",
              "spData",
              "foreign",     
              "pastecs",
              "ggplot2",   
              "tidyverse",
              "caret",
              "leaps", 
              "Hmisc", 
              "boot",
              "ggplot2",
              "plotly",
              "lmtest",
              "Rmisc",
              "dplyr",
              "aod", 
              "readxl",
              "lfe",
              "psych", 
              "rgdal", 
              "sp", 
              "raster",
              "geos",
              "magrittr", 
              "rgeos", 
              "stargazer",
              "lubridate") 

# Install all packages, which are not installed yet

lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)

#setwd("C:/Users/Carina/Dokumente/R/aeer/3 Wednesday Diff in Diff")
#setwd("C:/Users/Carina/Google Drive/Studium Arbeit/Studium/3. TU ab WS1920/4 SS21/Applied environmental econometrics in R/3 Paper")

setwd("C:/Users/johan/Desktop/UNI/SS21/AEER")
#setwd("~/Desktop/AEER/Paper")




############ read data ############ 

#todo: hier vollen Abgabe- Code einf?gen aus Abgabe merge code 

# read von Carina
data <- readRDS("data_merged_2005_2018.rds") #complete data
data_regression <- readRDS("data_merged_2005_2018_regression.rds")   # limited dataset with only caap = 1 
polution_2005_2018 <- readRDS("data_2005_2018.rds")

lez<-readRDS("lez_shp.rds")
states <- read_sf("vg2500_bld.shp")
caaps <- read_excel("caaps.xlsx")

weather <- readRDS("weather.rds")
weather <- st_as_sf(weather, coords = c("lon", "lat"), crs = 4326, agr = "constant")
weather_stats = weather %>% distinct(id, name, state, geometry)

# read von david
data <- readRDS("data_wea.rds") 

data_regression <- readRDS("data_merged_2005_2018_regression.rds") 

lez<-readRDS("lez_shp.rds")

polution_2005_2018 <- readRDS("data_2005_2018.rds")

states <- read_sf("vg2500_bld.shp")

caaps <- read_excel("caaps.xlsx")

weather <- readRDS("weather.rds")


############ visualize lez and stations germany wide ############

weather_stats <- weather %>% distinct(id, name, state, lat, lon)
# todo: umordnen, hier haben wir data noch gar nicht kreiert 

stations = polution_2005_2018 %>% distinct(station,  type, area, longitude, latitude, nuts_code, kreis, kreis_type, bl_nr, bl, ags, mun, mun_type, ) #dataset without created variables
#todo: nicht möglich fehler : "Speicher erschöpft"
stations_full = data %>% dplyr::distinct(d_stage1, d_stage2, d_stage3, first_stage, second_stage, third_stage, d_lez_location, station,  type, area, longitude, latitude, nuts_code, kreis, kreis_type, bl_nr, bl, ags, mun, mun_type )




#659 stations in germany 

# "summarize" longitude and latitude to geography column   
stations <- st_as_sf(stations, coords = c("longitude", "latitude"), 
                     crs = 4326, agr = "constant")
polution_2005_2018 <- st_as_sf(polution_2005_2018, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant")
lez <- st_as_sf(lez, coords = geometry, 
                crs = 4326, agr = "constant")

weather <- st_as_sf(weather, coords = c("lon", "lat"), 
                    crs = 4326, agr = "constant")

weather_stats <- st_as_sf(weather_stats, coords = c("lon", "lat"), 
                          crs = 4326, agr = "constant")

# visualization of pollution stations and weather stations 

# create distance matrix of pollution stations and weather stations
# pollution_stations als Spalten: 659 Spalten
# wetter_stations als Zeilen: 694 Zeilen
distance_stats_weather <-  as.data.frame(st_distance(weather_stats, stations))
# create additional columns
distance_stats_weather <- distance_stats_weather %>% mutate(station = NA, weather_station = NA, distance = NA) 
# create two list of weather station names and poluution station names
stations_name <- stations$station
weather_stats_name <- weather_stats$id

# iteration over columns/stations in distance matrix 
# write the minimum value from the respective column in the distance column of distance_stats_weather
# find index of minimum value from the respetive column 
# find the right station with the help of the index and write it in the station column of distance_stats_weather

for (i in 1:length(stations_name)) {
  distance_stats_weather$station[i] <- stations_name[i]
  distance_stats_weather$distance[i] <- min(distance_stats_weather[,i])
  index <- which(distance_stats_weather[,i] == min(distance_stats_weather[,i]))
  distance_stats_weather$weather_station[i] <- weather_stats_name[index]
}

# select needed columns, drop any other columns
distance_stats_weather <- distance_stats_weather %>% dplyr::select(station, weather_station, distance) 
# drop all na rows
distance_stats_weather <- na.omit(distance_stats_weather)

# wir haben 659 pollution_messstationen
unique(distance_stats_weather$station)
# und 313 wetter-messstation
unique(distance_stats_weather$weather_station)

# save stations geometry data in stats_geo dataframe
stats_geo <- stations %>% dplyr::select(station, kreis, bl, mun, geometry)

stat_weat <- inner_join(distance_stats_weather, weather_stats, by = c("weather_station"="id"))
stat_weat <- inner_join(stat_weat, stats_geo, by = c("station"="station"), suffix = c("_wea", "_pol"))

# visualization of the LEZ and the 659 pollution stations (in red) and the associated 313 weather stations (in blue) in Germany 
ggplot() + geom_sf(data = states, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data = stat_weat$geometry_pol, alpha=0.7, size=0.3, color = "red") +
  geom_sf(data = stat_weat$geometry_wea, alpha=0.7, size=0.3, color = "blue") +
  geom_sf(data= lez, color= "aquamarine4", fill ="aquamarine4")

# visualization of stations and lez at the federal state level:
##### 

#only for state Baden-WÃ¼rttemberg for better vision

BW = filter(states, GEN == states$GEN[7]) 
stationsBW =filter(stations, bl == "Baden-Württemberg") 
# 69 stations in BW
lezBW =filter(lez, state == "Baden-Württemberg")
# 30 lez in BW

ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBW, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsBW, alpha=0.7, size=0.5, color = "darkgrey")

#####
# for all other BL as well:
#####

#only for state Bayern for better vision

BY = filter(states, GEN == "Bayern") 
stationsBY =filter(stations, bl == "Freistaat Bayern") 
lezBY =filter(lez, state == "Bayern")

ggplot() + geom_sf(data = BY, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBY, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsBY, alpha=0.7, size=0.5, color = "darkgrey")  


#only for state NRW for better vision

NRW = filter(states, GEN == "Nordrhein-Westfalen") 
stationsNRW =filter(stations, bl == "Nordrhein-Westfalen") 
lezNRW =filter(lez, state == "Nordrhein-Westfalen")

ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsNRW, alpha=0.7, size=0.5, color = "darkgrey")  


#only for state Berlin for better vision

BE = filter(states, GEN == "Berlin") 
stationsBE =filter(stations, bl == "Berlin") 
# 17 stations in Berlin
lezBE =filter(lez, state == "Berlin")
# 1 lez in Berlin

ggplot() + geom_sf(data = BE, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBE, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsBE, alpha=0.7, size=0.5, color = "darkgrey")  


#only for state Brandenburg for better vision

BB = filter(states, GEN == "Brandenburg") 
stationsBB =filter(stations, bl == "Brandenburg") 

lezBB =filter(lez, state == "Brandenburg")
# no lEZ in Brandenburg

ggplot() + geom_sf(data = BB, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBB, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsBB, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Bremen for better vision

HB = filter(states, GEN == "Bremen") 
stationsHB =filter(stations, bl == "Freie Hansestadt Bremen") 
# 10 stations in Bremen
lezHB =filter(lez, state == "Bremen")
# 1 lEZ in Bremen

ggplot() + geom_sf(data = HB, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezHB, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsHB, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Hamburg for better vision

HH = filter(states, GEN == "Hamburg") 
stationsHH =filter(stations, bl == "Freie und Hansestadt Hamburg") 
# 24 stations in Hamburg
lezHH =filter(lez, state == "Hamburg")
# no lEZ in Hamburg

ggplot() + geom_sf(data = HH, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezHH, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsHH, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Hessen for better vision

HE = filter(states, GEN == "Hessen") 
stationsHE =filter(stations, bl == "Hessen") 
lezHE =filter(lez, state == "Hessen")


ggplot() + geom_sf(data = HE, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezHE, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsHE, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Mecklenburg-Vorpommern for better vision

MV = filter(states, GEN == "Mecklenburg-Vorpommern") 
stationsMV =filter(stations, bl == "Mecklenburg-Vorpommern") 
lezMV =filter(lez, state == "Mecklenburg-Vorpommern")
# no lEZ in Mecklenburg-Vorpommern

ggplot() + geom_sf(data = MV, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezMV, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsMV, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Niedersachsen for better vision

NI = filter(states, GEN == "Niedersachsen") 
stationsNI =filter(stations, bl == "Niedersachsen") 

lezNI =filter(lez, state == "Niedersachsen")


ggplot() + geom_sf(data = NI, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNI, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsNI, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Rheinland-Pfalz for better vision

RP = filter(states, GEN == "Rheinland-Pfalz") 
stationsRP =filter(stations, bl == "Rheinland-Pfalz") 
lezRP =filter(lez, state == "Rheinland-Pfalz")
# 1 lEZ in Rheinland-Pfalz

ggplot() + geom_sf(data = RP, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezRP, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsRP, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Saarland for better vision

SL = filter(states, GEN == "Saarland") 
stationsSL =filter(stations, bl == "Saarland") 

lezSL =filter(lez, state == "Saarland")
# no lEZ in Saarland

ggplot() + geom_sf(data = SL, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezSL, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsSL, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Sachsen for better vision

SN = filter(states, GEN == "Sachsen") 
stationsSN =filter(stations, bl == "Freistaat Sachsen") 
lezSN =filter(lez, state == "Sachsen")
# 1 lEZ in Sachsen

ggplot() + geom_sf(data = SN, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezSN, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsSN, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Sachsen-Anhalt for better vision

ST = filter(states, GEN == "Sachsen-Anhalt") 
stationsST =filter(stations, bl == "Sachsen-Anhalt") 

lezST =filter(lez, state == "Sachsen-Anhalt")
# 2 lEZ in Sachsen-Anhalt

ggplot() + geom_sf(data = ST, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezST, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsST, alpha=0.7, size=0.5, color = "darkgrey")


#only for state Schleswig-Holstein for better vision

SH = filter(states, GEN == "Schleswig-Holstein") 
stationsSH =filter(stations, bl == "Schleswig-Holstein") 
# 35 stations Schleswig-Holstein
lezSH =filter(lez, state == "Schleswig-Holstein")


ggplot() + geom_sf(data = SH, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezSH, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsSH, alpha=0.7, size=0.5, color = "darkgrey")


#only for state ThÃ¼ringen for better vision

TH = filter(states, RS == 16) 
stationsTH =filter(stations, bl == "Freistaat Thüringen") 
lezTH =filter(lez, state == "Thüringen")

# 1 lEZ in ThÃ¼ringen

ggplot() + geom_sf(data = TH, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezTH, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsTH, alpha=0.7, size=0.5, color = "darkgrey")

#####
#plot germany wide:
#####

plot_lez <- ggplot() + geom_sf(data = states, color="grey", fill= "white") +  
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())+ 
  geom_sf(data= lez, color= "aquamarine4", fill ="aquamarine4")

plot_lez #see only lez

plot_lez +  geom_sf(data = stations, alpha=0.7, size=0.5, color = "darkgrey") #lez plus stations






# todo code rein der ?berschriften: 

# ############ combine datasets polution panel data and lez information ############ und 

############ clean and prepare dataset ############


# *** ALGORITHMUS fÃ¼r "closest LEZ" und "distance to closest LEZ" ***
# filter alle messstation die auÃŸerhalb einer Umweltzone liegen und speichere sie in einem dataframe mit den spalten: name und geodaten
# berechne die mittelpunkte aller 76 lez 
# erzeuge eine matrix: 76 zeilen x die Ã¼briggebliebenen Messstationen als Spalte

# erzeuge eine neue Spalte "distance": gehe jede Spalte der Matrix durch, und schreibe den minimalen wert in die Spalte distance der jeweiligen Messstation rein 
# erzeuge eine neue Spalte "closest zone": finde den index des minimalen Wertes und benutze ihn um den namen der Umweltzone zu bekommen
# nehme diese umweltzone und speichere ihn in closest zone rein

# filter stations outside lez in year 2008

#todo: andere Jahre müssen noch ausgeführt werden
stat_out_lez_2008 <- data %>% filter(d_lez == 0 & year == 2008) %>%
  dplyr::select(station, geometry) 

# create a subset dataframe of stat_out_lez_2008 with unique stations
# works exactly like distinct but the distinct function was lasting kind of forever...
stat_out_lez_2008 <- stat_out_lez_2008[!duplicated(stat_out_lez_2008$station), ]

#stat_out_lez_2008 <- NULL

# create two new columns in stat_out_lez_2008
stat_out_lez_2008 <- stat_out_lez_2008 %>% mutate(closest_lez = NA, distance_lez = NA) 

# todo: raus -> code von carina fÃ¼r lez 
lez <- st_as_sf(lez, coords = geometry, 
                crs = 4326, agr = "constant")

sf::sf_use_s2(FALSE)  
lez<-st_make_valid(lez)
unique(st_is_valid(lez, reason = TRUE))

# create a distance matrix: 76 x 648 (lez x stations)
distance_matrix_2008 <-  as.data.frame(st_distance(lez, stat_out_lez_2008))

# create two list of lez names and station names
lez_name <- lez$zone
station_name_2008 <- stat_out_lez_2008$station

# iteration over columns/stations in distance matrix 
# write the minimum value from the respective column in the distance_lez column of stat_out_lez_2008
# find index of minimum value from the respetive column 
# find the right lez with the help of the index and write it in the closest_lez column of stat_out_lez_2008
for (i in 1:length(station_name_2008)) {
  stat_out_lez_2008$distance_lez[i] <- min(distance_matrix_2008[,i])
  index <- which(distance_matrix_2008[,i] == min(distance_matrix_2008[,i]))
  stat_out_lez_2008$closest_lez[i] <- lez_name[index]
}

View(stat_out_lez_2008)



# filter stations outside lez in year 2018

stat_out_lez_2018 <- data %>% filter(d_lez == 0 & year == 2018) %>%
  dplyr::select(station, geometry) 

# create a subset dataframe of stat_out_lez_2018 with unique stations
# works exactly like distinct but the distinct function was lasting kind of forever...
stat_out_lez_2018 <- stat_out_lez_2018[!duplicated(stat_out_lez_2018$station), ]

#stat_out_lez_2018 <- NULL

# create two new columns in stat_out_lez_2018
stat_out_lez_2018 <- stat_out_lez_2018 %>% mutate(closest_lez = NA, distance_lez = NA) 

# todo: raus -> code von carina fÃ¼r lez 
lez <- st_as_sf(lez, coords = geometry, 
                crs = 4326, agr = "constant")

sf::sf_use_s2(FALSE)  
lez<-st_make_valid(lez)
unique(st_is_valid(lez, reason = TRUE))

# create a distance matrix: 76 x 648 (lez x stations)
distance_matrix_2018 <-  as.data.frame(st_distance(lez, stat_out_lez_2018))

# create two list of lez names and station names
lez_name <- lez$zone
station_name_2018 <- stat_out_lez_2018$station

# iteration over columns/stations in distance matrix 
# write the minimum value from the respective column in the distance_lez column of stat_out_lez_2018
# find index of minimum value from the respetive column 
# find the right lez with the help of the index and write it in the closest_lez column of stat_out_lez_2018
for (i in 1:length(station_name_2018)) {
  stat_out_lez_2018$distance_lez[i] <- min(distance_matrix_2018[,i])
  index <- which(distance_matrix_2018[,i] == min(distance_matrix_2018[,i]))
  stat_out_lez_2018$closest_lez[i] <- lez_name[index]
}

View(stat_out_lez_2018)

unique(stat_out_lez_2018$station)

############ explore dataset ############

# get an overview when the stages were implemented: 

summary(data$first_stage) # we see the first stage was earliest introduced in 01.01.2008 (latest 31.12.2018)
# todo: distribution der EInf?hrungen? 
#hist(data$first_stage)
summary(data$second_stage) # we see the first stage was earliest introduced in 01.01.2009 (latest 31.12.2018)
summary(data$third_stage) # we see the first stage was earliest introduced in 01.01.2010 (latest 31.12.2018)
summary(data$date) #todo: weg: the first observation is from 01.01.2015 (latest 31.12.2018)-> 
# -> we can not make conclusions about the effect of the introduction of a zone and its stage when it happend before 2015

#todo: senkrechte linie einf?gen die zeigt, ab wo wir die observtionen haben (2015-18)

count(data$d_stage1)
count(data$d_stage2)
count(data$d_stage3)
class(data$first_stage)

hist(lez$first_stage, breaks= "weeks",freq= TRUE, xlab = "", ylab= "Frequency", main="Introductions of LEZ Stage 1 (per week)")
# most of the introdctions happend before 2015 
hist(lez$second_stage, breaks= "weeks",freq= TRUE, xlab = "", ylab= "Frequency", main="Introductions of LEZ Stage 2 (per week)")
hist(lez$third_stage, breaks= "weeks",freq= TRUE, xlab = "", ylab= "Frequency", main="Introductions of LEZ Stage 3 (per week)")


count(data$lez_name) #see appearance of Zones/ no Zone 
stations_merged<- st_join(stations, lez,join = st_intersect, largest=TRUE) #merges the unique stations with belonging lez 
#create a dummy variable if a station is inside a lez, independend of the time of introduction 
stations_merged <- stations_merged %>% mutate(d_lez_location = ifelse(is.na(zone) ==TRUE,0,1) )


#Which stations are within the LEZ
addmargins(table(stations_merged$type, stations_merged$d_lez_location, useNA = "ifany"))
addmargins(table(stations_merged$area, stations_merged$d_lez_location, useNA = "ifany"))
addmargins(table(stations_merged$type, stations_merged$area, useNA = "ifany"))



### list all stations within LEZ (NRW, to observe/ visualize better)
stations_merged %>% filter(d_lez_location==1)
stationsNRW =filter(stations_merged, bl == "Nordrhein-Westfalen") 


#todo: legende genererell visualisierung besser machen ? jitter? 

#NRW state only
#plot by belonging to a lez or not 
ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(),  
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank())  +
  geom_sf(data = stationsNRW, aes(color = as.factor(d_lez_location)), size = 1.5)+ 
  geom_sf(data = lezNRW, color= "aquamarine3", fill ="aquamarine3", alpha= 0.7) 

#complete germany
ggplot() + geom_sf(data = states, color="grey", fill= "white") +
  theme(panel.background = element_blank(),  
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank())  +
  geom_sf(data = stations_merged, aes(color = as.factor(d_lez_location)), size = 1)+ 
  geom_sf(data = lez, color= "grey", fill ="grey", alpha= 0.5)







############ Descriptive analysis ############  


##### 
###### 1. with the full data set "data" ######
##### 

#polutations of interest: no, no2, pm10, ozon todo: richtige pollutants?


# first, have a look at the time trends of the  levels

#todo: the following is reaaaaaally genau das was nicole gemacht hat, abwandlungsidee: vielleicht zwei graphen ploten, einmal in umweltzone, einmal nicht in lez 
# einfach einiges rauslassen

#### create dummy variables for fixed effects (new year,....)

data <- data %>% mutate(d_NY = ifelse(date == "2005-01-01", 1,0) 
                        | ifelse(date == "2005-12-31", 1,0)
                        | ifelse(date == "2006-01-01", 1,0)
                        | ifelse(date == "2006-12-31", 1,0)
                        | ifelse(date == "2007-01-01", 1,0)
                        | ifelse(date == "2007-12-31", 1,0)
                        | ifelse(date == "2008-01-01", 1,0)
                        | ifelse(date == "2008-12-31", 1,0)
                        | ifelse(date == "2009-01-01", 1,0)
                        | ifelse(date == "2009-12-31", 1,0)
                        | ifelse(date == "2010-01-01", 1,0)
                        | ifelse(date == "2010-12-31", 1,0)
                        | ifelse(date == "2011-01-01", 1,0)
                        | ifelse(date == "2011-12-31", 1,0)
                        | ifelse(date == "2012-01-01", 1,0)
                        | ifelse(date == "2012-12-31", 1,0)
                        | ifelse(date == "2013-01-01", 1,0)
                        | ifelse(date == "2013-12-31", 1,0)
                        | ifelse(date == "2014-01-01", 1,0)
                        | ifelse(date == "2014-12-31", 1,0)
                        | ifelse(date == "2015-01-01", 1,0)
                        | ifelse(date == "2015-12-31", 1,0)
                        | ifelse(date == "2016-01-01", 1,0)
                        | ifelse(date == "2016-12-31", 1,0)
                        | ifelse(date == "2017-01-01", 1,0)
                        | ifelse(date == "2017-12-31", 1,0)
                        | ifelse(date == "2018-01-01", 1,0)
                        | ifelse(date == "2018-12-31", 1,0))
#                         
data$dweek <- ifelse(data$weekday  %in% c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag"), 1, 0)

data$djanuar <- ifelse(data$month  %in% c("Januar"), 1, 0)
data$dfebruar <- ifelse(data$month  %in% c("Februar"), 1, 0)
data$djmaerz <- ifelse(data$month  %in% c("März"), 1, 0)
data$dapril <- ifelse(data$month  %in% c("April"), 1, 0)
data$dmai <- ifelse(data$month  %in% c("Mai"), 1, 0)
data$djuni <- ifelse(data$month  %in% c("Juni"), 1, 0)
data$djuli <- ifelse(data$month  %in% c("Juli"), 1, 0)
data$daugust <- ifelse(data$month  %in% c("August"), 1, 0)
data$dseptember <- ifelse(data$month  %in% c("September"), 1, 0)
data$doktober <- ifelse(data$month  %in% c("oktober"), 1, 0)
data$dnovember <- ifelse(data$month  %in% c("November"), 1, 0)
data$ddezember <- ifelse(data$month  %in% c("Dezember"), 1, 0)


data$d2005 <- ifelse(data$year == 2005, 1, 0)
data$d2006 <- ifelse(data$year == 2006, 1, 0)
data$d2007 <- ifelse(data$year == 2007, 1, 0)
data$d2008 <- ifelse(data$year == 2008, 1, 0)
data$d2009 <- ifelse(data$year == 2009, 1, 0)
data$d2010 <- ifelse(data$year == 2010, 1, 0)
data$d2011 <- ifelse(data$year == 2011, 1, 0)
data$d2012 <- ifelse(data$year == 2012, 1, 0)
data$d2013 <- ifelse(data$year == 2013, 1, 0)
data$d2014 <- ifelse(data$year == 2014, 1, 0)
data$d2015 <- ifelse(data$year == 2015, 1, 0)
data$d2016 <- ifelse(data$year == 2016, 1, 0)
data$d2017 <- ifelse(data$year == 2017, 1, 0)
data$d2018 <- ifelse(data$year == 2018, 1, 0)

##### create weather dummys according malina and fischer

data$dnorain <- ifelse(data$precipitation == 0, 1, 0)
data$dcoldday <- ifelse(data$temperature < 0, 1, 0)
data$dlowwind <- ifelse(data$wind_speed < 3.4, 1, 0)

#weather interactions

data$norainAPM <- data$dnorain*data$atm_pressure
data$colddaynorain <- data$dnorain*data$dcoldday
data$colddayAPM <- data$dcoldday*data$atm_pressure
data$lowwindAPM <- data$dlowwind*data$atm_pressure
data$TKMUPM <- data$temperature*data$humidity
data$TMKsun <- data$temperature*data$sunshine
data$TXK_TMK <- data$temperature_max-data$temperature_min

#... by year 
year <- data %>% group_by(year) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE) #gives the mean of polutants pm10, no2, no, o3 over all years available in the dataset
str(year)
year <- tidyr::gather(year, contaminant, value, so2:o3)
ggplot() + 
  geom_line(data = year, aes(y = value, x = year, group = contaminant), lwd = 1) +
  geom_point(data = year, aes(y = value, x = year, group = contaminant), size = 2) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank())
#plot  levels over the years 


#... by month 
month <- data %>% group_by(month) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE)#gives the mean of polutants pm10, no2, no, o3 over all months available in the dataset
str(month)
month <- tidyr::gather(month, contaminant, value, so2:o3)
month$month <- factor(month$month, levels = c("Januar", "Februar", "MÃ¤rz", "April", "Mai", "Juni", "Juli", "August", "September",
                                              "Oktober", "November", "Dezember"))
ggplot() +
  geom_line(data = month, aes(y = value, x = month, group = contaminant), lwd = 1) +
  geom_point(data = month, aes(y = value, x = month, group = contaminant), size = 2) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), axis.text.x = element_text(angle = 90))
#analog to year 


#by Weekdays
weekday <- data %>% group_by(weekday) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE)
weekday <- tidyr::gather(weekday, contaminant, value, so2:o3)
weekday$weekday <- factor(weekday$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
ggplot() +
  geom_line(data = weekday, aes(y = value, x = weekday, group = contaminant), lwd = 1) +
  geom_point(data = weekday, aes(y = value, x = weekday, group = contaminant), size = 2) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), axis.text.x = element_text(angle = 90))
#analog to year 




# now we look at the time trend grouped by station types
year <- data %>% group_by(year, type) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE)
year <- tidyr::gather(year, contaminant, value, so2:o3)
ggplot_pol_type <-ggplot() + 
  geom_line(data = year, aes(y = value, x = year, group = type, color = type), lwd = 1) +
  geom_point(data = year, aes(y = value, x = year, group = contaminant), size = 2) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank())

ggplot_pol_type

##### 
##### lez effect
##### 

# now we look at the time trend by LEZ belonging or not, only inside, when the zone was actually introduced 


year <- data %>% group_by(year, d_lez) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE) # takes a little time 
year <- tidyr::gather(year, contaminant, value, so2:o3)
year <- mutate(year, d_lez = ifelse(d_lez == 1, "Inside LEZ", "Outside LEZ"))

mean(data$first_stage,na.rm=T) #2010
mean(data$second_stage,na.rm=T) #2012
mean(data$third_stage,na.rm=T) # 2013

#the vertical lines are the mean introduction year of stages 1-3

ggplot_pol_lez_1data <- ggplot() +
  geom_line(data = year, aes(y = value, x = year, group = d_lez, color = d_lez), lwd = 1) +
  geom_point(data = year, aes(y = value, x = year, group = contaminant), size = 2) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank())+
  labs(title = "Pol levels, yearly (all cities)")
ggplot_pol_lez_1data

# we see no clear trend for all polutants, that the belonging t oa group reduces the level of polutants 
# explanation: the true effect could be shaded by the differences between lez and outside lez stations, 
# for example lez are in areas where the traffic is normally higher anyway like urban areas
# todo: ist das so richtig, dass hier die ohne lez bessere werte haben? , falsch herum in den mutate funktionen? , sieht bei nicole genauso aus 

# same, but now with 95% confidence intervals
year <- data %>% group_by(year, d_lez) %>% summarise(mean = mean(pm10, na.rm = T),
                                                       sd = sd(pm10, na.rm = T),
                                                       nobs = n(),
                                                       se = sd/sqrt(nobs))
ggplot_pm10_lez_95_1data <-ggplot() + 
  geom_line(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), lwd = 1) +
  geom_point(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), size = 2) +
  geom_errorbar(data = year, aes(x = year, ymin = mean - 1.96*se, ymax = mean + 1.96*se, color = as.factor(d_lez)), width=0.2) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013)+
  theme_minimal() +
  labs(title = "PM10 levels, yearly (all cities)") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  ylim(0, 35) +
  xlim(2005, 2018)
# we see values start only in 2008, because there the first lez were inrtoduced
ggplot_pm10_lez_95_1data 


#the same for monthly averages
year_month = data %>% mutate(year_month = floor_date(date, unit = "month")) %>%
  group_by(year_month, d_lez) %>% 
  summarise(mean = mean(pm10, na.rm = T))

ggplot_pm10_lez_yearmon_1data <- ggplot() + 
  geom_line(data = year_month, aes(y = mean, x = year_month, color = as.factor(d_lez)), lwd = 0.8)  +
  scale_x_date(date_labels = "%b %y", breaks = "year")  +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs(title = "PM10 levels, monthly (all cities)")+
  ylim(13, 50) 

ggplot_pm10_lez_yearmon_1data  #todo: noch linien für einführung 

# todo: besser ausformlieren summary: we could not observe the expected trend 


##### 
##### 
###### 2. with the limited data set "data_regression"
##### 

#remember: we changed the controll group to make them more comparable 

year <- data_regression %>% group_by(year, d_lez) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE) # takes a little time 
year <- tidyr::gather(year, contaminant, value, so2:o3)
year <- mutate(year, d_lez = ifelse(d_lez == 1, "Inside LEZ", "Outside LEZ"))


mean(data_regression$first_stage,na.rm=T) #2010
mean(data_regression$second_stage,na.rm=T) #2012
mean(data_regression$third_stage,na.rm=T) # 2013

#the vertical lines are the mean introduction year of stages 1-3
ggplot_pol_lez_2reg <- ggplot() +
  geom_line(data = year, aes(y = value, x = year, group = d_lez, color = d_lez), lwd = 1) +
  geom_point(data = year, aes(y = value, x = year, group = contaminant), size = 2) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank())+
  labs(title = "Pol levels, monthly (caap cities)")

ggplot_pol_lez_2reg
ggplot_pol_lez_1data

# we still see not the expected result, but the difference got smaller compared to the full dataset
# todo: auch problematisch weil skalen nicht alle einzeln eingestellt sind



# same, but now with 95% confidence intervals
year <- data_regression %>% group_by(year, d_lez) %>% summarise(mean = mean(pm10, na.rm = T),
                                                     sd = sd(pm10, na.rm = T),
                                                     nobs = n(),
                                                     se = sd/sqrt(nobs))
ggplot_pm10_lez_95_2reg <- ggplot() + 
  geom_line(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), lwd = 1) +
  geom_point(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), size = 2) +
  geom_errorbar(data = year, aes(x = year, ymin = mean - 1.96*se, ymax = mean + 1.96*se, color = as.factor(d_lez)), width=0.2) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013)+
  theme_minimal() +
  labs(title = "PM10 levels, yearly (caap cities)") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  ylim(0, 35) +
  xlim(2005, 2018)
ggplot_pm10_lez_95_2reg
ggplot_pm10_lez_1data

# we see values start only in 2008, because there the first lez were inrtoduced
# wee see the lez 1 line does not change, it is because no zones / observatiosn were deleted from data to data regression,
# only ones outside lez 
# in the comparision of both plots we see that only the "not - lez" line changes, this means the smaller dataset did not exclude observations from lez=1 stations,
# proof: 

view(filter(data, d_caap== 0 & d_lez==1)) # no observations found, means: treatment group stays the same, only controll changes 



#the same for monthly averages
year_month = data_regression %>% mutate(year_month = floor_date(date, unit = "month")) %>%
  group_by(year_month, d_lez) %>% 
  summarise(mean = mean(pm10, na.rm = T))

ggplot_pm10_lez_yearmon_2reg <-  ggplot() + 
  geom_line(data = year_month, aes(y = mean, x = year_month, color = as.factor(d_lez)), lwd = 0.8)  +
  scale_x_date(date_labels = "%b %y", breaks = "year")  +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs(title = "PM10 levels, yearly (caap cities)")+
  ylim(13, 50) 

ggplot_pm10_lez_yearmon_2reg
ggplot_pm10_lez_yearmon_1data




###### 3. with data set with group of lez introduced at the same time
##### 

# here we also need to pay attention to the introduction date of the specific zone, 
# to see wheather a difference was observed after introduction, so we need to search for a suitable zone 
# binary variable is allready created in d_stage 1/2/3 is 1 when the observation was made when the station is in the lez introduction 1 


# search a suitable zone: needs to include some stations, and some outside within small distance, 
# it is also okay if the zone is actually several zones, when they introduced the zone within within the same time 


##### set up ##### 

# observe the different zones in BW state:

ggplot() + geom_sf(data = BW , color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBW, aes(fill=zone)) +
  geom_sf(data = stationsBW, alpha=0.7, size=0.5, color = "darkgrey")

view(lezBW)

for (i in 1:30) {
  print(ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
          theme(panel.background = element_blank(), 
                axis.title = element_blank(), 
                axis.text = element_blank(), 
                axis.ticks = element_blank()) + 
          geom_sf(data= lezBW[i,1], aes(fill=zone)) +
          geom_sf(data = stationsBW, alpha=0.7, size=0.5, color = "darkgrey"))
}


# add here wheather they belong to a zone or not 

ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBW, color= "aquamarine4",fill= "aquamarine4", alpha = 0.5) +
  geom_sf(data = stationsBW, size=1)

# visualize when the zones were introducted
ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezBW, aes(fill=first_stage) , alpha = 0.5) +
  geom_sf(data = stationsBW, size=1)



view(lez[order(lez$first_stage),])


# search stations that have the same introduction dates as heilbronn
did_set_Heilbronn <- filter(lez, zone=="Umweltzone Heilbronn")
did_set_Frankfurt <- filter(lez, zone=="Umweltzone Frankfurt a.M.")


heilbronn_first <-did_set_Heilbronn$first_stage
heilbronn_second <-did_set_Heilbronn$second_stage
heilbronn_third <-did_set_Heilbronn$third_stage

frankfurt_first <-did_set_Frankfurt$first_stage
frankfurt_second <-did_set_Frankfurt$second_stage
frankfurt_third <-did_set_Frankfurt$third_stage


did_set_Heilbronn <- filter(lez, first_stage == heilbronn_first)
did_set_Heilbronn <- filter(did_set_Heilbronn, second_stage == heilbronn_second)
did_set_Heilbronn <- filter(did_set_Heilbronn, third_stage ==heilbronn_third)

did_set_Frankfurt <- filter(lez, first_stage == frankfurt_first)
did_set_Frankfurt <- filter(did_set_Frankfurt, second_stage == frankfurt_second)
did_set_Frankfurt <- filter(did_set_Frankfurt, third_stage == frankfurt_third)
view(did_set_Heilbronn) # we see a set of 6 lez that have the same introduction times 
unique(did_set_Heilbronn$state) #only bw stations


# cut dataset accordingly to this, choose only those observations that are of the "group heilbronn" or are a caap and all are from bw 


#controll <-filter(data, d_caap==1 & d_lez_location == 0 & state =="Baden-Württemberg") # according to the paper todo: which paper? we choose the cities with a caap but no lez as controll group
controll <-filter(data, d_caap==1 & d_lez_location == 0 ) # control group whole germany
treatment_BW <- filter(data, mun == "Heilbronn"| mun =="Karlsruhe" | mun=="Ulm"| mun =="Pforzheim" | mun == "Muehlacker" | mun == "Herrenberg") # gibt es auch messstationen in "Heilbronn" die nicht im LEZ liegen? -> weiter filtern nach d_lez_location = 1
treatment_BW <- filter(treatment_BW, d_lez_location == 1 ) # macht das überhaupt sinn? -> wir brauchen ja eigentlich auch zeitpunkte vor der einführung der lez
treatment_FFM_M <- filter(data, mun == "Frankfurt am Main"| mun =="München")

# merge controll and treatmentgroups:

group_heilbronn <- rbind(controll, treatment_BW)
group_frankfurt <- rbind(controll, treatment_FFM_M)
group_germany <- rbind(controll, treatment_GER)

karlsruhe <- filter(lezBW, zone =="Umweltzone Karlsruhe")
ulm <- filter(lezBW, zone =="Umweltzone Ulm")
pforzheim <- filter(lezBW, zone =="Umweltzone Pforzheim")
heilbronn <- filter(lezBW, zone =="Umweltzone Heilbronn")
muehlacker <- filter(lezBW, zone =="Umweltzone Muehlacker")
herrenberg <-  filter(lezBW, zone =="Umweltzone Herrenberg")

#muehlacker and herrenberg do not contain any stations, so our data also does not contain them
# proof visualize again:

ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= karlsruhe, color= "blue", fill= "blue" , alpha = 0.5)+ 
  geom_sf(data= ulm, color= "blue", fill= "blue" , alpha = 0.5)+ 
  geom_sf(data= pforzheim, color= "blue", fill= "blue" , alpha = 0.5)+ 
  geom_sf(data= muehlacker, color= "red", fill= "red" , alpha = 0.5)+ 
  geom_sf(data= herrenberg, color= "red", fill= "red" , alpha = 0.5)+   geom_sf(data = stationsBW, size=1)

ggplot() + geom_sf(data = BW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= karlsruhe, color= "aquamarine4", fill= "aquamarine4" , alpha = 0.5)+ 
  geom_sf(data= ulm, color= "aquamarine4", fill= "aquamarine4" , alpha = 0.5)+ 
  geom_sf(data= pforzheim, color= "aquamarine4", fill= "aquamarine4" , alpha = 0.5)+ 
  geom_sf(data= muehlacker, color= "aquamarine4", fill= "aquamarine4" , alpha = 0.5)+ 
  geom_sf(data= herrenberg, color= "aquamarine4", fill= "aquamarine4" , alpha = 0.5)+   
  geom_sf(data = controll, size=0.5, color= "grey")+   
  geom_sf(data = treatment, size=0.5, color= "black")

#which stations are inside the set: 
unique(group_heilbronn$station) #8 stations, 4 of them are the treatment

##### 
##### describtive analysis  ##### 
# now we look at the time trend by LEZ belonging or not, only inside, when the zone was actually introduced 


year <- group_heilbronn %>% group_by(year, d_lez) %>% summarise_at(vars(so2:o3), mean, na.rm = TRUE) # takes a little time 
year <- tidyr::gather(year, contaminant, value, so2:o3)
year <- mutate(year, d_lez = ifelse(d_lez == 1, "Inside LEZ", "Outside LEZ"))

mean(group_heilbronn$first_stage,na.rm=T) #2009
mean(group_heilbronn$second_stage,na.rm=T) #2012
mean(group_heilbronn$third_stage,na.rm=T) # 2013

#the vertical lines are the mean introduction year of stages 1-3

ggplot_pol_lez_3groupheilbronn <- ggplot() +
  geom_line(data = year, aes(y = value, x = year, group = d_lez, color = d_lez), lwd = 1) +
  geom_point(data = year, aes(y = value, x = year, group = contaminant), size = 2) +
  geom_vline(xintercept = 2009) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013) +
  facet_wrap(~contaminant, scales = "free") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank())
ggplot_pol_lez_3groupheilbronn
ggplot_pol_lez_1data
ggplot_pol_lez_2reg #todo: wieso so strange?

# only for pm10
year <- group_heilbronn %>% group_by(year, d_lez) %>% summarise(mean = mean(pm10, na.rm = T),
                                                                sd = sd(pm10, na.rm = T),
                                                                nobs = n(),
                                                                se = sd/sqrt(nobs))
ggplot_pm10_lez_3groupheilbronn <-ggplot() + 
  geom_line(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), lwd = 1) +
  geom_point(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), size = 2) +
  geom_vline(xintercept = 2009) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013)+
  theme_minimal() +
  labs(title = "PM10 levels, yearly (heilbronn group vs. caap)") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  ylim(0, 35) +
  xlim(2005, 2018)
# we see values start only in 2008, because there the first lez were inrtoduced
ggplot_pm10_lez_3groupheilbronn
ggplot_pm10_lez_1data
ggplot_pm10_lez_2reg



#the same for monthly averages
year_month = group_heilbronn %>% mutate(year_month = floor_date(date, unit = "month")) %>%
  group_by(year_month, d_lez) %>% 
  summarise(mean = mean(pm10, na.rm = T))


ggplot_pm10_lez_yearmon_3groupheilbronn <- ggplot() + 
  geom_line(data = year_month, aes(y = mean, x = year_month, color = as.factor(d_lez)), lwd = 0.8)  +
  scale_x_date(date_labels = "%b %y", breaks = "year")  +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs(title = "PM10 levels, monthly (heilbronn group vs. caap)")+
  ylim(13, 50) 

ggplot_pm10_lez_yearmon_3groupheilbronn
ggplot_pm10_lez_yearmon_2reg
ggplot_pm10_lez_yearmon_1data

# todo: besser ausformlieren summary: we could not observe the expected trend 


# same, but now with 95% confidence intervals
year <- group_heilbronn %>% group_by(year, d_lez) %>% summarise(mean = mean(pm10, na.rm = T),
                                                                sd = sd(pm10, na.rm = T),
                                                                nobs = n(),
                                                                se = sd/sqrt(nobs))
ggplot_pm10_lez_95_3groupheilbronn <- ggplot() + 
  geom_line(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), lwd = 1) +
  geom_point(data = year, aes(y = mean, x = year, color = as.factor(d_lez)), size = 2) +
  geom_errorbar(data = year, aes(x = year, ymin = mean - 1.96*se, ymax = mean + 1.96*se, color = as.factor(d_lez)), width=0.2) +
  geom_vline(xintercept = 2009) +
  geom_vline(xintercept = 2012) +
  geom_vline(xintercept = 2013)+
  theme_minimal() +
  labs(title = "PM10 levels, yearly 95% CI (caap cities)") +
  theme(panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
  ylim(0, 35) +
  xlim(2005, 2018)
ggplot_pm10_lez_95_3groupheilbronn
ggplot_pm10_lez_95_2reg
ggplot_pm10_lez_95_1data
#heilbronn group has a greater spread of values inside the 95 ci



#####
############ Regression analysis ############  
#####

###### simple binary linear regression ####

# 1 binary linear regression over germany for all cities

lm_bin_1germ <- lm(pm10~ d_lez, data= data)
summary(lm_bin_1germ) #indicates a significant relation between beeing located in a lez and the pm10 level 
#we would assume that pm10 level decreases with lez introduction, since traffic is causing pm10 levels to rise,
#the true effect could be covered, maybe lez are mainly introduced in urban areas, where pm10 is higher anyway 

mean(data$pm10[data$d_lez == 1], na.rm= TRUE)-
  mean(data$pm10[data$d_lez == 0], na.rm= TRUE) #this is the naive mean comparison, it is the same as the regressor from the simple linear regression


#2 binary linear regression over germany for only caap cities

lm_bin_germ_2reg <- lm(pm10~ d_lez, data= data_regression)
summary(lm_bin_germ_2reg)
# our regression presents a significant negative effect of having a lez # todo: besser erklären 
#todo: widerspricht die regrssion nicht den plots?:S keine ahnung 

mean(data_regression$pm10[data_regression$d_lez == 1], na.rm= TRUE)-
  mean(data_regression$pm10[data_regression$d_lez == 0], na.rm= TRUE)
#same as regression


#3 binary linear regression for heilbronn group data 

lm_bin_germ_3heilbronn <- lm(pm10~ d_lez, data= group_heilbronn)
summary(lm_bin_germ_3heilbronn)
# our regression presents a significant negative effect of having a lez # todo: besser erklären 
#todo: widerspricht die regrssion nicht den plots?:S keine ahnung 
# hier nur noch bw im group data set, vorher coeffizient -2,... mit allen controll 
# nur noch ** significant (mit filter auf controll, d_lez_location =0, also nie lez)
# nur noch 90% confident wenn d_lez_location


mean(group_heilbronn$pm10[group_heilbronn$d_lez == 1], na.rm= TRUE)-
  mean(group_heilbronn$pm10[group_heilbronn$d_lez == 0], na.rm= TRUE)


coef(lm_bin_germ_3heilbronn);coef(lm_bin_germ_2reg);coef(lm_bin_1germ)
# wee see the effect of lez intrpduction on pm10 level gets from positive to negative


#4 binary linear regression for heilbronn group data only BW 
#todo: irgendwie failt das hier immer, keine ahnung wieso:
group_heilbronn_BW <- filter(group_heilbronn, bl.y == "	Baden-Württemberg")

lm_bin_germ_4heilbronnBW <- lm(pm10~ d_lez, data= group_heilbronn_BW)
summary(lm_bin_germ_4heilbronnBW)


##### 
#####
###### Difference in Difference Analysis on the Group of LEZ #####
#####
heilbronn_first
group_heilbronn = group_heilbronn %>% group_by(station) %>% mutate(stage1_group = max(d_stage1_10, na.rm=T),
                                                                   post09 = ifelse(date>=heilbronn_first, 1, 0),
                                                                   stage2_group = max(d_stage2_10, na.rm=T),
                                                                   post12 = ifelse(date>=heilbronn_second, 1, 0),
                                                                   stage3_group = max(d_stage3_10, na.rm=T),
                                                                   post13 = ifelse(date>=heilbronn_third, 1, 0)) %>% ungroup() # 6 new variables 

group_frankfurt = group_frankfurt %>% group_by(station) %>% mutate(stage1_group = max(d_stage1_10, na.rm=T),
                                                                   post09 = ifelse(date>=frankfurt_first, 1, 0),
                                                                   stage2_group = max(d_stage2_10, na.rm=T),
                                                                   post12 = ifelse(date>=frankfurt_second, 1, 0),
                                                                   stage3_group = max(d_stage3_10, na.rm=T),
                                                                   post13 = ifelse(date>=frankfurt_third, 1, 0)) %>% ungroup() # 6 new variables 

#### erster versuch DID mit felm

results_group_heilbronn <- felm( pm10 ~ d_stage1_10 + d_stage2_10 + d_stage3_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                  d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 +
                                  wind_speed + precipitation + sunshine + snow + vapor_pressure + atm_pressure + temperature + humidity +
                                  dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK| 0 | 0, group_heilbronn)

results_group_heilbronn_1 <- felm( pm10 ~ d_stage1_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 +
                                     wind_speed + precipitation + sunshine + snow + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK | 0 | 0, group_heilbronn)
summary(results_group_heilbronn_1)

results_group_heilbronn_2 <- felm( pm10 ~ d_stage2_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                 d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 +
                                   wind_speed + precipitation + sunshine + snow + vapor_pressure + atm_pressure + temperature + humidity +
                                   dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK | 0 | 0, group_heilbronn)

summary(results_group_heilbronn_2)

results_group_heilbronn_3 <- felm( pm10 ~ d_stage3_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 + 
                                     wind_speed + precipitation + sunshine + snow + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK | 0 | 0, group_heilbronn)

summary(results_group_heilbronn_3)

results_group_frankfurt_1 <- felm( pm10 ~ d_stage1_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_1)
results_group_frankfurt_2 <- felm( pm10 ~  d_stage2_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_2)
results_group_frankfurt_3 <- felm( pm10 ~  d_stage3_10 + d_NY + dweek | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_3)
##### 
##### stage 1
#####

##### zweiter versuch DID mit felm

# snow macht probleme...

results_group_heilbronn_1 <- felm( pm10 ~ d_stage1_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_heilbronn)
summary(results_group_heilbronn_1)
results_group_heilbronn_2 <- felm( pm10 ~ d_stage2_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_heilbronn)
summary(results_group_heilbronn_2)
results_group_heilbronn_3 <- felm( pm10 ~ d_stage3_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_heilbronn)
summary(results_group_heilbronn_3)
results_group_heilbronn <- felm( pm10 ~ d_stage1_10 + d_stage2_10 + d_stage3_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   | djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_heilbronn)
summary(results_group_heilbronn)
# mit fixed effects stations negative werte für alle stations
results_group_germany <- felm( o3 ~ d_lez + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_germany)
summary(results_group_germany)
# frankfurt
# aktueller Top-Kandidat, Ergebnisse stimmen mit erwartetter hypothese überein
# alle schadstoffe negativ korreliert, co und so2(placebo) positiv

results_group_frankfurt_1 <- felm( o3 ~ d_stage1_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_1)

results_group_frankfurt_2 <- felm( o3 ~ d_stage2_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_2)
results_group_frankfurt_3 <- felm( o3 ~ d_stage3_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                     dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                   |station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                     d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt_3)
results_group_frankfurt <- felm( pm10 ~ d_stage1_10 + d_stage2_10 + d_stage3_10 + d_NY + dweek + wind_speed + precipitation + sunshine + vapor_pressure + atm_pressure + temperature + humidity +
                                   dnorain + dcoldday + dlowwind + norainAPM + colddaynorain + colddayAPM + lowwindAPM + TKMUPM + TMKsun + TXK_TMK
                                 | station + djanuar + dfebruar + djmaerz + dapril + dmai + djuni + djuli + daugust + dseptember + doktober + dnovember + ddezember + 
                                   d2005 + d2006 + d2007 + d2008 + d2009 + d2010 + d2011 + d2012 + d2013 + d2014 + d2015 + d2016 + d2017 + d2018 | 0 | 0, group_frankfurt)
summary(results_group_frankfurt)


# look only at data right before and after the introduction of stage 1, so 2008 and 2009 (introduction was first of january)
# find the average daily PM10 level for the complete pre- and post-treatment time 
# plus naive mean comparison 
 

group_heilbronn_stage1 <- group_heilbronn %>% filter(year == 2008 & month == "Dezember"| year== 2009 & month=="Januar")  # selects only dezember 2008 and janaury 2009 , because we are interested only in the effect from the introduction
group_heilbronn_0809<- group_heilbronn %>% filter(year == 2008 | year== 2009)  

year_month = group_heilbronn_0809 %>% mutate(year_month = floor_date(date, unit = "month")) %>%
  group_by(year_month, stage1_group) %>% 
  summarise(mean = mean(pm10, na.rm = T))


ggplot_pm10_lez_yearmon_3groupheilbronn_0809 <- ggplot() + 
  geom_line(data = year_month, aes(y = mean, x = year_month, color = as.factor(stage1_group)), lwd = 0.8)  +
  scale_x_date(date_labels = "%b %y", breaks = "year")  +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs(title = "PM10 levels, monthly (heilbronn group vs. caap)")+
  ylim(13, 50) 
ggplot_pm10_lez_yearmon_3groupheilbronn_0809 #problem: 1.1. is highly poluted allways ad everywhere


# todo: kein plan wieso das nicht geht 

means <- group_heilbronn %>% group_by(stage1_group, post09) %>% 
  summarize(mean = mean(pm10, na.rm=T)) 

(means$mean[means$stage1_group == 1 & means$post09 == 1] - means$mean[means$stage1_group == 1 & means$post09 == 0]) -
  (means$mean[means$stage1_group == 0 & means$post09 == 1] - means$mean[means$stage1_group == 0 & means$post09 == 0])

# the same result should be found with the following DiD regression "without additional covariates"
summary(lm(pm10 ~ stage1_group + post09 + stage1_group:post09, data = group_heilbronn_stage1)) #todo: interpretation
summary(lm(pm10 ~ stage1_group + post09 + stage1_group:post09, data = group_heilbronn)) #todo: interpretation

# todo: machts sinn die regression nur über dezember 2008 and januar 2009 zu machen?


# we can also add the stations fixed effects 

summary(lm(pm10 ~ stage1_group + post09 + stage1_group:post09 + as.factor(station), data = group_heilbronn)) # todo: auch hier interpretation


# we reapeat just now with the plm() function 
# todo: was ist das denn hier eigentlich? 

group_heilbronn = pdata.frame(group_heilbronn, index = c("station", "date"))
summary(plm(pm10 ~ stage1_group + post09 + stage1_group:post09 , data = group_heilbronn, model = "within", effect = "individual"))
# wee see negative effects 

summary(plm(pm10 ~ d_stage1_10 + post09 + d_stage1_10:post09  , data = group_heilbronn, model = "within", effect = "individual"))
# todo: interpretation 


# aus paper replication :
summary(lm(pm10 ~ d_lez+ post09 + d_lez:post09 + as.factor(station) , data = group_heilbronn))


## todo: hier Wetteranalyse einfügen wie bei nicole ab Zeile 262





#####
#### Stage 2 
#####

#####
#### Stage 3 
#####


#####
#### ensure parallel trend:
#####

# see visualization from descriptive analysis 

ggplot_pm10_lez_95_3groupheilbronn 








#todo : eigentlich raus wenn wir das nicht machen für berlin oder andere zone allein




###### indivudual analysis of one zone ###### 

# search nrw:
lezNRW <- lezNRW[order(lezNRW$lez_area),]


ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW, aes(fill=zone), alpha=0.5) +
  geom_sf(data = stationsNRW , size=1, color = "black")

view(lezBW)

for (i in 1:27) {
  print(ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
          theme(panel.background = element_blank(), 
                axis.title = element_blank(), 
                axis.text = element_blank(), 
                axis.ticks = element_blank()) + 
          geom_sf(data= lezNRW[i,1], aes(fill=zone)) +
          geom_sf(data = stationsNRW, alpha=0.7, size=0.5, color = "darkgrey"))
}


# add here wheather they belong to a zone or not 

ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW, color= "aquamarine4",fill= "aquamarine4", alpha = 0.5) +
  geom_sf(data = stationsNRW, size=1)

# visualize when the zones were introducted
ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW, aes(fill=first_stage) , alpha = 0.5) 


# umweltzone köln2? köln ist eigentlich nur eine zone, aber sie wurde vergrößert 

stationskoeln <- filter(stationsNRW, kreis== "Köln")
lezNRW[26,1]; lezNRW[13,1]
ggplot() +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW[26,1] , color= "aquamarine4", fill="aquamarine4" ,alpha = 0.5)+ 
  geom_sf(data= lezNRW[13,1] , color= "aquamarine4", fill="aquamarine4", alpha = 0.5) + 
  geom_sf(data=filter(stationsNRW, kreis== "Köln"))



# create minimized data set only with lez relevant and than match stations within a distance of x of the zone, which do not belong to a zone

# than look at the breaks from no zone -> zone 1 


#aus Zoom Nicole: heterogenity analysis: Split dataset by traffic/background stations

#wenn Wetterdaten kompliziert sind, dann ?ber Nord/S?d Dummy Wettereinfl?sse einbeziehen?

#stargazer package nutzen um Output h?bsch und einheitlich zu haben wie tim in Zoom gezeigt hat