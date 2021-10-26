############ preparation ############

remove(list=ls())
rm()

# needed packages 

packages <- c("MatchIt",
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
              "stargazer") 

# Install all packages, which are not installed yet

lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)

setwd("C:/Users/Carina/Google Drive/Studium Arbeit/Studium/3. TU ab WS1920/4 SS21/Applied environmental econometrics in R/3 Paper")




############ read data ############

polution<-readRDS("air_pollution_2015_2018.rds")
str(polution)
class(polution) #not jet geographic sf data but with long/latitude
# panel data 
#observational unit: station column 
#polution data, for every day and station from 2015-2018

lez<-readRDS("lez_shp.rds")
str(lez)
class(lez)
# observational unit: lez of a city  
# geographic sf data

states <- read_sf("vg2500_bld.shp")
class(states)
# observational unit: Bundesländer" states of germany
# geographic sf data

#caaps <- read_excel("caaps.xlsx")
#str(caaps)
#observational unit: city 
#carbon action plans with years of update 

#sf <- readShapeSpatial("shapefiles_de.7z")
#shapefiles with administrative units 




############ visualize lez and stations germany wide ############

stations = polution %>% distinct(station,  type, area, longitude, latitude, nuts_code, kreis, kreis_type, bl_nr, bl, ags, mun, mun_type)
#458 stations in germany 

# "summarize" longitude and latitude to geography column   
stations <- st_as_sf(stations, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")
polution <- st_as_sf(polution, coords = c("longitude", "latitude"), 
                        crs = 4326, agr = "constant")
lez <- st_as_sf(lez, coords = geometry, 
                        crs = 4326, agr = "constant")

plot_lez <- ggplot() + geom_sf(data = states, color="grey", fill= "white") +  
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())+ 
  geom_sf(data= lez, color= "aquamarine4", fill ="aquamarine4")

plot_lez #see only lez

plot_lez +  geom_sf(data = stations, alpha=0.7, size=0.5, color = "darkgrey") #lez plus stations
      



#only for state NRW for better vision

NRW = filter(states, GEN == "Nordrhein-Westfalen") 
stationsNRW =filter(stations, bl == "Nordrhein-Westfalen") 
# 68 stations in nrw
lezNRW =filter(lez, state == "Nordrhein-Westfalen")
# 27 lez in nrw 

ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank()) + 
  geom_sf(data= lezNRW, color= "aquamarine4", fill ="aquamarine4") +
  geom_sf(data = stationsNRW, alpha=0.7, size=0.5, color = "darkgrey")  




############ combine datasets polution panel data and lez information ############

unique(st_is_valid(lez, reason = TRUE)) #find invalid geo
unique(st_is_valid(polution, reason = TRUE))

# fix invalid geography in lez data, self intersection: (they make the intersect function impossible for the 76 lez yet):

sf::sf_use_s2(FALSE)  
lez<-st_make_valid(lez)
unique(st_is_valid(lez, reason = TRUE)) # fixed, only valid geometries  
unique(st_is_valid(polution, reason = TRUE))

# merge with st_join function 

data_merged<- st_join(polution, lez,join = st_intersects) 


nrow(data_merged)-nrow(polution) 
#we see, that after merging, we find more rows



#explanation:

values_merged <-vector()
values_polution <-vector()
stationnames <- as.data.frame(count(polution$station)[,1])


for (i in 1:458) {
  values_merged[i] <- count(data_merged$station)[i,2] #counts the station appearance in merged data 
  values_polution[i] <- count(polution$station)[i,2]} #counts the station appearance in original polution data 

values_merged<-as.data.frame(values_merged)
values_polution<-as.data.frame(values_polution)
comparison <- cbind(stationnames,values_merged,values_polution)  
names(comparison)[1] <- "StationName"
head(comparison)

comparison <- comparison %>% 
  mutate(dif = values_merged -values_polution) 
head(comparison)
#the diff column gives us back wheather there is a difference in the number of appearance of a station between the original polution data 
#and the the merged 

difference <- filter(comparison, dif !=0) #look at the ones with a difference
difference 
sum(difference$dif)
nrow(data_merged)-nrow(polution) #the calculations are equal, this are exactly the differences in rownumber 


filter_data_merged <- filter(data_merged, station == "DENW021")

# explanation for what is special with this 18 stations that they appear twice as much in the merged data?: 
# the data for station DENW021 (in NRW) reveals that the stations seem to be positioned in two lez at the same time (Bottrop and Ruhrgebiet)



#visualization for intersecting lez bottrop and ruhrgebiet: 

stationsBottrop =filter(stations, kreis == "Bottrop") 
#one station
lezRuhrgebiet =filter(lez, zone == "Umweltzone Ruhrgebiet")
lezBottrop =filter(lez, zone == "Umweltzone Bottrop - Ruhrgebiet")


ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(),  
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank())  +
  geom_sf(data = stationsNRW, alpha=0.7, size=0.5, color = "darkgrey") + 
  geom_sf(data= lezRuhrgebiet, color= "aquamarine3", fill ="aquamarine3", alpha= 0.7) +
  geom_sf(data= lezBottrop, color= "aquamarine4", fill ="aquamarine4") 

#we can clearly see that the station lays within the other 


#correction with a modified sf_join function 
# within instead if intersect does not change anything, no position directly on the border

data_merged<- st_join(polution, lez,join = st_intersect, largest=TRUE) #takes really much time (from line 1-here ~8min) to execute but solves the double appearance problem
nrow(data_merged)-nrow(polution) #perfect, now they are equal






############ clean and prepare dataset ############

names(data_merged)
names(data_merged)[22] <- "LEZName"

#create a dummy variable if a station is inside a lez, independend of the time of introduction 
data_merged <- data_merged %>% mutate(d_lez_location = ifelse(is.na(LEZName) ==TRUE,0,1) )

#reorder, so that dummy of location within/outside is next to lez name column 
names(data_merged)
1:32
data_merged <- data_merged[,c(1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 32, 23, 24, 25, 26, 27, 28, 29, 30, 31)]  


#create three dummy variables 1 if the station observation is within a LEZ of stage 1 (the same for stage 2 and 3)
#carefull, example: stage 1 is introduced 01.01.2017, than the dummy is 1 only from 01.01.2017 on, before 0, use column "first/second/ third_stage"


#create a dummy variable 1 if the station observation is within a LEZ, 0 if not (no difference between which stage it is), use dummy variables for stage 1-3 "eather or" condution 


#-spezielle dummies sichern ab, dass wenn eine phase "übersprungen" wurde, nur ein dummi wirkt. 
#beispiel: wenn eine Stadt die lez gleich als stufe 2 einführt, sollte der dummi für stufe 1 nie 1 sein. (könnte auch direkt in den 3 spalten gelöst werden) 


#(extra dummies wenn Einführung der Phase übersprungen ist, vgl Zoom mit Nicole)


#(closest zone column = own LEZ (allways, due to nearest = True in st_join, when not inside lez,than nearest)


#(distance = use gretas code to create the middle point of lez, than calculate distance)
#gretas code from zeile 108:

# identify the center of mass for Canterbury region and save output in a new
# object. Hint: use st_centroid
canterbury_centroid = nz %>% filter(Name == "Canterbury") %>%
  st_centroid()

# calculate the distance between the geographic centroid of the Canterbury region
# and the highest point in New Zealand. Hint, first store the highest point 
# in a new object then use "st_distance"

nz_heighest = nz_height %>% top_n(n =1, wt = elevation)
st_distance(nz_heighest, canterbury_centroid)

# st_distance also comes with a unit. This way, it is clear that the distance
# between those two points is not 100,000 inches but meters.

# you can also store the distance between each mountain and canterbury_centroid
# directly in the data frame by using dplyr functionality
nz_height = nz_height %>% mutate(distance = st_distance(nz_height, canterbury_centroid))


#(wheather data?, merge like st_join?)


#(traffic data?, merge like st_join?)




############ explore dataset ############

summary(data_merged$first_stage) # we see the first stage was earliest introduced in 01.01.2008 (latest 31.12.2018)
#distribution der EInführungen? 
#hist(data_merged$first_stage)
summary(data_merged$second_stage) # we see the first stage was earliest introduced in 01.01.2009 (latest 31.12.2018)
summary(data_merged$third_stage) # we see the first stage was earliest introduced in 01.01.2010 (latest 31.12.2018)
summary(data_merged$date) #the first observation is from 01.01.2015 (latest 31.12.2018)-> 
# -> we can not make conclusions about the effect of the introduction of a zone and its stage when it happend before 2015

count(data_merged$LEZName) #see appearance of Zones/ no Zone 
stations_merged<- st_join(stations, lez,join = st_intersect, largest=TRUE)
#create a dummy variable if a station is inside a lez, independend of the time of introduction 
stations_merged <- stations_merged %>% mutate(d_lez_location = ifelse(is.na(zone) ==TRUE,0,1) )


#Which stations are within the LEZ
addmargins(table(stations_merged$type, stations_merged$d_lez_location, useNA = "ifany"))
addmargins(table(stations_merged$area, stations_merged$d_lez_location, useNA = "ifany"))
addmargins(table(stations_merged$type, stations_merged$area, useNA = "ifany"))

### show all stations within LEZ (NRW, to visualize better)
stations_merged %>% filter(d_lez_location==1)
stationsNRW =filter(stations_merged, bl == "Nordrhein-Westfalen") 


#plot by belonging to a lez or not 
#TODO: ist noch nicht so geil, besser machen 
ggplot() + geom_sf(data = NRW, color="grey", fill= "white") +
  theme(panel.background = element_blank(),  
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank())  +
  geom_sf(data = stationsNRW, aes(color = d_lez_location), size = 1.5)+ 
  geom_sf(data= lezNRW, color= "aquamarine3", fill ="aquamarine3", alpha= 0.7) 



############ Descriptive analysis ############  

#wenn alles so ist wie im "pol " data set aus der LEZ R Übung, können wir hier viel übernehmen 

#aus Zoom Nicole: heterogenity analysis: Split dataset by traffic/background stations

#wenn Wetterdaten kompliziert sind, dann über Nord/Süd Dummy Wettereinflüsse einbeziehen?

#stargazer package nutzen um Output hübsch und einheitlich zu haben wie tim in Zoom gezeigt hat