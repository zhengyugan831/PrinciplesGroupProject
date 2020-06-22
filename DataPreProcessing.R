#-----------------------------Install and load the needed library-----------------------------#
library(shiny)
library(rlang)
library(highcharter)
library(stringr)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(skimr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#-------------------------Community Mobility Dataset----------------------
# import and read the data collected, and assign it to a variable 
globalMobility <- read.csv(file = "Global_Mobility_Report.csv",header = T, stringsAsFactors=FALSE)
head(globalMobility)
str(globalMobility)

table(sapply(globalMobility,class))

"skim() is an alternative to summary(), quickly providing a broad overview of a data frame.
It handles data of all types, dispatching a different set of summary functions based on the
types of columns in the data frame."
skim(globalMobility)

sum(is.na(globalMobility)) #there are 894156 of NA value. Thus, we need perform cleaning
colnames(globalMobility)

# 1) country_region_code - Nominal
sum(is.na(globalMobility$country_region_code))# there are 824 NA
globalMobility_country_region_code_na <- globalMobility %>%
  filter(is.na(country_region_code))

unique(globalMobility_country_region_code_na$country_region)

"As we can see, its only the Namibia and the the two-letter country abbreviation for Namibia is NA. Thus we should
treat its country_region_code as string value of NA instead of NA value"

# We have to add in new factor "NA" manually as its a reservered string
globalMobility[which(globalMobility$country_region == "Namibia" & is.na(globalMobility$country_region_code)),]["country_region_code"] = "NA"

sum(is.na(globalMobility$country_region_code)) ## its now cleaned with NA
table(globalMobility$country_region_code)

# 2) country_region - Nominal
sum(is.na(globalMobility$country_region))# there is 0 NA

# 3) sub_region_1 - Nominal
sum(is.na(globalMobility$sub_region_1))# there is 0 NA

# 4) sub_region_2 - Nominal
sum(is.na(globalMobility$sub_region_2))# there is 0 NA


"
We can see that country_region_code,country_region,sub_region_1, and sub_region_2 are meant for represent a specific
location. Since its only the sub_region_2 containing empty string factor and may just meaning that there is really
no sub_region_2, we can group these column to represent a specific location.
"

# 5) date - Interval
sum(is.na(globalMobility$date))# there is 0 NA
head(globalMobility$date)
globalMobility$date <- as.Date(globalMobility$date)
min(globalMobility$date)
max(globalMobility$date)

# We will trim all whitespace and convert all to upper for comparison
globalMobility <- data.frame(lapply(globalMobility, function(v) {
  if (is.character(v)) return(toupper(trimws(v)))
  else return(v)
}), stringsAsFactors=FALSE)

head(globalMobility)
str(globalMobility)

#-------------------------World cities dataset----------------------
worldCities <- read.csv(file = "worldcities.csv",header = T, stringsAsFactors=FALSE)
head(worldCities)

# We will trim all whitespace and convert all to upper for comparison
worldCities <- data.frame(lapply(worldCities, function(v) {
  if (is.character(v)) return(toupper(trimws(v)))
  else return(v)
}), stringsAsFactors=FALSE)

# for those small country like SG and HK, its admin_name default will be empty. Thus we will replace it with
# city in order increase the matching rate with community mobility dataset.
worldCities$admin_name <- ifelse(worldCities$admin_name == "", worldCities$city, worldCities$admin_name)

# for those small country like SG and HK, its admin_name default will be empty. Thus we will replace it with
# city in order increase the matching rate with community mobility dataset.
globalMobility <- globalMobility %>%
  mutate(sub_region_1 = ifelse((is.na(sub_region_1) | sub_region_1 == ""), country_region, sub_region_1))

# this is to is there any empty record with admin_name == "",expected to be 0
worldCities[worldCities$admin_name == "",]
worldCities[is.na(worldCities$admin_name),]
worldCities[worldCities$iso2 %in% c("SG","HK","MY"),]

# lets take a look at our country code which is MY
unique(worldCities[worldCities$iso2 == "MY","admin_name"])
unique(globalMobility[globalMobility$country_region_code == "MY","sub_region_1"])

worldCities$admin_name = str_replace(worldCities$admin_name,c("KUALA LUMPUR"), c("FEDERAL TERRITORY OF KUALA LUMPUR"))
worldCities$admin_name = str_replace(worldCities$admin_name,"LABUAN", "LABUAN FEDERAL TERRITORY")
worldCities$admin_name = str_replace(worldCities$admin_name,"MELAKA", "MALACCA")
worldCities$admin_name = str_replace(worldCities$admin_name,"PULAU PINANG", "PENANG")

unique(worldCities[worldCities$iso2 == "MY","admin_name"])
unique(globalMobility[globalMobility$country_region_code == "MY","sub_region_1"])

worldCities[which(worldCities$country == "NAMIBIA" & is.na(worldCities$iso2)),]["iso2"] = "NA"

sum(is.na(worldCities$country)) ## its now cleaned with NA

worldCities[(worldCities$iso2 == "MY" & worldCities$admin_name == "PAHANG"),]
str(worldCities)

#due the decimal wil be rounded during summarise, we will convert lat and lng to character first
worldCities$lat <- as.character(worldCities$lat)
worldCities$lng <- as.character(worldCities$lng)

str(worldCities)
world_city <- worldCities %>%
  select(iso2,admin_name,city,lat,lng) %>%
  group_by(iso2,admin_name) %>%
  summarise(City = first(city),lat = first(lat),lng = first(lng))

world_city[(world_city$iso2 == "MY" & world_city$admin_name == "PAHANG"),]

globalMobility <- left_join(globalMobility,world_city,by = c("country_region_code" = "iso2","sub_region_1" = "admin_name"))
globalMobility[(globalMobility$country_region_code == "MY" & globalMobility$sub_region_1 == "JOHOR"),]
colnames(globalMobility)

# As you can see that its only those country_region_code = US only having sub_region_2. We will drop those record
# with sub_region_2 because we will join with air quality dataset which have only country code and city
globalMobility %>%
  filter(sub_region_2 != "")

# we will drop sub_region_2 since it will be empty
globalMobility <- globalMobility[ , !(names(globalMobility) %in% c("sub_region_2"))]

globalMobility[(globalMobility$country_region_code == "MY" & globalMobility$sub_region_1 == "PAHANG"),]

# reverse back the previous copy logic after join dataset with world city dataset
globalMobility <- globalMobility %>%
  mutate(sub_region_1 = ifelse(sub_region_1 == country_region, "", sub_region_1))

##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

print(colnames(globalMobility))
globalMobility <- arrange.vars(globalMobility, c("country_region_code"=1, "country_region"=2,"sub_region_1"=3,
                                                 "City"=4,"lat"=5,"lng"=6))

# sub_region_1 = "" meaning it only selecting record based on country, thus the City, lat and lng will be empty
globalMobility[(globalMobility$country_region_code == "MY" & globalMobility$sub_region_1 != ""),]

saveRDS(globalMobility, "globalMobility.rds")
saveRDS(worldCities, "worldCities.rds")

#-------------------------Air Quality dataset----------------------
airQuality_2019Q4 <- read.csv(file = "waqi-covid19-airqualitydata-2019Q4.csv",header = T, comment.char = '#', stringsAsFactors=FALSE)
head(airQuality_2019Q4)

airQuality_2020 <- read.csv(file = "waqi-covid19-airqualitydata-2020.csv",header = T, comment.char = '#', stringsAsFactors=FALSE)
head(airQuality_2020)

airQuality <- rbind(airQuality_2019Q4,airQuality_2020)
airQuality$Date <- as.Date(airQuality$Date)
airQuality <- airQuality %>% arrange(Date,Country,City,Specie)
head(airQuality)
tail(airQuality)

# We will trim all whitespace and convert all to upper for comparison
airQuality <- data.frame(lapply(airQuality, function(v) {
  if (is.character(v)) return(toupper(trimws(v)))
  else return(v)
}), stringsAsFactors=FALSE)

head(airQuality)

sum(is.na(airQuality))

unique(airQuality$Specie)

# we will keep only 'CO', 'NO2', 'O3', 'PM10', 'PM25', 'SO2', 'AQI', and 'PM1'
airQuality <- airQuality %>%
  filter(Specie %in% c('CO', 'NO2', 'O3', 'PM10', 'PM25', 'SO2', 'AQI', 'PM1'))

head(airQuality)
tail(airQuality)

# we will drop 'count','min','max','variance' since it will be empty
airQuality_median <- airQuality[ , !(names(airQuality) %in% c('count','min','max','variance'))]
head(airQuality_median)
str(airQuality_median)

airQuality_median <- airQuality_median %>%
  group_by(Date,Country,City,Specie) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = Specie,values_from = median,values_fill=list(median = 0)) %>%
  ungroup()
head(airQuality_median)
airQuality_median <- arrange.vars(airQuality_median, c("row"=1, "Date"=2,"Country"=3,
                                                       "City"=4,"AQI"=5,"PM1"=9))
head(airQuality_median)
saveRDS(airQuality_median, "airQuality_median.rds")
#-------------------------Merging Community dataset with Air Quality dataset----------------------
# lets take a look at our country code which is MY
unique(airQuality_median[airQuality_median$Country == "MY","City"])
unique(globalMobility[globalMobility$country_region_code == "MY","City"])

globalMobility$City = str_replace(globalMobility$City,"MELAKA", "MALACCA")

globalMobility_airQuality <- inner_join(globalMobility,airQuality_median,by = c("country_region_code" = "Country","City" = "City", "date" = "Date"))
head(globalMobility_airQuality)

# renam columns
globalMobility_airQuality <- globalMobility_airQuality %>% 
  rename(
    Country = country_region_code,
    Date = date,
    State = sub_region_1
  )

saveRDS(globalMobility_airQuality, "globalMobility_airQuality.rds")

plot_all_out_get_country_region_code <- function(oriData){
  unique(oriData$country_region_code)
}

plot_all_out_get_sub_region <- function(oriData,countryRegionCode){
  if(str_length(countryRegionCode) == 0)
    return(paste('Country Region Code cannot be empty'))
  unique(oriData[oriData$country_region_code == countryRegionCode,"sub_region_1"])
}

plot_all_out <- function(oriData,countryRegionCode,subRegion=''){
  if(str_length(countryRegionCode) == 0)
    return(paste('Country Region Code cannot be empty'))
  if(count(oriData[(oriData$country_region_code == countryRegionCode) & (oriData$sub_region_1 == subRegion),]) == 0)
    return(paste("Country Region Code:",countryRegionCode,",Sub Region:",subRegion,"is not available"))
  
  data = oriData[(oriData$country_region_code == countryRegionCode) & (oriData$sub_region_1 == subRegion),]
  head(data)
}

plot_all_out_get_country_region_code(oriData=globalMobility)
selected_country_region_code = "MY"
paste("Selected Sub Region list:",selected_country_region_code)
plot_all_out_get_sub_region(oriData=globalMobility,selected_country_region_code)

plot_all_out(oriData=globalMobility,countryRegionCode='MY',subRegion='JOHOR')

plot_all_out_2_get_country <- function(oriData){
  unique(oriData$Country)
}

plot_all_out_2_get_city <- function(oriData,Country){
  if(str_length(Country) == 0)
    return(paste('Country cannot be empty'))
  unique(oriData[oriData$Country == Country,"City"])
}

plot_all_out_2 <- function(oriData,Country,City){
  if(str_length(Country) == 0)
    return(paste('Country cannot be empty'))
  if(str_length(City) == 0)
    return(paste('City cannot be empty'))
  
  if(count(oriData[(oriData$Country == Country) & (oriData$City == City),]) == 0)
    return(paste("Country:",Country,",City:",City,"is not available"))
  
  data = oriData[(oriData$Country == Country) & (oriData$City == City),]
  head(data)
}

plot_all_out_2_get_country(oriData=airQuality_median)
selected_country = "HK"
paste("Selected City list:",selected_country)
plot_all_out_2_get_city(oriData=airQuality_median,selected_country)

plot_all_out_2(oriData=airQuality_median,Country="HK",City="HONG KONG")

plot_all_out_2_get_country(oriData=globalMobility_airQuality)
selected_country = "HK"
paste("Selected City list:",selected_country)
plot_all_out_2_get_city(oriData=globalMobility_airQuality,selected_country)

plot_all_out_2(oriData=globalMobility_airQuality,Country="HK",City="HONG KONG")