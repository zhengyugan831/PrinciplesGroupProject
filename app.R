#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#-----------------------------Install and load the needed library-----------------------------#
packages = c("shiny", "corrplot", "DataExplorer", "ggplot2",
             "dplyr",  "tidyverse", "corrr", "skimr",
             "expss", "fastDummies", "ggpubr", "scales", "lubridate", "stringr",
             "highcharter")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# getwd()
#-------------------------Community Mobility Dataset----------------------
# import and read the data collected, and assign it to a variable 
globalMobility <- read.csv(file = "Dataset/CommunityMobility/Global_Mobility_Report.csv",header = T, stringsAsFactors=FALSE)
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
worldCities <- read.csv(file = "Dataset/worldcities.csv",header = T, stringsAsFactors=FALSE)
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

#-------------------------Air Quality dataset----------------------
airQuality_2019Q4 <- read.csv(file = "Dataset/AirQuality/waqi-covid19-airqualitydata-2019Q4.csv",header = T, comment.char = '#', stringsAsFactors=FALSE)
head(airQuality_2019Q4)

airQuality_2020 <- read.csv(file = "Dataset/AirQuality/waqi-covid19-airqualitydata-2020.csv",header = T, comment.char = '#', stringsAsFactors=FALSE)
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
# =================================================================================

# Drop Down List Selection
# =================================================================================
country_worldCities <- worldCities %>% 
  select('country', 'iso2')

country_worldCities <- unique(country_worldCities)

country_airQuality_median <- airQuality_median %>% 
  select('Country')
country_airQuality_median <- unique(country_airQuality_median)

country_worldCities <- country_worldCities %>% 
  filter(iso2 %in% country_airQuality_median$Country)

country_worldCities <- separate(country_worldCities, country, into = c("Col_country_1", "Col_country_2", "Col_country_3", "Col_country_4"))
country_worldCities[is.na(country_worldCities)] <- " "
country_worldCities$Col_country_1 = paste0(substr(country_worldCities$Col_country_1, 0, 1), tolower(substr(country_worldCities$Col_country_1, 2, length(country_worldCities$Col_country_1))))
country_worldCities$Col_country_2 = paste0(substr(country_worldCities$Col_country_2, 0, 1), tolower(substr(country_worldCities$Col_country_2, 2, length(country_worldCities$Col_country_2))))
country_worldCities$Col_country_3 = paste0(substr(country_worldCities$Col_country_3, 0, 1), tolower(substr(country_worldCities$Col_country_3, 2, length(country_worldCities$Col_country_3))))
country_worldCities$Col_country_4 = paste0(substr(country_worldCities$Col_country_4, 0, 1), tolower(substr(country_worldCities$Col_country_4, 2, length(country_worldCities$Col_country_4))))
country_worldCities$Country <- paste(country_worldCities$Col_country_1, country_worldCities$Col_country_2, country_worldCities$Col_country_3, country_worldCities$Col_country_4)
country_worldCities$Country <- trimws(country_worldCities$Country, which = c("right"))
country_worldCities <- country_worldCities %>% 
  select(-c(Col_country_1, Col_country_2, Col_country_3, Col_country_4)) %>% 
  select(0, Country, everything()) %>% 
  rename(Code = iso2)

country_worldCities$Country[country_worldCities$Code == "CI"] <- "Ivory Coast"
country_worldCities$Country[country_worldCities$Code == "CW"] <- "Curacao"
country_worldCities <- country_worldCities[order(country_worldCities$Country),]

dropdown_country_list_air_quality <- setNames(country_worldCities$Code, country_worldCities$Country)

country_city_airQuality_median <- airQuality_median %>% 
  select('Country', 'City')
country_city_airQuality_median <- country_city_airQuality_median %>% 
  distinct(Country, City, .keep_all = TRUE)

country_state_globalMobility_airQuality <- globalMobility_airQuality %>% 
  select('Country', 'State', 'City')
country_state_globalMobility_airQuality <- country_state_globalMobility_airQuality %>% 
  distinct(Country, State, City, .keep_all = TRUE)

country_list_globalMobility <- globalMobility_airQuality %>%
  select('Country', 'country_region')
country_list_globalMobility <- country_list_globalMobility %>% 
  distinct(Country, country_region, .keep_all = TRUE)
dropdown_country_list_globalMobility <- setNames(country_list_globalMobility$Country, country_list_globalMobility$country_region)

# =================================================================================

# Create Geographic Heatmap Dataframe
# =================================================================================
df_air_quality_heatmap <- airQuality_median %>% 
  select("Date", "Country", "City", "AQI", "NO2", "O3")

# Change Column Value To Capitalize First Letter Of Each Word
df_air_quality_heatmap <- separate(df_air_quality_heatmap, City, into = c("Col_City_1", "Col_City_2", "Col_City_3", "Col_City_4"))
df_air_quality_heatmap[is.na(df_air_quality_heatmap)] <- " "

df_air_quality_heatmap$Col_City_1 = paste0(substr(df_air_quality_heatmap$Col_City_1, 0, 1), tolower(substr(df_air_quality_heatmap$Col_City_1, 2, length(df_air_quality_heatmap$Col_City_1))))
df_air_quality_heatmap$Col_City_2 = paste0(substr(df_air_quality_heatmap$Col_City_2, 0, 1), tolower(substr(df_air_quality_heatmap$Col_City_2, 2, length(df_air_quality_heatmap$Col_City_2))))
df_air_quality_heatmap$Col_City_3 = paste0(substr(df_air_quality_heatmap$Col_City_3, 0, 1), tolower(substr(df_air_quality_heatmap$Col_City_3, 2, length(df_air_quality_heatmap$Col_City_3))))
df_air_quality_heatmap$Col_City_4 = paste0(substr(df_air_quality_heatmap$Col_City_4, 0, 1), tolower(substr(df_air_quality_heatmap$Col_City_4, 2, length(df_air_quality_heatmap$Col_City_4))))

df_air_quality_heatmap$City <- paste(df_air_quality_heatmap$Col_City_1, 
                                     df_air_quality_heatmap$Col_City_2, 
                                     df_air_quality_heatmap$Col_City_3, 
                                     df_air_quality_heatmap$Col_City_4)

# Remove Tailing Space
df_air_quality_heatmap$City <- trimws(df_air_quality_heatmap$City, which = c("right"))
df_air_quality_heatmap <- df_air_quality_heatmap %>% 
  select(-c(Col_City_1, Col_City_2, Col_City_3, Col_City_4)) %>% 
  select(0:Country, City, everything())

clean_globalMobility_airQuality <- globalMobility_airQuality %>% 
  select('Date', 'Country', 'country_region', 'State', 'City', 'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline', 'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline', 'residential_percent_change_from_baseline')

# =================================================================================

# Define UI 
# =================================================================================
ui <- fluidPage(
  wellPanel(
    fluidRow(
      column(
        width = 12,
        titlePanel("Impact of Community Mobility to Air Quality during COVID-19"),
        tags$head(tags$style(
          HTML('
            #add_bg {
              background-color: #85c1e9;
            }
          ')
        )),
        
        tabsetPanel(
          tabPanel("Home",
                   h3("Introduction"),
                   br(),
                   p("Due to the spreading of Covid-19, many countries had announced differrent movement control order from time to time. Most common one that we know is social distancing at initial stage. Eventually, it upgraded to country lock down which forbidden people to travel into their country or travel out from their country. Covid-19 had snatched so many lives without showing any mercy. All of us gotta stay at home for safe in order reduce any body contact between human. We are not encouraged to do any size of gathering during this period. Some of us even shop for grocery once in a week to avoid crowded area. This project is to show you how its going for our community mobility in each country. Also, we are about show you how is the air quality index being impacted."),
                   br()),
          tabPanel("Objective & Questions",
                   h3("Objective"),
                   br(),
                   p("To prove Air Quality Index is impacted by Community Mobility during Covid-19 period."),
                   br(),
                   br(),
                   h3("Interesting Question"),
                   br(),
                   p("1. What is the impact to the community mobility during this period of Covid-19 globally?"),
                   p("2. What is the impact to the air quality during this period of Covid-19 globally?"),
                   p("3. Is it truth that the air quality is being impacted by the community mobility?"),
                   br()),
          tabPanel("Knowledge",
                   h3("Something you need to know"),
                   br(),
                   p("WHO states: \"In most urban environments in Europe, the principal source of NO2 is NOx from motor vehicles of all types and energy production in some places [e.g., power plants, domestic heating].\""),
                   br(),
                   p("Nitrogen dioxide primary emission of nitrogen oxide (NOx) in the environment. Over the past 50 years in Europe, the motor vehicle exhaust has largely replaced the other natural sources of NOx, for example, the burning of forest and fossil fuels such as coal, oil, and gas."),
                   br(),
                   p("The motor vehicle emission the NOx is more than half of the total of NOx in Europe. Based on the data in 1990, the total of NOx emission in Europe is higher than United States of America."),
                   br(),
                   p("Moreover, a higher concentration of NO2 inflames the human respiratory system and causes a lot of health problems such as coughing, bronchitis, and difficult breathing. When the NO2 combines and interacts with other chemicals will form acid rain, make air hazy, and even polluted the nutrient in coastal waters."),
                   br(),
                   p("Thus, a higher concentration of NO2 will impact the whole ecosystem seriously."),
                   br(),
                   p("Source: "),
                   a("https://www.greenfacts.org/en/nitrogen-dioxide-no2/level-2/03-exposure.htm#1", href="https://www.greenfacts.org/en/nitrogen-dioxide-no2/level-2/03-exposure.htm#1"),
                   br()),
          tabPanel("Air Quality (Map)",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       radioButtons("aqi_no2_radio", label = "Mode",
                                    choices = list("AQI" = 1, "NO2" = 2), 
                                    selected = 1),
                       uiOutput("country_input"),
                       sliderInput("date",
                                   "",
                                   min = as.Date(min(df_air_quality_heatmap$Date),"%Y-%m-%d"),
                                   max = as.Date(max(df_air_quality_heatmap$Date),"%Y-%m-%d"),
                                   value = as.Date(min(df_air_quality_heatmap$Date)),
                                   timeFormat = "%Y-%m-%d",
                                   animate = animationOptions(loop = FALSE, interval = 500))
                     ),
                     
                     mainPanel(
                       width = 9,
                       id="add_bg",
                       highchartOutput('map_diagram', height = "500px")
                     )
                   )
          ),
          tabPanel("Air Quality (Graph)",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       selectInput("region2", "Country:", 
                                   choices = dropdown_country_list_air_quality),
                       uiOutput("city_input"),
                       # list(HTML('<p><img src="air_quality_level.png"/></p>'))
                     ),
                     
                     mainPanel(
                       width = 9,
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_AQI'),
                         highchartOutput('line_diagram_CO')
                       ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_NO2'),
                         highchartOutput('line_diagram_O3')
                       ),
                       # splitLayout(
                       #   style = "border: 1px solid silver;",
                       #   cellArgs = list(style = "padding: 6px"),
                       #   highchartOutput('line_diagram_PM1'),
                       #   highchartOutput('line_diagram_PM10')
                       # ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_PM10'),
                         highchartOutput('line_diagram_PM25')
                       ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_SO2')
                       )
                     )
                   )
          ),
          tabPanel("Mobility (Graph)",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       selectInput("region3", "Country:", 
                                   choices = dropdown_country_list_globalMobility),
                       uiOutput("state_input")
                     ),
                     
                     mainPanel(
                       width = 9,
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_retail_and_recreation_percent_change_from_baseline'),
                         highchartOutput('line_diagram_grocery_and_pharmacy_percent_change_from_baseline')
                       ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_parks_percent_change_from_baseline'),
                         highchartOutput('line_diagram_transit_stations_percent_change_from_baseline')
                       ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_workplaces_percent_change_from_baseline'),
                         highchartOutput('line_diagram_residential_percent_change_from_baseline')
                       )
                     )
                   )
          )
        )
      )
    )
  )
)
# =================================================================================

# Define server logic required
# =================================================================================
server <- function(input, output) {
  # Reactive
  # =================================================================================
  selected_date <- reactive({
    req(input$date)
    if(str_length(input$date) > 0)
      return(input$date)
    else
      return("2020-03-01")
  })

  selected_region1 <- reactive({
    req(input$region1)
    if(str_length(input$region1) > 0)
      return(input$region1)
    else
      return("MY")
  })
  
  selected_region2 <- reactive({
    req(input$region2)
    if(str_length(input$region2) > 0)
      return(input$region2)
    else
      return("MY")
  })
  
  selected_region3 <- reactive({
    req(input$region3)
    if(str_length(input$region3) > 0)
      return(input$region3)
    else
      return("MY")
  })
  
  selected_city <- reactive({
    req(input$city)
    if(str_length(input$city) > 0)
      return(input$city)
    else
      return("KUALA LUMPUR")
  })
  
  selected_state <- reactive({
    req(input$state)
    if(str_length(input$state) > 0)
      return(input$state)
    else
      return("JOHOR")
  })
  
  mode <- reactive({
    req(input$aqi_no2_radio)
    if(input$aqi_no2_radio == 1)
      return("AQI")
    else
      return("NO2")
  })
  
  country_data <- reactive({
    req(input$aqi_no2_radio)
    
    if(mode() == "AQI")
      df_temp <- airQuality_median[(airQuality_median$AQI != 0),]
    else
      df_temp <- airQuality_median[(airQuality_median$NO2 != 0),]
    
    df_country_airquality <- df_temp %>% 
      select('Country', 'City')
    
    df_country_airquality <- unique(df_country_airquality) %>% 
      rename(Code = Country)
    
    df_country_airquality <- merge(x = df_country_airquality, y = country_worldCities, by = "Code", all.x = TRUE)
    df_country_airquality <- df_country_airquality[order(df_country_airquality$Country),]
    
    country_list <- setNames(df_country_airquality$Code, df_country_airquality$Country)
    return(country_list)
  })
  
  city_data <- reactive({
    req(input$region2)
    city_list <- setNames(country_city_airQuality_median$City[country_city_airQuality_median$Country == input$region2], country_city_airQuality_median$City[country_city_airQuality_median$Country == input$region2])
    return(city_list)
  })
  
  state_data <- reactive({
    req(input$region3)
    state_list <- setNames(country_state_globalMobility_airQuality$City[country_state_globalMobility_airQuality$Country == input$region3], country_state_globalMobility_airQuality$City[country_state_globalMobility_airQuality$Country == input$region3])
    return(state_list)
  })
  # =================================================================================
  
  # Function
  # =================================================================================
  mapdata_processing <- function(df) {
    df <- df %>%
      filter(Date %in% as.Date(selected_date())) %>%
      filter(Country %in% toupper(selected_region1()))
    
    df$Country <- tolower(df$Country)
    
    # MALAYSIA SET
    df$City[df$City == "Alor Setar"] <- "Kedah"
    df$City[df$City == "George Town"] <- "Pulau Pinang"
    df$City[df$City == "Ipoh"] <- "Perak"
    df$City[df$City == "Johor Bahru"] <- "Johor"
    df$City[df$City == "Klang"] <- "Selangor"
    df$City[df$City == "Kota Bharu"] <- "Kelantan"
    df$City[df$City == "Kuantan"] <- "Pahang"
    df$City[df$City == "Kuching"] <- "Sarawak"
    df$City[df$City == "Malacca"] <- "Melaka"
    df$City[df$City == "Seremban"] <- "Negeri Sembilan"
    # UK SET
    df$City[df$City == "London"] <- "Westminster"
    df$City[df$City == "Norwich"] <- "Norfolk"
    df$City[df$City == "Preston"] <- "Lancashire"
    df$City[df$City == "Southend On Sea"] <- "Southend-on-Sea"
    # AU SET
    df$City[df$City == "Adelaide"] <- "South Australia"
    df$City[df$City == "Brisbane"] <- "Queensland"
    df$City[df$City == "Hobart"] <- "Tasmania"
    df$City[df$City == "Melbourne"] <- "Victoria"
    df$City[df$City == "Perth"] <- "Western Australia"
    df$City[df$City == "Sydney"] <- "New South Wales"
    df$City[df$City == "Darwin"] <- "Northern Territory"
    df$City[df$City == "Canberra"] <- "Australian Capital Territory"
    # CN SET
    df$City[df$City == "???r???mqi"] <- "Xinjiang"
    df$City[df$City == "Changchun"] <- "Jilin"
    df$City[df$City == "Changsha"] <- "Hunan"
    df$City[df$City == "Chengdu"] <- "Sichuan"
    df$City[df$City == "Guangzhou"] <- "Guangdong"
    df$City[df$City == "Guiyang"] <- "Guizhou"
    df$City[df$City == "Haikou"] <- "Hainan"
    df$City[df$City == "Hangzhou"] <- "Zhejiang"
    df$City[df$City == "Harbin"] <- "Heilongjiang"
    df$City[df$City == "Hefei"] <- "Anhui"
    df$City[df$City == "Hohhot"] <- "Inner Mongol"
    df$City[df$City == "Jinan"] <- "Shandong"
    df$City[df$City == "Kunming"] <- "Yunnan"
    df$City[df$City == "Lanzhou"] <- "Gansu"
    df$City[df$City == "Nanchang"] <- "Jiangxi"
    df$City[df$City == "Nanjing"] <- "Jiangsu"
    df$City[df$City == "Nanning"] <- "Guangxi"
    df$City[df$City == "Shenyang"] <- "Liaoning"
    df$City[df$City == "Shijiazhuang"] <- "Hebei"
    df$City[df$City == "Taiyuan"] <- "Shanxi"
    df$City[df$City == "Wuhan"] <- "Hubei"
    df$City[df$City == "Xi??????n"] <- "Shaanxi"
    df$City[df$City == "Xiamen"] <- "Fujian"
    df$City[df$City == "Xining"] <- "Qinghai"
    df$City[df$City == "Yinchuan"] <- "Ningxia"
    df$City[df$City == "Zhengzhou"] <- "Henan"
    return(df)
  }
  
  create_hc <- function(t) {
    df <- airQuality_median[(airQuality_median$Country == toupper(selected_region2()) & airQuality_median$City == toupper(selected_city())),]
    
    d <- df$AQI
    d_desc <- 'Air Quality Index(AQI)'
    if(t == 'AQI'){
      d <- df$AQI
      d_desc <- 'Air Quality Index(AQI)'
      plotBands_list <- list(list(from = 0, to = 50, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 50, to = 100, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 100, to = 150, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 150, to = 200, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 200, to = 300, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 300, to = 500, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'CO'){
      d <- df$CO
      d_desc <- 'Carbon Monoxide(CO)'
      plotBands_list <- list(list(from = 0, to = 5, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 5, to = 10, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 10, to = 12, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 12, to = 15, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 15, to = 30, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 30, to = 50, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'NO2'){
      d <- df$NO2
      d_desc <- 'Nitrogen Dioxide(NO2)'
      plotBands_list <- list(list(from = 0, to = 54, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 54, to = 100, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 100, to = 360, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 360, to = 650, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 650, to = 1250, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 1250, to = 2049, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'O3'){
      d <- df$O3
      d_desc <- 'Ozone(O3)'
      plotBands_list <- list(list(from = 0, to = 55, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 55, to = 70, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 70, to = 85, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 85, to = 105, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 105, to = 200, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 200, to = 604, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'PM10'){
      d <- df$PM10
      d_desc <- 'Particulate Matter 10(PM10)'
      plotBands_list <- list(list(from = 0, to = 55, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 55, to = 155, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 155, to = 255, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 255, to = 355, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 355, to = 425, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 425, to = 604, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'PM25'){
      d <- df$PM25
      d_desc <- 'Particulate Matter 2.5(PM2.5)'
      plotBands_list <- list(list(from = 0, to = 12, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 12, to = 35, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 35, to = 55, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 55, to = 150, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 150, to = 250, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 250, to = 500, color = "rgba(128, 0, 0, 0.3)"))
    }else if(t == 'SO2'){
      d <- df$SO2
      d_desc <- 'Sulfur Dioxide(SO2)'
      plotBands_list <- list(list(from = 0, to = 35, color = "rgba(0, 255, 0, 0.3)"), 
                             list(from = 35, to = 75, color = "rgba(255, 255, 0, 0.3)"), 
                             list(from = 75, to = 185, color = "rgba(255, 128, 0, 0.3)"), 
                             list(from = 185, to = 305, color = "rgba(255, 0, 0, 0.3)"), 
                             list(from = 305, to = 605, color = "rgba(128, 0, 128, 0.3)"), 
                             list(from = 605, to = 1004, color = "rgba(128, 0, 0, 0.3)"))
    }
    
    highchart() %>%
      hc_title(text = d_desc,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'line',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, 
               tickInterval = 30, 
               title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Median"),
               plotBands = plotBands_list) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE)
  }
  
  create_hc_mobility <- function(t) {
    df <- clean_globalMobility_airQuality[(clean_globalMobility_airQuality$Country == toupper(selected_region3()) & clean_globalMobility_airQuality$City == toupper(selected_state())),]
    
    d <- df$retail_and_recreation_percent_change_from_baseline
    d_desc <- 'Retail & Recreation'
    d_decs_sub <- mean(df$retail_and_recreation_percent_change_from_baseline)
    if(t == 'retail_and_recreation_percent_change_from_baseline'){
      d <- df$retail_and_recreation_percent_change_from_baseline
      d_desc <- 'Retail & Recreation'
      d_decs_sub <- mean(df$retail_and_recreation_percent_change_from_baseline)
    }else if(t == 'grocery_and_pharmacy_percent_change_from_baseline'){
      d <- df$grocery_and_pharmacy_percent_change_from_baseline
      d_desc <- 'Grocery & Pharmacy'
      d_decs_sub <- mean(df$grocery_and_pharmacy_percent_change_from_baseline)
    }else if(t == 'parks_percent_change_from_baseline'){
      d <- df$parks_percent_change_from_baseline
      d_desc <- 'Parks'
      d_decs_sub <- mean(df$parks_percent_change_from_baseline)
    }else if(t == 'transit_stations_percent_change_from_baseline'){
      d <- df$transit_stations_percent_change_from_baseline
      d_desc <- 'Transit Stations'
      d_decs_sub <- mean(df$transit_stations_percent_change_from_baseline)
    }else if(t == 'workplaces_percent_change_from_baseline'){
      d <- df$workplaces_percent_change_from_baseline
      d_desc <- 'Workplaces'
      d_decs_sub <- mean(df$workplaces_percent_change_from_baseline)
    }else if(t == 'residential_percent_change_from_baseline'){
      d <- df$residential_percent_change_from_baseline
      d_desc <- 'Residential'
      d_decs_sub <- mean(df$residential_percent_change_from_baseline)
    }
    
    highchart() %>%
      hc_title(text = paste(d_desc,br(), '(', as.integer(d_decs_sub), '% compared to baseline)'),
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'line',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, tickInterval = 30, title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "% change from baseline")) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE)
  }
  # =================================================================================
  
  # Output
  # =================================================================================
  output$map_diagram <- renderHighchart({
    # Heatmap
    # =================================================================================
    if(mode() == "NO2") {
      colors <- c('#50B432', '#DDDF00', '#ED561B', "red")
      max <- 10
    } else {
      colors <- c('#008000', '#FFFF00', '#FF8000', '#FF0000', '#800080', "#800000")
      max <- 301
    }
    
    map_url <- paste0('https://code.highcharts.com/mapdata/countries/', tolower(input$region1), '/', tolower(input$region1), '-all.js')
    mapdata <- get_data_from_map(download_map_data(map_url))
    
    state_map <- mapdata %>% 
      select(name) %>% 
      arrange(name)
    
    map <- hcmap(map_url, data = mapdata_processing(df_air_quality_heatmap), value = mode(),
                 joinBy = c("name", "City"), name = mode(),
                 dataLabels = list(enabled = TRUE, format = '{point.name}'),
                 borderColor = "#FAFAFA", borderWidth = 1.0,
                 tooltip = list(valueDecimals = 0))

    map %>%
      hc_title(text = country_worldCities$Country[(country_worldCities$Code == input$region1)]) %>%
      hc_subtitle(text = paste('Period:', input$date)) %>%
      hc_colorAxis(stops = color_stops(colors = colors), max = max) %>%
      hc_mapNavigation(enabled = TRUE, 
                       enableMouseWheelZoom = TRUE,
                       enableDoubleClickZoom = TRUE,
                       buttonOptions = list(verticalAlign = 'bottom')) %>% 
      hc_legend(layout = "vertical", align = "right",
                floating = TRUE, valueDecimals = 0)
    # =================================================================================
  })
  
  # =================================================================================
  output$country_input <- renderUI({
    req(input$aqi_no2_radio)
    selectInput("region1", "Country:", country_data(), selected = "my")
  })
  
  output$city_input <- renderUI({
    req(input$region2)
    selectInput("city", "City:", city_data())
  })
  
  output$line_diagram_AQI <- renderHighchart({create_hc("AQI")})
  output$line_diagram_CO <- renderHighchart({create_hc("CO")})
  output$line_diagram_NO2 <- renderHighchart({create_hc("NO2")})
  output$line_diagram_O3 <- renderHighchart({create_hc("O3")})
  output$line_diagram_PM10 <- renderHighchart({create_hc("PM10")})
  output$line_diagram_PM25 <- renderHighchart({create_hc("PM25")})
  output$line_diagram_SO2 <- renderHighchart({create_hc("SO2")})
  # =================================================================================
  
  # =================================================================================
  output$state_input <- renderUI({
    req(input$region3)
    selectInput("state", "City:", state_data())
  })
  
  output$line_diagram_retail_and_recreation_percent_change_from_baseline <- renderHighchart({create_hc_mobility("retail_and_recreation_percent_change_from_baseline")})
  output$line_diagram_grocery_and_pharmacy_percent_change_from_baseline <- renderHighchart({create_hc_mobility("grocery_and_pharmacy_percent_change_from_baseline")})
  output$line_diagram_parks_percent_change_from_baseline <- renderHighchart({create_hc_mobility("parks_percent_change_from_baseline")})
  output$line_diagram_transit_stations_percent_change_from_baseline <- renderHighchart({create_hc_mobility("transit_stations_percent_change_from_baseline")})
  output$line_diagram_workplaces_percent_change_from_baseline <- renderHighchart({create_hc_mobility("workplaces_percent_change_from_baseline")})
  output$line_diagram_residential_percent_change_from_baseline <- renderHighchart({create_hc_mobility("residential_percent_change_from_baseline")})
  # =================================================================================
}

# Run the application 
shinyApp(ui = ui, server = server)