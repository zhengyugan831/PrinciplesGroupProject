#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#-----------------------------Install and load the needed library-----------------------------#
library(shiny)
library(shinyWidgets)
library(rlang)
library(highcharter)
library(stringr)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(skimr)
library(broom)
library(magick)

#-------------------------Community Mobility Dataset----------------------
# import and read the data collected, and assign it to a variable 
globalMobility <- readRDS("globalMobility.rds")
airQuality_median <- readRDS("airQuality_median.rds")
globalMobility_airQuality <- readRDS("globalMobility_airQuality.rds")
worldCities <- readRDS("worldCities.rds")

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

# Dropdown List of Residential vs NO2
list_residential_NO2 <- globalMobility_airQuality %>%
  select("Country","country_region","City","residential_percent_change_from_baseline","NO2")
list_residential_NO2 <- list_residential_NO2[(list_residential_NO2$residential_percent_change_from_baseline != 0 & list_residential_NO2$NO2 != 0),]
list_residential_NO2 <- na.omit(list_residential_NO2)
country_list_residential_NO2 <- list_residential_NO2 %>% 
  select("Country","country_region")
country_list_residential_NO2 <- unique(country_list_residential_NO2)

dropdown_country_residential_NO2 <- setNames(country_list_residential_NO2$Country, country_list_residential_NO2$country_region)

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
  setBackgroundImage(
    src = "http://havakalitesi.atasehir.bel.tr/media/filer_public_thumbnails/filer_public/4f/1a/4f1af93f-63d7-4189-b5b9-04f2e1f2a0e5/slider.jpg__2132x1411_q85_subsampling-2.jpg"
  ),
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
                   p("Due to the spreading of Covid-19, many countries had announced differrent movement control order from time to time. Most common one that we know is social distancing at initial stage. Eventually, it upgraded to country lock down which forbidden people to travel into their country or travel out from their country. Covid-19 had snatched so many lives without showing any mercy. All of us gotta stay at home for safe in order reduce any body contact between human. We are not encouraged to do any size of gathering during this period. Some of us even shop for grocery once in a week to avoid crowded area. This project is to show you how its going for our community mobility in each country. Also, we are about show you how is the air quality index being impacted."),
                   br(),
                   h3("Objective"),
                   p("To promote enviromental awareness among society to reduce human activities/community mobility which affecting the air quality."),
                   br(),
                   h3("Interesting Question"),
                   p("1. What is the impact to the community mobility during this period of Covid-19 globally?"),
                   p("2. What is the impact to the air quality during this period of Covid-19 globally?"),
                   p("3. Is it truth that the air quality is being impacted by the community mobility?"),
                   br(),
                   h3("Something you need to know"),
                   p("WHO states: \"In most urban environments in Europe, the principal source of NO2 is NOx from motor vehicles of all types and energy production in some places [e.g., power plants, domestic heating].\""),
                   p("Nitrogen dioxide primary emission of nitrogen oxide (NOx) in the environment. Over the past 50 years in Europe, the motor vehicle exhaust has largely replaced the other natural sources of NOx, for example, the burning of forest and fossil fuels such as coal, oil, and gas."),
                   p("The motor vehicle emission the NOx is more than half of the total of NOx in Europe. Based on the data in 1990, the total of NOx emission in Europe is higher than United States of America."),
                   p("Moreover, a higher concentration of NO2 inflames the human respiratory system and causes a lot of health problems such as coughing, bronchitis, and difficult breathing. When the NO2 combines and interacts with other chemicals will form acid rain, make air hazy, and even polluted the nutrient in coastal waters."),
                   p("Thus, a higher concentration of NO2 will impact the whole ecosystem seriously."),
                   a("Source: https://www.greenfacts.org/en/nitrogen-dioxide-no2/level-2/03-exposure.htm#1", href="https://www.greenfacts.org/en/nitrogen-dioxide-no2/level-2/03-exposure.htm#1"),
                   br(),
                   br(),
                   br(),
                   imageOutput("img_ozone", height = "100%"),
                   br(),
                   a("Source : http://www.apis.ac.uk/overview/pollutants/overview_o3.htm", href="http://www.apis.ac.uk/overview/pollutants/overview_o3.htm"),
                   br(),
                   br(),
                   br(),
                   imageOutput("img_aqi", height = "100%"),
                   br(),
                   a("Source : https://www.epa.gov/wildfire-smoke-course/wildfire-smoke-and-your-patients-health-air-quality-index", href="https://www.epa.gov/wildfire-smoke-course/wildfire-smoke-and-your-patients-health-air-quality-index"),
          ),
          tabPanel("Geographical Heatmap",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       radioButtons("aqi_no2_radio", label = h4("Species"),
                                    choices = list("AQI" = 1, "NO2" = 2), 
                                    selected = 1),
                       helpText("Please select the mode to filter AQI or NO2 result are you interested"),
                       br(),
                       uiOutput("country_input"),
                       helpText("Please select country to view other country result"),
                       br(),
                       sliderInput("date", 
                                   label = h4("Date"),
                                   min = as.Date(min(df_air_quality_heatmap$Date),"%Y-%m-%d"),
                                   max = as.Date(max(df_air_quality_heatmap$Date),"%Y-%m-%d"),
                                   value = as.Date(min(df_air_quality_heatmap$Date)),
                                   timeFormat = "%Y-%m-%d",
                                   animate = animationOptions(loop = FALSE, interval = 200)),
                       helpText("You may adjust slider to change the result by date")
                     ),
                     
                     mainPanel(
                       width = 9,
                       id="add_bg",
                       highchartOutput('map_diagram', height = "500px")
                     )
                   )
          ),
          tabPanel("Density Plot of Air Quality",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       selectInput("region2", h4("Country"), 
                                   choices = dropdown_country_list_air_quality,
                                   selected = "GB"),
                       helpText("Please select country to view other country result"),
                       br(),
                       uiOutput("city_input"),
                       helpText("Please select city to view other city result"),
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
          tabPanel("Types of Community Mobility",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       selectInput("region3", h4("Country"), 
                                   choices = dropdown_country_list_globalMobility,
                                   selected = "GB"),
                       helpText("Please select country to view other country result"),
                       br(),
                       uiOutput("state_input"),
                       helpText("Please select city to view other city result"),
                       br()
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
          ),
          tabPanel("Factor and Effect",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       selectInput("region4", h4("Country"), 
                                   choices = dropdown_country_residential_NO2,
                                   selected = "GB"),
                       helpText("Please select country to view other country result"),
                       br(),
                       uiOutput("state_input1"),
                       helpText("Please select city to view other city result"),
                       br()
                     ),
                     
                     mainPanel(
                       width = 9,
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('line_diagram_residential_scatter'),
                         highchartOutput('line_diagram_NO2_scatter')
                       ),
                       splitLayout(
                         style = "border: 1px solid silver;",
                         cellArgs = list(style = "padding: 6px"),
                         highchartOutput('scatter_residential_NO2')
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
  
  selected_region4 <- reactive({
    req(input$region4)
    if(str_length(input$region4) > 0)
      return(input$region4)
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
  
  selected_state1 <- reactive({
    req(input$state1)
    if(str_length(input$state1) > 0)
      return(input$state1)
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
    df_country_airquality <- df_country_airquality %>% 
      select("Code", "Country")
    df_country_airquality <- unique(df_country_airquality)
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
  
  state_data1 <- reactive({
    req(input$region4)
    city_list_residential_NO2 <- list_residential_NO2$City[(list_residential_NO2$Country == input$region4)]
    city_list_residential_NO2 <- sort(unique(city_list_residential_NO2))
    state_list <- setNames(city_list_residential_NO2, city_list_residential_NO2)
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
    df_2019_mean <- mean(df$AQI[df$Date < '2020-01-01'])
    df_2020_mean <- mean(df$AQI[df$Date > '2019-12-31'])
    
    if(t == 'AQI'){
      d <- df$AQI
      d_desc <- 'Air Quality Index(AQI)'
      df_2019_mean <- mean(df$AQI[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$AQI[df$Date > '2019-12-31'])
    }else if(t == 'CO'){
      d <- df$CO
      d_desc <- 'Carbon Monoxide(CO)'
      df_2019_mean <- mean(df$CO[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$CO[df$Date > '2019-12-31'])
    }else if(t == 'NO2'){
      d <- df$NO2
      d_desc <- 'Nitrogen Dioxide(NO2)'
      df_2019_mean <- mean(df$NO2[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$NO2[df$Date > '2019-12-31'])
    }else if(t == 'O3'){
      d <- df$O3
      d_desc <- 'Ozone(O3)'
      df_2019_mean <- mean(df$O3[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$O3[df$Date > '2019-12-31'])
    }else if(t == 'PM10'){
      d <- df$PM10
      d_desc <- 'Particulate Matter 10(PM10)'
      df_2019_mean <- mean(df$PM10[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$PM10[df$Date > '2019-12-31'])
    }else if(t == 'PM25'){
      d <- df$PM25
      d_desc <- 'Particulate Matter 2.5(PM2.5)'
      df_2019_mean <- mean(df$PM25[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$PM25[df$Date > '2019-12-31'])
    }else if(t == 'SO2'){
      d <- df$SO2
      d_desc <- 'Sulfur Dioxide(SO2)'
      df_2019_mean <- mean(df$SO2[df$Date < '2020-01-01'])
      df_2020_mean <- mean(df$SO2[df$Date > '2019-12-31'])
    }
    
    highchart() %>%
      hc_title(text = d_desc,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'area',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, 
               tickInterval = 30, 
               title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Median")) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE) %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 25, y = as.integer(df_2019_mean), xAxis = 0, yAxis = 100), text = paste('Average Concentration In 2019: ',as.integer(df_2019_mean))),
            list(point = list(x = 175, y = as.integer(df_2020_mean), xAxis = 0, yAxis = 100), text = paste('Average Concentration In 2020: ',as.integer(df_2020_mean)))
          )
        )
      )
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
      hc_title(text = d_desc,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'area',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, tickInterval = 30, title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "% change from baseline")) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE) %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 25, y = as.integer(d_decs_sub), xAxis = 0, yAxis = 0), text = paste(as.integer(d_decs_sub), '% compared to baseline'))
          )
        )
      )
  }
  
  create_hc_residential <- function(t) {
    df <- clean_globalMobility_airQuality[(clean_globalMobility_airQuality$Country == toupper(selected_region4()) & clean_globalMobility_airQuality$City == toupper(selected_state1())),]
    
    d <- df$residential_percent_change_from_baseline
    d_desc <- 'Residential'
    d_decs_sub <- mean(df$residential_percent_change_from_baseline)
    
    highchart() %>%
      hc_title(text = d_desc,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'area',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, tickInterval = 30, title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "% change from baseline")) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE) %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 25, y = as.integer(d_decs_sub), xAxis = 0, yAxis = 0), text = paste(as.integer(d_decs_sub), '% compared to baseline'))
          )
        )
      )
  }
  
  create_hc_NO2 <- function(t) {
    df <- airQuality_median[(airQuality_median$Country == toupper(selected_region4()) & airQuality_median$City == toupper(selected_state1())),]
    
    d <- df$NO2
    d_desc <- 'Nitrogen Dioxide(NO2)'
    df_2019_mean <- mean(df$NO2[df$Date < '2020-01-01'])
    df_2020_mean <- mean(df$NO2[df$Date > '2019-12-31'])
    
    highchart() %>%
      hc_title(text = d_desc,
               style = list(fontSize = "15px")) %>%
      hc_chart(type = 'area',
               polar = FALSE) %>%
      hc_xAxis(categories = df$Date, 
               tickInterval = 30, 
               title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Median")) %>%
      hc_add_series(data = d, name = t, showInLegend = FALSE) %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 25, y = as.integer(df_2019_mean), xAxis = 0, yAxis = 100), text = paste('Average Concentration In 2019: ',as.integer(df_2019_mean))),
            list(point = list(x = 175, y = as.integer(df_2020_mean), xAxis = 0, yAxis = 100), text = paste('Average Concentration In 2020: ',as.integer(df_2020_mean)))
          )
        )
      )
  }
  
  create_hc_scatter <- function() {
    df_residential_NO2 <- globalMobility_airQuality[(globalMobility_airQuality$Country == toupper(selected_region4()) & globalMobility_airQuality$City == toupper(selected_state1())),]
    df_residential_NO2 <- df_residential_NO2 %>% 
        select('residential_percent_change_from_baseline','NO2')
    df_residential_NO2 <- df_residential_NO2[(df_residential_NO2$NO2 != 0),]
    df_residential_NO2 <- df_residential_NO2[order(df_residential_NO2$residential_percent_change_from_baseline),]
    
    model <- lm(NO2 ~ residential_percent_change_from_baseline, data = df_residential_NO2)
    fit <- augment(model) %>% arrange(NO2)
    
    df_residential_NO2 %>% 
      hchart('scatter', hcaes(x = residential_percent_change_from_baseline, y = NO2)) %>% 
      hc_title(text = paste0("Relationship Between Residential Percentage and The Concentration of NO2"),
               style = list(fontSize = "15px", useHTML = TRUE)) %>% 
      hc_add_series(fit, type = "line", hcaes(x = residential_percent_change_from_baseline, y = .fitted),name = "Fit", id = "fit")
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
    selectInput("region1", h4("Country"), country_data(), selected = "MY")
  })
  
  output$city_input <- renderUI({
    req(input$region2)
    selectInput("city", h4("City"), city_data())
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
    selectInput("state", h4("City"), state_data())
  })
  
  output$line_diagram_retail_and_recreation_percent_change_from_baseline <- renderHighchart({create_hc_mobility("retail_and_recreation_percent_change_from_baseline")})
  output$line_diagram_grocery_and_pharmacy_percent_change_from_baseline <- renderHighchart({create_hc_mobility("grocery_and_pharmacy_percent_change_from_baseline")})
  output$line_diagram_parks_percent_change_from_baseline <- renderHighchart({create_hc_mobility("parks_percent_change_from_baseline")})
  output$line_diagram_transit_stations_percent_change_from_baseline <- renderHighchart({create_hc_mobility("transit_stations_percent_change_from_baseline")})
  output$line_diagram_workplaces_percent_change_from_baseline <- renderHighchart({create_hc_mobility("workplaces_percent_change_from_baseline")})
  output$line_diagram_residential_percent_change_from_baseline <- renderHighchart({create_hc_mobility("residential_percent_change_from_baseline")})
  
  output$state_input1 <- renderUI({
    req(input$region4)
    selectInput("state1", h4("City"), state_data1())
  })
  
  output$line_diagram_residential_scatter <- renderHighchart({create_hc_residential("residential_percent_change_from_baseline")})
  output$line_diagram_NO2_scatter <- renderHighchart({create_hc_NO2("NO2")})
  output$scatter_residential_NO2 <- renderHighchart({create_hc_scatter()})
  
  output$img_ozone <- renderImage({
    
    image <- image_read("Fig_One_Ozone_Overview.jpg")
    
    # Numeric operators
    tmpfile <- image %>%
      image_resize('70%') %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  output$img_aqi <- renderImage({
    
    image <- image_read("aqitableforcourse.png")
    
    # Numeric operators
    tmpfile <- image %>%
      image_resize('60%') %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # Return a list
    list(src = tmpfile, contentType = "image/png")
  })
  
  
  # =================================================================================
}

# Run the application 
shinyApp(ui = ui, server = server)