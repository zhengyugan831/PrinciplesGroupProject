With the COVID-19 spreading out all over the world, the World Air Quality Index project team saw a surge in
 requests for global data covering the whole world map. As a result, the WAQI project is now providing a new
 dedicated data-set, updated 3 times a day, and covering about 380 major cities in the world,
 from January 2020 until now.

The data for each major cities is based on the average (median) of several stations. The data set provides min,
 max, median and standard deviation for each of the air pollutant species (PM2.5,PM10, Ozone ...) as well as
 meteorological data (Wind, Temperature, ...). All air pollutant species are converted to the US EPA standard
 (i.e. no raw concentrations). All dates are UTC based. The count column is the number of samples used for
 calculating the median and standard deviation.

The CSV data sets can be downloaded programatically: The url is

https://aqicn.org/data-platform/covid19/report/14844-adab615a/{period}

Where period is any of 2019Q1, 2019Q2, 2019Q3, 2019Q4, 2018H1, 2017H1, 2016H1, 2015H1.

For instance using curl:
curl --compressed -o waqi-covid-2020.csv   https://aqicn.org/data-platform/covid19/report/14844-adab615a/2020
curl --compressed -o waqi-covid-2019Q1.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2019Q1
curl --compressed -o waqi-covid-2019Q2.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2019Q2
curl --compressed -o waqi-covid-2019Q3.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2019Q3
curl --compressed -o waqi-covid-2019Q4.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2019Q4
curl --compressed -o waqi-covid-2018H1.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2018H1
curl --compressed -o waqi-covid-2017H1.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2017H1
curl --compressed -o waqi-covid-2016H1.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2016H1
curl --compressed -o waqi-covid-2015H1.csv https://aqicn.org/data-platform/covid19/report/14844-adab615a/2015H1
To download the list of stations with latitude, longitude and EPA feed:
curl --compressed -o airquality-covid19-cities.json https://aqicn.org/data-platform/covid19/airquality-covid19-cities.json