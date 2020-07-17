---
title: "Isaac-FlexDashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(ggplot2)
library(leaflet)
library(sp)
library(sf)
knitr::opts_chunk$set(echo = FALSE)
covid <- read_csv(
  "data/covid_19_data.csv",
  col_types = cols(
    ObservationDate = col_date(format="%m/%d/%Y")
  )) %>%
  group_by(`Country/Region`, ObservationDate) %>%
  summarize(
    cases = sum(Confirmed),
    deaths = sum(Deaths),
    recovered = sum(Recovered)
  ) %>%
  mutate(
    first_occurrence = first(ObservationDate),
    days_in_country = ObservationDate - first_occurrence
  ) %>%
  rename("country" = `Country/Region`, "date" = ObservationDate)
```

Column {data-width=650}
--------------------------------------------------------------------



```{r}
covid_data <-covid %>%
filter(country == "US"| country == "Brazil"| country== "India"| country =="Russia"| country == "Peru")
```

```{r}
covid_data2 <- covid %>%
filter(country == "US"| country == "South Korea")
```


### Chart A: World Comparison Heatmap

```{r}
countries <- geojsonio::geojson_read("worldmap.geojson", what = "sp")
countries <- st_as_sf(countries)%>%
  mutate(geounit = recode(geounit,`United States of America` = "US", `United Kingdom` = "UK", `China` = "Mainland China", `Myanmar` = "Burma", `Greenland` = "Denmark", `United Arab Emirates` = "UAE"))
covid_data3 <- covid %>%
  filter(date == "2020-07-01")

countries_and_covid <- left_join(countries, covid_data3, by = c("geounit" = "country"))
bins <- c(0, 1000, 10000, 50000, 100000, 1000000, 5000000, Inf)
pal <- colorBin("YlOrRd", domain = countries_and_covid$cases, bins = bins)

leaflet(countries_and_covid) %>%
  setView(0, 0, 1) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
  fillColor = ~pal(cases),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)%>%
addLegend(pal = pal, values = ~cases, opacity = 0.7, title = NULL,
  position = "bottomright")


```



Column {data-width=350}
-----------------------------------------------------------------------

### Chart B: US Compared to South Korea

```{r, echo = FALSE}
a <- ggplot(data = covid_data2)+
  geom_line(mapping = aes(x=date,y =cases, color = country))
ggplotly(a)
```

### Chart C : Country Comparison 

```{r, echo = FALSE}
 p <- ggplot(data = covid_data)+
  geom_line(mapping =aes(x=date,y =cases, color = country))
ggplotly(p)
```

