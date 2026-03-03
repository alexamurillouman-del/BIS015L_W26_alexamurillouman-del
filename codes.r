#Run data
data <- read_csv("data/dataname.csv") %>% 
clean_names()

#Explore Data
glimpse()
summary()
names()

#Filter(Keep Certain Rows)
filter(variable == "value") #(Keep specific category)
filter(variable > 10) #Keep numeric condition
filter(year >= 2000 & year <= 2020) #Keep range

Select #(Keep Certain Columns)
select(column1, column2) #Keep only certain variables, 
select(where(is.numeric)) #Keep all numeric columns

#Arrange(Sort)
data %>% arrange(variable) #Sort ascending = small to big
arrange(desc(variable)) #Sort descending = big to small

Group
group_by()

Summarize
data %>% summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))
#This gives ONE answer for the whole dataset.

Count
count(category) #Count how many in each category

#Mutate(Create new column)
mutate()

#Plot
data %>%
  ggplot(aes(x = variable1, y = variable2)) +
  geom_point()
Scatterplot
Boxplot #geom_boxplot()
Barplot #geom_bar()
Histogram #geom_histogram(bins = 30)

#NA Fix
mean(variable, na.rm = TRUE) #Ignore missing values

#Pivot
pivot_longer () #WIDE to LONG
pivot_wider () #LONG to WIDE

#Separate
separate(col_to_split, into=c("col1","col2"), sep="_")
#when a cell has 2 things in it (ex: patient_m)

#Unite (combine columns)
unite(new_col, col1, col2, sep="_")

#ggmap bounding box + base map
lat <- c(lat_min, lat_max)
long <- c(lon_min, lon_max)
bbox <- make_bbox(long, lat, f=0.03)
map1 <- get_stadiamap(bbox, maptype="stamen_terrain", zoom=7)
ggmap(map1)
#get a background map

#ggmap add points
ggmap(map1) +
geom_point(data=data, aes(longitude, latitude), size=1, color="blue", alpha=0.8)+
  abs(x="Longitude", y="Latittude", title="")

#leaflet interactive map (OpenStreetMap)
data %>% 
  summarize(
    lon_min=min(longitude, na.rm=T),
    lon_max=max(longitude, na.rm=T),
    lat_min=min(latitude, na.rm=T),
    lat_max=max(latitude, na.rm=T
    ))
lon_min<--
lon_max<--
lat_min<--
lat_max<--

  leaflet(data) %>% 
  addProviderTiles(providers$Stadia.StamenTerrain) %>% 
  addCircleMarkers(
    lng = ~longitude,
    lat= ~latitude,
    radius=2,
    stroke= FALSE,
    fillOpacity = 0.7
  ) %>% 
  addScaleBar(position="bottomleft") %>% 
  fitBounds(lng1=lon_min, lat1=lat_min,
            lng2=lon_max, lat2=lat_max)

leaflet(data) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircleMarkers(
    lng = ~longitude,
    lat= ~latitude,
    radius=2,
    stroke= FALSE,
    fillOpacity = 0.7
  ) %>% 
  addScaleBar(position="bottomleft") %>% 
  fitBounds(lng1=lon_min, lat1=lat_min,
            lng2=lon_max, lat2=lat_max)

#Shiny App

ui <- fluidPage()

server <- function(input, output, session) {}

shinyApp(ui, server)
















