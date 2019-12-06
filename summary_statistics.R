# --------------------------------------------------------------
# 95-845 AAMLP: Final Project
# Nathan Deron, David Contreras, Laura Goyeneche
# Last update: November 21, 2019
# Summary statistics
# --------------------------------------------------------------


#load required libraries:


loadlibs = function(libs) {
  for(lib in libs) {
    class(lib)
    if(!do.call(require,as.list(lib))) {install.packages(lib)}
    do.call(require,as.list(lib))
  }
}
libs = c('ggplot2', 'dplyr', 'sf', 'here', 'leaflet', 'stringr', 'widgetframe', 'magrittr', 
         'devtools', 'mapview', 'htmlwidgets','pastecs', 'xtable')
loadlibs(libs)


#Setting working directory

setwd(getwd())

#______________________________________________________________
#load the final dataset:

final_dataset = readRDS("./data_rds/df_final.rds")

#______________________________________________________________
#Maps of number of pick-ups and drop-off per community area

## Import community area shapefiles:

areas <- here("data_shp", "community_area.shp") %>%
  st_read() %>% mutate(community = str_to_title(community))

## Plot the maps

### Map of pickup area:

#### Create the dataset and join with the shapefile
(areas_pickup <- areas %>%
    dplyr::select(community, area_numbe) %>%
    mutate(area_numbe = as.numeric(as.character(area_numbe))) %>%
    left_join(final_dataset %>% dplyr::select(pickup_community_area) %>% 
                group_by(pickup_community_area) %>%
                summarise(count=n()) %>% set_names(c('variable', 'number')) %>%
                mutate(variable = as.numeric(as.character(variable))),
              by = c("area_numbe" = "variable")))

#### Create a color bin and save the map

pal <- colorBin("YlOrRd", domain = areas_pickup$number)


map_pickup = areas_pickup %>%
                leaflet() %>%
                addTiles() %>%
                addPolygons(label = ~community,
                  fillColor = ~pal(number),
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
                addLegend(pal = pal,
                    values = ~number,
                    opacity = 0.7,
                    title = NULL,
                    position = "bottomright") %>%
                frameWidget()
rm(areas_pickup)
#### Save the image as png file.

#saveWidget(map_pickup, file="m.html")

#___________________________________________________
## Map of drop off area:

#### Create the dataset and join with the shapefile
(areas_dropoff <- areas %>%
    dplyr::select(community, area_numbe) %>%
    mutate(area_numbe = as.numeric(as.character(area_numbe))) %>%
    left_join(final_dataset %>% dplyr::select(dropoff_community_area) %>% 
                group_by(dropoff_community_area) %>%
                summarise(count=n()) %>% set_names(c('variable', 'number')) %>%
                mutate(variable = as.numeric(as.character(variable))),
              by = c("area_numbe" = "variable")))

#### Create a color bin and save the map

pal <- colorBin("YlOrRd", domain = areas_dropoff$number)


map_dropoff = areas_dropoff %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(label = ~community,
              fillColor = ~pal(number),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = ~number,
            opacity = 0.7,
            title = NULL,
            position = "bottomright") %>%
  frameWidget()
rm(areas_dropoff)
#### Save the image as png file.

#saveWidget(map_dropoff, file="m.html")
rm(map_dropoff)


## Histogram: number of trips per miles:

## Choose subset of data randomly

final_dataset <- final_dataset[final_dataset(1:nrow(final_dataset), rodu ,
                          replace=FALSE),]


size = round(0.01*nrow(final_dataset))
final_dataset_sample <- final_dataset[sample(1:nrow(final_dataset), size,
                          replace=FALSE),]

### Drop observations if trip miles are greater than 50 miles:

table_stats = stat.desc(final_dataset_sample%>%select(trip_miles, fare,  pickup_black.pop, pickup_white.pop,
                                        pickup_asian.pop, pickup_amindian.pop, pickup_pacific.pop), desc=F)

xtable(table_stats, type = "latex", file = "table_summary.tex")
