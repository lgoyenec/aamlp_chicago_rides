# --------------------------------------------------------------
# 95-845 AAMLP: Final Project
# Nathan Deron, David Contreras, Laura Goyeneche
# Last update: November 27, 2019
# --------------------------------------------------------------

rm(list = ls())

# Define libraries
# --------------------------------------------------------------
libs = c('dplyr',
         'lubridate',
         'rgdal',
         'fastDummies')

# Attach libraries
invisible(suppressMessages(lapply(libs, library, character.only = T)))

# Working directory
# --------------------------------------------------------------
cd = 'C:/Users/lgoye/OneDrive/Documents/Github/aamlp_chicago_rides'
setwd(cd)

# Import data
df_taxi   = readRDS(paste0(cd, '/data_rds/df_taxi_imputed.rds'))
df_other  = readRDS(paste0(cd, '/data_rds/df_other_imputed.rds'))
df_crime  = readRDS(paste0(cd, '/data_rds/df_crime_imputed.rds'))

  # Rename variables
  names(df_taxi)  = gsub("_imputed", "", names(df_taxi), fixed = T)
  names(df_other) = gsub("_imputed", "", names(df_other), fixed = T)

# Import shapefile
sh_census = readOGR(dsn    = paste0(cd, '/data_shp'), 
                   layer   = 'census_tract',
                   verbose = F)


# Classify crimes by census tract
# --------------------------------------------------------------
# Select longitude and latitude
temp = 
  df_crime %>%
  select(id, latitude_imputed, longitude_imputed) %>%
  rename(latitude  = latitude_imputed,
         longitude = longitude_imputed)

# Create coordinates
coordinates(temp) = c("longitude", "latitude")

# Assign same reference system
proj4string(temp) = proj4string(sh_census)

# Assign census geoid to each crime id 
temp$census = over(temp, sh_census)$geoid10

# Merge crime with temp 
df_crime = left_join(df_crime, temp@data, by = "id")

rm(temp)

# Drop those observation without census id 
df_crime =
  df_crime %>%
  filter(!is.na(census))

# Collapse df_crime data
crime = 
  df_crime %>%
  select(-id, 
         -date, 
         -primary_type,
         -location_description,
         -beat, 
         -district,
         -year, 
         -month, 
         -hm_start, 
         -h_start,
         -ward_imputed,
         -community_area_imputed,
         -latitude_imputed,
         -longitude_imputed,
         -ward_missing,
         -community_area_missing,
         -latitude_missing,
         -longitude_missing) %>%
  mutate(total_crime = 1) %>%
  group_by(census, day) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(census = as.numeric(as.character(census)))

# Merge crime with df_taxi and df_other
# --------------------------------------------------------------

# Create temporary data for pickup and dropoff
# Pickup
  temp = crime 
  names(temp) = paste0("pickup_", names(temp))
  
  temp  = temp %>% rename(pickup_census_tract = pickup_census, day = pickup_day)
  taxi  = left_join(df_taxi, temp, by = c("pickup_census_tract","day"))
  other = left_join(df_other, temp, by = c("pickup_census_tract","day"))

# Dropoff
  temp = crime 
  names(temp) = paste0("dropoff_", names(temp))
  
  temp  = temp %>% rename(dropoff_census_tract = dropoff_census, day = dropoff_day)
  taxi  = left_join(taxi, temp, by = c("dropoff_census_tract","day"))
  other = left_join(other, temp, by = c("dropoff_census_tract","day"))

rm(temp)

# Append taxi and other (ride sharing services)
# --------------------------------------------------------------
df_final = 
  taxi %>%
  select(-taxi_id,
         -taxi_id_missing,
         -tolls,
         -tolls_missing,
         -payment_type,
         -company) %>%
  bind_rows((other %>%
               select(-shared_trip_authorized,
                      -trips_pooled))) %>%
  replace(., is.na(.), 0)

# --------------------------------------------------------------