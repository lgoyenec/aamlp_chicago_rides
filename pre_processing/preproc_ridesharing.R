# --------------------------------------------------------------
# 95-845 AAMLP: Final Project
# Nathan Deron, David Contreras, Laura Goyeneche
# Last update: November 21, 2019
# --------------------------------------------------------------

rm(list = ls())

# Define libraries
# --------------------------------------------------------------
libs = c('httr','jsonlite',
         "finalfit",
         'dplyr',
         'stringr',
         'readr','data.table',
         'lubridate',
         'mice', 'lattice', 'VIM',
         'knitr','kableExtra',
         'fastDummies',
         'papeR',
         'ggcorrplot','plotly','GGally',
         'leaflet')

# Attach libraries
invisible(suppressMessages(lapply(libs, library, character.only = T)))

# Working directory
# --------------------------------------------------------------
cd = 'C:/Users/lgoye/OneDrive/Documents/Github/aamlp_chicago_rides'
setwd(cd)

# Import data
# City of Chicago
  # Other providers trips: 8.9 out of 101 million observations

df_other = readRDS(paste0(cd, '/data_rds/df_other.rds'))

# Data pre-processing 
# --------------------------------------------------------------

# Transformation of variables' names
names(df_other) = tolower(names(df_other))
names(df_other) = gsub(".", "_", names(df_other), fixed = T)

# Create new dataset
other = 
  df_other %>%
  
  # Create time variables: year, month, hour, day
  mutate(date_start = mdy_hms(trip_start_timestamp %>% as.character()),
         date_end   = mdy_hms(trip_end_timestamp %>% as.character()),
         year       = year(date_start),
         month      = month(date_start),
         day        = day(date_start)) %>%
  
  # Remove timestamp variables and pickup location
  select(-trip_start_timestamp,
         -trip_end_timestamp,
         -pickup_centroid_location,
         -dropoff_centroid_location) %>%
  
  # Generate factor variables
  mutate(trip_id = as.factor(trip_id)) %>%
  
  # Rename additional charges and tip
  # Same name as df_taxi dataset
    
  rename(extras = additional_charges,
         tips   = tip) %>% 
  
  # Destring numeric variables
  mutate_if(is.character, as.numeric) %>%
  
  # Create hour start trip
  mutate(hm_start = format(date_start, "%H:%M"),
         h_start  = format(date_start, "%H"))

# Summary NAs
# --------------------------------------------------------------

other %>%
  #filter(day == 1) %>%
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

  # Max # NA: pickup and dropoff centroid, CA and census tract
  # centroid    : (6% , 8%) 
  # CA          : (6% , 8% )
  # census track: (28%, 30%)
  
  # For Day 1: 309,334 observations
  # Only NAs in centroid, CA, and census track
  # centroid    : (0.19%, 0.24%)
  # CA          : (0.20%, 0.24%)
  # census track: (0.94%, 0.97%)
  
  # centroid ~ CA, make sense that have the same range of NAs

# Evaluate missigness patterns
# --------------------------------------------------------------

# Part 1:
# CA and Census track vs day and hour
# --------------------------------------------------------------

# Community area
table(other$day, is.na(other$pickup_community_area)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)

  # No clear pattern

table(other$hm_start, is.na(other$pickup_community_area))

  # Between 12:00 and 18:15 are higher number of NAs
  # However, as proportion, there isn't clear pattern

table(other$h_start, is.na(other$pickup_community_area)) #%>%
  as.data.frame.array(optional = T) %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)

  # Using just the hour, we observe that between 5:00 - 18:00 they're more NAs

# Census tract
table(other$day, is.na(other$pickup_census_tract)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)
  
  # No clear pattern
  # We'll assume that this isn't problematic and they aren't significant diff
  
table(other$hm_start, is.na(other$pickup_census_tract))
  
  # More NAs between 13:15 and 18:15 
  # As proportion we observe a clear reduction in NA during those hours

table(other$h_start, is.na(other$pickup_census_tract)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)
  
  # By hour we observe a higher proportion of NA between 4:00 and 6:00
  # Higher NA early morning

# Part 2:
# Visualization of NA across variables of interest
# --------------------------------------------------------------

other %>%
  select(-trip_id,
         -fare,
         -tips,
         -extras,
         -pickup_centroid_latitude,
         -pickup_centroid_longitude,
         -dropoff_centroid_latitude,
         -dropoff_centroid_longitude,
         -date_start,
         -date_end,
         -year,
         -month) %>%
  filter(day == 1) %>%
  missing_plot()

  # For every day, there is no important differences
  # No clear relation between the observations and NA values

other %>%
  select(-trip_id,
         -fare,
         -tips,
         -extras,
         -pickup_centroid_latitude,
         -pickup_centroid_longitude,
         -dropoff_centroid_latitude,
         -dropoff_centroid_longitude,
         -date_start,
         -date_end,
         -year,
         -month) %>%
  filter(day == 2) %>% 
  #filter(day < 11) %>%
  missing_pattern() 

  # Day 1 (309,334 obs) findings
    # 12 different NA patterns
    # Around 8,000 rows of 3 out 12 variables have NAs
    
    # No clear pattern with community area NAs
    # CA NA might be correlated with census tract NAs
  
  # Higher number of days included in the analysis, higher number of different patterns
  # This is desirable, not clear correlation between the missigness patterns in the variables

table(is.na(other$pickup_community_area), is.na(other$pickup_census_tract))

  # Approx. 7% of the observation present NA in both variables
  # Those observation with NA in CA, also present NA in census tract
  # ~23% census obs with NA and no CA NAs

other %>% 
  filter(day == 1) %>%
  mutate(idCA       = ifelse(is.na(pickup_community_area),1,0),
         idCA2      = ifelse(is.na(pickup_community_area),0,1)) %>%
  select(idCA, idCA2, hm_start) %>% 
  group_by(hm_start) %>%
  summarise(n  = sum(idCA),
            n2 = sum(idCA2)) %>%
  plot_ly(x =~ hm_start, 
          y =~ n2,
          name = "NA",
          type = "bar") %>%
  add_trace(y =~ n,
            name = "No NA") %>%
  layout(barmode = "stack")

  # Decision with CA: drop values
  # We drop this observations, so that the # observations to be imputed is smaller 

other %>%
  filter(!is.na(pickup_community_area), !is.na(dropoff_community_area)) %>% 
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)
  
  # Only NA in trip_miles (28 obs) and pickup / dropoff census tract
  # By ommiting CA NAs, census tract presents 20% of missigness

other %>% 
  filter(!is.na(pickup_community_area), !is.na(dropoff_community_area)) %>% 
  filter(day == 1) %>%
  mutate(idCA       = ifelse(is.na(pickup_census_tract),1,0),
         idCA2      = ifelse(is.na(pickup_census_tract),0,1)) %>%
  select(idCA, idCA2, hm_start) %>% 
  group_by(hm_start) %>%
  summarise(n  = sum(idCA),
            n2 = sum(idCA2),) %>%
  plot_ly(x =~ hm_start, 
          y =~ n2,
          name = "NA",
          type = "bar") %>%
  add_trace(y =~ n,
            name = "No NA") %>%
  layout(barmode = "stack")

  # No clear NA pattern
  # Despite the large number of NA: we'll use multiple imputation

  # We still need to define if we have MCAR or MAR

other = 
  other %>% 
  
  # Drop ~6.5% of the observations
  filter(!is.na(pickup_community_area), !is.na(dropoff_community_area))

rm(df.in)

# Imputation Procedure
# --------------------------------------------------------------

NA_val = 
  mice(other %>%
         select(trip_miles,
                pickup_census_tract,
                dropoff_census_tract), 
       m      = 1,
       maxit  = 1, 
       method = "sample")

# Get imputed data
imput = 
  mice::complete(NA_val, action = 1) %>%
  as_tibble()

# Rename imputed variables
names(imput) = lapply(names(imput), paste0, "_imputed")

# Remove NA cat and cont
rm(NA_val)

# Generate final data 
other_imput = 
  other %>%
  select(-trip_miles,
         -pickup_census_tract,
         -dropoff_census_tract) %>%
  bind_cols(imput,
            (is.na(other %>% 
                     select(trip_miles,
                            pickup_census_tract,
                            dropoff_census_tract))) %>%
              as_tibble() %>% 
              rename_all(~ paste0(., "_missing"))) %>% 
  as_tibble()

# Missigness validation

other_imput %>%
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

# Perfect: no NAs

# Remove cat and cont imput
rm(imput)

# Save data final in RDS format
saveRDS(other_imput , paste0(cd, '/data_rds/df_other_imputed.rds'))

# --------------------------------------------------------------