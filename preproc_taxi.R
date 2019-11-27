# --------------------------------------------------------------
# 95-845 AAMLP: Final Project
# Nathan Deron, David Contreras, Laura Goyeneche
# Last update: November 21, 2019
# --------------------------------------------------------------

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
  # Taxi trips: 1.3 out of 187 million observations 

  df_taxi  = readRDS(paste0(cd, '/data_rds/df_taxi.rds'))

# Data pre-processing 
# --------------------------------------------------------------

# Mutate and create variables
# --------------------------------------------------------------
  
# Transformation of variables' names
names(df_taxi) = tolower(names(df_taxi))
names(df_taxi) = gsub(".", "_", names(df_taxi), fixed = T)

# Create new dataset
taxi = 
  df_taxi %>%
  
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
         -dropoff_centroid__location) %>%
  
  # Generate factor variables
  mutate(payment_type = as.factor(payment_type),
         company      = as.factor(company),
         trip_id      = as.factor(trip_id),
         taxi_id      = as.factor(taxi_id)) %>%
  
  # Destring numeric variables
  mutate_if(is.character, as.numeric) %>%
  
  # Create hour start trip
  mutate(hm_start = format(date_start, "%H:%M"),
         h_start  = format(date_start, "%H"))

# Summary NAs
# --------------------------------------------------------------

taxi %>%
  #filter(day == 1) %>%
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

  # Max # NA: pickup and dropoff centroid, CA and census tract
  # centroid    : (7% , 10%) 
  # CA          : (7% , 10%)
  # census track: (34%, 35%)
  
  # For Day 1: 27,267 observations
  # centroid    : (6% , 9%) 
  # CA          : (6% , 9%)
  # census track: (46%, 48%)
  
  # centroid ~ CA, make sense that have the same range of NAs

# Evaluate missigness patterns
# --------------------------------------------------------------

# Part 1:
# CA and Census track vs day and hour
# --------------------------------------------------------------

# Community area
table(taxi$day, is.na(taxi$pickup_community_area)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)
  
  # No clear pattern
  
table(taxi$hm_start, is.na(taxi$pickup_community_area))

  # Between 7:45 and 20:15 are higher number of NAs
  # However, as proportion, there isn't clear pattern

table(taxi$h_start, is.na(taxi$pickup_community_area)) %>%
  as.data.frame.array(optional = T) %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)

  # Using just the hour, we observe that between 7:00 - 9:00 and 16:00 they're more NA

# Census tract
table(taxi$day, is.na(taxi$pickup_census_tract)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)
  
  # No clear pattern
  # Only for day 8th and and 29th we observe more NAs
  # We'll assume that this isn't problematic and they aren't significant diff

table(taxi$hm_start, is.na(taxi$pickup_census_tract))

  # More NAs between 9:45 and 20:30 
  # As proportion we observe a clear reduction in NA during those hours
  # Between 4:00 and 5:45 higher NA as proportion

table(taxi$h_start, is.na(taxi$pickup_census_tract)) %>%
  as.data.frame.array() %>% 
  mutate(NoP   = round(`FALSE`/rowSums(.),3)*100,
         YesP  = round(`TRUE`/rowSums(.),3)*100) %>%
  rename(No    = `FALSE`,
         Yes   = `TRUE`)

  # By hour we observe a higher proportion of NA betwwn 22:00 and 6:00
  # Higher NA early morning and late night

# Part 2:
# Visualization of NA across variables of interest
# --------------------------------------------------------------

taxi %>%
  select(-trip_id,
         -fare,
         -tips,
         -tolls, 
         -extras,
         -pickup_centroid_latitude,
         -pickup_centroid_longitude,
         -dropoff_centroid_latitude,
         -dropoff_centroid_longitude,
         -company,
         -date_start,
         -date_end,
         -year,
         -month) %>%
  filter(day == 1) %>%
  missing_plot()
  
  # For every day, there is no important differences
  # No clear relation between the observations and NA values

taxi %>%
  select(-trip_id,
         -fare,
         -tips,
         -tolls, 
         -extras,
         -pickup_centroid_latitude,
         -pickup_centroid_longitude,
         -dropoff_centroid_latitude,
         -dropoff_centroid_longitude,
         -company,
         -date_start,
         -date_end,
         -year,
         -month) %>%
  filter(day == 1) %>% 
  #filter(day < 11) %>%
  missing_pattern() 

  # Day 1 findings
    # 10 different NA patterns
    # Around 452 rows of 4 out 15 variables have NAs
  
    # For fare, tips, tolls, extras, trip total: 
      # For some days they present same NA, but across days there isn't clear pattern
      # Because of the collinearity between this variables, we have in a same pattern group NAs for all of them
      # Solution: MI + random indicators
  
    # No clear pattern with community area NAs
    # CA NA might be correlated with census tract NAs

  # Day 1 - 10 findings
    # 32 different NA patterns
    # Having a large number of patterns indicates that the pattern is random rather than systematic
    # Same observation for trip costs variables

  # Higher number of days included in the analysis, higher number of different patterns

  table(is.na(taxi$pickup_community_area), is.na(taxi$pickup_census_tract))
  
    # Approx. 7% of the observation present NA in both variables
    # Those observation with NA in CA, also present NA in census tract
  
taxi %>% 
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

taxi %>%
  filter(!is.na(pickup_community_area), !is.na(dropoff_community_area)) %>% 
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

  # By ommiting CA NAs, census tract presents 24% of missigness
  
taxi %>% 
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
  
taxi = 
  taxi %>% 
  
  # Drop ~6.2% of the observations
  filter(!is.na(pickup_community_area), !is.na(dropoff_community_area))

rm(df.in)

# Imputation Procedure
# --------------------------------------------------------------

NA_cont = 
  mice(taxi %>%
         select(trip_seconds,
                trip_miles,
                fare, 
                tips,
                tolls, 
                extras,
                trip_total), 
       m      = 2,
       maxit  = 2, 
       method = "pmm")

NA_cat = 
  mice(taxi %>%
         select(taxi_id, 
                pickup_census_tract,
                dropoff_census_tract) %>%
         mutate(taxi_id = as.numeric(taxi_id)),
       m      = 2, 
       maxit  = 2, 
       method = "mean")

# Get imputed data
cont_imput = 
  mice::complete(NA_cont, action = 1) %>% 
  as_tibble()

cat_imput = 
  mice::complete(NA_cat, action = 1) %>%
  as_tibble()

# Rename imputed variables
names(cont_imput) = lapply(names(cont_imput), paste0, "_imputed")
names(cat_imput)  = lapply(names(cat_imput), paste0, "_imputed")

# Remove NA cat and cont
rm(NA_cat, NA_cont)

# Generate final data 
taxi_imput = 
  taxi %>%
  select(-trip_seconds,
         -trip_miles,
         -fare, 
         -tips,
         -tolls, 
         -extras,
         -trip_total,
         -taxi_id, 
         -pickup_census_tract,
         -dropoff_census_tract) %>%
  bind_cols(cont_imput, 
            cat_imput,
            (is.na(taxi %>% 
                     select(trip_seconds,
                            trip_miles,
                            fare,
                            tips,
                            tolls,
                            extras,
                            trip_total,
                            taxi_id,
                            pickup_census_tract,
                            dropoff_census_tract))) %>%
                     as_tibble() %>% 
                     rename_all(~ paste0(., "_missing"))) %>% 
  as_tibble()

# Missigness validation

taxi_imput %>%
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

  # Perfect: no NAs

# Remove cat and cont imput
rm(cat_input, cont_input)

# Save data final in RDS format
saveRDS(taxi_imput , paste0(cd, '/data_rds/df_taxi_imputed.rds'))

# --------------------------------------------------------------