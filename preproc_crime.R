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
    # Crime in Chicago: 35,494 out of 7 million observations

df_crime = readRDS(paste0(cd, '/data_rds/df_crime.rds'))

# Data pre-processing 
# --------------------------------------------------------------

# Transformation of variables' name
names(df_crime) = tolower(names(df_crime))
names(df_crime) = gsub(".", "_", names(df_crime), fixed = T)

# Create new dataset
crime = 
  df_crime %>%
  
  # Create time variables: month, day
  mutate(date  = mdy_hms(date %>% as.character()),
         month = month(date),
         day   = day(date)) %>%
  
  # Remove variables 
  select(-case_number,
         -block,
         -description,
         -iucr,
         -fbi_code,
         -x_coordinate, 
         -y_coordinate,
         -updated_on,
         -location) %>%

  # Destring numeric variables
  mutate_if(is.character, as.numeric) %>%
  
  # Create hour start trip
  mutate(hm_start = format(date, "%H:%M"),
         h_start  = format(date, "%H")) %>%
  
  # Arrest and Domestic as dummies
  mutate(arrest   = ifelse(arrest   == "true", 1, 0), 
         domestic = ifelse(domestic == "true", 1, 0))

# Create dummy variables for primary_type
crime = dummy_cols(crime, "primary_type", remove_first_dummy = T)

# Transformation of variables' name
names(crime) = tolower(names(crime))
names(crime) = gsub("primary_type", "pt", names(crime), fixed = T)

crime = 
  crime %>%
  rename(primary_type = pt)

# Summary NAs
# --------------------------------------------------------------
crime %>%
  is.na() %>%
  colSums() %>%
  data.frame(countNA = .) %>%
  add_rownames(var = "Variable") %>%
  filter(countNA > 0) %>%
  kable(col.names = c("","Number of NAs")) %>%
  kable_styling(full_width = F)

# Imputation Procedure
# --------------------------------------------------------------

NA_val = 
  mice(crime %>%
         select(ward,
                community_area,
                latitude,
                longitude), 
       m      = 4,
       maxit  = 4, 
       method = "pmm")

# Get imputed data
imput = 
  mice::complete(NA_val, action = 1) %>%
  as_tibble()

# Rename imputed variables
names(imput) = lapply(names(imput), paste0, "_imputed")

# Remove NA cat and cont
rm(NA_val)

# Generate final data 
crime_imput = 
  crime %>%
  select(-ward,
         -community_area,
         -latitude,
         -longitude) %>%
  bind_cols(imput,
            (is.na(crime %>% 
                     select(ward,
                            community_area,
                            latitude,
                            longitude))) %>%
              as_tibble() %>% 
              rename_all(~ paste0(., "_missing"))) %>% 
  as_tibble()

# Missigness validation

crime_imput %>%
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
saveRDS(crime_imput , paste0(cd, '/data_rds/df_crime_imputed.rds'))

# --------------------------------------------------------------