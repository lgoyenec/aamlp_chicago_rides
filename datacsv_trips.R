# Dats .csv to .rds
# -------------------------------------------------------------------

rm(list = ls())

# Working directory
cd = 'C:/Users/lgoye/OneDrive/Education/CarnegieMellonUniversity/PPMDACourses/95845_ML_Pipeline/Final Project'
setwd(cd)

# Import data
# City of Chicago

# Import 3 datasets
# Taxi trips 1.3 out of 187 million observations 
# Other providers trips: 8.9 out of 101 million observations
# Crime in Chicago: 35,494 out of 7 million observations

  df_taxi  = read.csv(paste0(cd, '/data_csv/taxi_trips.csv'))        %>% as_tibble()
  df_other = read.csv(paste0(cd, '/data_csv/ridesharing_trips.csv')) %>% as_tibble()
  df_crime = read.csv(paste0(cd, '/data_csv/crimes_chicago.csv'))    %>% as_tibble()
  
  # Save in R format
  # Around 10 times faster than csv format
  
  saveRDS(df_taxi , paste0(cd, '/data_rds/df_taxi.rds'))
  saveRDS(df_other, paste0(cd, '/data_rds/df_other.rds'))
  saveRDS(df_crime, paste0(cd, '/data_rds/df_crime.rds'))

# -------------------------------------------------------------------