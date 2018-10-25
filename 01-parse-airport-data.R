
# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# data ---------------------------------------------

# if we haven't read the file yet, read it in
if(!exists('jan2017')) {
  
  # if the file doesn't exist yet, download it
  if( !file.exists('data/airline_data_mco_2017.csv')) {
    
      # you may need to use a different method argument for Mac/Linux
      # download.file(
            # url = 'http://tsdata.bts.gov/PREZIP/On_Time_On_Time_Performance_2015_1.zip'
          # , destfile = 'data/On_Time_On_Time_Performance_2015_1.zip'
      # )
    
      # unzip the data
      unzip('data/On_Time_On_Time_Performance_2017_1.zip', exdir = 'data')
  }


  # read in the file
  jan2017  <- read.csv('data/airline_data_mco_2017.csv'
                , stringsAsFactors = F
  )  
}


# initial manipulation ----------------------------------------------------

airlinedata <- jan2017  %>%  
  mutate(
      # make the flight date into a date
      FlightDate = ymd(FL_DATE)
    , DepTime    = ymd_hm(
                     paste(
                          FL_DATE
                        , sub("-.*", "", DEP_TIME_BLK )
                     ) 
                   )
      
      # turn category data into factors
    , Carrier         = factor(UNIQUE_CARRIER)
    , OriginAirportID = factor(ORIGIN)
    , Origin          = factor(ORIGIN_CITY_NAME)
    # , OriginState     = factor(OriginState)
    , DestAirportID   = factor(DEST)
    , Dest            = factor(DEST_CITY_NAME)
    
    # , hasweatherdelay = !is.na(WeatherDelay)
  )

# use a random subset of the data for general demonstration purposes ----------------------------

set.seed(123) # so we all get the same data
mydata <- sample_n(airlinedata, 10000)  %>% 
  tbl_df
  
