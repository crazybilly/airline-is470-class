
# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyr)

# data ---------------------------------------------

# if we haven't read the file yet, read it in
if(!exists('jan2017')) {
  
  # if the file doesn't exist yet, download it
  if( !file.exists('data/On_Time_On_Time_Performance_2017_1.zip')) {
    
      # you may need to use a different method argument for Mac/Linux
      # download.file(
            # url = 'http://tsdata.bts.gov/PREZIP/On_Time_On_Time_Performance_2015_1.zip'
          # , destfile = 'data/On_Time_On_Time_Performance_2015_1.zip'
      # )
    
      # unzip the data
      unzip('data/On_Time_On_Time_Performance_2017_1.zip', exdir = 'data')
  }


  # read in the file
  jan2017  <- read.csv('data/On_Time_On_Time_Performance_2017_1.csv'
                , stringsAsFactors = F
  )  
}


# initial manipulation ----------------------------------------------------

ourdata  <- jan2017  %>%  
  mutate(
      # make the flight date into a date
      FlightDate = ymd(FlightDate)
    , DepTime    = ymd_hm(
                     paste(
                          FlightDate
                        , sub( '(..)$' , ':\\1' , DepTime )
                     ) 
                   )
      
      # turn category data into factors
    , Carrier         = factor(Carrier)
    , OriginAirportID = factor(OriginAirportID)
    , Origin          = factor(Origin)
    , OriginState     = factor(OriginState)
    , DestAirportID   = factor(DestAirportID)
    , Dest            = factor(Dest)
    
    , hasweatherdelay = !is.na(WeatherDelay)
  )

# use a random subset of the data for general demonstration purposes ----------------------------

set.seed(123) # so we all get the same data
bearabledata  <- sample_n(ourdata, 10000)  %>% 
  tbl_df
  
