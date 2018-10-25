# libraries -----------------------------


library(dplyr)
library(lubridate)
library(tidyr)

library(ggplot2)


# general data analysis ---------------------------------------------------

# general form
glimpse(mydata)
glimpse(mydata, list.len = 999 )

# size and columns
dim(mydata)
nrow(mydata)
names(mydata)

# "I can't see it all!!"
View(mydata)

# what am I dealing with
class(airlinedata)
class(mydata)
class(airlinedata$MONTH)
class(airlinedata$UNIQUE_CARRIER)


# subsetting data with traditional R methods -------------------------------

  # with $ (returns a vector)
  airlinedata$DEP_DELAY
  
  # with single [ ]
  airlinedata[3] # a 1 column data frame (because data frames are lists)
  airlinedata[1:2, 3:5] # a 2:3 data frame
  
  # with double [[ ]]
  airlinedata[[3]] # a vector
  airlinedata[[2, 3]] # a single value
  
  
# subsetting and manipulation with dplyr ---------------------------
  
  
  # subsetting
  bearable2 <- mydata  %>% 
    
    # subset by criteria
    filter(
        !is.na(FlightDate)
      , str_detect(Origin, "TX") # origin has "TX" in it
      , DEP_DELAY <= 100
      , !is.na(ARR_DELAY)
    )   %>% 
    
    # only grab the columns we need and rename a few
    select(
        FlightDate
      , Carrier
      , fromcity = Origin
      , ddelay = DEP_DELAY
      , adelay = ARR_DELAY
    )  %>% 
    
    # add calculated columns
    mutate(
        logddelay = log(ddelay + 1)
      , logadelay = log(adelay + 1)
      , fromDallas = grepl("Dallas", fromcity)
      , delaytype = ifelse(adelay >= 30, "big delay", "small delay")
    )
  
  
  # quick pivot tables
    # summaries by Destination State
  mydata %>% 
    group_by(Carrier)  %>% 
    summarize(
        n = n()
      , meanDelay = mean(ARR_DELAY, na.rm = T)
      , totalDelayMin = sum(ARR_DELAY, na.rm = T)
    )
  
  # advanced pivots 
  #    ie. going from long to wide, see http://vita.had.co.nz/papers/tidy-data.pdf
  #   summaries with carrier in rows and delay group in columns
  bearable2  %>% 
    group_by(Carrier, delaytype)  %>% 
    summarize(n = n() )  %>% 
    tidyr::spread( delaytype, n, fill = 0)  %>% 
    arrange(-`big delay`)
    
  

# general statistical analysis -------------------------------------
  
summary(mydata$DEP_DELAY)
  
sum(mydata$DEP_DELAY, na.rm = T)
mean(mydata$DEP_DELAY, na.rm = T)
median(mydata$DEP_DELAY, na.rm = T)
IQR(mydata$DEP_DELAY, na.rm = T)
quantile(mydata$DEP_DELAY, na.rm = T)


# general plotting --------------------------------------------------------

# histogram of arrival delays
hist(mydata$ARR_DELAY)

# boxplot of delay by carrier
boxplot(mydata$ARR_DELAY ~ mydata$Carrier)
boxplot(mydata$ARR_DELAY ~ mydata$Carrier, ylim = c(0, 40) )

# delay over time 
plot(mydata$FlightDate, mydata$ARR_DELAY)


# better plotting with ggplot2 --------------------------------------------

# dates vs. Arrival delay, color coded by carrier
ggplot(mydata, aes(x = FlightDate, y = ARR_DELAY)) + 
  geom_point(aes(color = Carrier)) + 
  geom_smooth() 

# dates vs. Arrival delay, color coded by carrier
    # with ymax = 100 and some jitter
ggplot(mydata, aes(x = FlightDate, y = ARR_DELAY)) + 
  geom_jitter(alpha = .5, aes(color = Carrier)) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0,100))


# dates vs. Arrival delay, color coded and faceted by carrier
    # with ymax = 100 and some jitter
ggplot(mydata, aes(x = FlightDate, y = ARR_DELAY)) + 
  geom_jitter(alpha = .5, aes(color = Carrier)) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0,100)) + 
  facet_wrap(~Carrier)


