# libraries -----------------------------


library(dplyr)
library(lubridate)
library(tidyr)

library(ggplot2)


# general data analysis ---------------------------------------------------

# general form
str(bearabledata)
str(bearabledata, list.len = 999 )

# size and columns
dim(bearabledata)
nrow(bearabledata)
names(bearabledata)

# "I can't see it all!!"
View(bearabledata)

# what am I dealing with
class(ourdata)
class(bearabledata)
class(ourdata$Year)
class(ourdata$Carrier)


# subsetting data with traditional R methods -------------------------------

  # with $ (returns a vector)
  ourdata$DepDelayMinutes
  
  # with single [ ]
  ourdata[3] # a 1 column data frame (because data frames are lists)
  ourdata[1:2, 3:5] # a 2:3 data frame
  
  # with double [[ ]]
  ourdata[[3]] # a vector
  ourdata[[2, 3]] # a single value
  
  
# subsetting and manipulation with dplyr ---------------------------
  
  
  # subsetting
  bearable2 <- bearabledata  %>% 
    # subset by criteria
    filter(
        !is.na(FlightDate)
      , OriginState == 'TX'
      , DepDelayMinutes <= 100
      , !is.na(ArrivalDelayGroups)
    )   %>% 
    # only grab the columns we need and rename a few
    select(
        FlightDate
      , Carrier
      , city = OriginCityName
      , st = OriginState
      , ddelay = DepDelayMinutes
      , adelay = ArrDelayMinutes
      , adelaygroup = ArrivalDelayGroups
    )  %>% 
    # add calculated columns
    mutate(
        logddelay = log(ddelay + 1)
      , logadelay = log(adelay + 1)
      , fromDallas = grepl("Dallas", city)
      , delaytype = ifelse(adelay >= 30, "big delay", "small delay")
    )
  
  
  # quick pivot tables
    # summaries by Destination State
  bearabledata %>% 
    group_by(DestState)  %>% 
    summarize(
        n = n()
      , meanDelay = mean(ArrDelay, na.rm = T)
      , totalDelayMin = sum(ArrDelayMinutes, na.rm = T)
    )
  
  # advanced pivots 
  #    ie. going from long to wide, see http://vita.had.co.nz/papers/tidy-data.pdf
  #   summaries with carrier in rows and delay group in columns
  bearable2  %>% 
    group_by(Carrier, adelaygroup)  %>% 
    summarize(n = n() )  %>% 
    tidyr::spread( adelaygroup, n, fill = 0)  %>% 
    arrange(-`-2`)
    
  

# general statistical analysis -------------------------------------
  
summary(bearabledata$DepDelayMinutes)
  
sum(bearabledata$DepDelayMinutes, na.rm = T)
mean(bearabledata$DepDelayMinutes, na.rm = T)
median(bearabledata$DepDelayMinutes, na.rm = T)
IQR(bearabledata$DepDelayMinutes, na.rm = T)
quantile(bearabledata$DepDelayMinutes, na.rm = T)


# general plotting --------------------------------------------------------

foo  <- rnorm(100)
foo2  <- rnorm(100)
bar  <- rnorm(6)
boxplot(foo,bar)

hist(bearabledata$ArrivalDelayGroups)

plot(bearabledata$FlightDate, bearabledata$ArrDelayMinutes)



# better plotting with ggplot2 --------------------------------------------

# dates vs. Arrival delay, color coded by carrier
ggplot(bearabledata, aes(x = FlightDate, y = ArrDelayMinutes)) + 
  geom_point(aes(color = Carrier)) + 
  geom_smooth() 

# dates vs. Arrival delay, color coded by carrier
    # with ymax = 100 and some jitter
ggplot(bearabledata, aes(x = FlightDate, y = ArrDelayMinutes)) + 
  geom_jitter(alpha = .5, aes(color = Carrier)) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0,100))


# dates vs. Arrival delay, color coded and faceted by carrier
    # with ymax = 100 and some jitter
ggplot(bearabledata, aes(x = FlightDate, y = ArrDelayMinutes)) + 
  geom_jitter(alpha = .5, aes(color = Carrier)) + 
  geom_smooth() + 
  coord_cartesian(ylim = c(0,100)) + 
  facet_wrap(~Carrier)


