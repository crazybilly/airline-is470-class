
# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyr)

library(broom)


# linear model arrival delay as a function of departure delay ------------------------------------

arrivalfdeparture <- lm(ARR_DELAY ~ DEP_DELAY, data = airlinedata)


# linear model arrival delay v day of the month and departure delay -------------------------------


delayfothers   <- lm(ARR_DELAY ~ DAY_OF_MONTH + DEP_DELAY, data = airlinedata)

readablefothers  <- broom::tidy(delayfothers) 

# are some carriers worse than others? -----------------------------


delay_by_carrier <- mydata %>% 
  group_by(Carrier) %>% 
  summarize(
      n = n()
    , total_delay  = sum(ARR_DELAY, na.rm = T)
    , mean_delay   = mean(ARR_DELAY, na.rm = T) 
    , median_delay = median(ARR_DELAY, na.rm = T) 
  )  %>% 
  arrange(-mean_delay)


#make a model
delaybycarriermodel <- lm( ARR_DELAY ~ Carrier, data = airlinedata) 


# lets plot out our coefficients

  # get the confidence intervals
  confintcarrier <- confint(delaybycarriermodel)

  # make a data frame with all the data we want in it
  confdf  <- data_frame(
      carrier = sub("Carrier", "", names(delaybycarriermodel$coefficients) )
    , coef = delaybycarriermodel$coefficients
    , low  = confintcarrier[,1]
    , high = confintcarrier[,2]
  )
  
  # plot it out
  ggplot(confdf, aes(x = carrier)) + 
    
    # dot the coefficients
    geom_point(aes(y = coef)) + 
    
    # draw the range
    geom_errorbar(aes(ymin = low, ymax = high)) + 
    
    # label the bar above it for clarity
    geom_text(aes(label = carrier, y = high+.02))

  

