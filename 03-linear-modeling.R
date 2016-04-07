
# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyr)

library(broom)


# linear model hasweather v delay time ------------------------------------

arrivalfdeparture <- lm(ArrDelayMinutes ~ DepDelayMinutes , data = ourdata)


# linear model delay time v weather + state -------------------------------


delayfothers   <- lm(ArrDelay ~ hasweatherdelay + DepDelayMinutes , data = ourdata)

readablefothers  <- broom::tidy(delayfothers)  %>% 
  mutate(
    term = sub("factor\\(OriginState\\)","",term)
  )  %>% 
  arrange(-estimate)



# are some states worse about weather delays? -----------------------------


weatherdelaybystate  <- ourdata  %>% 
  select(hasweatherdelay, OriginState)  %>% 
  group_by(OriginState)  %>% 
  summarize(
      n_weather = sum(hasweatherdelay)
    , n = n()
    , pct_with_weather = sum(hasweatherdelay)/n() 
  )  %>% 
  arrange(-pct_with_weather)


# is a box plot useful?
boxplot(ourdata$hasweatherdelay ~ ourdata$OriginState)

#make a model
weatherfstate  <- lm( hasweatherdelay ~ OriginState, data = ourdata) 

# are our overall results significant?
anovaweather  <- anova(weatherfstate)

# lets plot it out
  # get the confidence intervals
  confintweather  <- confint(weatherfstate)

  # make a data frame with all the data we want in it
  confdf  <- data_frame(
      state = sub("OriginState", "", names(weatherfstate$coefficients) )
    , coef = weatherfstate$coefficients
    , low = confintweather[,1]
    , high = confintweather[,2]
  )
  
  # plot it out
  ggplot(confdf, aes(x = state)) + 
    
    # dot the coefficients
    geom_point(aes(y = coef)) + 
    
    # draw the range
    geom_errorbar(aes(ymin = low, ymax = high)) + 
    
    # label the bar above it for clarity
    geom_text(aes(label = state, y = high+.02))

  

