
# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyr)

library(broom)
# data ---------------------------------------------


# linear model hasweather v delay time ------------------------------------

delayfweather  <- lm(ArrDelay ~ hasweatherdelay , data = ourdata)


# linear model delay time v weather + state -------------------------------


delayfothers   <- lm(ArrDelay ~ hasweatherdelay + OriginState, data = ourdata)

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
    , pct_weather = sum(hasweatherdelay)/n() 
  )


weatherfstate  <- lm( hasweatherdelay ~ OriginState, data = ourdata) 

anovaweather  <- anova(weatherfstate)
confintweather  <- confint(weatherfstate)

boxplot(ourdata$hasweatherdelay ~ ourdata$OriginState)
