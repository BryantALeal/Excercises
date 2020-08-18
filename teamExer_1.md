---
title: 'Exercises'
author: "Bryant Leal, Adam Hoard, Kevin Huan, Suchit Das"
date: "08/18/2020"
output:
  pdf_document: 
    fig_width: 6
    fig_height: 4
  html_document:
    df_print: paged
---

# Visual Story Telling Part 1: Green Buildings

Green certification has been very popular in recent times, and it is important that the effects of being green certified are stratified from other effects since the economic implications of this decision can mean 5 million dollars invested or 5 million dollars wasted. To begin, we will be cleaning the data to include those buildings in the top 75% of buildings for size, which starts at 50891 Square Feet. 

```{r,cleaning, echo=FALSE}
green <- read.csv('greenbuildings.csv')
summary(green$size)
# removed the bottom 25% of buildings based on size. 
green_clean = subset(green, size >= 50891)
green_clean2 = subset(green_clean, stories == 15)
```
After cleaning the data, the leasing rate of a building (as a proxy for populariry) will be plotted against rent, and a color indicator will be used to get a high level stratification between a green rated building, and a regular building. 
```{r,inital_plot, echo=FALSE}

#lets plot the rent of buildings that are green_rating
library(ggplot2)
ggplot(data = green_clean) + geom_point(mapping = aes(x = leasing_rate , y = Rent, color = green_rating))
```

It seems that there are more non-green rated buildings that have the highest rents 
when compared to green rated buildings. Lets seperate these effects.

```{r,rent_faceted, echo=FALSE}
ggplot(data = green_clean) + geom_point(mapping = aes(x = leasing_rate , y = Rent, color = green_rating)) + facet_wrap(~green_rating, nrow = 1,labeller = label_both)
```

Buildings rated green seem to cluster toward a higher leasing rate yet do not exhibit higher rents. Could the size of the data set skew our insights?

```{r,rent_faceted2,echo=FALSE}
ggplot(data = green_clean) + geom_point(mapping = aes(x = leasing_rate , y = Rent, color = green_rating)) + facet_wrap(class_a~green_rating, nrow = 2,labeller = label_both)
```

We stratify by whether the building is green certified and if it is considered a high quality building. It is clear that rents for green buildings not in class_a have flat rents when compared to the non-certified counterpart. It is also interesting to see that class for non certified buildings did not change the pattern significantly. Lets check to see if we can stratify by amenities offered.

```{r, amenities, echo=FALSE}
#lets plot the rent of buildings that are Amenities 
ggplot(data = green_clean) + 
  geom_point(mapping = aes(x = leasing_rate , y = Rent, color = green_rating))+ facet_wrap(amenities~green_rating, nrow = 2,labeller = label_both)

```

Replacing class with amenities changes the tail of the non-certified buildings with amenities. It begins to thin out and look like the tail of the green rated buildings in class_a. We also see extereme outliers for rent in the bottom left plot. There is evidence that green rated buildings may not actually yield economic benefit on their own. 


```{r, amenities_facet,echo=FALSE}
ggplot(data = green_clean) + 
  geom_point(mapping = aes(x = leasing_rate , y = Rent, color=renovated)) + 
  facet_wrap(green_rating ~ amenities , nrow = 2,labeller = label_both) #both renovated and type A rooms are more sought after. 
#very intriguing...
#seems to be more telling issues with why rents are higher...

```

In this situation, the data is faceted by green rating and amenities while seperating the points by the indicator renovated. It is clear from these plots that while there is some variation in rent for green rated buildings with amenities, there exists even more variation for buildings that are not green rated, but do include amenities. 

```{r,stuff,echo=FALSE}
ggplot(data = green_clean) + 
  geom_point(mapping = aes(x = age , y = Rent, color=renovated)) + 
  facet_wrap( ~ green_rating , nrow = 2,labeller = label_both)
```

Lastly, we want to see if green rated buildings last much longer than traditional buildings. The X axis was changed to the age of the building in years. In both instances, it seems that older buildings in general usually need a renovation, which does not come as a surpise. However, the data shows an overwhelming number of non-green certified buildings being much older than green rated buildings. While this may not be an indication of green building longevity, it does scrutinize many of the benefits that green rated buildings are suppose to have.

```{r, similar_to_last, echo=FALSE}
ggplot(data = green_clean2) + 
  geom_point(mapping = aes(x = leasing_rate , y = Rent, color=renovated)) + 
  facet_wrap( ~ green_rating , nrow = 2,labeller = label_both) #both renovated and 

```

A way to potentially deal with the confounding of the variables is to shrink the data down to a smaller subset that would be indicative of the investment being made. For example, the data is subsetted by the top 75% of buildings by square footage and buildings that 15 stories. That way, we measure the relationships that are relevant to the investment at hand. 

### Conclusion

The stats guru's model is straight forward and easy to understand, but has some issues. Mainily, the analysis does not take into account the interrelated dependencies that the various predictors have with each other, so creating an answer with superficial data that does not respect these relationships may lead to an uninformed decision. For example, the analysis takes the median of rent for green rated buildings versus normal buildings, but could not account for buildings of higher quality or those that provided amenities. We cannot assume that a green building provides value, and the data seems to support the idea that a green building on its own does not value when referencing higher rents. 

------

# Visual Storing Telling Part 2

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

## R Markdown

```{r, results='hide', echo=FALSE}
library(mosaic)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
library(ggcorrplot)
library(ggExtra)

```

```{r, results='hide'}
ABIA = read.csv('ABIA.csv')
```

#**Question: Which Carriers are the most Unreliable?**
```{r}
d1 = ABIA %>%
  group_by(UniqueCarrier) %>%
  summarize(canc_pctAL = sum(Cancelled=='1')/n())
d1

ggplot(data = d1) + 
  geom_bar(mapping = aes(x=UniqueCarrier, y=canc_pctAL), stat='identity')+ 
  coord_flip() +
  labs(title="Cancellation by Carrier", 
       subtitle="Unique Carrier vs Cancellation Percentage",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Cancellation Percentage")

```

```{r}
d1 %>% 
  arrange(desc(canc_pctAL)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=UniqueCarrier, y=canc_pctAL))+
  geom_bar(stat='identity')+ 
  labs(title="Top 5 Cancellation PCT by Carrier", 
        subtitle="Unique Carrier vs Cancellation Percentage",
        caption="Source: ABIA dataset",
        x="Carrier",
        y="Cancellation Percentage")

```


**9E, AA, EV MQ and OH** seem to be the most unreliable.

```{r, results='hide'}
#Changining Number notation to month Notation
ABIA$Month2[ABIA$Month == 1] <- 'Jan'
ABIA$Month2[ABIA$Month == 2] <- 'Feb'
ABIA$Month2[ABIA$Month == 3] <- 'Mar'
ABIA$Month2[ABIA$Month == 4] <- 'Apr'
ABIA$Month2[ABIA$Month == 5] <- 'May'
ABIA$Month2[ABIA$Month == 6] <- 'Jun'
ABIA$Month2[ABIA$Month == 7] <- 'Jul'
ABIA$Month2[ABIA$Month == 8] <- 'Aug'
ABIA$Month2[ABIA$Month == 9] <- 'Sep'
ABIA$Month2[ABIA$Month == 10] <- 'Oct'
ABIA$Month2[ABIA$Month == 11] <- 'Nov'
ABIA$Month2[ABIA$Month == 12] <- 'Dec'
ABIA$Month2
ABIA$Month2 = factor(ABIA$Month2, levels = month.abb)

```
##Lets examine the impact the months have on Cancellation
```{r}
d2 = ABIA %>%
  group_by(Month2) %>%
  summarize(canc_pctM = sum(Cancelled=='1')/n())
d2

ggplot(data = d2) + 
  geom_bar(mapping = aes(x=Month2, y=canc_pctM), stat='identity')+ 
  coord_flip() + 
  labs(title="Cancellation by Month", 
   subtitle="Month vs Cancellation Percentage",
   caption="Source: ABIA dataset",
   x="Month",
   y ="Cancellation Percentage")
```

```{r}
d2 %>% 
  arrange(desc(canc_pctM)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=Month2, y=canc_pctM))+
  geom_bar(stat='identity') +
  labs(title="Cancellation by Month (Top 5)", 
   subtitle="Month vs Cancellation Percentage",
   caption="Source: ABIA dataset",
   x="Month",
   y ="Cancellation Percentage")
```

No Surprise that the Winter and Spring Months have the Highest Cancellations. Cold and Rainy...
 
 
Lets Put these in the Same Graph
```{r}
d3 = ABIA %>%
  group_by(Month2, UniqueCarrier) %>%
  summarize(canc_pctB = sum(Cancelled=='1')/n())
d3


d3 %>% 
  arrange(desc(canc_pctB)) %>%
  ggplot(., aes(x=UniqueCarrier, y=canc_pctB, fill= Month2))+
  geom_bar(stat='identity') +
  labs(title="Cancellation by Carrier", 
       subtitle="Unique Carrier vs Cancellation Percentage",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Cancellation Percentage",
       fill = 'Month')

```


We can see Jan, Feb, and Mar have the most weight in each of the columns.
Our top 5 "Unreliable" Airlines: 9E, AA, EV, MQ and OH seem to follow this trend of cancelling more in Cold Months.
Interestingly, they do not have the same proportion of Cancellation for each month
**9E has a very low Cancellation PCT in Jan but EV has a very high one.**


```{r}
d10 = ABIA %>%
  group_by(CancellationCode, UniqueCarrier) %>%
  summarize(canc_pctCode = sum(Cancelled=='1')/n())
d10

d10 %>% 
  arrange(desc(canc_pctCode)) %>%
  ggplot(., aes(x=UniqueCarrier, y=canc_pctCode, fill= CancellationCode))+
  geom_bar(stat='identity') +
  labs(title="Reason for Cancellation", 
       subtitle="Unique Carrier vs Cancellation Percentage",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Cancellation Percentage",
       fill = 'Reason for Cancellation')

```


A is Carrier
B is Weather
C is NAS (type of weather delays that could be reduced with corrective action by the airports or the Federal Aviation Administration)
Pretty even Split for most airlines


##Now Which Airlines have the most Delay??
```{r}
d4 = ABIA %>%
  group_by(UniqueCarrier) %>%
  summarise(AVGDelay = mean(ArrDelay, na.rm = T))
d4

ggplot(data = d4) + 
  geom_bar(mapping = aes(x=UniqueCarrier, y=AVGDelay), stat='identity')+ 
  coord_flip() +
  labs(title="Average Delay by Carrier", 
       subtitle="Unique Carrier vs Average Delay",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Average Delay")
```

```{r}
d4 %>% 
  arrange(desc(AVGDelay)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=UniqueCarrier, y=AVGDelay))+
  geom_bar(stat='identity') +
  labs(title="Average Delay by Carrier (Top 5)", 
       subtitle="Unique Carrier vs Average Delay",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Average Delay")
```

**We see EV and OH show up again...**


##Do months have an effect on delay too???
```{r}
d5 = ABIA %>%
  group_by(Month2) %>%
  summarise(AVGDelay = mean(ArrDelay, na.rm = T))
d5

ggplot(data = d5) + 
  geom_bar(mapping = aes(x=Month2, y=AVGDelay), stat='identity')+ 
  coord_flip()+
  labs(title="Average Delay by Month", 
       subtitle="Month vs Average Delay",
       caption="Source: ABIA dataset",
       x="Month",
       y ="Average Delay")
```

```{r}
d5 %>% 
  arrange(desc(AVGDelay)) %>%
  slice(1:5) %>%
  ggplot(., aes(x=Month2, y=AVGDelay))+
  geom_bar(stat='identity')+
  labs(title="Average Delay by Month (Top 5)", 
       subtitle="Month vs Average Delay",
       caption="Source: ABIA dataset",
       x="Month",
       y ="Average Delay")

```


We can see a different set of months that have the most delay.
However, Feb and March show up again.



Were EV and OH victims of these months?
```{r}
d6 = ABIA %>%
  group_by(UniqueCarrier, Month2) %>%
  summarise(AVGDelay2 = mean(ArrDelay, na.rm = T))
d6

d6 %>% 
  arrange(desc(AVGDelay2)) %>%
  ggplot(., aes(x=UniqueCarrier, y=AVGDelay2, fill= Month2))+
  geom_bar(stat='identity')+
  labs(title="Average Delay by Carrier", 
       subtitle="Unique Carrier vs Average Delay",
       caption="Source: ABIA dataset",
       x="Carrier",
       y ="Average Delay",
       fill = 'Month')

```


It seems something went wrong for EV in November.
OH had a bad November and December which is better since we predicted December was a bad month for all airlines.

**Conclusion so far: Don't Fly EV (ExpressJet)**


##Could other Factors influence Delay
```{r}
d7 = ABIA %>%
  group_by(Distance) %>%
  summarise(AVGDelay = mean(ArrDelay, na.rm = T))
d7

ggplot(data = d7) + 
  geom_point(mapping = aes(x = Distance, y = AVGDelay))+
  labs(title="Delay by Distance", 
       subtitle="Distance vs Average Delay",
       caption="Source: ABIA dataset",
       x="Distance",
       y ="Average Delay")
```

**Distance doesn't have an impact.**

```{r}
d8 = ABIA %>%
  group_by(DepTime) %>%
  summarise(AVGDelay = mean(ArrDelay, na.rm = T))
d8

ggplot(data = d8) + 
  geom_point(mapping = aes(x = DepTime, y = AVGDelay))+
  labs(title="Average Delay by Depart Time", 
       subtitle="Depart Time vs Average Delay",
       caption="Source: ABIA dataset",
       x="Depart Time",
       y ="Average Delay")
```

**Depart time does have an impact on Delay, however this can be explained.** 
**If a flight experiences a Delay, it is more likely that it will depart at a later more unconventional time.**

Sorry ExpressJet, you're out of luck!

**Conclusion: Don't Fly ExpressJet**



------

# Portfolio Modeling

```{r setup2, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

First we will go ahead and set up the required libraries

```{r}
library(mosaic)
library(quantmod)
library(foreach)
```

## Scenario 1 : Here we are going ahead with Marketing based growth equites
## Portfolios that we will go ahead with are:
## 1) PXH : Invesco FTSE RAFI Emerging Markets ETF
## 2) JPEM : JPMorgan Diversified Return Emerging Markets Equity ETF
## 3) ECON : Columbia Emerging Markets Consumer ETF
## 4) EDIV : SPDR S&P Emerging Markets Dividend ETF
## 5) DEM :  WisdomTree Emerging Markets Equity Income Fund

```{r}
mystocks = c("PXH", "ECON", "DGRE", "EDIV", "DEM")
getSymbols(mystocks, from = "2015-01-01")
options("getSymbols.warning4.0"=FALSE)
```

## We would need to adjust for splits and dividends

```{r}
options(warn=-1)
PXHa = adjustOHLC(PXH)
```

```{r}
DGREa = adjustOHLC(DGRE)
```

```{r}
ECONa = adjustOHLC(ECON)
```

```{r}
EDIVa = adjustOHLC(EDIV)
```

```{r}
DEMa = adjustOHLC(DEM)
```

## After adjusting for splits and dividends, we look up for close to close changes on a daily level for the selected stocks

```{r }
plot(ClCl(PXHa))
```

```{r }
plot(ClCl(ECONa))
```

```{r}
plot(ClCl(DGREa))
```

```{r}
plot(ClCl(EDIVa))
```

```{r}
plot(ClCl(DEMa))
```


## Now we combine all the data into a single dataframe and remove the NA values

```{r}
all_returns = cbind(ClCl(PXHa),ClCl(ECONa),ClCl(DGREa), ClCl(EDIVa), ClCl(DEMa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)
```

## As the next step we check if there is a correlation amongst all the stocks

```{r}
pairs(all_returns)
```

## The correlation plot seems to suggest that there seems to be some sort of correlation amongst the stocks

## Now we plot the market returns over time

```{r}
plot(all_returns[,1], type='l')
plot(all_returns[,2], type='l')
plot(all_returns[,3], type='l')
plot(all_returns[,4], type='l')
plot(all_returns[,5], type='l')
```

## The dip in the market returns near the Mar 2020 period can be attributed to the ongoing Covid situation

## Now we look at the auto correlation plot

```{r}
acf(all_returns[,5])
```

## As it turns out there is not much correlation there!

## Now we simulate for a single day return from our previous historical values

```{r}
return.today = resample(all_returns, 1, orig.ids=FALSE)
```

## After getting a single day simulation we can go ahead with building our very own portfolio

```{r}
total_wealth = 100000
my_weights = c(0.2,0.2,0.2, 0.2, 0.2)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)
```

## In the above code we take the total wealth as $100000 and distribute it equally among all the stocks and also get the new combination of holding based on our returns from the single day simulation

## After getting the holdings we calculate the new total wealth

```{r}
holdings
total_wealth = sum(holdings)
total_wealth
```

## We can repeat the above process for 4 trading weeks and try to see what it gives

```{r}
total_wealth = 100000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * total_wealth
n_days = 20  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
```


## Now we simulate the given code for many times and see what is the frequency of the returns

```{r}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}
```

```{r}
head(sim1)
hist(sim1[,n_days], 25)

mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

quantile(sim1[,n_days]- initial_wealth, prob=0.05)

```
## The 5% VAR at risk for this portfolio comes out as above

## Scenario 2: Here we are considering the currency ETF's

Currency ETFs offer investors exposure to a single currency or a basket of currencies. The funds are comprised of currency futures contracts.

## UUP : Invesco DB US Dollar Index Bullish Fund
## FXE	Invesco CurrencyShares Euro Currency Trust
## FXY	Invesco CurrencyShares Japanese Yen Trust

```{r}
mystocks = c("UUP","FXE","FXY")
getSymbols(mystocks, from = "2015-01-01")
options("getSymbols.warning4.0"=FALSE)
```

Adjusting for splits and dividends

```{r}
options(warn=-1)
UUPa = adjustOHLC(UUP)
```

```{r}
FXEa = adjustOHLC(FXE)

```

```{r}
FXYa = adjustOHLC(FXY)
```

## Plotting the close to close values for adjusted splits and dividends

```{r}
plot(ClCl(UUPa))
```

```{r}
plot(ClCl(FXEa))
```

```{r}
plot(ClCl(FXYa))
```

## It is evident from the graph that the there is a fluctuation in the close to close values for the above given plortfolios sometime during the Mar 2020

## Now we create a single data frame for all the 3 ETS returns

```{r}
all_returns = cbind(ClCl(UUPa),ClCl(FXEa),ClCl(FXYa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)
```

## After getting all the ETFs into a single portfolio , we try to simulate a single trading day return and run it over 4 trading weeks to to get our total wealth over the given period of time

```{r}
total_wealth = 100000
weights = c(0.3, 0.3, 0.3)
holdings = weights * total_wealth
n_days = 20  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')

```


## Now we run the code against many time to get a value of what the returns might look in the long run

```{r}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.3, 0.3, 0.3)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

### And this gives us the 5% VAR for the currency portfolio that we have

## Scenario 3: Here we are considering oil and gas ETF's

Oil & Gas ETFs invest directly in oil or gas and/or their subsidiary commodities. Note that these funds almost always utilize futures exposure to invest in their respective commodities.

### USO	United States Oil Fund
### DBO	Invesco DB Oil Fund
### BNO	United States Brent Oil Fund

```{r}
mystocks = c("USO","DBO","BNO")
getSymbols(mystocks, from = "2015-01-01")
options("getSymbols.warning4.0"=FALSE)
```

```{r}
options(warn=-1)
USOa = adjustOHLC(USO)
```

```{r}
DBOa = adjustOHLC(DBO)
```

```{r}
BNOa = adjustOHLC(BNO)
```

### Now we plot the close to close value for each of the stocks

```{r}
plot(ClCl(USOa))
```

```{r}
plot(ClCl(DBOa))
```

```{r}
plot(ClCl(BNOa))
```

### As expected all the stocks show high amount of fluctuations during the covid pandemic

### Now we get all the returns into a single data frame

```{r}
all_returns = cbind(ClCl(USOa),ClCl(DBOa),ClCl(BNOa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))
N = nrow(all_returns)
```

### After getting the stocks into a single dataframe we can now directly run our simulation model for n number of times over 4 trading weeks period

```{r}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.3, 0.3, 0.3)
  holdings = weights * total_wealth
  n_days = 20
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

 And above is the 5% VAR for oil and gas based ETFs




------

# Market Segmentation

```{r, read_data, echo=FALSE}
seg <- read.csv('social_marketing.csv')

#Cleaning data for categories known to be skewed/bot-influenced
seg$chatter<- NULL
seg$spam <- NULL
seg$adult <- NULL
seg$uncategorized <- NULL
clean_seg <- seg[-c(1)] # removing the individual tags, not necessary
```

This correlation uses hierachical clustering to show what variables are correlated the most. It will serve as our baseline for our clustering model.

```{r,k-means,echo=FALSE}
#k-means clustering
set.seed(1)
X = scale(clean_seg, center=TRUE, scale=TRUE)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 5 clusters and 25 starts
clust1 = kmeans(X, 5, nstart=25)

library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(clean_seg), hc.order = TRUE)+ 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.2))

```

Below are graphs that reflect the complexity versus variance explained trade-off. We also decided to focus on a clustering equal to five since the data seperates itself fairly well at k=5.


```{r, PCA_Analysis, echo=FALSE}
library(fpc)
plotcluster(seg[,2:32], clust1$cluster, pch=16)


clusters_combined <- as.data.frame(cbind(clust1$center[1,]*sigma + mu, 
                                          clust1$center[2,]*sigma + mu,
                                          clust1$center[3,]*sigma + mu,
                                          clust1$center[4,]*sigma + mu,
                                          clust1$center[5,]*sigma + mu))

#adding names
names(clusters_combined) <-  c('Cluster_1',
                               'Cluster_2',
                               'Cluster_3',
                               'Cluster_4',
                               'Cluster_5')

#sum of all clusters
summary(clusters_combined)

#creating a new type to plot on
clusters_combined$type <- row.names(clusters_combined)

```


To create these plots, we first had to create a data frame of each cluster, rename the resulting data frame columns, then add back the names of the categories by adding another column so that it can be used to sort the x values of the resulting plots.


```{r, fitting PC_clust, echo=FALSE}
#Cluster 1
ggplot(clusters_combined, aes(x =reorder(type, -Cluster_1) , y=Cluster_1)) +
  geom_bar(stat="identity", position ="dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.1)) + 
  labs(title="Cluster 1", 
       x ="Category", y = "Cluster centre values") 

#cluster 2 
ggplot(clusters_combined, aes(x =reorder(type, -Cluster_2) , y=Cluster_2)) +
  geom_bar(stat="identity", position ="dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.1)) + 
  labs(title="Cluster 2", 
       x ="Category", y = "Cluster centre values")

#Cluster 3
ggplot(clusters_combined, aes(x =reorder(type, -Cluster_3) , y=Cluster_3)) +
  geom_bar(stat="identity", position ="dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.1)) + 
  labs(title="Cluster 3", 
       x ="Category", y = "Cluster centre values")

#Cluster 4
ggplot(clusters_combined, aes(x =reorder(type, -Cluster_4) , y=Cluster_4)) +
  geom_bar(stat="identity", position ="dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.1)) + 
  labs(title="Cluster 4", 
       x ="Category", y = "Cluster centre values")

#cluster 5
ggplot(clusters_combined, aes(x =reorder(type, -Cluster_5) , y=Cluster_5)) +
  geom_bar(stat="identity", position ="dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=-40, hjust=.1)) + 
  labs(title="Cluster 5", 
       x ="Category", y = "Cluster centre values")
```

------

### Conclusion
We plan on using these clusters as market segments based on the the major categories found in each cluster. 

Cluster 1 includes tweets considered politics, travel and news. Politics and travel do appear closely together in the hierarchical correlation matrix, but news does not. These individuals may include business professionals who pay close attention to the news and politcal information. They are also known to travel often.

Cluster 2 includes sports fandom, religion, food and parenting. This is the biggest correlation matrix found in our baseline analysis. This cluster may be geared toward family oriented participants who share these values, but it is odd to see sports fandom in this mix. It seems like an outlier when compared to the other categories.

Cluster 3 includes cooking, photo sharing, fashion and beauty. Photo sharing is not included in the correlation matrix cluster that was observed for cooking, fashion, and beauty. This cluster may be based on gender, since fashion and beauty would be more associated with females rather than males.

Cluster 4 includes categories such as photo sharing, college/universities, current_events, shopping, and online gaming. While the category of current_events seems out of place in these categories, we imagine cluster 2 to include younger individuals in college who are more more active on the internet.

Cluster 5 is dominated by health/nutrition then followed by personal fitness and cooking. The correlation matrix captures two of these factors closely together, but cooking is replaced with outdoors instead. It seems to be that these individuals are very health and fitness focused. 

------ 

# Association Rule Mining
```{r echo =FALSE, include=FALSE}
#Load Packages
library(tidyverse)
library(arules)
library(arulesViz)
```
Read in groceries dataset, make each line contain all the items in the transaction

```{r echo = FALSE, include = FALSE}
groceries_raw = scan("groceries.txt", what = "", sep = "\n")
```

We split each transaction into baskets, and transformed the data into a "transaction class".
We found that whole milk is the most frequent item in the transactions, occuring in 2513 baskets out of the 9835 transaction.



```{r echo=FALSE, include = FALSE}
groceries = strsplit(groceries_raw, ",")
groceriestrans = as(groceries, "transactions")
summary(groceriestrans)
```

Our plot shows the ten most frequent items in the transactions

```{r echo = FALSE}
itemFrequencyPlot(groceriestrans, topN = 10)

```

Let us explore rules with low support > 0.02, confidence > 0.1 and the length being 2 to find relationships between two items.

```{r echo = FALSE, include = FALSE}
groceryrule1 = apriori(groceriestrans, parameter = list(support = 0.02, confidence = 0.1, minlen =2))

```

We ended up with 120 rules, because we had low support and higher confidence. For the first rule, given someone had frozen vegetables in their basket we are 42% positive that they'll also get whole milk. Whole milk is a very common occurence as shown by the data before.

```{r echo = FALSE, include=FALSE}
inspect(groceryrule1)
plot(groceryrule1, method='graph')
```

Let us decrease support and also increase confidence with support > 0.01 and confidence > 0.2

```{r echo = FALSE, include = FALSE}
groceryrule2 = apriori(groceriestrans, parameter = list(support = 0.02, confidence = 0.3, minlen =2))
```

With a higher confidence we lowered the number of rules. We decided to plot by lift as it shows a change in probability of getting an item when another is in the basket.

```{r echo = FALSE}
inspect(groceryrule2)
plot(head(groceryrule2,3, by = "lift"), method='graph')
```

Lets try to decrease support further and increase confidence even more with support > 0.005, confidence > 0.6 and length > 2

```{r echo = FALSE, include = FALSE}
groceryrule3 = apriori(groceriestrans, parameter = list(support = 0.001, confidence = 0.6, minlen =2))
```

We then plot the rules for 3 rules by the lift.
From the plot we see that people who buy processed cheese and ham are more likely to buy white bread, possibly for sandwiches.

```{r echo = FALSE}

plot(head(groceryrule3,3, by = "lift"), method='graph')
```

In Conclusion:
1. Whole milk is the most common item purchased by customers. 
2. People who buy processed cheese and ham are more likely to buy white bread.
3. People who buy popcorn and soda are more likely to buy salty snacks.

# Author Attribution


```{r,author_attribution, include=FALSE}
library(tm) 
library(tidyverse)
library(slam)
library(proxy)

## tm has many "reader" functions.  Each one has
## arguments elem, language, id
## (see ?readPlain, ?readPDF, ?readXML, etc)
## This wraps another function around readPlain to read
## plain text documents in English.
# I've stored this function as a Github "gist" at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff

data = VCorpus(DirSource(directory="C:/Users/bryan/OneDrive/Desktop/Summer 2020/ReutersC50/C50train",recursive = TRUE),readerControl = list(language ='en'))

#lower-case text
data_lc <- tm_map(data, content_transformer(tolower))

dtm <- DocumentTermMatrix(data_lc)

```

The idea for this problem was to compile all of the texts, clean the data down so that it is easier to analyze then combine the names of the authors that corresponds to each text file. Then we could comput a TF-IDF for each document that would be associated for each author which would be used to predict which document belongs to a certain author. Some models we considered using were k-mean clustering and tree models. This is where we would implement the test data set into our models to generate a prediction accuracy and compare across the models we considered. 
