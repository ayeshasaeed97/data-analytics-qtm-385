# !diagnostics off
############################################################
## Pseudo Churn CLV Analysis
## August 2023
## Joe Sutherland <joseph.lyons.sutherland@emory.edu>
############################################################
##
## Load our packages
##
############################################################

library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)

setwd("~/Dropbox/Emory/Courses/Applied QTM - Data Analytics/1 - Case Study - Customer Lifetime Value/")

############################################################
##
## Load the CLV data.
##
############################################################

# working with a big table - read a few rows in.
temp <- read_csv("./data/brand_acq_tnsactions.csv.gz", n_max=10)
temp

# see if the delims work
temp <- read_delim("./data/brand_acq_tnsactions.csv.gz", delim="\t", n_max=10) 
temp

# read the full thing
df <- read_delim("./data/brand_acq_tnsactions.csv.gz", delim="\t") 
df

# if you need to take a sample of the data to work with,
# you should sample customers, not rows
# NOT RUN
cust_samp <- df %>% distinct(custKey) %>% sample_n(30000) %>% unlist()
# df <- df %>% filter(custKey %in% cust_samp)

############################################################
##
## EDA - exploratory data analysis
##
############################################################

# how many customers?
df %>%
  distinct(custKey) %>%
  nrow() %>%
  comma()

# what are the most purchased SKUs?
df %>%
  group_by(SKU) %>%
  summarize(tnsCount=sum(tnsCount)) %>%
  arrange(-tnsCount) %>%
  ungroup() %>%
  left_join(df %>% distinct(SKU, productDesc))

# what are the most purchased product lines?
df %>%
  group_by(productDesc) %>%
  summarize(tnsCount=sum(tnsCount)) %>%
  arrange(-tnsCount) %>%
  ungroup()

# what products bring in the most money?
df %>%
  group_by(productDesc) %>%
  summarize(gross=sum(gross)) %>%
  arrange(-gross) %>%
  ungroup()

# how many customers make each order type?
ordertypec <- df %>%
  distinct(orderType, custKey) %>%
  group_by(orderType) %>%
  summarize(ncust=n()) %>%
  arrange(-ncust)

order <- df %>%
  group_by(orderType) %>%
  summarize(ncust=n())

ggplot(ordertypec, aes(x=orderType, y=ncust)) +
  geom_bar(stat="identity", fill="lightblue")+
  xlab("Order Type") + ylab("Number of Customers")

ggplot(order, aes(x=orderType, y=ncust)) +
  geom_bar(stat="identity", fill="coral3")+
  xlab("Order Type") + ylab("Number of Orders")

# what is the time range for our data?
df <- df %>%
  left_join(
    df %>%
      distinct(dtKey) %>%
      mutate(
        date = ymd(as.character(dtKey))
      )
  )

min(df$date)
max(df$date)



####Time lag


############################################################
##
## Run some arithmetic for the baseline CLV analysis
##
############################################################
# Create some additional columns so that we can resample
############################################################
df <- df %>%
  mutate(
    year=year(date),
    month=month(date),
    week=week(date),
    ymo=ymd(sprintf("%04d-%02d-01", year, month))
            )
  )

############################################################
# Create resampled dataset.
############################################################

# resample by month as the period
resamp <- df %>%
  group_by(
    custKey, ymo, week
  ) %>%
  summarize(
    gross = sum(gross),
    num_sku = n()
  ) %>%
  arrange(custKey, ymo)


# Create an indicator for when each customer entered dataset,
# and the order of the purchase
resamp <- resamp %>%
  ungroup() %>%
  left_join(
    resamp %>%
      ungroup() %>%
      distinct(ymo) %>%
      arrange(ymo) %>%
      mutate(
        j = row_number()
      )
  ) %>%
  group_by(custKey) %>%
  arrange(ymo) %>%
  mutate(
    purchase_seq = row_number(),
    customer_entry = min(j),
    lifetime_seq = (j - customer_entry) + 1
  )

resamp %>%
  filter(ymo >= as.Date("2017-02-02"))

############################################################
# Basic arithmetic CLV
############################################################

# avg cart per purchase
cart_mu <- mean(resamp$gross)
cart_mu

# purchases per period
# in this case, we've resampled, so there is only one
#  purchase per period.
avg_purch <- 1

# periods over lifetime
avg_periods <- resamp %>%
  group_by(custKey) %>%
  arrange(-purchase_seq) %>%
  slice(1) %>%
  ungroup() %>%
  select(purchase_seq) %>%
  unlist() %>%
  mean()

# compute basic clv (poor estimate)
arithmetic_clv <- cart_mu * avg_purch * avg_periods
arithmetic_clv


############################################################
##
## run a logistic regression for the pseudo churn analysis
##
##
############################################################


# create the lifetimes table
# tells
today <- max(resamp$j)

lifetime <- resamp %>%
  distinct(custKey, lifetime_seq, gross, week) %>%
  group_by(custKey) %>%
  arrange(lifetime_seq) %>%
  summarize(
    # Summary lifetime info.
    firstdate=min(lifetime_seq),
    lastdate=max(lifetime_seq),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n(),
    mupurchase = mean(gross, na.rm=T),
    histvalue = sum(gross, na.rm=T),
  ) %>%
  mutate(
    churned = as.integer(currentperiod > 8)
    )

# NOT RUN IN CLASS
# # Data-based method
today <- max(df$week)
other <- resamp %>%
   distinct(custKey, week, gross, lifetime_seq) %>%
   group_by(custKey) %>%
   arrange(lifetime_seq) %>%
  mutate(
    lweek=lag(week),
    diff=week-lweek,
  ) %>%
  summarize(
     # Summary lifetime info.
     firstdate=min(week),
     lastdate=max(week),
     lifetime=as.integer(lastdate-firstdate)+1,
     currentperiod=as.integer(today-lastdate),
     npurchases=n(),
    
#Interpurchase info.
     muperiod=mean(diff, na.rm=T),
     muperiod=as.integer(ifelse(is.nan(muperiod), 0, muperiod)),
     sdperiod=sd(diff, na.rm=T),
     sdperiod=ifelse(is.na(sdperiod), 116, sdperiod),
     
     # Monetary info.
     mupurchase = mean(gross, na.rm=T),
     histvalue = sum(gross, na.rm=T))

ggplot(other, aes(x=currentperiod)) +
  geom_bar()
ggplot(other, aes(x=lifetime)) +
  geom_bar()

# # Interpurchase heterogeneity
 expperiod <- lifetime %>%
   filter(npurchases>1) %>%
   group_by(npurchases) %>%
   summarize(
     mu=mean(muperiod),
     sd=sd(muperiod),
     lwr=mu-1.96*sd,
     lwr=ifelse(lwr<0, 0, lwr),
     upr=mu+1.96*sd
   ) %>%
   arrange(npurchases)


# Start creating our GLM framework
resamp <- resamp %>%
  left_join(
    lifetime %>%
      select(custKey, j=lastdate, churned)
  ) %>%
  mutate(
    churned = ifelse(is.na(churned), 0, churned)
  )

############################################################
# Run churn prediction
############################################################

# We still need to make sure we have RFM feats
# we have F = purchase_seq
# we have M = gross
# Need R
resamp <- resamp %>%
  group_by(custKey) %>%
  arrange(ymo) %>%
  mutate(
    
    # recency
    timelag = j - lag(j),
    timelag = ifelse(is.na(timelag), 0, timelag)
    
  )

hist(resamp$timelag)

# Set up our train test split
train.rat <- 0.70
N <- nrow(resamp)
train <- sample(1:N, size = ceiling(train.rat*N))
test <- (1:N)[-train]

df.train <- resamp[train,]
df.test <- resamp[test,]

# Our very basic model
glm.churn <- glm(churned ~ timelag + lifetime_seq + gross, 
                 data = df.train, family = binomial(link="logit"))
summary(glm.churn)

# Make some churn predictions on our test set
df.test <- df.test %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, df.test, type = "response")
  )
hist(df.test$yhat, breaks = 30)

# Threshold
df.test <- df.test %>%
  mutate(
    churn_pred = as.integer(yhat > 0.02)
  )


############################################################
# Estimate customer lifetime values
############################################################

# Make some churn predictions
resamp <- resamp %>%
  ungroup() %>%
  mutate(
    yhat = predict(glm.churn, resamp, type = "response")
  )
hist(resamp$yhat)

# Threshold
# Probability of alive = projection factor
resamp <- resamp %>%
  mutate(
    churn_pred = as.integer(yhat > 0.023),
    alive_pr = 1 - yhat,
    alive_pr_rescale = rescale(alive_pr, c(0, 1))
  )
hist(resamp$alive_pr)

# We only need the final probs
final_probs <- resamp %>%
  group_by(custKey) %>%
  arrange(week) %>%
  filter(row_number() == n())
hist(final_probs$alive_pr)
hist(final_probs$alive_pr_rescale)

# We map and then aggregate
lifetime <- lifetime %>%
  left_join(
    final_probs %>%
      select(-gross)
  )

# Project over estimated remaining lifetime per customer
clv <- lifetime %>%
  select(custKey, mupurchase, histvalue, lifetime, alive_pr_rescale) %>%
  ungroup() %>%
  mutate(
    est_lifetime = ceiling(mean(lifetime, na.rm=T)),
    remain_life = ifelse(
      lifetime <= est_lifetime, 
      est_lifetime - lifetime,
      lifetime + 12
    )
  )

# Now, do the geometric summations!
clv

pr_discount <- 0.05

# noteice this is not a vectorized formula....
calc_future_clv <- function(money, remaining_life, pr_churn, pr_discount) {
  
  # No money accumulated yet...
  base_clv <- 0
  
  # for each future time period, calculate the marginal addition to CLV
  for (t in 1:remaining_life) {
    
    discount_factor <- (( 1 + pr_discount )^t * (1 + pr_churn)^t)
    
    period_value <- money / discount_factor
    
    base_clv <- base_clv + period_value
    
  }
  
  base_clv
  
}

# test with hypothetical customer
calc_future_clv(100, 12, 1-.888, pr_discount)

# iterate thru the customer base
clv_estimates <- data.frame()

clv2 <- clv[1:1000,]

clv3 <- sample_n(clv, 1000)

for (i in 1:nrow(clv3)) {
  
  cust_i <- clv3[i,]
  
  
  m <- cust_i$mupurchase
  rl <- cust_i$remain_life
  prc <- 1 - cust_i$alive_pr_rescale
  
  clv_hat_i <- calc_future_clv(m, rl, prc, pr_discount)
  
  clv_estimates <- rbind(
    clv_estimates,
    data.frame(
      i = i,
      clv_hat = clv_hat_i
    )
  )
  
}

clv_estimates <- clv_estimates %>%
  as_tibble()

clv_estimates

hist(clv_estimates$clv_hat)

summary(is.na(clv_estimates))

summary(clv_estimates)

clv2 <- clv2 %>%
  mutate(
    clv_hat = unlist(clv_estimates$clv_hat)
  )

lifetime



############################################################
##
## make some plots to show results visually
##
##
############################################################

