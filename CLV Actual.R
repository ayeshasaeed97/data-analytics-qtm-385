### CLV-Actual###


# Loading Packages
library(tidyverse)
library(scales)
library(lubridate)
library(gmodels)

# Uploading dataset as dataframe
df <- read_delim("/Users/ayeshasaeed/Desktop/Fall 2023/QTM 385 Data Analytics Business/datasets/brand_acq_tnsactions.csv.gz", delim = "\t")


############################################################
# Create some additional columns so that we can resample
############################################################
df <- df %>%
  left_join(
    df %>%
      distinct(dtKey) %>%
      mutate(
        date = ymd(as.character(dtKey))
      )
  )





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

# Create an indicator for when each customer entered dataset,
# and the order of the purchase
df2 <- df %>%
  ungroup() %>%
  left_join(
    df %>%
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

df2 %>%
  filter(ymo >= as.Date("2017-02-02"))


# create the lifetimes table
# tells
today <- max(df2$j)

lifetime <- df2 %>%
  distinct(custKey, lifetime_seq) %>%
  group_by(custKey) %>%
  arrange(lifetime_seq) %>%
  summarize(
    # Summary lifetime info.
    firstdate=min(lifetime_seq),
    lastdate=max(lifetime_seq),
    lifetime=as.integer(lastdate-firstdate)+1,
    currentperiod=as.integer(today-lastdate),
    npurchases=n()
  ) %>%
  mutate(
    churned = as.integer(currentperiod > 8)
  )


# Start creating our GLM framework
df3 <- df2 %>%
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
df3 <- df3 %>%
  group_by(custKey) %>%
  arrange(ymo) %>%
  mutate(
    
    # recency
    timelag = j - lag(j),
    timelag = ifelse(is.na(timelag), 0, timelag)
    
  )

hist(df3$timelag)

# Set up our train test split
train.rat <- 0.70
N <- nrow(df3)
train <- sample(1:N, size = ceiling(train.rat*N))
test <- (1:N)[-train]

df.train <- df3[train,]
df.test <- df3[test,]

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

for (i in 1:nrow(clv)) {
  
  cust_i <- clv[i,]
  
  
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

summary(clv_estimates)

clv <- clv %>%
  mutate(
    clv_hat = unlist(clv_estimates$clv_hat)
  )

lifetime
