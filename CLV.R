# !Diagnostics off

#######################################################
## Psuedo Churn CLV Analysis
## August 2023
## Ayesha Saeed
#######################################################

## Load packages
library(tidyverse)
library(scales)
library(lubridate)
library(gmodels)

#######################################################
## Load CLV data




#######################################################
# working with a big table - read a few rows in
temp <- read_csv("./,", m_max=10)


temp <- read_delim("", delim"\t", n_max=10)


df <- read_delim("./brand_acq_tnsactions.csv.gz", delim = "\t")
df2 <- read_delim("./brand_acq_tnsactions.csv.gz", delim = "\t", n_max=350000)


#######################################################
## EDA - Exploratory Data Analysis

df %>%
  group_by(productDesc) %>%
  summarize(tnsCount=sum(tnsCount)) %>%
  arrange(-tnsCount) %>%
  ungroup()


cust_sample <- resamp %>%
  ungroup() %>%
  distinct((custKey)) %>%
  sample_n(5000)


#######################################################
## Run some basic arithmetic for the basic CLV analysis

#avg cart


#######################################################
## Run a logistic regression for the pseudo chrun analysis



#######################################################
## Make some plots to show results visually



#######################################################
# Professor's Notes
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
df %>%
  distinct(orderType, custKey) %>%
  group_by(orderType) %>%
  summarize(ncust=n()) %>%
  arrange(-ncust)

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

############################################################
# Create resampled dataset.
############################################################

# resample by month as the period
resamp <- df %>%
  group_by(
    custKey, ymo
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
    lifetime_seq = (j - customer_entry) + 1 #lifetime duration variable
  )

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


##################################
# Pseudo-churn
# have to think of every customer as a time series
resamp

# create the lifetimes tables





