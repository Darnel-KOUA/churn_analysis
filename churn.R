# Package
library(readr)
library(tidyverse)

# Load data

df <- read_delim("Data/DataSet_TDD.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- df %>% select(-ID) # delete ID

# first six observations
head(df)

# last six observations
tail(df)

# Number of observations and variables
dim(df)

# Summary of data
summary(df)

# See the type of variables
map_vec(df, typeof)  #no type complies for some variables, need correction for this.

# percentage of missing values for each variable
map_vec(df, \(x) round((sum(is.na(x))/dim(df)[1])*100, 2))

