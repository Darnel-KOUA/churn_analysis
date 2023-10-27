# Package
library(readr)
library(tidyverse)

# Load data

df <- read_delim("Data/DataSet_TDD.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- df %>% select(-ID) # delete ID
head(df)
dim(df)
summary(df)
