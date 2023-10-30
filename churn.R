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

skimr::skim(df)

# See the type of variables
map_vec(df, typeof)  #no type complies for some variables, need correction for this.

# percentage of missing values for each variable
map_vec(df, \(x) round((sum(is.na(x))/dim(df)[1])*100, 2))

#treatment of outliers
imputation <- function(x, y){
  if_else(is.na(x) & y>0, 1, if_else(is.na(x) & y <= 0, 0, x))
}

df <- df %>% mutate(STATUT_TRAFIC_M1 = imputation(STATUT_TRAFIC_M1, VOL_DATA_M1),
                    STATUT_TRAFIC_M2 = imputation(STATUT_TRAFIC_M2, VOL_DATA_M2),
                    STATUT_FACT_M1 = imputation(STATUT_FACT_M1, MONTANT_FACT_HT_M1),
                    STATUT_FACT_M2 = imputation(STATUT_FACT_M2, MONTANT_FACT_HT_M2))

# transform categorical variables into factor
df <- df %>% modify_at(vars(CHURN:STATUT_FACT_M2), as.factor)

# Barplot for CHURN target
barplot(table(df$CHURN), xlab = "CHURN", ylab = "Effectif", 
        legend.text = c("No Churn", "Churn"), col = c("blue", "red"), 
        args.legend = list(x = "topright"))
# we can see the imbalance between classes
