getwd()

library(readxl)
library(readr)
library(tidyverse)
library(MASS)
library(klaR)

D15.Count <- read.csv("Diatom15_Results.csv", stringsAsFactors = FALSE) %>% select(-1, -3) %>% arrange(Species) %>% t()
colnames(D15.Count) <-D15.Count[1,]
D15.Count <- D15.Count[-1,]
str(D15.Count)

D15.Count.df <- data.frame(data.matrix(D15.Count))

# Convert Factors to Numeric
index <- sapply(D15.Count.df, is.factor)
D15.Count.df[index] <- lapply(D15.Count.df[index], function(x) as.numeric(as.character(x)))

# Replace NA with 0
D15.Count.df[is.na(D15.Count.df)] <-0

# Get the data frame (15*204) with species in columns and observations in rows.
str(D15.Count.df)

D15.Count.df$Site <- c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C")
lda.D15 <- lda(Site ~ ., D15.Count.df)

How to code? How to perform LDA here?
                              
                              
