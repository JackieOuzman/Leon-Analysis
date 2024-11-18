library(readxl)
library(tidyverse)
library(dplyr)
library(FSA) 
library(agricolae)
library(multcomp)
library(multcomp)
library(lsmeans)
library(multcompView)
library(Rmisc)

library(ggplot2)
library(car)
library(DescTools)

# Leons dataset

data <- "C:/Users/ouz001/working_from_home_post_Sep2022/Leon Bishop/R analysis covariate.csv"

## download the data using the specified file path above

summary_data_all <- read_csv(data)
names(summary_data_all)
subset_of_data <- dplyr::select(summary_data_all, ID,
                                Treatment, 
                                `Cup size`, 
                                Emergence)
str(subset_of_data$Treatment)
unique(subset_of_data$Treatment)
## turn treatment into a factor and order as you wish               
subset_of_data$Treatment <- factor(subset_of_data$Treatment,
                                       levels = c("Control", "GA" ,"MT","GA+MT"))

str(subset_of_data$Treatment)

Summarize(Emergence ~ Treatment,
          data=subset_of_data,
          digits=3)

#ANOVA 
#*Step 1a*     Fitted a linear model to data.
#*Step 1b*     One-way ANOVA (using type II, or I or III).
#*Step 1c*     Best fit of model (can be used if choosing between one-way or two-way ANOVA)
#*Step 1d*     Check for assumptions of analysis

model = lm(Emergence ~ Treatment,
           data=subset_of_data)

Anova(model, type="II") # Can use type="III"
