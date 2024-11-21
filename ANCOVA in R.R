
library(readxl)
library(tidyverse)
library(dplyr)
library(car)
library(multcomp)

#https://www.r-bloggers.com/2021/07/how-to-perform-ancova-in-r/


# Leons dataset

data <- "C:/Users/ouz001/working_from_home_post_Sep2022/Leon Bishop/R analysis covariate.csv"

## download the data using the specified file path above

summary_data_all <- read_csv(data)
names(summary_data_all)
subset_of_data <- dplyr::select(summary_data_all, ID,
                                Treatment, 
                                `Cup size`, 
                                Emergence)
summary(subset_of_data)
#Turn Treatment into factor
subset_of_data$Treatment <- as.factor(subset_of_data$Treatment)
#subset_of_data$`Cup size` <- as.factor(subset_of_data$`Cup size`)

subset_of_data
#change cup size into a value (not sure if this is correct its still categorical data)

unique(subset_of_data$ `Cup size`)
subset_of_data <- subset_of_data %>% mutate(
  cup_size =
  case_when(
    `Cup size` == "S" ~1,
    `Cup size` == "L" ~2)
  )

# dim(subset_of_data)
# random_number <- sample(1:9, 28, replace = TRUE)/10


# lets pretend is a continuous variable to see if we can do this type of analysis
subset_of_data <- subset_of_data %>% mutate(cup_cont =  sample(1:9, 128, replace = TRUE)/10)



subset_of_data %>%
  group_by(Treatment) %>%  
  summarise(mean_Emergence = mean(Emergence),
            sd_Emergence = sd(Emergence),
            mean_cup_size = mean(cup_cont),
            sd_cup_size = sd(cup_cont))



ggplot(subset_of_data, aes(x= Treatment, y= Emergence))+
  geom_point()+
  geom_boxplot(alpha = 0.5)+
  labs(title = "Emergence by Treatment")

ggplot(subset_of_data, aes(x= Treatment, y= cup_cont))+ 
  geom_point()+
  geom_boxplot(alpha = 0.5)+
  labs(title = "Cup size by Treatment") 


################################################################################

#1 Assumption. The covariate and the treatment are independent
#Need to verify that the covariate in this cup_cont and the Treatment are independent to each other.
# we can make use of the ANOVA model.

model <- aov(cup_cont ~ Treatment, data = subset_of_data) 
summary(model)

#The p-value is 0.872 that is greater than 0.05, 
#so the covariate cup_cont and the treatment technique are independent to each other.



################################################################################
#Assumption 2.Homogeneity of variance
#use of Levene’s Test

leveneTest(Emergence~Treatment, data = subset_of_data)

#The p-value of the test is 07948 , 
#which indicates that the variances among the groups is equal. 

################################################################################
#Fit analysis of covariance model ANCOVA in R

ancova_model <- aov(Emergence ~ Treatment + cup_cont, data = subset_of_data)
Anova(ancova_model, type="III")

#Result, when we control for cup_con variable 
# Treatment variable is not statistically significant. 
#It indicates that the Treatment variable has significantly contributed to the model.


################################################################################
# but if something was sign different you might want to know What different ? 
# Which Treatment are different from each other.


ancova_model <- aov(Emergence ~ Treatment + cup_cont, data = subset_of_data)
postHocs <- glht(ancova_model, linfct = mcp(Treatment = "Tukey"))
summary(postHocs)


################################################################################
## Try something else

ancova.example <- lm(Emergence ~ Treatment * cup_cont, data = subset_of_data)
anova(ancova.example)



lm1<-lm(Emergence ~ Treatment, data = subset_of_data)  
anova(lm1) 
summary(lm1)


library(emmeans)
lm1.results<-summary(emmeans(lm1, ~Treatment)) 



ggplot(lm1.results,aes(Treatment,emmean))+
  geom_bar(stat="identity", width=.4)+
  #geom_errorbar(aes(ymin = , ymax = ) + #incomplete
                  ylim(0,1)+
                  labs(y = "Mean Emerg") 

