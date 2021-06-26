title: "Labor Force Survey Analysis For Istanbul"
author: "Ferhat Eren Dalçýk"
date: "14 05 2021"
output:
html_document: default
pdf_document: default
  

knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(haven)
library(PerformanceAnalytics)
library(corrplot)
library(car)
library(effects)
library(dplyr)



labordatas <- read_dta(file = "hia_ist-19(1).dta")
datas <- labordatas[-c(1)]
datas <- na.omit(datas)


datas <- datas %>%                               
  mutate(education = replace(education, education == 0, 2)) # Recoding education with correct values.
datas <- datas %>%                               
  mutate(education = replace(education, education == 1, 5))
datas <- datas %>%                               
  mutate(education = replace(education, education == 2, 8))
datas <- datas %>%                               
  mutate(education = replace(education, education == 31, 11))
datas <- datas %>%                               
  mutate(education = replace(education, education == 32, 12))
datas <- datas %>%                               
  mutate(education = replace(education, education == 4, 15))
datas <- datas %>%                               
  mutate(education = replace(education, education == 5, 18))

summary(datas)


mean(datas$wage, na.rm = T) #Mean of Wage
median(datas$wage, na.rm =T) #Median of Wage
var(datas$wage, na.rm = T)  #Variance of Wage
sd(datas$wage, na.rm = T) #Standart Deviation of Wage


correlations <- cor(datas[,c (1,3, 4, 7)])
corrplot(correlations, method="circle")


outliers <-boxplot.stats(datas$wage)$out

boxplot(datas$wage,
        height = 330,
        width= 330,
        ylim = c(0,8000),
        ylab = "Wages",
        main = "Boxplot of Wage With Outliers",
        col ="orange")

out<- which(datas$wage %in% c(outliers))

 
scatterplotMatrix ( ~ wage + age + work_hour , data = datas )

  
wage.mod <- lm(wage ~ male + age + education + work_hour , data = datas)



S(wage.mod)


wage.mod2 <- lm(log2(wage) ~ male +  age + education + work_hour , data = datas)
S(wage.mod2)



coef(wage.mod2)
S(wage.mod2)

plot(predictorEffects(wage.mod2))

### Question 2 Answer:


gender= ggplot(datas, aes(x=male)) +
  geom_bar(fill='red')
gender


boxplot(age ~ male , data = datas)
  
wage.mod3 <- lm (log(wage) ~ male, data = datas)


coef(wage.mod3)
S(wage.mod3)


  
  