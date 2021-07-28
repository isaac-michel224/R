library(haven)
GSS2018 <- read_dta("Grad School Stuff/Spring Semester 2021/Applied Econometrics/Mid Term/2018_stata/GSS2018.dta")
View(GSS2018)
library(aod)
library(ggplot2)
head(GSS2018)
summary(GSS2018)

# Keep only the variables you need

keep_vars <- 
  c( "natspac" , "educ" , "income" , "polviews" , "race" , "region" ,
     "age" , "sex" ) 

library(survey)

#Tabulate Variables

table(GSS2018$natspac)

table(GSS2018$polviews)

# Chi-squared Test

chisq.test(xtabs(~ natspac + polviews, data=GSS2018), simulate.p.value=TRUE)

chisq.test(xtabs(~ natspac + age, data=GSS2018), simulate.p.value=TRUE)

df <- GSS2018
df$natspac <- as.factor(df$natspac)
df %>% welch_anova_test(educ ~ natspac)