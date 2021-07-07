install.packages("gapminder")
library(gapminder)
data(gapminder)

summary(gapminder)
mean(gapminder$gdpPercap)
x <- mean(gapminder$gdpPercap)
x
attach(gapminder)
median(pop)
hist(lifeExp)
hist(pop)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ gdpPercap)
plot(lifeExp ~ log(gdpPercap))

library(dplyr)
gapminder %>%
  select(country, lifeExp) %>% 
  filter(country == "South Africa" |
           country == "Ireland") %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

df1 <- gapminder %>%
  select(country, lifeExp) %>% 
  filter(country == "South Africa" |
           country == "Ireland")

t.test(data = df1, lifeExp ~ country)

library("ggplot2")

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=year, size=pop))+
  geom_point(alpha=0.3)+
  geom_smooth(method = lm)+
  facet_wrap(~continent)

#Regression Analysis
summary(lm(lifeExp ~ gdpPercap+pop))

#ANOVA

Group1 <- c(2,3,5,2,6)
Group2 <- c(10,8,7,5,10)
Group3 <- c(10,13,14,13,15)

Combined_Groups <- data.frame(cbind(Group1,Group2,Group3))
Stacked_Groups <- stack(Combined_Groups)
Combined_Groups
Stacked_Groups

Anova_Results <- aov(values ~ ind, data = Stacked_Groups)
summary(Anova_Results)

attributes(Anova_Results)

# Cross-Tabulation

xtabs(~ bmi + smokes, data=modified_heart_attack_data)

crosstabs <- xtabs(~ bmi + smokes, data=modified_heart_attack_data)
crosstabs

# Convert cross tabulation to proportions by row
prop.table(crosstabs, 1)

# Convert cross tabulation to proportions by column
prop.table(crosstabs, 2)

# Convert cross tabulation to percentages
100 * prop.table(crosstabs, 2)

# Round to 2 places after decimal

round(100 * prop.table(crosstabs, 2), 2)


# Assign last cross tables table to an object

crosstabs_v2 <- round(100 * prop.table(crosstabs, 2), 2)

print(crosstabs_v2)

summary(lm(bmi ~ smokes, data = modified_heart_attack_data))

#T-test example

t.test(data = modified_heart_attack_data, age ~ attack)

#ANOVA example

aov(age ~ marstatus, data = modified_heart_attack_data)

#Chi-Square Examples

chisq.test(modified_heart_attack_data)

heart.attack <- data.frame(modified_heart_attack_data$attack, modified_heart_attack_data$smokes)

heart.attack <- table(modified_heart_attack_data$attack, modified_heart_attack_data$smokes)

print(heart.attack)

print(chisq.test(heart.attack))

print(str(heart.attack))

chisq.test(modified_heart_attack_data$attack, modified_heart_attack_data$smokes)

##Ordinal Logit

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(RColorBrewer)

as.factor(GSS2018$natspac)
prop.table(table(GSS2018$natspac))
prop.table(table(GSS2018$sex))




ggplot(GSS2018,aes(x=natspac, fill=sex)) +
theme_classic() +
geom_bar() +
labs(y="Number of Participants",
title ="Space Thoughts")

ftable(xtabs(~natspac + sex + polviews, data = GSS2018)) 

#Model
GSS2018$natspac <- factor(GSS2018$natspac)
m <- polr(natspac ~ race + income + polviews + educ + sex + age + region, data = GSS2018, Hess = TRUE)
summary(m)

exp(coef(m))

#https://www.sfu.ca/~mjbrydon/tutorials/BAinR/recode.html#
