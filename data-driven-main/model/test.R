library(readxl)
library(ggplot2)
library(MASS)
library(faraway)

df = read_excel('Dm_model_2.xlsx')

lm1_1 = lm(`Avg Number of Seats` ~ .-State-`Average Starting MSRP`-`Average Safety Score`-`Most Common Fuel Type`,
         data = df)
summary(lm1_1)
boxcox(`Average Number of Seats` ~ .-State-`Average Starting MSRP`-`Average Safety Score`-`Most Common Fuel Type`,
       data = df,plotit=TRUE, lambda = seq(6,18,by=0.1))
lm1_2 = lm(((`Average Number of Seats`)^11-1)/11 ~ .-State-`Average Starting MSRP`-`Average Safety Score`-`Most Common Fuel Type`,
         data = df)
summary(lm1_2)
plot(lm1_1)
vif(lm1_1)


lm2_1 = lm(`Average Starting MSRP` ~ .-State-`Average Number of Seats`-`Average Safety Score`-`Most Common Fuel Type`,
         data = df)
summary(lm2_1)
boxcox(`Average Starting MSRP` ~ .-State-`Average Number of Seats`-`Average Safety Score`-`Most Common Fuel Type`,
       data = df,plotit=TRUE, lambda = seq(0,10,by=0.1))
lm2_2 = lm(((`Average Starting MSRP`)^4-1)/4 ~ .-State-`Average Number of Seats`-`Average Safety Score`-`Most Common Fuel Type`,
           data = df)
summary(lm2_2)
plot(lm2_1)
vif(lm2_1)

