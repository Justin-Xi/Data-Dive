library(readxl)
library(ggplot2)
df = read_xlsx('Dm.xlsx')

ggplot(df, aes(x = `EWM Education Score`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.01, fill = "skyblue", 
                 color = "black") +
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title='Education', 
       x=('Education Score'),y=('Density'))


ggplot(df, aes(x = `Disability Rate (%)`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", 
                 color = "black") +
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title='Disability Rate (%)', 
       x=('Disability Rate (%)'),y=('Density'))


ggplot(df, aes(x = `TOPSIS Income Score(0-1)`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.03, fill = "skyblue", 
                 color = "black") +
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title='Income', 
       x=('Income Score'),y=('Density'))

ggplot(df, aes(x = `Households with Children Under 18 (%)`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.3, fill = "skyblue", 
                 color = "black") +
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title='Households with Children under 18 (%)', 
       x=('Households with Children Under 18 (%)'),y=('Density'))

df$Total_income = df$`Households Mean income (dollars)`+df$`Families Mean income (dollars)`+df$`Married-couple families Mean income (dollars)`+df$`Nonfamily households Mean income (dollars)`
ggplot(df, aes(x = Total_income)) +
  geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "skyblue", 
                 color = "black") +
  geom_density(alpha = .2, fill = "#FF6666")+
  labs(title='Total Mean Income', 
       x=('Total Mean Income'),y=('Density'))

r_squared = cor(df$Total_income,df$`Disability Rate (%)`)
ggplot(df, aes(x = Total_income, y = `Disability Rate (%)`)) +
  geom_point() +  
  geom_smooth(method = "lm", color = 'green',se = FALSE, formula = y~x)+
  geom_smooth(method = 'lm', color = 'red', se = FALSE, formula = y~x+I(x^2))+
  geom_smooth(method = 'lm', color = 'blue', se = FALSE, formula = y~x+I(x^2)+I(x^3))+
  labs(title='Income vs Disability Rates', x='Total Mean Income')+
  geom_text(aes(label = sprintf("R² = %.2f", r_squared), x = Inf, y = Inf), hjust = 1.1, vjust = 1.1, check_overlap = TRUE)

  
  
  