#Work Directory and libraries ----
setwd("C:/Users/nilsd/OneDrive/Documents/Statistical Learning in Marketing/Individual Assignments")

#Libraries
library(tidyverse)
library(lmtest)
library(aTSA)
library(urca)
library(strucchange)
library(vars)
library(gplots)
library(RColorBrewer)
library(lmtest)

#Importing Dataset  ----
Ixmor.df <- read_csv("timeseries.csv")

#Data overview ----
str(Ixmor.df)
summary(Ixmor.df)
sum(is.na(Ixmor.df))

# Data Overview ----

## Individual Line Plots ----

### Weeks and LnSales ----
plot(Ixmor.df$Week, Ixmor.df$LnSales, type="l", col="red", lwd=5,
     xlab="Week", ylab="Log of Ixmor Sales", main="Ixmor Sales over time (weeks)")
model_LnSales <- lm(Ixmor.df$LnSales ~ Ixmor.df$Week)
abline(model_LnSales, col = "blue", lwd = 2)
summary(model_LnSales)
#trend line shows LnSales is increasing over time, and this model is statistically significant (p = 2.09e-06)

### Weeks and Ixmor LnAdvertising ----
plot(Ixmor.df$Week, Ixmor.df$LnAdvertising, type="l", col="red", lwd=5,
     xlab="Week", ylab="Log of Ixmor Advertising", main="Ixmor Advertising over time (weeks)")
model_LnAdvertising <- lm(Ixmor.df$LnAdvertising ~ Ixmor.df$Week)
summary(model_LnAdvertising)
abline(model_LnAdvertising, col = "blue", lwd =2)
#trendline shows LnAdvertising is increasing over time, and this model is statistically significant (p = 0.00707)

### Weeks and LnCompAdvertising ----
plot(Ixmor.df$Week, Ixmor.df$LnCompAdvertising, type="l", col="red", lwd=5,
     xlab="Week", ylab="Log of Competitor Advertising", main="Competitor Adveritisng over time (weeks)")
model_LnCompAdvertising <- lm(Ixmor.df$LnCompAdvertising ~ Ixmor.df$Week)
abline(model_LnCompAdvertising, col = "blue", lwd =2)
summary(model_LnCompAdvertising)
#trendline shows LnCompAdvertising is decreasing over time, but this model is statistically insignificant (p = 0.311)

### Weeks and LnPrice ----
plot(Ixmor.df$Week, Ixmor.df$LnPrice, type="l", col="red", lwd=5,
     xlab="Week", ylab="Log of Ixmor Price", main="Ixmor Price over time (weeks)")
model_LnPrice <- lm(Ixmor.df$LnPrice ~ Ixmor.df$Week)
abline(model_LnPrice, col = "blue", lwd = 2)
summary(model_LnPrice)
#trendline shows LnPrice is decreasing over time, and this model is statistically significant (p = .00309)

### Weeks and LnCompPrice ----
plot(Ixmor.df$Week, Ixmor.df$LnCompPrice, type="l", col="red", lwd=5,
     xlab="Week", ylab="Log of Ixmor CompPrice", main="Ixmor CompPrice over time (weeks)")
model_LnCompPrice <- lm(Ixmor.df$LnCompPrice ~ Ixmor.df$Week)
abline(model_LnCompPrice, col = "blue", lwd = 2)
summary(model_LnCompPrice)
#trendline shows LnCompPrice is increasing over time, and this model is significant (p < 2e-16)

# LnSales, LnPrice, LnCompPrice, and LnAdvertising are significantly correlated to Weeks, while LnCompAdvertising is not.

## Combination Line Plots ----

### LnAdvertising and LnCompAdvertising ----
ggplot(data = Ixmor.df, aes(x = Week)) +
  geom_line(aes(y = LnAdvertising, color = "LnAdvertising")) + 
  geom_line(aes(y = LnCompAdvertising, color = "LnCompAdvertising")) + 
  scale_color_manual(values = c("LnAdvertising" = "red", "LnCompAdvertising" = "blue")) +  
  labs(x = "Week", y = "LnAdvertising", color = "Legend") +  
  theme_minimal() 

### LnPrice and LnCompPrice ----
ggplot(data = Ixmor.df, aes(x = Week)) +
  geom_line(aes(y = LnPrice, color = "LnPrice")) + 
  geom_line(aes(y = LnCompPrice, color = "LnCompPrice")) + 
  scale_color_manual(values = c("LnPrice" = "red", "LnCompPrice" = "blue")) +  
  labs(x = "Week", y = "LnPrice and LnCompPrice", color = "Legend") +  
  theme_minimal() 

## Bar Plots ----

### Advertising Bar Plots ----
barplot(Ixmor.df$LnAdvertising, main = "LnAdvertising", col = "red")
barplot(Ixmor.df$LnCompAdvertising, main = "LnCompAdvertising", col = "blue")

### Binary  stacked Bar Plot of LnAdvertising and LnCompAdvertising ----
LnAdvertising_binary <- ifelse(Ixmor.df$LnAdvertising > 0, 1, 0)
LnCompAdvertising_binary <- ifelse(Ixmor.df$LnCompAdvertising > 0, 1, 0)
barhelp.data <- data.matrix(cbind(LnAdvertising_binary, LnCompAdvertising_binary))
barhelp2.data <- t(barhelp.data)
barplot(barhelp2.data, main = "Presence of Advertising (red) and CompAdvertising (blue)", col=c("red", "blue"), xlab="weeks")

# Granger Causality Tests----

## Granger Causality for loop formula ----
find_lowest_pvalue <- function(dependent_var, independent_var, max_lag, data) {
  lowest_pvalue <- 1  
  best_lag <- 0  
  for (lag in 1:max_lag) {
    test_result <- grangertest(as.formula(paste(dependent_var, "~", independent_var)), order = lag, data = data)
    p_value <- test_result$`Pr(>F)`[2]
    if (p_value < 0.1 && p_value < lowest_pvalue) {
      lowest_pvalue <- p_value
      best_lag <- lag
    }
  }
  if (best_lag == 0) {
    message("No significant Granger causality found for any lag.")
  } else {
    message(paste("Lowest significant p-value:", lowest_pvalue, "at lag", best_lag))
  }
  return(list(best_lag = best_lag, lowest_pvalue = lowest_pvalue))
}

Ixmor.df <- as.data.frame(Ixmor.df)

# Lags (weeks) up to 15 are used, to account for atleast 1 quarter

### LnSales ~ LnAdvertising ----
LnSales_LnAdvertising <- find_lowest_pvalue("LnSales", "LnAdvertising", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0971242074723915 at lag 1

### LnSales ~ LnCompAdvertising ----
LnSales_LnCompAdvertising <- find_lowest_pvalue("LnSales", "LnCompAdvertising", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0512949190692117 at lag 1

### LnSales ~ LnPrice ----
LnSales_LnPrice <- find_lowest_pvalue("LnSales", "LnPrice", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0200102944389257 at lag 1

### LnSales ~ LnCompPrice ----
LnSales_LnCompPrice <- find_lowest_pvalue("LnSales", "LnCompPrice", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0184872116614598 at lag 11

### LnAdvertising ~ LnSales ----
LnAdvertising_LnSales <- find_lowest_pvalue("LnAdvertising", "LnSales", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0329780008824494 at lag 1

### LnAdvertising ~ LnCompAdvertising ----
LnAdvertising_LnCompAdvertising <- find_lowest_pvalue("LnAdvertising", "LnCompAdvertising", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnAdvertising ~ LnPrice ----
LnAdvertising_LnPrice <- find_lowest_pvalue("LnAdvertising", "LnPrice", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnAdvertising ~ LnCompPrice ----
LnAdvertising_LnCompPrice <- find_lowest_pvalue("LnAdvertising", "LnCompPrice", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnCompAdvertising ~ LnSales ----
LnCompAdvertising_LnSales <- find_lowest_pvalue("LnCompAdvertising", "LnSales", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0120526738145634 at lag 2

### LnCompAdvertising ~ LnAdvertising ----
LnCompAdvertising_LnAdvertising <- find_lowest_pvalue("LnCompAdvertising", "LnAdvertising", max_lag = 15, data = Ixmor.df)
#No significant Granger causality found for any lag.

### LnCompAdvertising ~ LnPrice ----
LnCompAdvertising_LnPrice <- find_lowest_pvalue("LnCompAdvertising", "LnPrice", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.000136296112651133 at lag 13

### LnCompAdvertising ~ LnCompPrice ----
LnCompAdvertising_LnCompPrice <- find_lowest_pvalue("LnCompAdvertising", "LnCompPrice", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnPrice ~ LnSales ----
LnPrice_LnSales <- find_lowest_pvalue("LnPrice", "LnSales", max_lag = 15, data = Ixmor.df)
#No significant Granger causality found for any lag.

### LnPrice ~ LnAdvertising ----
LnPrice_LnAdvertising <- find_lowest_pvalue("LnPrice", "LnAdvertising", max_lag = 15, data = Ixmor.df)
#No significant Granger causality found for any lag.

### LnPrice ~ LnCompAdvertising ----
LnPrice_LnCompAdvertising <- find_lowest_pvalue("LnPrice", "LnCompAdvertising", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnPrice ~ LnCompPrice ----
LnPrice_LnCompPrice <- find_lowest_pvalue("LnPrice", "LnCompPrice", max_lag = 15, data = Ixmor.df)
#Lowest significant p-value: 0.00712815411410155 at lag 11

### LnCompPrice ~ LnSales ----
LnCompPrice_LnSales <- find_lowest_pvalue("LnCompPrice", "LnSales", max_lag = 15, data = Ixmor.df)
# Lowest significant p-value: 0.0149253242555046 at lag 1

### LnCompPrice ~ LnAdvertising ----
LnCompPrice_LnAdvertising <- find_lowest_pvalue("LnCompPrice", "LnAdvertising", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnCompPrice ~ LnCompAdvertising ----
LnCompPrice_LnCompAdvertising <- find_lowest_pvalue("LnCompPrice", "LnCompAdvertising", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

### LnCompPrice ~ LnPrice ----
LnCompPrice_LnPrice <- find_lowest_pvalue("LnCompPrice", "LnPrice", max_lag = 15, data = Ixmor.df)
# No significant Granger causality found for any lag.

# LnAdvertising, LnCompAdvertising, and LnPrice are granger causing LnSales at 1 lag.
# LnCompPrice is granger causing LnSales at 11 lags.
# LnSales is granger causing LnAdvertising at 1 lag.
# LnSales is granger causing LnCompAdvertising at 2 lags.
# LnPrice is granger causing LnCompAdvertising at 13 lags.
# LnCompPrice is granger causing LnPrice at 11 lags
# LnSales is granger causing LnCompPrice at 1 lag

# Testing whether variables are stationary or evolving ----
# # PP test is most important. Theoretical presumption is that variables are type 2 or 3

##LnSales ----
adf.test(Ixmor.df$LnSales, nlag = NULL, output = TRUE) 
## Series is non-stationary for type 1, but stationary for type 2 and 3
pp.test(Ixmor.df$LnSales, output = TRUE)
## Series is non-stationary for type 1, but stationary for type 2 & 3
kpss.test(Ixmor.df$LnSales, output = TRUE)
## Series is non-stationary for for type 2, but stationary for type 1 & 3

# Decision: LnSales is stationary

##LnAdvertising ----
adf.test(Ixmor.df$LnAdvertising, nlag = NULL, output = TRUE) 
## Series is non-stationary for type 1, but stationary for type 2 and 3
pp.test(Ixmor.df$LnAdvertising, output = TRUE)
## Series is stationary for type 1, 2, and 3. (Type 1 is at .0696 significance)
kpss.test(Ixmor.df$LnAdvertising, output = TRUE)
## Series is non-stationary for type 1, but non-stationary for type 2 and 3

## Decision: LnAdvertising is stationary

##LnCompAdvertising ----
adf.test(Ixmor.df$LnAdvertising, nlag = NULL, output = TRUE) 
## Series is stationary for lags up to 3 (10% significance), but non-stationary afterwards for type 1. Series is stationary for type 2 and 3.
pp.test(Ixmor.df$LnAdvertising, output = TRUE)
## Series is non-stationary for type 1, and stationary for type 2 and 3.
kpss.test(Ixmor.df$LnAdvertising, output = TRUE)
## Series is non-stationary for type 1, and stationary for type 2 and 3.

# Decision: LnCompAdvertising is stationary.

##LnPrice ----
adf.test(Ixmor.df$LnPrice, nlag = NULL, output = TRUE) 
## Series is non-stationary for type 1, but stationary for type 2 and 3.
pp.test(Ixmor.df$LnPrice, output = TRUE)
## Series is non-stationary for type 1, but stationary for type 2 and 3.
kpss.test(Ixmor.df$LnPrice, output = TRUE)
## Series is stationary for all 3 types.

# Decision: LnPrice is stationary

##LnCompPrice ----
adf.test(Ixmor.df$LnCompPrice, nlag = NULL, output = TRUE) 
## Series is non-stationary for type 1, but stationary for type 2 and 3.
pp.test(Ixmor.df$LnCompPrice, output = TRUE)
## Series is non-stationary for type 1, but stationary for type 2 and 3.
kpss.test(Ixmor.df$LnCompPrice, output = TRUE)
## Series is stationary for type 1, but non-stationary for type 2 and 3.

# Decision: LnCompPrice is stationary

## Scatterplots to confirm there is no cointegration ----

### LnSales & LnAdvertising ----
ggplot(data = Ixmor.df, aes(x = LnSales, y = LnAdvertising)) +
  geom_point() +
  labs(x = "LnSales", y = "LnAdvertising", title = "Scatter Plot of LnSales vs LnAdvertising")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnSales & LnCompAdvertising ----
ggplot(data = Ixmor.df, aes(x = LnSales, y = LnCompAdvertising)) +
  geom_point() +
  labs(x = "LnSales", y = "LnCompAdvertising", title = "Scatter Plot of LnSales vs LnCompAdvertising")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnSales & LnPrice ----
ggplot(data = Ixmor.df, aes(x = LnSales, y = LnPrice)) +
  geom_point() +
  labs(x = "LnSales", y = "LnPrice", title = "Scatter Plot of LnSales vs LnPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnSales & LnCompPrice ----
ggplot(data = Ixmor.df, aes(x = LnSales, y = LnCompPrice)) +
  geom_point() +
  labs(x = "LnSales", y = "LnCompPrice", title = "Scatter Plot of LnSales vs LnCompPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnAdvertising & LnCompAdvertising ----
ggplot(data = Ixmor.df, aes(x = LnAdvertising, y = LnCompAdvertising)) +
  geom_point() +
  labs(x = "LnAdvertising", y = "LnCompAdvertising", title = "Scatter Plot of LnAdvertising vs LnCompAdvertising")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnAdvertising & LnPrice ----
ggplot(data = Ixmor.df, aes(x = LnAdvertising, y = LnPrice)) +
  geom_point() +
  labs(x = "LnAdvertising", y = "LnPrice", title = "Scatter Plot of LnAdvertising vs LnPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnAdvertising & LnCompPrice ----
ggplot(data = Ixmor.df, aes(x = LnAdvertising, y = LnCompPrice)) +
  geom_point() +
  labs(x = "LnAdvertising", y = "LnCompPrice", title = "Scatter Plot of LnAdvertising vs LnCompPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnCompAdvertising & LnPrice
ggplot(data = Ixmor.df, aes(x = LnCompAdvertising, y = LnPrice)) +
  geom_point() +
  labs(x = "LnCompdvertising", y = "LnPrice", title = "Scatter Plot of LnCompAdvertising vs LnPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnCompAdvertising & LnCompPrice
ggplot(data = Ixmor.df, aes(x = LnCompAdvertising, y = LnCompPrice)) +
  geom_point() +
  labs(x = "LnCompdvertising", y = "LnCompPrice", title = "Scatter Plot of LnCompAdvertising vs LCompPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

### LnPrice & LnCompPrice
ggplot(data = Ixmor.df, aes(x = LnPrice, y = LnCompPrice)) +
  geom_point() +
  labs(x = "LnPrice", y = "LnCompPrice", title = "Scatter Plot of LnPrice vs LCompPrice")
## Scatter plot does not show a clear line, confirms no cointegration.

# VAR ----

## Endogenous Variables ----
Ixmor.endo <- Ixmor.df[, c("LnSales", "LnAdvertising", "LnCompAdvertising", "LnPrice", "LnCompPrice")]

## Exogenous Control Variables ----
Ixmor.exo <- Ixmor.df[, c("Qrtr1", "Qrtr2", "Qrtr3")]

## Lag selection ----
lag_selection <- VARselect(Ixmor.endo, lag.max = 13, type = "const")
print(lag_selection)
#all criteria suggest to use 1 lag.

## VAR model ----
Ixmor_var <- VAR(Ixmor.endo, p=1, type = "const", exogen = Ixmor.exo)
summary(Ixmor_var)

# Results:
# LnSales is positively affected by LnSales and LnCompPrice but negatively affected by past LnPrice at lag 1.
# LnAdfvertising is positively affected by  LnAdvertising at lag 1. There are notable seasonal effects on LnAdvertising through the quarter dummies.
# LnCompAdvertising is positively affected by LnCompAdvertising at lag 1.
# LnPrice is positively affected by past LnPrice at lag 1.
# LnCompPrice is positively affected by LnSales, LnCompPrice at lag 1.
# LnAdvertising and LnCompAdveritisng have insignificant intercepts.

#Impulse Response function ----

## LnSales ----
irf_sales <- irf(Ixmor_var, impulse = NULL, response = "LnSales", n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_sales)
irf_sales_cum <- irf(Ixmor_var, impulse = NULL, response = "LnSales", n.ahead = 12, ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_sales_cum)

## LnAdvertising ----
irf_Advertising <- irf(Ixmor_var, impulse = NULL, response = "LnAdvertising", n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_Advertising)
irf_Advertising_cum <- irf(Ixmor_var, impulse = NULL, response = "LnAdvertising", n.ahead = 12, ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_Advertising_cum)

## LnCompAdvertising ----
irf_CompAdvertising <- irf(Ixmor_var, impulse = NULL, response = "LnCompAdvertising", n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_CompAdvertising)
irf_CompAdvertising_cum <- irf(Ixmor_var, impulse = NULL, response = "LnCompAdvertising", n.ahead = 12, ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_CompAdvertising_cum)

## LnPrice ----
irf_Price <- irf(Ixmor_var, impulse = NULL, response = "LnPrice", n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_Price)
irf_Price_cum <- irf(Ixmor_var, impulse = NULL, response = "LnPrice", n.ahead = 12, ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_Price_cum)

## LnCompPrice ----
irf_CompPrice <- irf(Ixmor_var, impulse = NULL, response = "LnCompPrice", n.ahead = 12, ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_CompPrice)
irf_CompPrice_cum <- irf(Ixmor_var, impulse = NULL, response = "LnCompPrice", n.ahead = 12, ortho = TRUE, cumulative = TRUE, boot = TRUE, ci = 0.68, runs = 500)
plot(irf_CompPrice_cum)

#FEVD ----
Ixmorfevd <-fevd(Ixmor_var,n.ahead = 12)
Ixmorfevd
barbasis1 <- Ixmorfevd[1]
barbasis2 = as.matrix(unlist(barbasis1),ncol =5, byrow = TRUE)

bartry = Reduce(rbind,Ixmorfevd)
bartry2 = t(bartry)
bartry2 = bartry2[,c(12,24,36, 48, 60)]

par(mar = c(5, 4, 4, 8) + 0.1)
barplot(bartry2, 
        col = brewer.pal(5, "YlOrRd"), 
        names.arg = c("LnSales", "LnAdv", "LnCompAdv", "LnPrice", "LnCompPrice"), 
        xlab = "Variables",  
        ylab = "Variance Decomposition")
legend("topright", inset = c(-0.3, 0),                       
       legend = c("Shock to LnSales", "Shock to LnAdvertising", "Shock to LnCompAdvertising", "Shock to LnPrice", "Shock to LnCompPrice"),  
       fill = brewer.pal(5, "YlOrRd"),    
       title = "Contributing Shocks", xpd = TRUE)
