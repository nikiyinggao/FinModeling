#File GARCH
Fit a GARCH(1,1) model and forecast out of sample one-month realized volatility to compare to actual realized volatility
for the S&P 500 Index

required library:

library(quantmod)
library(tseries)
library(xts)
library(stats)
library(rugarch)
library(e1071)
library(MASS)
library(zoo)
library(roll)
library(car)
