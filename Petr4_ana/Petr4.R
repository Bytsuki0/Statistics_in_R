library(dplyr)
library(ggplot2)
library(tidyr)
library(xts)
library(lubridate)
library(highfrequency)         
library(quantmod)    
library(tseries)     
library(FinTS)       
library(stochvol)    

os = Sys.info()["sysname"]

if(os == "Windows")
  data <- data <- read.csv("D:/Code/R_studio/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE, 
                           sep = ";",dec = ",")

if (os =="Linux")
  data <- data <- read.csv("~/Documentos/Coding/Statistics_in_R/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE, 
                         sep = ";",dec = ",")



dt1 <- as_tibble(data) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) 

price_xts <- as.xts(dt1$Ret.1min, order.by = dt1$Period)
price_log_xts <- as.xts(dt1$Close.1min, order.by = dt1$Period)


#data ret.1min ja esta em retorno de log essa parte é desnecessaria 
log_returns <- diff(log(price_log_xts))
log_returns[is.infinite(log_returns)] = NA
log_returns <- na.omit(log_returns)


plot(price_xts, main="PETR4 Price (1-min data)", ylab="Price", col="black")

par(mfrow=c(1,1))

hist(coredata(log_returns), breaks=50, main="Histogram of Log Returns", xlab="Log Return", probability = TRUE)
lines(density(coredata(log_returns)), col="blue", lwd=2)
qqnorm(coredata(log_returns)); qqline(coredata(log_returns), col="red")

# --- Step 4. Test for ARCH Effects ---
# Using the ARCH LM test (e.g., ArchTest from FinTS package)
arch_test <- ArchTest(coredata(log_returns), lags = 10)
print(arch_test)

# --- Step 5. Fit an ARCH and a GARCH Model ---
# Define a GARCH(1,1) specification as in equations (5.22) and (5.23)
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"  # You can try "std" for a Student's t distribution as well
)

# Fit the model to the log returns
garch_fit <- ugarchfit(spec = garch_spec, data = log_returns)
print(garch_fit)

# Extract conditional variance and standardized residuals
garch_vol <- sigma(garch_fit)
garch_resid <- residuals(garch_fit, standardize = TRUE)

# Plot the estimated conditional volatility
plot(garch_vol, main="Estimated Conditional Volatility from GARCH(1,1)", col="darkgreen")

# --- Step 6. Fit an EGARCH Model (Extension) ---
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

egarch_fit <- ugarchfit(spec = egarch_spec, data = log_returns)
print(egarch_fit)

# --- Step 7. (Optional) Fit a Stochastic Volatility Model ---
# The 'stochvol' package estimates a basic stochastic volatility model.
# This uses Bayesian estimation methods.
sv_model <- svsample(coredata(log_returns))
print(sv_model)

# Plot the latent volatility estimates
plot(sv_model, main="Stochastic Volatility Estimates")

# --- Step 8. Model Diagnostics ---
# Check standardized residuals for autocorrelation and normality
acf(garch_resid, main="ACF of Standardized Residuals")
acf(garch_resid^2, main="ACF of Squared Standardized Residuals")
qqnorm(garch_resid); qqline(garch_resid, col="red")

    
