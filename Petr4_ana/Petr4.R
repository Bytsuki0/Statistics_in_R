library(tidyverse)
library(xts)
library(lubridate)
library(highfrequency)
library(xts)         # For time series objects
library(quantmod)    # For financial data handling
library(rugarch)     # For ARCH/GARCH family models
library(tseries)     # For time series tests
library(FinTS)       # For ARCH tests (e.g., ArchTest)
library(stochvol)    # For stochastic volatility estimation

# --- Step 1. Data Import and Preparation ---
# Read the CSV file. Adjust the 'sep' or header arguments if needed.
# Assume the CSV has columns: "Date", "Time", "Price" (or "Close")
data <- data <- read.csv("D:/Code/R_studio/Petr4_ana/dt_1min_PETR4_2021_metatrader.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE, 
                         sep = ";",dec = ",")

# If the file includes separate date and time columns, combine them:
# (Assume the date format is "YYYY-MM-DD" and time is "HH:MM:SS")
dt1 <- as_tibble(data) %>% 
  mutate(Period = ymd_hms(X)) %>%
  select(-X) 


# If your file already contains a datetime column, use that column instead.
# Create an xts object using the price column (adjust the column name if needed).
price_xts <- as.xts(dt1$Ret.1min, order.by = dt1$Period)

# --- Step 2. Compute Log Returns ---
# Compute log returns: r_t = log(P_t) - log(P_t-1) as in equation (5.1)
log_returns <- diff(log(price_xts))
log_returns <- na.omit(log_returns)

# Plot the price and returns
par(mfrow=c(2,1))
plot(price_xts, main="PETR4 Price (1-min data)", ylab="Price", col="blue")
plot(log_returns, main="Log Returns (1-min)", ylab="Return", col="red")
par(mfrow=c(1,1))

# --- Step 3. Basic Data Diagnostics ---
# Plot histogram and Q-Q plot for returns
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

# Summary:
# This script demonstrates how to:
#   • Compute log returns from high-frequency price data (as in equation 5.1).
#   • Test for ARCH effects in the return series.
#   • Fit a basic ARCH/GARCH model (equations 5.22-5.25) using rugarch.
#   • Explore extensions like EGARCH to capture asymmetries.
#   • Optionally estimate a stochastic volatility model.
#
# These steps implement the core ideas from Chapter 5 “Modelos para a Volatilidade”
# as summarized above (see :contentReference[oaicite:0]{index=0}, :contentReference[oaicite:1]{index=1}, and :contentReference[oaicite:2]{index=2}).

# End of R script.
