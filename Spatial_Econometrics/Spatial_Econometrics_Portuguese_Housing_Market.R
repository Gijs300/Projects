#Install and load packages
install.packages("spdep")
install.packages("sphet")
install.packages("spatialreg")
install.packages("psych")
install.packages("sf")
install.packages("mapview")

library(spdep)
library(sphet)
library(spatialreg)
library(psych)
library(sf)
library(mapview)
library(readxl)

#Load data
PCAsanddep <- "C:/Users/gijsn/Desktop/Documents/PCAsanddep.xlsx"
data<-read_excel(PCAsanddep)
w_matrix<-"C:/Users/gijsn/Desktop/Documents/Wmatrix.xlsx"

# Convert Spatial weight matrix to listw object
w_matrix<-W_matrix[,-1]
w_matrix <- as.matrix(w_matrix)          
W_listw <- mat2listw(w_matrix, style = "W")  

#Dependent variable: Average value of buildings transacted
# Principal component regression
ols_model <- lm(precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
                data = data)
summary(ols_model)

# Moran's I test on residuals, spatial autocorrelation test
morantest<-moran.test(residuals(ols_model), W_listw)
summary(morantest)
moran.plot(precmed, W_listw)

# Lagrange Multiplier tests to choose spatial model
lm_tests <- lm.LMtests(ols_model, W_listw, test = "all")
print(lm_tests)

lm_tests2<-lm.RStests(ols_model,w_listw)


spreg(precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4, 
      data = data, HAC=TRUE, model="ivhac", listw=W_listw, distance=NULL )

#SARAR model
sarma_model <- lagsarlm(
  precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw,
  type = "sarma",
)
summary(sarma_model)

#SAR model
sar_model <- lagsarlm(
  precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw,Durbin=TRUE, type="mixed"
)
summary(sar_model)

#SEM model
sem_model <- errorsarlm(
  precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw
)
summary(sem_model)

#SARAR with robust standard errors
sarma_model <- sacsarlm(
  precmed ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,           
  listw = W_listw              
)
summary(sarma_model)

# Compute the robust variance-covariance matrix
vcov_robust <- vcov(sarma_model, method = "robust")

# Calculate the robust standard errors
robust_se <- sqrt(diag(vcov_robust))

# Extract coefficients from the model summary
coefs <- summary(sarma_model)$coefficients

# Combine the coefficients and robust standard errors
robust_results <- cbind(
  Estimate = coefs[, 1],       
  Robust_SE = robust_se,        
  z_value = coefs[, 1] / robust_se,  
  Pr_z = 2 * pnorm(-abs(coefs[, 1] / robust_se))  
)
print(robust_results)

#Dependent variable: Median bank valuation per sq. meter
# Principal component regression
ols_model <- lm(medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
                data = data)
summary(ols_model)

# Moran's I test on residuals, spatial autocorrelation test
morantest <- moran.test(residuals(ols_model), W_listw)
summary(morantest)
moran.plot(medsq2, W_listw)

# Lagrange Multiplier tests to choose spatial model
lm_tests <- lm.LMtests(ols_model, W_listw, test = "all")
print(lm_tests)

lm_tests2 <- lm.RStests(ols_model, w_listw)

spreg(medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4, 
      data = data, HAC = TRUE, model = "ivhac", listw = W_listw, distance = NULL)

# SARAR model
sarma_model <- lagsarlm(
  medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw,
  type = "sarma"
)
summary(sarma_model)

# SAR model
sar_model <- lagsarlm(
  medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw, Durbin = TRUE, type = "mixed"
)
summary(sar_model)

# SEM model
sem_model <- errorsarlm(
  medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,
  listw = W_listw
)
summary(sem_model)

# SARAR with robust standard errors
sarma_model <- sacsarlm(
  medsq2 ~ D1 + D2 + D3 + D4 + D5 + S1 + S2 + S3 + S4,
  data = data,           
  listw = W_listw              
)
summary(sarma_model)

# Compute the robust variance-covariance matrix
vcov_robust <- vcov(sarma_model, method = "robust")

# Calculate the robust standard errors
robust_se <- sqrt(diag(vcov_robust))

# Extract coefficients from the model summary
coefs <- summary(sarma_model)$coefficients

# Combine the coefficients and robust standard errors
robust_results <- cbind(
  Estimate = coefs[, 1],       
  Robust_SE = robust_se,        
  z_value = coefs[, 1] / robust_se,  
  Pr_z = 2 * pnorm(-abs(coefs[, 1] / robust_se))  
)

print(robust_results)
