library(sp)
library(spData)
library(spgwr)
plot(gSRDF)
summary(gSRDF)
data(gSRouter)
install.packages(sf)
library(sf)
library(spdep)
#Exploratory Spatial Data Analysis 
getwd()
airbnb_Chicago_sf = st_read("~/Desktop/airbnb/airbnb_Chicago.shp")
table(st_is_valid(airbnb_Chicago_sf))

library(sf)
file_path <- file.choose(airbnb_Chicago.shp)
shapefile <- st_read("~/Desktop/airbnb/airbnb_Chicago2015.shp")
summary(shapefile)
plot(shapefile)

library(readxl)
Chi_SC <- read_excel("socioeconomic_indicators_in_Chicago.xls")
nrow(Chi_SC) 
head(Chi_SC)
summary(Chi_SC)
library(pastecs)
library(stats)
stat.desc(Chi_SC)
hist(Chi_SC$PctPov,main="Histogram of Proverty Rate",xlab="Proverty Rate",breaks=25)

library(readxl)
Chi_SC <- read_excel("Desktop/airbnb/socioeconomic_indicators_in_Chicago.xls")
View(Chi_SC)
nrow(Chi_SC) 
head(Chi_SC)
summary(Chi_SC)
library(pastecs)
library(stats)
stat.desc(Chi_SC)
hist(Chi_SC$PctHOUSINGCROWDED,main="Histogram of Housing Crowded Rate",xlab="Housing Crowded Rate",breaks=15)
qqnorm(Chi_SC$PctHOUSINGCROWDED, main = "QQ Plot of Housing Crowded Rate")
qqline(Chi_SC$PctHOUSINGCROWDED, main = "QQ Plot of Housing Crowded Rate")
shapiro.test(Chi_SC$PctHOUSINGCROWDED)

log_transformed_PctHOUSINGCROWDED <- log(Chi_SC$PctHOUSINGCROWDED)
hist(log_transformed_PctHOUSINGCROWDED, main="Log Histogram",xlab="Housing Crowded Rate",breaks=15)

hist(Chi_SC$`PctHOUSEHOLDS BELOW POVERTY`,main="Histogram of Households Below Poverty Rate ",xlab="Households Below Poverty Rate",breaks=15)
qqnorm(Chi_SC$`PctHOUSEHOLDS BELOW POVERTY`, main = "QQ Plot of Households Below Poverty Rate")
qqline(Chi_SC$`PctHOUSEHOLDS BELOW POVERTY`, main = "QQ Plot of Households Below Poverty Rate")
log_transformed_PctHOUSEHOLDSBELOWPOVERTY<- log(Chi_SC$`PctHOUSEHOLDS BELOW POVERTY`)
hist(log_transformed_PctHOUSEHOLDSBELOWPOVERTY, main="Log Histogram",xlab="Households Below Poverty Rate",breaks=15)

hist(Chi_SC$`PctAGED16+UNEMPLOYED`,main="Histogram of Unemployment Rate ",xlab="Unemployment Rate",breaks=15)
qqnorm(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Unemployment Rate")
qqline(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Unemployment Rate")
# Transform data using square root
transformed_data <- sqrt(Chi_SC$`PctAGED16+UNEMPLOYED`)
hist(transformed_data, main="SQRT Histogram",xlab="Unemployment Rate",breaks=15)


hist(Chi_SC$`PctNONDIPLOMA`,main="Histogram of Without Diploma Rate ",xlab="Without Diploma Rate",breaks=15)
qqnorm(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Without Diploma Rate")
qqline(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Without Diploma Rate")
log_transformed_PctNONDIPLOMA<- log(Chi_SC$`PctNONDIPLOMA`)
hist(log_transformed_PctNONDIPLOMA, main="Log Histogram",xlab="Without Diploma Rate",breaks=15)

hist(Chi_SC$`PctAGEDUNDER18OROVER64`,main="Histogram of Dependent People Rate ",xlab="Dependent People Rate",breaks=15)
qqnorm(Chi_SC$`PctAGEDUNDER18OROVER64`, main = "QQ Plot of Dependent People Rate")
qqline(Chi_SC$`PctAGEDUNDER18OROVER64`, main = "QQ Plot of Dependent People Rate")
log_transformed_PctAGEDUNDER18OROVER64<- log(Chi_SC$`PctAGEDUNDER18OROVER64`)
hist(log_transformed_PctAGEDUNDER18OROVER64, main="Log Histogram",xlab="Dependent People Rate",breaks=15)
transformed_data1 <- sqrt(Chi_SC$`PctAGEDUNDER18OROVER64`)
hist(transformed_data1, main="SQRT Histogram",xlab="Dependent People Rate",breaks=15)


hist(Chi_SC$`PctNONDIPLOMA`,main="Histogram of Without Diploma Rate ",xlab="Without Diploma Rate",breaks=15)
qqnorm(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Without Diploma Rate")
qqline(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Without Diploma Rate")
log_transformed_PctNONDIPLOMA<- log(Chi_SC$`PctNONDIPLOMA`)
hist(log_transformed_PctNONDIPLOMA, main="Log Histogram",xlab="Without Diploma Rate",breaks=15)

hist(Chi_SC$`HARDSHIPINDEX`,main="Histogram of Hardship Rate ",xlab="Hardship Rate",breaks=30)
qqnorm(Chi_SC$`HARDSHIPINDEX`, main = "QQ Plot of Hardship Rate")
qqline(Chi_SC$`HARDSHIPINDEX`, main = "QQ Plot of Hardship Rate")
log_transformed_HARDSHIPINDEX<- log(Chi_SC$`HARDSHIPINDEX`)
hist(log_transformed_HARDSHIPINDEX, main="Log Histogram",xlab="Hardship Rate",breaks=30)
transformed_data2 <- sqrt(Chi_SC$`HARDSHIPINDEX`)
hist(transformed_data2, main="SQRT Histogram",xlab="Hardship Rate",breaks=15)


qqnorm(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Log Unemployment Rate")
qqline(Chi_SC$`PctAGED16+UNEMPLOYED`, main = "QQ Plot of Unemployment Rate")

hist(Chi_SC$`PctAGEDUNDER18OROVER64`,main="Histogram of Dependent People Rate ",xlab="Dependent People Rate",breaks=15)
qqnorm(Chi_SC$`PctAGEDUNDER18OROVER64`, main = "QQ Plot of Dependent People Rate")
qqline(Chi_SC$`PctAGEDUNDER18OROVER64`, main = "QQ Plot of Dependent People Rate")
log_transformed_PctAGEDUNDER18OROVER64<- log(Chi_SC$`PctAGEDUNDER18OROVER64`)
# Transform data using square transformation
transformed_data3 <- Chi_SC$`PctAGEDUNDER18OROVER64`^2
hist(transformed_data3, main="Square Histogram",xlab="Dependent People Rate",breaks=15)
transformed_data1 <- sqrt(Chi_SC$`PctAGEDUNDER18OROVER64`)
hist(transformed_data1, main="SQRT Histogram",xlab="Dependent People Rate",breaks=15)

transformed_data4 <- Chi_SC$`HARDSHIPINDEX`^2
hist(transformed_data4, main="Square Histogram",xlab="Hardship Rate",breaks=15)

#Exploratory Spatial Data Analysis (2pts): 
library(sp)
library(spgwr)
library(spData)
data(georgia)
plot(gSRDF)
data(gSRouter)
install.packages(sf)
library(sf)
library(spdep)
#Exploratory Spatial Data Analysis 
library(readxl)
Chi_SC <- read_excel("Desktop/airbnb/socioeconomic_indicators_in_Chicago.xls")
class(st_geometry(Chi_SC))
head(Chi_SC)
Chi_sc <- poly2nb(Chi_SC, queen= FALSE)
GA_sc
GA_sf_as_sp <- as(Chi_SC, "Spatial")
par = (mar=c(0,0,0,0))
plot(GA_sf_as_sp, border="yellow")
plot(GA_sc, coordinates(GA_sf_as_sp), add=TRUE, col="blue")
#Global Moran’s I
lw <- nb2listw(Chi_SC, style = "B")
names (GA_sf)
Rate <- GA_sf$PctPov
head(Rate)
#Monte Carlo permutation
moran.test(Rate, listw= lw, randomisation = FALSE)
set.seed(800)
moran.mc(Rate, listw=lw, nsim=1999)
EBImoran.mc(Rate, listw = lw, nsim =199)
#Moran Scatterplot
moran.plot(Rate,listw=nb2listw(GA_sc, style="C"))
NY8_sf = st_read(system.file("shapes/Chicago_parcels.shp", package="spData") )
table(st_is_valid(NY8_sf))

GA_sf = st_read(system.file("shapes/shapefile <- st_read("Users/jennifer/Desktop/airbnb/airbnb_Chicago2015.shp")", package="spgwr") )
shapefile <- st_read("Users/jennifer/Desktop/airbnb/Chicago_parcels.shp")
GA_sf <- st_read("Users/jennifer/Desktop/airbnb/chicago_parcels/Chicago_parcels.shp")



library(readxl)
Chi_SC <- read_excel("Desktop/airbnb/Chi_sc.xls")
nrow(Chi_SC)
head(Chi_SC)
library(lmtest)

# Fit a linear regression model
m1 <- lm(PctHOUSINGCROWDED ~ PctHOUSEHOLDSBELOWPOVERTY + PctNONDIPLOMA + PctDependentpeople, data = Chi_SC)
summary(m1)
# Perform the LM test for heteroscedasticity
lmtest_result <- bptest(model)
# Residual plot (Residuals vs. Fitted Values)
residuals_m1 <- residuals(m1)
fitted_values_m1 <- fitted(m1)
library(spdep)    
library(ggplot2)  
library(tseries) 
library(lmtest)  

ggplot() +
  geom_point(aes(x = fitted_values_m1, y = residuals_m1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values for m2")


# Print the LM test results
print(lmtest_result)

m_0 <- lm(PctHOUSINGCROWDED ~ PctHOUSEHOLDSBELOWPOVERTY + PctNONDIPLOMA + PctDependentpeople)
summary(m_0)
lmtest_result <- bptest(ml)

install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)
robust_cov <- vcovHC(model)
robust_se <- coeftest(model, vcov = vcovHC(model))
# View the robust standard errors
print(robust_se)




# Load required packages
library(spdep)

w <- nb2listw(spdep::poly2nb(spdep::coordinates(spatial coordinates), queen = TRUE))

# Print the Moran's I test result
print(moran_test)

getwd(geo_export.shp)
setwd(geo_export.shp)

library(readxl)
Chi_SC <- read_excel("Desktop/airbnb/socioeconomic_indicators_in_Chicago.xls")
head(Chi_SC)
library(lmtest)
# Fit a linear regression model
m1 <- lm(PctHOUSINGCROWDED ~ PctHOUSEHOLDSBELOWPOVERTY + PctNONDIPLOMA + PctDependentpeople, data = Chi_SC)
summary(m1)
lmtest_result <- bptest(m1)
print(lmtest_result)

m2 <- lm(PctHOUSINGCROWDED ~ PctNONDIPLOMA + PctDependentpeople, data = Chi_SC)
summary(m2)
lmtest_result <- bptest(m1)
print(lmtest_result)

m3 = step(m1)
summary(m3)
n <- nrow(Chi_SC) #n is the sample size
m4 <- step(m1, k=log(n))
summary(m4)
lmtest_result2 <- bptest(m2)
print(lmtest_result2)

lmtest_result3 <- bptest(m4)
print(lmtest_result)

