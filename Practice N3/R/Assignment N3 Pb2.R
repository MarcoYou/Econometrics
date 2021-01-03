library(plm)
library(AER)
library(stargazer)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)

# Produce a table with descriptive statistics of these variables
data(Guns)
stargazer(Guns, type = "text")
summary(Guns)

# declare columns as variables
violent <- Guns$violent
law <- as.numeric(Guns$law)
density <- Guns$density
income <- Guns$income
population <- Guns$population
afam <- Guns$afam
cauc <- Guns$cauc
male <- Guns$male
state <- Guns$state
year <- Guns$year

# build separate matrices for violent (Y) and other variables (X)
Y  <- cbind(violent)
X  <- cbind(law, density, income, population, afam, cauc, male)

# new data.frame created from Guns modifying law's class from factor to numeric
Guns1 <- with(Guns, data.frame(violent, as.numeric(law) - 1, density, income, population, afam, cauc, male,
                               as.factor(state), as.factor(year)))
# rename columns
colnames(Guns1)[c(2,9,10)] <- c("law","state","year")

# Time average of Y and X within a state
ave.time.Y <- cbind(ave(Guns1$violent, Guns1$state, FUN = mean))
ave.time.X <- cbind(ave(Guns1$law, Guns1$state),    ave(Guns1$density, Guns1$state),
                    ave(Guns1$income, Guns1$state), ave(Guns1$population, Guns1$state),
                    ave(Guns1$afam, Guns1$state),   ave(Guns1$cauc, Guns1$state),
                    ave(Guns1$male, Guns1$state))

# Entity average of Y and X within a year
ave.state.Y  <- cbind(ave(Guns1$violent, Guns$year, FUN = mean))
ave.state.X  <- cbind(ave(Guns1$law, Guns1$year),    ave(Guns1$density, Guns1$year),
                      ave(Guns1$income, Guns1$year), ave(Guns1$population, Guns1$year),
                      ave(Guns1$afam, Guns1$year),   ave(Guns1$cauc, Guns1$year),
                      ave(Guns1$male, Guns1$year))

# demeaned Y and X with state index (Y/ave.time.Y because we will log it)
Y.dem.state = Y/ave.time.Y
X.dem.state = X - ave.time.X
# demeaned Y and X with time index (Y/ave.state.Y because we will log it)
Y.dem.time  = Y/ave.state.Y
X.dem.time  = X - ave.state.X

# estimation WITHOUT fixed effects
reg <- lm(formula= log(Y) ~ X, data= Guns)
stargazer(reg, type = "text")
# PANEL linear model without fixed effects
regp <- plm(formula= log(Y) ~ X, data= Guns, index= c("state","year"), model= "pooling", effect= "individual")
stargazer(regp, type = "text")

# estimation with ENTITY fixed effects
reg.state <- lm(formula= log(Y) ~ X + state - 1, data= Guns)
stargazer(reg.state, type = "text")

# estimation with TIME fixed effects
reg.year <- lm(formula= log(Y) ~ X + year - 1, data= Guns)
stargazer(reg.year, type = "text")

# estimation with ENTITY and TIME fixed effects
reg.state.year <- lm(formula= log(Y) ~ X + state + year - 1, data= Guns)
stargazer(reg.state.year, type = "text")

# LINEAR within transformed model with ENTITY fixed effects
l.within.state <- lm(formula= log(Y.dem.state) ~ X.dem.state, data= Guns1)
stargazer(l.within.state, type= "text")
# PANEL within transformed model with ENTITY fixed effects
within.state <- plm(formula= log(Y) ~ X, data= Guns, index= c("state","year"), 
                    model= "within", effect= "individual")
stargazer(within.state, type = "text")
fixef(within.state)

# LINEAR within transformed model with TIME fixed effects
l.within.time  <- lm(formula= log(Y.dem.time) ~ X.dem.time, data= Guns1)
stargazer(l.within.time, type= "text")
# PANEL within transformed model with TIME fixed effects
within.year <- plm(formula= log(Y) ~ X, data= Guns, index= c("state","year"), 
                   model= "within", effect= "time")
stargazer(within.year, type = "text")
fixef(within.year)

# LINEAR within transformed model with TWO-WAY fixed effects
ave.Y <- ave(Y)
ave.X <- ave(X)
Y.dem.twoway <- ((Y * ave.Y)/(ave.state.Y * ave.time.Y))
X.dem.twoway <- X - ave.state.X - ave.time.X + ave.X
l.within.twoway<- lm(formula= log(Y.dem.twoway) ~ X.dem.twoway, data= Guns1)
stargazer(l.within.twoway, type= "text")
# PANEL within transformed model with TWO-WAY fixed effects
within.state.year <- plm(formula= log(Y) ~ X, data= Guns, index= c("state","year"), model= "within", effect= "twoways")
stargazer(within.state.year, type="text")
fixef(within.state.year)

# SE under homoskedasticity, heteroskedasticity, and clustered (for only ENTITY fixed effects)
lfit  <- reg.state
pfit  <- within.state

# variance-covariance matrices
vcov.homo     <- sandwich::vcovHC.default(lfit,  type = "const") # homoskedasticity
vcov.hetero   <- sandwich::vcovHC.default(lfit,  type = "HC0")   # heteroskedasticity
vcov.cluster  <- plm::vcovHC.plm(pfit, type = c("HC0"), cluster = c("group")) # clustered
# SE test
stargazer(coeftest(lfit, vcov = vcov.homo),    type= "text") # SE = 0.018715 # compare with summary(reg.state)
stargazer(coeftest(lfit, vcov = vcov.hetero),  type= "text") # SE = 0.019168
stargazer(coeftest(pfit, vcov = vcov.cluster), type= "text") # SE = 0.041169

### unused codes ###
# demean(Guns$violent)
# Guns.binary <- with(Guns, data.frame(Guns))  # by default string == factor
# Guns.binary$law <- as.character(Guns.binary$law) # from factor to character
# Guns.binary$law[Guns.binary$law == "yes"] <- 1
# Guns.binary$law[Guns.binary$law == "no"]  <- 0

# Guns.dem <- with(Guns, data.frame(violent = violent - ave(violent, state), law, state, year))
# log(Y) - log(mean.Y) = log(Y/mean.Y)
# reg.state1 <- lm(formula= log(violent) ~ law -1, data= Guns.dem)
# stargazer(reg.state1, type = "text")

# Guns %>% 
#  ggplot(aes(x=law,y=log(violent))) + 
#  geom_point(alpha=0.3) + 
#  geom_abline(intercept = b0.hat.lm, slope = b1.hat.lm, col = "black", na.rm = FALSE)