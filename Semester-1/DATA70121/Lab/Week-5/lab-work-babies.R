# read the given table data and attach it
babies.data <- read.table('babies.data', header = TRUE)

# the attach function makes it easier to call all variables available in our data set by their name
attach(babies.data)

# using the dim command to see how many observations and variables are available in the data set.
# dim() = returns the lengths of the row.names attribute of x and of x 
#        (as the numbers of rows and columns respectively).
dim(babies.data)

# can check the beginning and end of the data
head(babies.data)
tail(babies.data)
View(babies.data)

# now we are fitting 2 models
# model 1 (fit0) -> this with only th eintercept term (known as 'null model')
# model 2 (fit1) -> this one with variable 'mom.weight' as predictor
# the model 2 is to see if the weight of the mother can be used as a predictor for babies weight.

fit0 <- lm(birth.weight ~ 1)
fit1 <- lm(birth.weight ~ mom.weight)

summary(fit0)
summary(fit1)

# we can plot and visualise the fitted model 'fit1'
# type = 'p' specifies scatter plot
# pch = 16 specifies to plot solid points (black solid in this case)
# main is the heading of plot, others are self-explanatory
plot(mom.weight, birth.weight, type = 'p', col = 'black' , pch = 16,
     main = "birth weight predicted by mom's weight", 
     xlab = "mom's weight", ylab = "birth weight")

# this abline() command is used to add a line to the plot and as fit1 in a regression model,
# this gives the best fit line
abline(fit1, col='red')

# coefficient estimate seems to be highly significant, we can see through r^2 in summary(fit1)
# that it is a very weak linear relationship. As this is a simple linear regression model,
# the r^2 value is simply square of correlation coefficients.

# this command is to see how all variables correlate with 'birth.weight'
cor(babies.data, birth.weight)

# by seeing the output below, we can see that b/w birth.weight & mom.weight, cor is 0.1559.
# ( and r^2  = 0.1559 x 0.1559 = 0.0243, which we got in summary(fit1) output).
# by the output, we can see 'gestation' has the strongest linear relationship with birth.weight


# now we make another model (fit2), with gestation as the predictor
fit2 <- lm(birth.weight ~ gestation)
summary(fit2)

# the plot function to plot and see like the one above
plot(gestation, birth.weight)
abline(fit2, lty = 2, col = "red")

# here we are making cook's distance plot which is a way to find outcomes that may distort
# results because they are outliers
case.numbers <- 1:length(gestation)
influence <- cooks.distance(fit2)
plot(case.numbers, influence)

# identify() command makes the plot clickable and get the point number for the outliers
# we run this on the scatter plots
identify( gestation, birth.weight)

# now trimming the data by removing these
trim.data <- babies.data[c(-239, -820),]
detach(babies.data)
attach(trim.data)

dim(trim.data)
fit2 <- lm(birth.weight ~ gestation)
summary(fit2)
# the results got a little better

# next model with multiple regression models
fit3 <- lm(birth.weight ~ gestation + mom.smokes)

par(mfrow = c(2,2))
plot(fit3)
dev.off()


