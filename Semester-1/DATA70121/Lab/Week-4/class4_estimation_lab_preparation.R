##### DATA70121: Statistics and Machine Learning 1
##### Department of Social Statistics, University of Manchester
##### Lecture 4: Estimation
##### Lecturer: Diego Perez Ruiz (diego.perezruiz@manchester.ac.uk)
##### Teacher Assistants: Wei Zhuang (wei.zhuang-2@postgrad.manchester.ac.uk) 
##### Teacher Assistants: Shing Yan Kwong <shingyan.kwong@postgrad.manchester.ac.uk>

# ------------------------------------ Normal Distribution ------------------------------------
# ---------------------------------------------------------------------------------------------

# One random value with the mean of 14.4 and the standard deviation of 3.1 (Normal Distribution).
rnorm(1,14.4,3.1)
?rnorm

# 100 random values with the mean of 14.4 and the standard deviation of 3.1 (Normal Distribution).
x <- rnorm(100,14.4,3.1)
x
# Showing the first 20 values.
x[1:20]

# Histogram of the values ('main' identifies the title of histogram).
hist(x, main = "100 values from N(14.4,3.1)")


mean(x) # Mean.
sd(x) # Standard deviation.

# Generate a random sample of 100 observations for 1000 times in a loop and storing the mean values in `results`.
results <- c() # Pre-define empty vector of results (c: combine values into a Vector).

for (i in 1:1000){			
  x <- rnorm(100,14.4,3.1)	
  mu <- mean(x)				
  results <- c(results, mu)
}

# this is the 2nd way to create this by using matrix
results2 <- matrix(0, nrow =1000, ncol = 1) # matrix command to make a matrix
# matrix( the value we want to apply for all places, the no. of rows, the no. of columns)
results2

for (i in 1:1000){			
  x <- rnorm(100,14.4,3.1)	
  mu <- mean(x)				
  results2[i,1] <- mu #
}

# this command apply is for applying mean.
# apply(on what we want to apply, 1 if row & 2 if column acc. to what we want to operate on, the function we want to apply)
apply(results2,2, mean)


results3 <- array(0, dim = 1000)
results3


for (i in 1:1000){			
  x <- rnorm(100,14.4,3.1)	
  mu <- mean(x)				
  results3[i] <- mu
}

results3
mean(results3)

?apply
# Showing the first 5 values.
results[1:5]

# Histogram of the stored mean values.
hist(results, main = "1000 averages from N(14.4,3.1)")

# ------------------------------------ Uniform Distribution ------------------------------------
# ----------------------------------------------------------------------------------------------

# 100 random values with the lower limit of 0 and the upper limit of 10 (Uniform Distribution).
x <- runif(100,0,10)

# Showing the first 10 values.
x[1:10]

mean(x) # Mean.
sd(x) # Standard deviation.

# Histogram of the values 
hist(x, main = "100 values from Uniform(0,10)")

# Generate a random sample of 100 observations for 1000 times in a loop and storing the mean values in `results`.
results <- c()	
for (i in 1:1000) {  
  results <- c(results, mean(runif(100,0,10))) }

mean(results)
sd(results)

# Histogram of the stored mean values.
hist(results, main="1000 averages from Uniform(0,10)")

# ------------------------------------ Maximum Likelihood Estimation ------------------------------------
# -------------------------------------------------------------------------------------------------------

samplesize <- 30
p <- 0.5
n <- 10

# 30 random values with the probability of 0.5 (50%) and the number of trials of 10 (Binomial Distribution).
data <- rbinom(samplesize, prob = 0.5, size = n)
table(data) # Contingency table of 'data'.

data_fac <- factor(data, levels = 0:n) # Adding levels for plotting (number of wins = 0:10).
par(las = 1, cex.lab = 1.2) # Defining style of figure.
barplot(table(data_fac), xlab = "Number of successes", ylab = "Frequency") # Bar graph of the contingency table.
hist(data, main = "Frequency of getting heads") # Histogram of data.

# 10 probabilities (Binomial Distribution).
binom1 <- dbinom(x = 0:10, prob = p, size = n) 
sum(binom1) # Sum of probabilities = 1.

plot(binom1, type = "h", col = "black", 
     xlab = "Number of successes", ylab = "Probability") # Bar graph of 10 probabilities.
points(binom1, col = "black", pch = 19) # Adding points to the plotted bar graph


##################### Varying P and N #####################

################### varying p.
p_vec <- c(0.2, 0.5, 0.9)
x <- 0:n # x = 0:10.
par(las = 1, cex.lab = 1.2) # Defining style of figure.

plot(x, y = dbinom(x, prob = p_vec[1], size = n), type = "n",
     xlab = "Number of successes", ylab = "Probability", ylim = c(0, 0.5)) # Figure.

for (i in 1:length(p_vec)){
  points(x, y = dbinom(x, prob = p_vec[i], size = n), col = i, type = "b",
         pch = 19, lty = 2) # Figure.
} # Adding probabilities to the figure.
legend("top", legend = p_vec, col = 1:3, pch = 19, title = "p") # Adding legends to the figure.


################### varying n.
n_vec <- c(5, 10, 20)
p <- 0.5 # p = 0.5 (50%).
x <- 0:20 # x = 0:20.
par(las = 1, cex.lab = 1.2) # Defining style of figure.

plot(x, y = dbinom(x, prob = p, size = n_vec[1]), type = "n",
     xlab = "Number of successes", ylab = "Probability")
for (i in 1:length(n_vec)){
  points(x, y = dbinom(x, prob = p, size = n_vec[i]), col = i, type = "b",
         pch = 19, lty = 2) # Figure.
}
legend("topright", legend = n_vec, col = 1:3, pch = 19, title = "N") # Adding legends to the figure.

################### Likelihood
# data: 30 random values with the probability of 0.5 (50%) and the number of trials of 10 (Binomial Distribution).
dbinom(x = data, prob = 0.5, size = 10) # Probability of every value in 'data'.
prod(dbinom(x = data, prob = 0.5, size = 10)) # Overall likelihood.
sum(dbinom(x = data, prob = 0.5, size = 10, log = T)) # Log likelihood.


# Defining a function. Inputs of this function are prob, n, and data (the likelihood for any parameter values).
log_likelihood <- function(par_p, par_n, data)
{
  LL <- sum(dbinom(x = data, prob = par_p, size = par_n, log = T))
  return(LL)
}

log_likelihood(par_p = 0.5, par_n = 10, data = data) # An example.


# visualizing the data with models of different parameter values.
par(las = 1, cex.lab = 1.2)
prop_dat <- table(data_fac)/samplesize
xbars <- barplot(prop_dat, xlab = "Number of successes",
                 ylab = "Proportion", ylim = c(0, 0.4))
p_vec <- seq(0.2, 0.8, by = 0.1) # Parameter values.
col_vec <- rainbow(length(p_vec))
n <- 10
x <- 0:10
for (i in 1:length(p_vec)) {
  points(xbars, dbinom(x, prob = p_vec[i], size = n), type = "b", col = col_vec[i], lty = 2, pch = 19)
}


################### Derive the MLE.
p_vec <- seq(0, 1, by = 0.01) # A sequence of parameter values.
logLik <- sapply(p_vec, FUN = log_likelihood, par_n = 10, data = data) # Apply the defined 'log_likelihood'.

# Line plot --> x-axis: p, y-axis: log-likelihood.
par(las = 1, cex.lab = 1.2)
plot(p_vec, logLik, type = "l", xlab = "p", ylab = "log-Likelihood")

# Finding the best p (maximizing log-likelihood).
imax <- which.max(logLik)
p_MLE <- p_vec[imax]
p_MLE  # This value is close to the true p.
points(p_MLE, max(logLik), pch = 19, col = "red") # Showing the best p.
abline(v = p_MLE, col = "red")

################### Summarize
par(las = 1, cex.lab = 1.2)
xbars <- barplot(prop_dat, xlab = "Number of successes",
                 ylab = "Proportion", ylim = c(0, 0.4)) # Data.

points(xbars, dbinom(x, prob = 0.5, size = 10), type = "b", col = "blue",
       pch = 19, lty = 2) # True prob.
points(xbars, dbinom(x, prob = p_MLE, size = 10), type = "b", col = "red",
       pch = 19, lty = 2) # Prob from MLE.

legend("topright", legend = c("Data","True p","MLE p"),
       col = c("gray", "blue","red"), pch = c(15, 19, 19)) # Adding legend.

################### Using the optimize function (analytical approach to maximum likelihood estimation).
p <- seq(0,1,0.001)
f <- p*(1-p)
plot(p,f,type="l",xlab="p",ylab="f(p)") # Plotting Bernoulli distribution over the specified interval for p (maximum at p=0.5).

## Proving this maximum value using optimize function (p=0.5).
fp <- function(p) {p*(1-p)}
optimize(fp,c(0,1),maximum=T)


x <- rpois(100,10) # Simulate 100 draws from a Poisson distribution (lambda = 10).
mean(x)

poiss <- function(lambda,data){sum(data)*log(lambda)-length(data)*lambda} # Likelihood function.
result <- optimize(poiss, c(0,10^6), maximum=T, data=x)
result$maximum # mean(x) and result$maximum are close to each other.
