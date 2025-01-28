# R script to generate the figures for lecture 2 of 
# Statistics and Machine Learning 1 (Semester 1, 2020)
# of the MSc in Data Science 
#
# Last update: 25/10/2020

library(dplyr)
library(ggplot2)

# Read the data and get the list of magnitudes
earthquake.df <- read.table("earthquakes_US_14Jul-13Aug_2018.csv", header=TRUE, sep=",")
magnitudes <- earthquake.df$mag

# How many measurements are there?
n <- length(magnitudes)

ggplot(earthquake.df, aes(x=mag,y=rep(1,n),stroke=0)) +
	geom_jitter(width=0,height=1) +
	ylim(0,2) + ylab("Random Jitter") + xlab("Magnitude")
ggsave("Figures/earth_r_jitter.pdf", width = 6, height = 3)

ggplot(earthquake.df, aes(x=mag)) +
	geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
	geom_density() + xlab("Magnitude")
ggsave("Figures/earth_r_distplot.pdf", width = 6, height = 3)

# Compute the mean and variance using tools from base R
mean(magnitudes)
var(magnitudes)

# Compute other moments, as well as a biased estimate of the variance
library(moments)
moment(magnitudes, order=2, central = TRUE)
ss <- sqrt(moment(magnitudes, order=2, central = TRUE))

# Skewness and excess kurtosis
moment(magnitudes, order=3, central = TRUE)/(ss^3)
(moment(magnitudes, order=4, central = TRUE)/(ss^4))-3



