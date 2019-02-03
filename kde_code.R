'
Kernel Density Estimation
'
# Load necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)


# Generate sample 
x_vec1 <- rnorm(150,0,1.2)
x_vec2 <- rnorm(150,4,1.2)
x_vec <- c(x_vec1,x_vec2)
min(x_vec)
max(x_vec)

ggplot() + geom_histogram(aes(x_vec, ..density..), bins = 70) #plot

# Probability Density Function
pdf_func <- function(x) {
  return(exp(-1 * ((x ** 2) / 2)) / sqrt(2 * pi))
}

# Kernel Density Estimation function
kde_func <- function (x,sample,band) {
  input = (x - sample)/band
  values = sapply (input , pdf_func) 
  est = sum(values)/(length(sample)*band) 
  return(est)
}

# Generating evaluation points
eval_points  = seq(-3,7,0.01)
length(eval_points)

#Obtaining KDE of he eval points for various h values
kde1 = sapply (eval_points,kde_func,sample = x_vec,band = 0.25)
kde2 = sapply (eval_points,kde_func,sample = x_vec,band = 0.50)
kde3 = sapply (eval_points,kde_func,sample = x_vec,band = 1)
kde4 = sapply (eval_points,kde_func,sample = x_vec,band = 1.5)
kde5 = sapply (eval_points,kde_func,sample = x_vec,band = 2)

#Plotting the KDE on the original plot
ggplot() + geom_histogram(aes(x_vec, ..density..), bins = 70) + geom_point(aes(eval_points, kde1), col = 'yellow') + geom_point(aes(eval_points, kde2), col = 'green') + geom_point(aes(eval_points, kde3), col = 'red') + geom_point(aes(eval_points, kde4), col = 'orange') + geom_point(aes(eval_points, kde5), col = 'purple') 

"
As we increase the value of h, the curve lines or density estimate curves become flatter as we can see from the plot generated.
For small values of h such as 0.25,0.50 the curve seems to take a shape closer to the real distribution.
In this case h = 0.50 seems ideal for this univariate distribution.
"

