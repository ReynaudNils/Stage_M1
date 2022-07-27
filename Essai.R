set.seed(120653)                              # Set random seed
x <- c(rnorm(1000, 6), rnorm(1000, 13, 4))    # Create example data
is.list(x)                                     # Head of example data
typeof(x)
hist(x, breaks = 100)                         # Histogram without logarithmic axis

hist(log(x), breaks = 100)                    # Histogram with logarithmic axis

install.packages("ggplot2")                   # Install & load ggplot2 package
library("ggplot2")

ggplot(data.frame(x), aes(x)) +               # Histogram without logarithmic axis
  geom_histogram(bins = 100)

ggplot(data.frame(log(x)), aes(log(x))) +     # Histogram with logarithmic axis
  geom_histogram(bins = 100)

ggplot(data.frame(x), aes(x)) +               # Histogram with log10 axis
  geom_histogram(bins = 100) +
  scale_x_log10()