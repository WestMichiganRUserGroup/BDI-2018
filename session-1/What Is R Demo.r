#! /usr/bin/env Rscript
data(iris)                                # Pre-loaded dataset

str(iris)                                 # Look at the structure of the data

summary(iris)                             # Summary statistics of each variable
aggregate(. ~ Species, data = iris, mean) # Summary stats by Species
table(iris$Species)

# Linear models in one line of code!
iris.lm <- lm(Petal.Length ~ Petal.Width, data = iris)
anova(iris.lm)   # ANOVA table
summary(iris.lm) # Other summary stats on output model

if (interactive()) {
  # Generate diagnostic plots in a snap!
  par(mfrow = c(2L, 2L))
  plot(iris.lm)
  
  # Libraries like ggplot2 add to base R's capabilities
  library(ggplot2)
  ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
    geom_point(aes(color = Species))
} else {
  cat("Plots available in interactive mode\n")
}
