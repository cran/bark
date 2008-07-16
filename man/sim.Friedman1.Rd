\name{sim.Friedman1}
\alias{sim.Friedman1}
\title{Simulated Regression Problem Friedman 1}
\usage{
sim.Friedman1(n, sd=1)
}
\arguments{
\item{n}{number of data points to create}
\item{sd}{Standard deviation of noise, with default value 1.}
}
\description{
The regression problem Friedman 1 as described in Friedman (1991) and
Breiman (1996). Inputs are 10 independent variables uniformly
distributed on the interval \eqn{[0,1]}, only 5 out of these 10 are actually
used. Outputs are created according to
the formula
\deqn{y = 10 \sin(\pi x1 x2) + 20 (x3 - 0.5)^2 + 10 x4 + 5 x5 + e}{
  y = 10 sin(\pi x1 x2) + 20 (x3 - 0.5)^2
  + 10 x4 + 5 x5 + e}

where e is N(0,sd).
}
\value{Returns a list with components
\item{x}{input values (independent variables)}
\item{y}{output values (dependent variable)}
}
\references{
Breiman, Leo (1996) Bagging predictors. Machine Learning 24, pages
123-140.

Friedman, Jerome H. (1991) Multivariate adaptive regression
splines. The Annals of Statistics 19 (1), pages 1-67. 
}
