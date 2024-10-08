% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_Friedman1.R
\name{sim_Friedman1}
\alias{sim_Friedman1}
\title{Simulated Regression Problem Friedman 1}
\usage{
sim_Friedman1(n, sd = 1)
}
\arguments{
\item{n}{number of data points to create}

\item{sd}{standard deviation of noise, with default value 1}
}
\value{
Returns a list with components
\item{x}{input values (independent variables)}
\item{y}{output values (dependent variable)}
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
 where e is \eqn{N(0,sd^2)}.
}
\examples{
sim_Friedman1(100, sd=1)
}
\references{
Breiman, Leo (1996) Bagging predictors. Machine Learning 24,
pages 123-140. \cr
Friedman, Jerome H. (1991) Multivariate adaptive regression
splines. The Annals of Statistics 19 (1), pages 1-67.
}
\seealso{
Other bark simulation functions: 
\code{\link{sim_Friedman2}()},
\code{\link{sim_Friedman3}()},
\code{\link{sim_circle}()}

Other bark functions: 
\code{\link{bark}()},
\code{\link{bark-package}},
\code{\link{bark-package-deprecated}},
\code{\link{sim_Friedman2}()},
\code{\link{sim_Friedman3}()},
\code{\link{sim_circle}()}
}
\concept{bark functions}
\concept{bark simulation functions}
