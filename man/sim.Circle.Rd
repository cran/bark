\name{sim.Circle}
\alias{sim.Circle}
\title{Simulated Classification Problem Circle}
\usage{
sim.Circle(n, dim=5)
}
\arguments{
\item{n}{number of data points to create}
\item{dim}{number of dimension of the problem, no less than 2.}
}
\description{
The classification problem Circle is described in the BARK paper (2008).
Inputs are \emph{dim} independent variables uniformly
distributed on the interval \eqn{[-1,1]}, only the first 2
out of these \emph{dim} are actually signals.
Outputs are created according to the formula

\deqn{y = 1(x1^2+x2^2 \le 2/\pi)}

}
\value{Returns a list with components
\item{x}{input values (independent variables)}
\item{y}{0/1 output values (dependent variable)}
}
\references{
  Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
  Duke University. Ph.D. dissertation, page 58.
  at:
  \url{http://stat.duke.edu/people/theses/OuyangZ.html}
}

