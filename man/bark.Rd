\name{bark}
\alias{bark}
\title{Bayesian Additive Regression Kernels}
\alias{bark}
\description{
  BARK is a Bayesian \emph{sum-of-kernels} model.\cr
  For numeric response \eqn{y}, we have
  \eqn{y = f(x) + \epsilon}{y = f(x) + e},
  where \eqn{\epsilon \sim N(0,\sigma^2)}{e ~ N(0,sigma\^2)}.\cr
  For a binary response \eqn{y}, \eqn{P(Y=1 | x) = F(f(x))}, where \eqn{F}
  denotes the standard normal cdf (probit link).

  In both cases, \eqn{f} is the sum of many Gaussian kernel functions.
  The goal is to have very flexible inference for the unknown
  function \eqn{f}.
  It uses an approximated Cauchy process as the prior distribution
  for the unknown function \eqn{f}.

  Feature selection can be achieved through the inference
  on the scale parameters in the Gaussian kernels.
  BARK accepts four different types of prior distributions,
  \emph{e}, \emph{d}, \emph{se}, \emph{sd}, enabling
  either soft shrinkage or hard shrinkage for the scale
  parameters.
}
\usage{
bark(x.train, y.train, x.test = matrix(0, 0, 0),
     type = "se", classification = FALSE,
     keepevery = 100, nburn = 100, nkeep = 100,
     printevery = 1000, keeptrain = FALSE,
     fixed = list(),
     tune = list(lstep=0.5, frequL=.2,
                 dpow=1, upow=0, varphistep=.5, phistep=1),
     theta = list()
    )
}
\arguments{
   \item{x.train}{
     Explanatory variables for training (in sample) data.\cr
     Must be a matrix of doubles,
     with (as usual) rows corresponding to observations
     and columns to variables.
   }

   \item{y.train}{
     Dependent variable for training (in sample) data.\cr
     If y is numeric a continous response model is fit (normal errors).\cr
     If y is a logical (or just has values 0 and 1),
     then a binary response model with a probit link is fit.
   }

   \item{x.test}{
     Explanatory variables for test (out of sample) data.\cr
     Should have same structure as x.train.
   }

   \item{type}{
     BARK type, \emph{e}, \emph{d}, \emph{se}, or \emph{sd}, default
     choice is \emph{se}.\cr
     \emph{e}: BARK with equal weights.\cr
     \emph{d}: BARK with diffeerent weights.\cr
     \emph{se}: BARK with selection and equal weights.\cr
     \emph{sd}: BARK with selection and different weights.\cr
   }

   \item{classification}{
     True false logical variable, indicating a
     classfication or regression problem.
   }
   
   \item{keepevery}{
     Every keepevery draw is kept to be returned to the user.\cr
   }
   
   \item{nburn}{
     Number of MCMC iterations (nburn\eqn{\times}keepevery)
     to be treated as burn in.
   }
   
   \item{nkeep}{
     Number of MCMC iterations kept for the posterior inference.\cr
     nkeep\eqn{\times}keepevery iterations after the burn in.
   }
   
   \item{printevery}{
     As the MCMC runs, a message is printed every printevery draws.
   }
   
   \item{keeptrain}{
     Logical, whether to keep results for training samples.
   }

   \item{fixed}{
     A list of fixed hyperparameters, using the default values if not
     sepecified.\cr
     alpha = 1: stable index, must be 1.\cr
     eps = 0.5: approximation parameter.\cr
     gam = 5: intensity parameter.\cr
     la = 1: first argument of the gamma prior on kernel scales.\cr
     lb = 2: second argument of the gamma prior on kernel scales.\cr
     pbetaa = 1: first argument of the beta prior on plambda.\cr
     pbetab = 1: second argument of the beta prior on plambda.\cr
     n: number of training samples, automatically generates.\cr
     p: number of explainatory variables, automatically generates.\cr
     meanJ: the expected number of kernels, automatically generates.
   }

   \item{tune}{
     A list of tuning parameters, not expected to change.\cr
     lstep: the stepsize of the lognormal random walk on lambda.\cr
     frequL: the frequency to update L.\cr
     dpow: the power on the death step.\cr
     upow: the power on the update step.\cr
     varphistep: the stepsize of the lognormal random walk on varphi.\cr
     phistep: the stepsize of the lognormal random walk on phi.
   }
   \item{theta}{
     A list of the starting values for the parameter theta,
     use the defaults if nothing is given.\cr
   }
}
\details{
   BARK is a Bayesian MCMC method. 
   At each MCMC interation, we produce a draw from the joint posterior
   distribution, i.e. a full configuration of regression coefficents,
   kernel locations and kernel parameters etc.

   Thus, unlike a lot of other modelling methods in R,
   we do not produce a single model object
   from which fits and summaries may be extracted.
   The output consists of values
   \eqn{f^*(x)}{f*(x)} (and \eqn{\sigma^*}{sigma*} in the numeric case)
   where * denotes a particular draw.
   The \eqn{x} is either a row from the training data (x.train)
   or the test data (x.test).
}
\value{
   \code{bark} returns a list, including:

   \item{fixed}{
     Fixed hyperparameters.
   }

   \item{tune}{
     Tuning parameters used.
   }

   \item{theta.last}{
     The last set of parameters from the posterior draw.
   }

   \item{theta.nvec}{
     A matrix with nrow(x.train)\eqn{+1} rows and (nkeep) columns,
     recording the  number of kernels at each training sample.
   }

   \item{theta.varphi}{
     A matrix with nrow(x.train)\eqn{+1} rows and (nkeep) columns,
     recording the precision in the normal gamma prior
     distribution for the regression coefficients.
   }

   \item{theta.beta}{
     A matrix with nrow(x.train)\eqn{+1} rows and (nkeep) columns,
     recording the regression coefficients.
   }

   \item{theta.lambda}{
     A matrix with ncol(x.train) rows and (nkeep) columns,
     recording the kernel scale parameters.
   }

   \item{thea.phi}{
     The vector of length nkeep,
     recording the precision in regression Gaussian noise
     (1 for the classification case).
   }
   
   \item{yhat.train}{
     A matrix with nrow(x.train) rows and (nkeep) columns.
     Each column corresponds to a draw \eqn{f^*}{f*} from
     the posterior of \eqn{f}
     and each row corresponds to a row of x.train.
     The \eqn{(i,j)} value is \eqn{f^*(x)}{f*(x)} for
     the \eqn{j^{th}}{j\^th} kept draw of \eqn{f}
     and the \eqn{i^{th}}{i\^th} row of x.train.\cr
     For classification problems, this is the value
     of the expectation for the underlying normal
     random variable.\cr
     Burn-in is dropped.
   }
   
   \item{yhat.test}{
     Same as yhat.train but now the x's are the rows of the test data.
   }
   
   \item{yhat.train.mean}{
     train data fits = row mean of yhat.train.
   }
   
   \item{yhat.test.mean}{
     test data fits = row mean of yhat.test.
   }
}
\references{
  Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
  Duke University. Ph.D. dissertation, page 58.
  \cr
  at:
  \url{http://stat.duke.edu/people/theses/OuyangZ.html}
}
\note{
  \emph{
    The code is not efficient in terms of calculating the
    kernel matrix in the updating. This is expected to
    be fixed in the next version.
  }
}
\examples{
\dontrun{
##Simulate regression example
# Friedman 2 data set, 200 noisy training, 1000 noise free testing
# Out of sample MSE in SVM (default RBF): 6500 (sd. 1600)
# Out of sample MSE in BART (default):    5300 (sd. 1000)
traindata <- sim.Friedman2(200, sd=125)
testdata <- sim.Friedman2(1000, sd=0)
fit.bark.d <- bark(traindata$x, traindata$y, testdata$x, classification=FALSE, type="d")
boxplot(as.data.frame(fit.bark.d$theta.lambda))
mean((fit.bark.d$yhat.test.mean-testdata$y)^2)

##Simulate classification example
# Circle 5 with 2 signals and three noisy dimensions
# Out of sample erorr rate in SVM (default RBF): 0.110 (sd. 0.02)
# Out of sample error rate in BART (default):    0.065 (sd. 0.02)
traindata <- sim.Circle(200, dim=5)
testdata <- sim.Circle(1000, dim=5)
fit.bark.se <- bark(traindata$x, traindata$y, testdata$x, classification=TRUE, type="se")
boxplot(as.data.frame(fit.bark.se$theta.lambda))
mean((fit.bark.se$yhat.test.mean>0)!=testdata$y)

}
}
