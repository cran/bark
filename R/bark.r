#' @title Nonparametric Regression using Bayesian Additive Regression Kernels
#' @description BARK is a Bayesian \emph{sum-of-kernels} model.\cr
#' For numeric response \eqn{y}, we have
#' \eqn{y = f(x) + \epsilon}{y = f(x) + e},
#' where \eqn{\epsilon \sim N(0,\sigma^2)}{e ~ N(0,sigma\^2)}.\cr
#' For a binary response \eqn{y}, \eqn{P(Y=1 | x) = F(f(x))},
#' where \eqn{F}
#' denotes the standard normal cdf (probit link).
#' \cr
#' In both cases, \eqn{f} is the sum of many Gaussian kernel functions.
#' The goal is to have very flexible inference for the unknown
#' function \eqn{f}.
#' BARK uses an approximation to a Cauchy process as the prior distribution
#' for the unknown function \eqn{f}.
#'
#' Feature selection can be achieved through the inference
#' on the scale parameters in the Gaussian kernels.
#' BARK accepts four different types of prior distributions,
#' \emph{e}, \emph{d}, enabling
#' either soft shrinkage or  \emph{se}, \emph{sd}, enabling hard shrinkage for the scale
#' parameters.
#'
#' @param formula model formula for the model with all predictors,
#' Y ~ X.  THe X variables will be centered and scaled as part of model fitting.
#' @param data a data frame.  Factors will be converted to numerical vectors based on
#' the using `model.matrix`.
#' @param subset an optional vector specifying a subset of observations to be
#' used in the fitting process.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. The default is "na.omit".
#' @param testdata Dataframe with test data for out of sample prediction.\cr
#' Should have same structure as data.
#' @param selection Logical variable indicating whether variable 
#' dependent kernel parameters \eqn{\lambda} may be set to zero in the MCMC; 
#' default is TRUE. \cr
#' @param common_lambdas Logical variable indicating whether  
#' kernel parameters \eqn{\lambda} should be predictor specific or common across
#' predictors;  default is TRUE.   Note if  \emph{common_lambdas = TRUE} and 
#' \emph{selection = TRUE} this applies just to the non-zero \eqn{lambda_j}. \cr
#' @param classification TRUE/FALSE logical variable,
#' indicating a classification or regression problem.
#' @param keepevery  Every keepevery draw is kept to be returned to the user
#' @param nburn  Number of MCMC iterations (nburn*keepevery)
#' to be treated as burn in.
#' @param nkeep Number of MCMC iterations kept for the posterior inference.\cr
#' nkeep*keepevery iterations after the burn in.
#' @param printevery As the MCMC runs, a message is printed every printevery draws.
#' @param keeptrain  Logical, whether to keep results for training samples.
#' @param verbose Logical, whether to print out messages
#' @param fixed  A list of fixed hyperparameters, using the default values if not
#' specified.\cr
#' alpha = 1: stable index, must be 1 currently.\cr
#' eps = 0.5: approximation parameter.\cr
#' gam = 5: intensity parameter.\cr
#' la = 1: first argument of the gamma prior on kernel scales.\cr
#' lb = 2: second argument of the gamma prior on kernel scales.\cr
#' pbetaa = 1: first argument of the beta prior on plambda.\cr
#' pbetab = 1: second argument of the beta prior on plambda.\cr
#' n: number of training samples, automatically generates.\cr
#' p: number of explanatory variables, automatically generates.\cr
#' meanJ: the expected number of kernels, automatically generates.
#' @param tune A list of tuning parameters, not expected to change.\cr
#' lstep: the stepsize of the lognormal random walk on lambda.\cr
#' frequL: the frequency to update L.\cr
#' dpow: the power on the death step.\cr
#' upow: the power on the update step.\cr
#' varphistep: the stepsize of the lognormal random walk on varphi.\cr
#' phistep: the stepsize of the lognormal random walk on phi.
#' @param theta  A list of the starting values for the parameter theta,
#' use defaults if nothing is given.
#'
#' @return \code{bark} returns a list, including:
#'  \item{call}{the matched call}
#'  \item{fixed}{Fixed hyperparameters}
#'  \item{tune}{Tuning parameters used}
#'  \item{theta.last}{The last set of parameters from the posterior draw}
#'  \item{theta.nvec}{A matrix with nrow(x.train)\eqn{+1} rows and (nkeep) columns,
#' recording the  number of kernels at each training sample}
#'  \item{theta.varphi}{ A matrix with nrow(x.train)
#'  \eqn{+1} rows and (nkeep) columns,
#'  recording the precision in the normal gamma prior
#'  distribution for the regression coefficients}
#'  \item{theta.beta}{A matrix with nrow(x.train)\eqn{+1} rows and (nkeep) columns,
#'  recording the regression coefficients}
#'  \item{theta.lambda}{A matrix with ncol(x.train) rows and (nkeep) columns,
#'   recording the kernel scale parameters}
#'  \item{thea.phi}{The vector of length nkeep,
#'  recording the precision in regression Gaussian noise
#'  (1 for the classification case)}
#'  \item{yhat.train}{A matrix with nrow(x.train) rows and (nkeep) columns.
#'  Each column corresponds to a draw \eqn{f^*}{f*} from
#'  the posterior of \eqn{f}
#'   and each row corresponds to a row of x.train.
#'  The \eqn{(i,j)} value is \eqn{f^*(x)}{f*(x)} for
#'  the \eqn{j^{th}}{j\^th} kept draw of \eqn{f}
#'  and the \eqn{i^{th}}{i\^th} row of x.train.\cr
#'  For classification problems, this is the value
#'  of the expectation for the underlying normal
#'  random variable.\cr
#'  Burn-in is dropped}
#' \item{yhat.test}{Same as yhat.train but now the x's
#' are the rows of the test data}
#' \item{yhat.train.mean}{train data fits = row mean of yhat.train}
#' \item{yhat.test.mean}{test data fits = row mean of yhat.test}
#'
#' @details BARK is implemented using a Bayesian MCMC method.
#' At each MCMC interaction, we produce a draw from the joint posterior
#' distribution, i.e. a full configuration of regression coefficients,
#' kernel locations and kernel parameters etc.
#'
#' Thus, unlike a lot of other modelling methods in R,
#' we do not produce a single model object
#' from which fits and summaries may be extracted.
#' The output consists of values
#' \eqn{f^*(x)}{f*(x)} (and \eqn{\sigma^*}{sigma*} in the numeric case)
#' where * denotes a particular draw.
#' The \eqn{x} is either a row from the training data (x.train)
#" or the test data (x.test).
#'
#' @references Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
#' Duke University. PhD dissertation, page 58.
#' @examples
#' ##Simulated regression example
#' # Friedman 2 data set, 200 noisy training, 1000 noise free testing
#' # Out of sample MSE in SVM (default RBF): 6500 (sd. 1600)
#' # Out of sample MSE in BART (default):    5300 (sd. 1000)
#' traindata <- data.frame(sim_Friedman2(200, sd=125))
#' testdata <- data.frame(sim_Friedman2(1000, sd=0))
# example with a very small number of iterations to illustrate 
#' fit.bark.d <- bark(y ~ ., data=traindata, testdata= testdata,
#'                    nburn=10, nkeep=100, keepevery=10,
#'                    classification=FALSE, 
#'                    common_lambdas = FALSE,
#'                    selection = FALSE)
#' boxplot(data.frame(fit.bark.d$theta.lambda))
#' mean((fit.bark.d$yhat.test.mean-testdata$y)^2)

#' \dontrun{
#'  ##Simulate classification example
#'  # Circle 5 with 2 signals and three noisy dimensions
#'  # Out of sample erorr rate in SVM (default RBF): 0.110 (sd. 0.02)
#'  # Out of sample error rate in BART (default):    0.065 (sd. 0.02)
#'  traindata <- sim_Circle(200, dim=5)
#'  testdata <- sim_Circle(1000, dim=5)
#'  fit.bark.se <- bark(y ~ ., 
#'                      data=data.frame(traindata), 
#'                      testdata= data.frame(testdata), 
#'                      classification=TRUE)
#'  boxplot(as.data.frame(fit.bark.se$theta.lambda))
#'  mean((fit.bark.se$yhat.test.mean>0)!=testdata$y)
#'}
#' @family bark functions
#' @export
bark <- function(formula, data, subset, na.action = na.omit, 
                     testdata = NULL,
                     selection = TRUE,
                     common_lambdas = TRUE,   
                     classification = FALSE,
                     keepevery = 100,
                     nburn = 100,
                     nkeep = 100,
                     printevery = 1000,
                     keeptrain = FALSE,
                     verbose = FALSE,
                     fixed = list(),
                     tune = list(lstep=0.5, frequL=.2,
                                 dpow=1, upow=0, varphistep=.5, phistep=1),
                     theta = list())
{

  # create design matrix  
  
  
  call <- match.call()
  mfall <- match.call(expand.dots = FALSE)
  mf <- match(
    c(
      "formula", "data", "subset", "na.action"
    ),
    names(mfall),
    0L
  )
  m <- mfall[c(1L, mf)]
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  if (!inherits(eval.parent(m$data), "data.frame"))
    stop("method is only for dataframes")
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m$na.action <- na.action
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  x.train <- model.matrix(Terms, m)
  y.train <- model.extract(m, "response")
  attr(x.train, "na.action") <- attr(y.train, "na.action") <- attr(m, "na.action")
  
  if (!is.logical(classification))
    stop("argument classification should be TRUE or FALSE")
  if (classification)
    problem <- "classification problem"
  else
    problem <- "regression problem"

  
  # specifiy type of model for lambda's
  if (common_lambdas) {
    type = "e"
    prior = "equal lambdas"}
  else {
    type = "d"
    prior = "different lambdas"
  }
  if (selection)  {
    type= paste0("s", type)
    prior = paste(prior, "with selection")
  }
  
  if (verbose)  print(paste("Starting BARK with", type, " for this ", problem, sep=""))
  
  
  # initializing fixed
  if (is.null(fixed$alpha))
    fixed$alpha <- 1
  if (fixed$alpha != 1)
    stop("current verison only work for stable index alpha = 1")
  if (is.null(fixed$eps))
    fixed$eps <- 0.5
  if (is.null(fixed$gam))
    fixed$gam <- 5
  if (is.null(fixed$la))
    fixed$la <- 1
  if (is.null(fixed$lb))
    fixed$lb <- 1
  if (is.null(fixed$pbetaa))
    fixed$pbetaa <- 1
  if (is.null(fixed$pbetab))
    fixed$pbetab <- 1
  fixed$n <- dim(x.train)[1];
  fixed$p <- dim(x.train)[2];
  fixed$meanJ <- getmeanJ(fixed$alpha, fixed$eps, fixed$gam);
  
  # centering and scaling
 
    x.train = scale(x.train);
    x.train.mean <- attr(x.train, "scaled:center");
    x.train.sd <- attr(x.train, "scaled:scale");
  
  # extract x.test
    if (!is.null(testdata)) {
      if (!is.data.frame(testdata)) {
        stop("Test data should be povided as a data.frame") }
      Terms <- delete.response(Terms)
      mtest <- model.frame(Terms, testdata, 
                           na.action=na.action)
      x.test = model.matrix(Terms, mtest)
      x.test = scale(x.test, center=x.train.mean, scale = x.train.sd);
    }
  else 
    x.test = matrix(NA, nrow=0,ncol=0)
  
  if ((nrow(x.test) > 0) && (ncol(x.test) != ncol(x.train)))
    stop("input x.test must have the same number of columns as x.train")
  
  
  if(fixed$p == 1){
    x.train <- matrix(x.train, ncol=1);
    x.test <-  matrix(x.test, ncol=1);
  }
  if(!classification){
    y.train.mean <- mean(y.train);
    y.train.sd <- sd(y.train);
    y.train <- (y.train - y.train.mean)/y.train.sd;
  }
  
  # initializing theta
  if(is.null(theta$nvec)){
    totalJ <- fixed$meanJ;
    theta$nvec <- as.vector(rmultinom(1, totalJ,
                                      rep(1, fixed$n+1)/(fixed$n+1)));
  }
  if(is.null(theta$varphi)){
    theta$varphi <- rgamma(fixed$n+1, fixed$alpha/2,
                           fixed$alpha*fixed$eps^2/2);
  }
  if(is.null(theta$L)){
    theta$L <- rep(1, fixed$p)
  }
  if(is.null(theta$phi)){
    theta$phi <- 1;
  }
  if(is.null(theta$lamzerop)){
    if (type == "d" | type == "e"){ # no selection
      theta$lamzerop <- 1;
    } else {
      theta$lamzerop <- .5;
    }
  }
  if(classification){
    if(is.null(theta$beta)){
      theta$beta <- rep(0, fixed$n+1);
    }
    if(is.null(theta$z)){
      theta <- updatez(y.train, x.train, theta, classification);
    }
  }
  
  # burning the markov chain
  fullXX <- NULL;
  for(i in 1:(keepevery*nburn)){
    cur <- rjmcmcone(y.train, x.train, theta, fixed, tune, classification, type, fullXX);
    theta <- cur$theta;
    fullXX <- cur$fullXX;
    if (verbose) {
      if(i %% printevery==0){
        print(paste("burning iteration ", i, ", J=", sum(theta$nvec),
                    ", max(nj)=", max(theta$nvec), sep=""));
      }
    }
  }
  
  
  # initializing the "saved" results
  if(keeptrain == TRUE){
    yhat.train <- matrix(NA, nrow=dim(x.train)[1], nkeep);
  }
  if(dim(x.test)[1] != 0){
    yhat.test <- matrix(NA, nrow=dim(x.test)[1], nkeep);
  }
  if(classification == FALSE){
    theta.phi <- rep(NA, nkeep);
  }else{
    theta.phi <- rep(1, nkeep);
  }
  theta.nvec <- matrix(NA, ncol=fixed$n+1, nrow=nkeep);
  theta.varphi <- matrix(NA, ncol=fixed$n+1, nrow=nkeep);
  theta.beta <- matrix(NA, ncol=fixed$n+1, nrow=nkeep);
  theta.lambda <- matrix(NA, ncol=fixed$p, nrow=nkeep);
  theta.phi <- matrix(NA, ncol=1, nrow=nkeep);
  colnames(theta.nvec) <- paste("n", 0:fixed$n, sep="");
  colnames(theta.varphi) <- paste("v", 0:fixed$n, sep="");
  colnames(theta.beta) <- paste("b", 0:fixed$n, sep="");
  colnames(theta.lambda) <- paste("l", 1:fixed$p, sep="");
  colnames(theta.phi) <- "phi";
  
  
  # running the Markov chain after burning
  for(i in 1:(keepevery*nkeep)){
    cur <- rjmcmcone(y.train, x.train, theta, fixed, tune, classification, type, fullXX);
    theta <- cur$theta;
    fullXX <- cur$fullXX;
    if (verbose) {
      if(i %% printevery==0){
       print(paste("posterior mcmc iteration ", i, ", J=", sum(theta$nvec),
                   ", max(nj)=", max(theta$nvec), sep=""));
    }}
    if(i %% keepevery==0){
      theta.nvec[i/keepevery, ] <- theta$nvec;
      theta.varphi[i/keepevery, ] <- theta$varphi;
      theta.lambda[i/keepevery, ] <- theta$L;
      theta.phi[i/keepevery, 1] <- theta$phi;
      if(!classification){
        theta <- updatebeta(y.train, x.train, theta, fixed, classification, fullXX);
      }
      theta.beta[i/keepevery, ] <- theta$beta;
      if(keeptrain == TRUE){
        yhat.train[, i/keepevery] <- getdesign(x.train, x.train, theta) %*%
          theta$beta[theta$nvec>0];
      }
      if(dim(x.test)[1] != 0){
        yhat.test[, i/keepevery] <- getdesign(x.test, x.train, theta) %*%
          theta$beta[theta$nvec>0];
      }
    }
  }
  
  # summarizing the result
  barkreturn <- list(fixed = fixed,
                     tune = tune,
                     theta.last = theta,
                     theta.nvec = theta.nvec,
                     theta.varphi = theta.varphi,
                     theta.beta = theta.beta,
                     theta.lambda = theta.lambda,
                     theta.phi = theta.phi);
  if(keeptrain == TRUE){
    #if(classification){
    #  yhat.train <- pnorm(yhat.train);
    #}
    yhat.train.mean <- apply(yhat.train, 1, mean);
    if(!classification){
      yhat.train.mean <- yhat.train.mean * y.train.sd + y.train.mean;
    }
    barkreturn$yhat.train <- yhat.train;
    barkreturn$yhat.train.mean <- yhat.train.mean;
  }
  
  if(dim(x.test)[1] != 0){
    #if(classification){
    #  yhat.test <- pnorm(yhat.test);
    #}
    yhat.test.mean <- apply(yhat.test, 1, mean);
    if(!classification){
      yhat.test.mean <- yhat.test.mean * y.train.sd + y.train.mean;
    }
    barkreturn$yhat.test <- yhat.test;
    barkreturn$yhat.test.mean <- yhat.test.mean;
  }
  class(barkreturn) <- c("bark")
  return(barkreturn);
}