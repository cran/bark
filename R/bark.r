##bark()
bark <- function(x.train,
                 y.train,
                 x.test = matrix(0, 0, 0),
                 type = "se",
                 classification = FALSE,
                 keepevery = 100,
                 nburn = 100,
                 nkeep = 100,
                 printevery = 1000,
                 keeptrain = FALSE,
                 fixed = list(),
                 tune = list(lstep=0.5, frequL=.2,
                   dpow=1, upow=0, varphistep=.5, phistep=1),
                 theta = list()
                 ){
  if ((!is.matrix(x.train)) || (typeof(x.train) != "double")) 
    stop("argument x.train must be a double matrix")
  if ((!is.matrix(x.test)) || (typeof(x.test) != "double")) 
    stop("argument x.test must be a double matrix")
  if ((!is.vector(y.train)) || !(typeof(y.train) %in% c("double", "integer", "logical")))
      stop("argument y.train must be a double/integer/logical vector")
  if (nrow(x.train) != length(y.train)) 
    stop("number of rows in x.train must equal length of y.train")
  if ((nrow(x.test) > 0) && (ncol(x.test) != ncol(x.train))) 
    stop("input x.test must have the same number of columns as x.train")
  if (!is.logical(classification))
    stop("argument classification is not logical")
  if (!(type %in% c("se", "e", "d", "sd")))
    stop("type is not recognized, should be 'e', 'se', 'd', or 'sd'")
  if (classification)
    problem <- "classification problem"
  else
    problem <- "regression problem"
  print(paste("Starting BARK-", type, " for this ", problem, sep=""))
  
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
  x.train.mean <- apply(x.train, 2, mean);
  x.train.sd <- apply(x.train, 2, sd);
  for(i in 1:fixed$p){
    x.train[,i] <- (x.train[,i]-x.train.mean[i])/x.train.sd[i];
    if(dim(x.test)[1] != 0){
      x.test[,i] <- (x.test[,i]-x.train.mean[i])/x.train.sd[i];
    }
  }
  if(fixed$p == 1){
    x.train <- matrix(x.train, ncol=1);
    x.test <- matrix(x.test, ncol=1);
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
    if (type == "d" | type == "e"){
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
    if(i %% printevery==0){
      print(paste("burning iteration ", i, ", J=", sum(theta$nvec),
                  ", max(nj)=", max(theta$nvec), sep=""));
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
    if(i %% printevery==0){
      print(paste("posterior mcmc iteration ", i, ", J=", sum(theta$nvec),
                  ", max(nj)=", max(theta$nvec), sep=""));
    }
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
  return(barkreturn);
}
