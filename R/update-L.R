##updateL()
updateL <- function(y,
                    X,
                    theta,
                    fixed,
                    tune,
                    classification,
                    type
                    ){
  if (type == "se"){
    cur <- updateL.se(y, X, theta, fixed, tune, classification)
  } else if (type == "e"){
    cur <- updateL.e(y, X, theta, fixed, tune, classification)
  } else if (type == "d"){
    cur <- updateL.d(y, X, theta, fixed, tune, classification)
  } else if (type == "sd"){
    cur <- updateL.sd(y, X, theta, fixed, tune, classification)
  }
  return(cur);
}
