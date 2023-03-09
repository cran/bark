# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
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
