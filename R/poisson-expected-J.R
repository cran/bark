# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#

# Calculate  design matrix for kernels
#

## getmeanJ()
# calculate meanJ from the approximated alpha stable process
getmeanJ <- function(alpha,
                     eps,
                     gam) {
  if (alpha <= 0 | alpha >= 2) {
    stop("ERROR: stable index alpha is not in (0, 2).")
  } else if (alpha == 1) {
    meanJ <- gam / eps
  } else {
    meanJ <- (gam * alpha^(1 - alpha / 2) / eps^alpha * sqrt(pi)) *
      (gamma(alpha) * gamma(alpha / 2) / gamma((alpha + 1) / 2)) * sin(pi * alpha / 2)
  }
  return(meanJ)
}

