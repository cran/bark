# bark-profile.R
library(profvis)
devtools::install()
library(bark)

profvis::profvis({
  set.seed(42)
  bark.new.lsd = bark(y ~ ., data=circle2, subset=train,
                     #                   testdata = as.matrix(circle2[-train, ]),
                     classification = TRUE,
                     nburn = 50,
                     nkeep = 50, 
                     selection = TRUE,
                     common_lambdas = FALSE,
                     printevery = 10^100)
})


set.seed(42)
bark.new.ld = bark(y ~ ., data=circle2, subset=train,
                 #                   testdata = as.matrix(circle2[-train, ]),
                 classification = TRUE,
                 nburn = 50,
                 nkeep = 50, 
                 selection = FALSE,
                 common_lambdas = FALSE,
                 printevery = 10^100)
set.seed(42)
bark.new.le = bark(y ~ ., data=circle2, subset=train,
                   #                   testdata = as.matrix(circle2[-train, ]),
                   classification = TRUE,
                   nburn = 50,
                   nkeep = 50, 
                   selection = FALSE,
                   common_lambdas = TRUE,
                   printevery = 10^100)
set.seed(42)
bark.new.lse = bark(y ~ ., data=circle2, subset=train,
                   #                   testdata = as.matrix(circle2[-train, ]),
                   classification = TRUE,
                   nburn = 50,
                   nkeep = 50, 
                   selection = TRUE,
                   common_lambdas = TRUE,
                   printevery = 10^100)
set.seed(42)
bark.new.lsd = bark(y ~ ., data=circle2, subset=train,
                    #                   testdata = as.matrix(circle2[-train, ]),
                    classification = TRUE,
                    nburn = 50,
                    nkeep = 50, 
                    selection = TRUE,
                    common_lambdas = FALSE,
                    printevery = 10^100)



install.packages("bark")
library(bark)

  set.seed(42)
  bark.old.ld = bark(y ~ ., data=circle2, subset=train,
              #                   testdata = as.matrix(circle2[-train, ]),
              classification = TRUE,
              nburn = 50,
              nkeep = 50,
              selection = FALSE,
              common_lambdas = FALSE,
              printevery = 10^100)
  set.seed(42)
  bark.old.le = bark(y ~ ., data=circle2, subset=train,
                     #                   testdata = as.matrix(circle2[-train, ]),
                     classification = TRUE,
                     nburn = 50,
                     nkeep = 50, 
                     selection = FALSE,
                     common_lambdas = TRUE,
                     printevery = 10^100)
  set.seed(42)
  bark.old.lse = bark(y ~ ., data=circle2, subset=train,
                      #                   testdata = as.matrix(circle2[-train, ]),
                      classification = TRUE,
                      nburn = 50,
                      nkeep = 50, 
                      selection = TRUE,
                      common_lambdas = TRUE,
                      printevery = 10^100)
  set.seed(42)
  bark.old.lsd = bark(y ~ ., data=circle2, subset=train,
                      #                   testdata = as.matrix(circle2[-train, ]),
                      classification = TRUE,
                      nburn = 50,
                      nkeep = 50, 
                      selection = TRUE,
                      common_lambdas = FALSE,
                      printevery = 10^100)


boxplot(bark.new$theta.lambda)
plot(apply(bark.new.lsd$theta.beta, 1, mean), apply(bark.old.lsd$theta.beta, 1, mean))
plot(apply(bark.new.lse$theta.beta, 1, mean), apply(bark.old.lse$theta.beta, 1, mean))
plot(apply(bark.new.ld$theta.beta, 1, mean), apply(bark.old.ld$theta.beta, 1, mean))
plot(apply(bark.new.le$theta.beta, 1, mean), apply(bark.old.le$theta.beta, 1, mean))
