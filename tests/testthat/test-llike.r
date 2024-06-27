test_that("log-like with p = 1", {
  
  n = 25; p = 1
  df = data.frame(y = rnorm(n), x = rnorm(n))
  bark.ok = bark(y ~ ., data=df, 
                       classification = FALSE,
                       nburn = 10,
                       nkeep = 100,
                       keepevery = 10,
                       printevery = 10^10)
  
 
 y = df$y
 x = as.matrix(df$x, ncol=1)
 theta = bark.ok$theta.last
 fullXX = bark:::getfulldesign(x,x,theta)
 
 llike_new = bark:::llike(y, x, theta, classification = FALSE, fullXX = NULL)
 llike_old = bark:::llike_old(y, x, theta, classification = FALSE, fullXX = NULL)
 llike_inC = bark:::llike_C(y, x, theta, classification = FALSE, fullXX = fullXX)
 expect_equal(llike_new, llike_old)
 
})


test_that("log-like with p = 1", {
  
  n = 25; 
  df = data.frame(sim_Friedman1(n));
  bark.ok = bark(y ~ ., data=df, 
                 classification = FALSE,
                 nburn = 10,
                 nkeep = 100,
                 keepevery = 10,
                 printevery = 10^10)
  
  
  y = df$y
  x = as.matrix(df[,-11])
  
  theta = bark.ok$theta.last
  fullXX = bark:::getfulldesign(x,x,theta)
  llike_new = bark:::llike(y, x, theta, classification = FALSE, fullXX = NULL)
  llike_old = bark:::llike_old(y, x, theta, classification = FALSE, fullXX = NULL)
  expect_equal(llike_new, llike_old)
})

