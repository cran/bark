test_that("simulations", {
  n = 10 

  dat = sim_Friedman1(n)
  expect_no_error(dat)
  expect_warning(sim.Friedman1(n))
  
  dat = sim_Friedman2(n)
  expect_no_error(dat)
   expect_warning(sim.Friedman2(n))
  
  
  dat = sim_Friedman3(n)
  expect_no_error(dat)
   expect_warning(sim.Friedman3(n))
  
  dat = sim_circle(n)
  expect_no_error(dat)
  expect_warning(sim.Circle(n))
})

