# check email fro results
devtools::check_win_devel()
devtools::check_win_release()

# or load from https://win-builder.r-project.org/upload.aspx

# checks for rhub

ch <- rhub::check_for_cran(".", show_status = FALSE)
ch$cran_summary()
ch$update() 

# check for M1Mac
url('https://mac.r-project.org/macbuilder/submit.html')

# https://mac.R-project.org/macbuilder/results/1677812311-886cdf4d4162fdf7/
# https://mac.R-project.org/macbuilder/results/1677812348-1457701cd8f178a9/  

# to submit to CRAN

devtools::submit_cran()
