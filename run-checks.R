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

# https://mac.R-project.org/macbuilder/results/1678219814-e5a6565487c07387/
# https://mac.R-project.org/macbuilder/results/1678219851-23a9c6a4871f9469/

# to submit to CRAN

devtools::spell_check()
devtools::document()
devtools::release()
devtools::submit_cran()
