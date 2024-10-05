# first merge main devel in terminal
# on devel
git merge main
# change to main
git checkout main
# merge changes from devel into main
git merge devel


# Check current CRAN check results
usethis::use_release_issue()


urlchecker::url_check()
devtools::build_readme()
devtools::check(remote = TRUE, manual = TRUE)

# devtools::install_github("r-lib/revdepcheck")

# revdepcheck::revdep_reset()
# revdepcheck::revdep_check(num_workers = 4)
# revdepcheck::revdep_report_cran()

# check email for results
devtools::check_win_devel()
devtools::check_win_release()

# or load from https://win-builder.r-project.org/upload.aspx

# checks for rhub

# check rhub. (see github actions to trigger rhub workflow
# rhub2::rhub_check(platforms = "valgrind")
# rhub2::rhub_check(platforms = "valgrind", branch="devel")


# to submit to CRAN

devtools::spell_check()
devtools::document()
devtools::release()
devtools::submit_cran()
