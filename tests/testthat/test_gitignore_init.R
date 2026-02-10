# test_gitignore_init
# use  withr::local_file() or with_file()  with_dir()  etc. to set up folders/files

#  manipulate the state of .gitignore files, to chcek creation and editing
# create files in directory and see if git is ignoring them or keeping them
# not sure how to use git from inside R

# check that git is installed.

test_that("set local git", {
  skip_if_not(grepl("version", system("git --version", intern = TRUE)))

  withr::local_dir(tempdir())
  # create a new gitignore
  system("git init")
  expect_message(gitignore_init(), ".gitignore file created")
  # make some files and directories
  file.copy(system.file("extdata/gitignore_template", package = "cctu"), "output.csv")
  dir.create("data")
  dir.create("data/output")
  file.copy(system.file("extdata/gitignore_template", package = "cctu"), "data/test.xls")
  file.copy(system.file("extdata/gitignore_template", package = "cctu"), "data/output/test.xls")
  system("git add .")

  # check what is ignored
  expect_contains(
    system("git ls-files --others --ignored --exclude-standard", intern = TRUE),
    c("output.csv", "data/output/test.xls")
  )
  # check what is kept
  expect_contains(system("git ls-files --exclude-standard", intern = TRUE), "data/test.xls")
  # start again without a gitignore
  unlink(".gitignore")
  system("git add .")
  # check now all files are tracked
  expect_contains(
    system("git ls-files --exclude-standard", intern = TRUE),
    c("output.csv", "data/output/test.xls", "data/test.xls")
  )

  # edit a new gitignore
  writeLines(c("output.csv", "/*.pdf"), ".gitignore")
  # re apply the gitignore
  system("git rm -r -f --cached . && git add .")
  # check only one is now ingored
  expect_contains(
    system("git ls-files --others --ignored --exclude-standard", intern = TRUE),
    c("output.csv")
  )
  expect_message(gitignore_init(), ".gitignore edited")
  # check the length of edited git ignore file
  git_length <- readLines(".gitignore") |> length()
  template_length <- system.file("extdata/gitignore_template", package = "cctu") |>
    readLines() |>
    length()
  expect_equal(git_length, template_length + 1)
  # previous checks should still all apply plus
  system("git rm -r -f --cached . && git add .")
  # check what is ignored
  expect_contains(
    system("git ls-files --others --ignored --exclude-standard", intern = TRUE),
    c("output.csv", "data/output/test.xls")
  )
  # check what is kept
  expect_contains(system("git ls-files --exclude-standard", intern = TRUE), "data/test.xls")
})


# grepl("version", system("git --version", intern=TRUE))
# #system("git clean -n", intern=TRUE)  #  play safe and don't use this
# # the commands below just get info, but don't change anything
# system("git ls-files --others --exclude-standard", intern=TRUE)
# # give untracked files, but applies the gitignore rule
# system("git ls-files --others --ignored --exclude-standard", intern=TRUE)
# # gives the explicit list of ignored files
# system("git ls-files --exclude-standard", intern=TRUE)
# # says what is current being tracked
