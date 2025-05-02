# Read example data
dt_a <- read.csv(system.file("extdata", "test_A.csv", package = "cctu"),
  colClasses = "character"
)
dt_b <- read.csv(system.file("extdata", "test_B.csv", package = "cctu"),
  colClasses = "character"
)

# Read DLU and CLU
dlu_a <- read.csv(system.file("extdata", "test_A_DLU.csv", package = "cctu"))
dlu_b <- read.csv(system.file("extdata", "test_B_DLU.csv", package = "cctu"))
clu_a <- read.csv(system.file("extdata", "test_A_CLU.csv", package = "cctu"))
clu_b <- read.csv(system.file("extdata", "test_B_CLU.csv", package = "cctu"))

com_cols <- c(
  "Trial", "Site", "Label", "PersonId", "VisitCycle",
  "FormCycle", "RepeatNumber"
)
dt_test <- merge(dt_a, dt_b, by = com_cols, all = TRUE, sort = FALSE)


test_that("Merge works", {
  # Merge dataset with merge_data function
  res <- merge_data(
    datalist = list(dt_a, dt_b),
    dlulist = list(dlu_a, dlu_b),
    clulist = list(clu_a, clu_b)
  )

  dt <- res$data
  dlu <- res$dlu
  clu <- res$clu

  expect_equal(nrow(dt), nrow(dt_test))

  dlu1 <- paste(dlu_a$ShortCode, 1, sep = "_")
  dlu2 <- paste(dlu_b$ShortCode, 2, sep = "_")

  clu1 <- paste(clu_a$ShortCode, 1, sep = "_")
  clu2 <- paste(clu_b$ShortCode, 2, sep = "_")

  expect_true(all(dlu1 %in% dlu$ShortCode))
  expect_true(all(dlu2 %in% dlu$ShortCode))
  expect_true(all(clu1 %in% clu$ShortCode))
  expect_true(all(clu2 %in% clu$ShortCode))
})

test_that("Check error", {
  dt_t <- dt_a[, -2]
  expect_error(
    merge_data(
      datalist = list(dt_t, dt_b),
      dlulist = list(dlu_a, dlu_b),
      clulist = list(clu_a, clu_b)
    ),
    "data in the datalist does not contain all the following"
  )
})

test_that("Check warnings", {
  tmp <- read.csv(text = "ShortCode,Visit/Form/Question,Description,Type
                  BRTHDATd,DAY1VISIT/LabRes/BRTHDATd,Participant dob,Date")
  dlu_t <- rbind(dlu_a, tmp)

  expect_warning(merge_data(
    datalist = list(dt_a, dt_b),
    dlulist = list(dlu_t, dlu_b),
    clulist = list(clu_a, clu_b)
  ))

  tmp <- read.csv(text = "ShortCode,CatCode,CatValue
PTONTRIAL,0,No
PTONTRIAL,1,Yes")

  clu_t <- rbind(clu_a, tmp)

  expect_warning(merge_data(
    datalist = list(dt_a, dt_b),
    dlulist = list(dlu_a, dlu_b),
    clulist = list(clu_t, clu_b)
  ))
})
