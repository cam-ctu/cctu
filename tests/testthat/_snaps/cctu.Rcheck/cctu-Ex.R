pkgname <- "cctu"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "cctu-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('cctu')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("all_is_numeric")
### * all_is_numeric

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: all_is_numeric
### Title: Check if All Elements in Character Vector are Numeric
### Aliases: all_is_numeric

### ** Examples

all_is_numeric(c("1", "1.2", "3"))
all_is_numeric(c("1", "1.2", "3a"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("all_is_numeric", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apply_macro_dict")
### * apply_macro_dict

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apply_macro_dict
### Title: Apply DLU/CLU file to the data frame
### Aliases: apply_macro_dict

### ** Examples


# Read MACRO data
dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"),
               colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

# Create subjid
dt$subjid <- substr(dt$USUBJID, 8, 11)

df <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)

# Following can give you the same dlu file used by apply_macro_dict
dlu <- tidy_dlu(dlu, clean_names = FALSE)

# Extract data from Lab form
lb <- extract_form(df, "Lab")

# Extract screening visit data from lab form
lb_base <- extract_form(df, "Lab", visit = "SCREENING")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apply_macro_dict", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("copy_lab")
### * copy_lab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: copy_lab
### Title: Copy variable label and value labels
### Aliases: copy_lab

### ** Examples

var_with_lab <- rep(1:2, 5)
var_lab(var_with_lab) <- "Income"
val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)
var_nolab <- rep(1:2, 10)
var_ut <- copy_lab(var_nolab, var_with_lab)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("copy_lab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cttab")
### * cttab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cttab
### Title: Generate an table of descriptive statistics.
### Aliases: cttab cttab.default cttab.formula

### ** Examples


# Read data
dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"))
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

dt$subjid <- substr(dt$USUBJID, 8, 11)

# Apply variable attributes
dt <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)

# Extract form data to be analysed
df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

########################################################
#  Simple analysis no group and variable subset
######################################################
# Variable as a vector
X <- cttab(x = c("AGE", "SEX", "BMIBL"),
             data = df,
             select = c("BMIBL" = "RACEN != 1"))

# Variable as a formula, equivalent to above
X1 <- cttab(AGE + SEX + BMIBL ~ 1,
            data = df,
            select = c("BMIBL" = "RACEN != 1"))

#############################################
#  Analysis by group
############################################
# Variable as a vector
X <- cttab(x = c("AGE", "SEX", "BMIBL"),
             group = "ARM",
             data = df,
             select = c("BMIBL" = "RACEN != 1"))

############################################
# Analysis by group and cycles
############################################

df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

X <- cttab(x = c("AST", "BILI", "ALT"),
                  group = "ARM",
                  data = df,
                  row_split = "AVISIT",
                  select = c("ALT" = "PERF == 1"))

############################################
# Group variables
############################################

df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))
base_lab <- extract_form(dt, "Lab", visit = "SCREENING", vars_keep = c("subjid"))

base_lab$ABNORMALT <- base_lab$ALT > 22.5
var_lab(base_lab$ABNORMALT) <- "ALT abnormal"
base_lab$ABNORMAST <- base_lab$AST > 25.5
var_lab(base_lab$ABNORMAST) <- "AST abnormal"

df <- merge(df, base_lab, by = "subjid")

X <- cttab(x = list(c("AGE", "SEX", "BMIBL"),
                      "Blood" = c("ALT", "AST"),
                      "Patients with Abnormal" = c("ABNORMAST", "ABNORMALT")),
          group = "ARM",
          data = df,
          select = c("BMIBL" = "RACEN != 1",
                     "ALT" = "PERF == 1"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cttab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_form")
### * extract_form

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_form
### Title: Extract data by form from MACRO dataset
### Aliases: extract_form

### ** Examples


# Read MACRO data
dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"),
               colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

# Create subjid
dt$subjid <- substr(dt$USUBJID, 8, 11)

df <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)

# Following can give you the same dlu file used by apply_macro_dict
dlu <- tidy_dlu(dlu, clean_names = FALSE)

# Extract data from Lab form
lb <- extract_form(df, "Lab")

# Extract screening visit data from lab form
lb_base <- extract_form(df, "Lab", visit = "SCREENING")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_form", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("format_pval")
### * format_pval

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: format_pval
### Title: Format p-value
### Aliases: format_pval

### ** Examples

pv <- c(-1, 0.00001, 0.0042, 0.0601, 0.1335, 0.4999, 0.51, 0.89, 0.9, 1)
format_pval(pv)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("format_pval", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("geom_stepribbon")
### * geom_stepribbon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: geom_stepribbon
### Title: Step ribbon plots.
### Aliases: geom_stepribbon GeomStepribbon
### Keywords: datasets

### ** Examples

library(ggplot2)
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
h <- ggplot(huron, aes(year))
h + geom_stepribbon(
  aes(
    ymin = level - 1,
    ymax = level + 1
  ),
  fill = "grey70"
) +
  geom_step(aes(y = level))
h + geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
  geom_line(aes(y = level))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("geom_stepribbon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_obj")
### * get_obj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_obj
### Title: Gets copies of objects from parent environments, or returns
###   alternative if not found
### Aliases: get_obj
### Keywords: internal

### ** Examples

library(cctu)
rm(PATH)
cctu:::get_obj("PATH")
cctu:::get_obj("PATH", alt = getwd())
PATH <- "C:/MyFile"
cctu:::get_obj("PATH")
cctu:::get_obj("PATH", alt = getwd())



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_obj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_empty")
### * is_empty

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_empty
### Title: Check whether string, list or vector is empty
### Aliases: is_empty

### ** Examples

is_empty("test")
is_empty("")
is_empty(NA)
is_empty(NULL)

# string is not empty
is_empty(" ")

# however, this trimmed string is
is_empty(trimws(" "))

# numeric vector
x <- 1
is_empty(x)
x <- x[-1]
is_empty(x)

# check multiple elements of character vectors
is_empty(c("", "a"))

# empty data frame
d <- data.frame()
is_empty(d)

# empty list
# is_empty(list(NULL))

# NA vector
x <- rep(NA, 5)
is_empty(x)
is_empty(x, na_empty = FALSE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_empty", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("km_ggplot")
### * km_ggplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: km_ggplot
### Title: Create a Kaplan-Meier plot using ggplot2
### Aliases: km_ggplot

### ** Examples

library(survival)
fit <- survfit(Surv(time, status) ~ rx, data = colon)
km_ggplot(fit)
## Change theme of the KM-plot
p <- km_ggplot(fit)
p$top <- p$top +
  ggplot2::theme_classic()
# Change the theme of the risktable
p$bottom <- p$bottom +
  ggplot2::theme_void()

plot(p)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("km_ggplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lab2val")
### * lab2val

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lab2val
### Title: Replace vector/matrix/data.frame values with corresponding value
###   labels.
### Aliases: lab2val

### ** Examples

data(mtcars)
mtcars <- within(mtcars, {
  var_lab(mpg) <- NULL
  val_lab(am) <- c(" automatic" = 0, " manual" = 1)
})

table(lab2val(mtcars$am))

summary(lm(mpg ~ ., data = lab2val(mtcars[, c("mpg", "am")])))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lab2val", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_data")
### * merge_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_data
### Title: Merge vertically split data
### Aliases: merge_data

### ** Examples


# Read example data
dt_a <- read.csv(system.file("extdata", "test_A.csv", package="cctu"),
                 colClasses = "character")
dt_b <- read.csv(system.file("extdata", "test_B.csv", package="cctu"),
                 colClasses = "character")

# Read DLU and CLU
dlu_a <- read.csv(system.file("extdata", "test_A_DLU.csv", package="cctu"))
dlu_b <- read.csv(system.file("extdata", "test_B_DLU.csv", package="cctu"))
clu_a <- read.csv(system.file("extdata", "test_A_CLU.csv", package="cctu"))
clu_b <- read.csv(system.file("extdata", "test_B_CLU.csv", package="cctu"))

# Merge dataset with merge_data function
res <- merge_data(datalist = list(dt_a, dt_b), dlulist = list(dlu_a, dlu_b),
                  clulist = list(clu_a, clu_b))
dt <- res$data # Extract combined data
dlu <- res$dlu # Extract combined DLU data
clu <- res$clu # Extract combined CLU data

# Apply CLU and DLU files
dt <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("num_stat")
### * num_stat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: num_stat
### Title: Compute some basic descriptive statistics.
### Aliases: num_stat cat_stat
### Keywords: utilities

### ** Examples

x <- exp(rnorm(100, 1, 1))
num_stat(x)

y <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
y[1:10] <- NA
cat_stat(y)
cat_stat(is.na(y))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("num_stat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("p_format")
### * p_format

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: p_format
### Title: formats p values with rounding and <0.001
### Aliases: p_format

### ** Examples

p_format(c(0.000001, 0.451234))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("p_format", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.code_tree")
### * plot.code_tree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.code_tree
### Title: plot the code tree.
### Aliases: plot.code_tree

### ** Examples

## Not run: 
##D plot(get_code_tree(), root_file = "main.R")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.code_tree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_data")
### * read_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_data
### Title: Automatic reading in data from a meta-table of external data
###   sets.
### Aliases: read_data read_data.data.frame read_data.character

### ** Examples

data_table <- data.frame(
  name = c("dirtydata", "meta"),
  file = c("dirtydata.csv", "meta_table.xlsx"),
  folder = system.file("extdata", package = "cctu"),
  stringsAsFactors = FALSE
)
data_table_summary(data_table)
options("verbose" = TRUE)
read_data(data_table)
summary(dirtydata)
summary(meta)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("regression_table")
### * regression_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: regression_table
### Title: Produce a table summarising a regression model for a study
###   report
### Aliases: regression_table

### ** Examples

library(survival)
cfit1 <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lung)
regression_table(cfit1,
  digits = 4,
  labels = c(
    "Age (per year)", "Sex (Female vs Male)",
    "Weight loss (per pound)"
  )
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("regression_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("render_cat")
### * render_cat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: render_cat
### Title: Render categorical values for table output.
### Aliases: render_cat
### Keywords: utilities

### ** Examples

y <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
y[1:10] <- NA
render_cat(y)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("render_cat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("render_numeric")
### * render_numeric

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: render_numeric
### Title: Render continuous values for table output.
### Aliases: render_numeric
### Keywords: utilities

### ** Examples

x <- exp(rnorm(100, 1, 1))
render_numeric(x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("render_numeric", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("signif_pad")
### * signif_pad

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: signif_pad
### Title: Round numbers with 0-padding.
### Aliases: signif_pad round_pad
### Keywords: utilities

### ** Examples

x <- c(0.9001, 12345, 1.2, 1., 0.1, 0.00001, 1e5)
signif_pad(x, digits = 3)
signif_pad(x, digits = 3, round.integers = TRUE)
round_pad(x, digits = 2)

# Compare:
as.character(signif(x, digits = 3))
format(x, digits = 3, nsmall = 3)
prettyNum(x, digits = 3, drop0trailing = TRUE)
prettyNum(x, digits = 3, drop0trailing = FALSE)

# This is very close.
formatC(x, format = "fg", flag = "#", digits = 3)
formatC(signif(x, 3), format = "fg", flag = "#", digits = 3)

# Could always remove the trailing "."
sub("[.]$", "", formatC(x, format = "fg", flag = "#", digits = 3))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("signif_pad", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("to_factor")
### * to_factor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: to_factor
### Title: Convert vector but keep value/value labels.
### Aliases: to_factor to_character to_numeric to_logical

### ** Examples

data(mtcars)
mtcars <- within(mtcars, {
  var_lab(am) <- "Transmission"
  val_lab(am) <- c(" automatic" = 0, " manual" = 1)
})

mtcars$am <- to_factor(mtcars$am)
mtcars$gear <- to_character(mtcars$gear)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("to_factor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("unlab")
### * unlab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: unlab
### Title: Drop variable label and value labels
### Aliases: unlab

### ** Examples

raw_var <- rep(1:2, 5)
var_with_lab <- raw_var
var_lab(var_with_lab) <- "Income"
val_lab(var_with_lab) <- c("Low" = 1, "High" = 2)
identical(raw_var, unlab(var_with_lab)) # should be TRUE




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("unlab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("val_lab")
### * val_lab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: val_lab
### Title: Set or get value labels
### Aliases: val_lab val_lab<- has.labels unval

### ** Examples

# toy example
set.seed(123)
# score - evaluation of tested product

score <- sample(-1:1, 20, replace = TRUE)
var_lab(score) <- "Evaluation of tested brand"
val_lab(score) <- c(
  "Dislike it" = -1,
  "So-so" = 0,
  "Like it" = 1
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("val_lab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("var_lab")
### * var_lab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: var_lab
### Title: Set or get variable label
### Aliases: var_lab var_lab.data.frame var_lab<- has.label drop_lab

### ** Examples

data(mtcars)
mtcars <- within(mtcars, {
  var_lab(mpg) <- "Miles/(US) gallon"
  var_lab(cyl) <- "Number of cylinders"
  var_lab(disp) <- "Displacement (cu.in.)"
  var_lab(hp) <- "Gross horsepower"
  var_lab(drat) <- "Rear axle ratio"
  var_lab(wt) <- "Weight (lb/1000)"
  var_lab(qsec) <- "1/4 mile time"
  var_lab(vs) <- "V/S"
  val_lab(vs) <- c("V-shaped" = 0, "straight" = 1)
  var_lab(am) <- "Transmission"
  val_lab(am) <- c(automatic = 0, manual = 1)
  var_lab(gear) <- "Number of forward gears"
  var_lab(carb) <- "Number of carburetors"
})

table(mtcars$am)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("var_lab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_plot")
### * write_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_plot
### Title: Function to save plot figures
### Aliases: write_plot

### ** Examples

## Not run: 
##D #####################################
##D # Below is a simple example ========#
##D #####################################
##D #
##D write_plot(plot_fn = plot, plot_args = list(x = iris[, 1], y = iris[, 2]))
##D # This is equivalent drawing the following plot and save it
##D plot(x = iris[, 1], y = iris[, 2])
##D 
##D # Below is user defined function plotting
##D # One can use this method to draw a complicated plot
##D new_plot <- function(x, y, h, v) {
##D   par(pty = "s", cex = 0.7) # adjust plot style
##D   plot(x, y)
##D   abline(h = h, v = v, lty = 2) # add some lines
##D }
##D write_plot(
##D   x = iris[, 1], y = iris[, 2], h = 2.5, v = 6.0,
##D   plot_fn = new_plot
##D )
##D 
##D 
##D ####################################################
##D # To draw a KM-plot from survminer package ========#
##D ####################################################
##D 
##D library("survival")
##D library("survminer")
##D fit <- survfit(Surv(time, status) ~ sex, data = lung)
##D # Drawing survival curves
##D p <- ggsurvplot(fit, data = lung)
##D write_plot(p, plot_fn = survminer:::print.ggsurvplot)
##D # The code above works because the p is a ggsurvplot object (check it with class(p))
##D # There's a printing function print.ggsurvplot to handle the printing of the KM-plot.
##D # But this function is not exported by survminer, so we need to use three colons.
##D 
##D #####################################
##D # Draw a consort diagram ===========#
##D #####################################
##D 
##D library(grid)
##D # Might want to change some settings
##D txt0 <- c("Study 1 (n=160)", "Study 2 (n=140)")
##D txt1 <- "Population (n=300)"
##D txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=15)"
##D 
##D # supports pipeline operator
##D g <- add_box(txt = txt0) |>
##D   add_box(txt = txt1) |>
##D   add_side_box(txt = txt1_side) |>
##D   add_box(txt = "Randomized (n=200)")
##D # Since you can draw the plot g with plot(g), the ploting function is plot
##D # The plotting function is \code{plot.consort}, so simple plot or plot.consort works
##D write_plot(g, plot_fn = plot)
##D # Or just
##D write_plot(g)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
