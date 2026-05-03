

# 3. Main Function
make_table1 <- function(dat, vars, group_vars = NULL, var_filter = NULL) {

  process_chunk <- function(.SD) {

    n_row <- data.table(
      Variable  = "N",
      Statistic = "",   # Leave blank or put "Count"
      Value     = as.character(nrow(.SD)),
      Var_ID    = 0,
      Stat_ID   = 1
    )

    # Iterate via Index (i) to capture the Variable Order
    chunk_list <- lapply(seq_along(vars), function(i) {
      col <- vars[i]

      # Check if this column has a specific filter in 'var_filter'
      if (!is.null(var_filter) && col %in% names(var_filter)) {

        filter_string <- var_filter[[col]]

        # We evaluate the string condition (e.g. "Gender == 'Male'")
        # inside the current data chunk (.SD)
        # We use tryCatch to prevent crashing if the string is invalid
        rows_to_keep <- tryCatch({
          expr <- parse(text = filter_string)
          # Evaluate expression within .SD environment
          # resulting in a TRUE/FALSE vector
          eval(expr, envir = .SD)
        }, error = function(e) {
          warning(paste("Filter failed for variable:", col, "-", e$message))
          return(TRUE) # Fallback: keep all rows if error
        })

        # Handle NA results in filtering (treat as FALSE)
        rows_to_keep[is.na(rows_to_keep)] <- FALSE

        # Subset the values
        val <- .SD[[col]][rows_to_keep]

      } else {
        # Standard case: Use all data in this group
        val <- .SD[[col]]
      }
      # ------------------------------------------

      # 1. Check for empty data
      if (is.null(val) || all(is.na(val))) return(NULL)

      # 2. Apply Render Function based on Class
      if (is.numeric(val)) {
        stats <- render_numeric(val)
      } else {
        # Assumes character/factor
        stats <- render_cat(val)
      }

      if (is.null(stats)) return(NULL)

      # 3. Create Table with Ordering IDs
      data.table(
        Variable  = col,
        Statistic = names(stats),
        Value     = as.character(stats),
        Var_ID    = i,                  # Keeps Variable Order (1st, 2nd, 3rd var)
        Stat_ID   = seq_along(stats)    # Keeps Statistic Order (1, 2, 3...)
      )
    })

    # Remove NULLs and bind
    chunk_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, chunk_list)
    if (length(chunk_list) == 0) return(NULL)
    rbindlist(c(list(n_row), chunk_list))
  }

  # --- Step C: Aggregate ---
  if (is.null(group_vars)) {
    long_res <- process_chunk(dat)
    setnames(long_res, "Value", "Overall")
    dcast_formula <- "Variable + Statistic ~ ."
  } else {
    valid_rows <- complete.cases(dat[, group_vars, with = FALSE])
    dat <- dat[valid_rows]
    long_res <- dat[, process_chunk(.SD), by = group_vars]
    rhs <- paste(group_vars, collapse = " + ")
    dcast_formula <- paste("Variable + Statistic ~", rhs)
  }

  # --- Enforce Order for Pivoting ---

  # 1. Sort the long table by the IDs we created
  setorder(long_res, Var_ID, Stat_ID)

  # 2. Lock Variable order
  long_res[, Variable := factor(Variable, levels = unique(Variable))]

  # 3. Lock Statistic order
  long_res[, Statistic := factor(Statistic, levels = unique(Statistic))]

  # --- Step E: Pivot ---
  final_res <- dcast(long_res, as.formula(dcast_formula),
                     value.var = "Value", fill = "-")

  return(final_res)
}

# Define variables
cols <- c("AGE", "SEX", "BMIBL", "RACEN", "SEX")

# --- Scenario A: One Grouping Variable ---
long_res <- build_summary(df, cols, group_vars = "group")

# --- Scenario B: Two Grouping Variables ---
cols <- c("AST", "BILI", "ALT", "inrange")
df$inrange <- to_factor(df$inrange)
long_res_2 <- make_table1(df, cols, group_vars = c("ARM", "AVISIT"))

# Pivot to wide format: Variable + Statistic ~ Treatment + Site
final_table_2 <- dcast(long_res_2,
                       Variable + Statistic ~ Treatment + Site,
                       value.var = "Value")

print("--- Two Group Variables (Treatment + Site) ---")
print(final_table_2)

options("cctu_print_plot" = FALSE)

# Need to add more

dt <- read.csv(system.file("extdata", "pilotdata.csv", package = "cctu"))
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package = "cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package = "cctu"))

dt$subjid <- substr(dt$USUBJID, 8, 11)
dt <- apply_macro_dict(dt, dlu, clu, clean_names = FALSE)

set_meta_table(cctu::meta_table_example)

# Create the population table
popn <- dt[, "subjid", drop = FALSE]
popn$safety <- TRUE

create_popn_envir("dt", popn)

tmp_dir <- tempdir()
# tidy up
rm(dlu)
.reserved <- ls()

options("cctu_digits_pct" = 1)

attach_pop("1.1")
df <- extract_form(dt, "PatientReg", vars_keep = c("subjid"))

expect_true("subjid" %in% names(df))

df$BMIBL[df$RACEN == 6] <- NA

df$group <- df$ARM

r <- cttab(
  x = c("AGE", "SEX", "BMIBL"),
  group = "group",
  data = df,
  select = c("BMIBL" = "RACEN != 1")
)


attach_pop("1.10")
df <- extract_form(dt, "Lab", vars_keep = c("subjid", "ARM"))

expect_true("ARM" %in% names(df))
df$inrange <- ifelse(df$AST < 20, "Low",
                     ifelse(df$AST > 40, "High", "Normal")
)
var_lab(df$inrange) <- "AST range"

r1 <- cttab(
  x = c("AST", "BILI", "ALT", "inrange"),
  group = "ARM",
  data = df,
  row_split = "AVISIT",
  select = c("ALT" = "PERF == 1")
)



final_table_2 <- dcast(r,
                       Variable + Statistic ~ group,
                       value.var = "Value",
                       fill = "-")

tmp <- group_data(final_table_2,
                  groups = c("Variable"),
                  shift = TRUE,
                  indent = TRUE)


final_table_3 <- dcast(r1,
                       Variable + AVISIT + Statistic ~ ARM,
                       value.var = "Value",
                       fill = "-")

tmp2 <- group_data(final_table_3,
                  groups = c("AVISIT", "Variable"),
                  shift = TRUE,
                  indent = TRUE)

tmp3 <- group_data(final_table_3,
                   groups = c("Variable", "AVISIT"),
                   shift = TRUE,
                   indent = TRUE)


