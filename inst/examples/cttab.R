
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

