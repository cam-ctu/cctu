
# Read MACRO data
dt <- read.csv(system.file("extdata", "pilotdata.csv", package="cctu"),
               colClasses = "character")
dlu <- read.csv(system.file("extdata", "pilotdata_dlu.csv", package="cctu"))
clu <- read.csv(system.file("extdata", "pilotdata_clu.csv", package="cctu"))

# Create subjid
dt$subjid <- substr(dt$USUBJID, 8, 11)

df <- apply_macro_dict(dt, dlu = dlu, clu = clu, clean_names = FALSE)

# Following can give you the same dlu file used by apply_macro_dict
dlu <- sep_dlu(dlu, clean_names = FALSE)

# Extract data from Lab form
lb <- extract_form(df, "Lab")

# Extract screening visit data from lab form
lb_base <- extract_form(df, "Lab", visit = "SCREENING")
