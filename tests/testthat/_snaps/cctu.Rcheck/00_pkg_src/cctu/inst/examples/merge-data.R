
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

