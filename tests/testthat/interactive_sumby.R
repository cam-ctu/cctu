# need to define fig_file="...." outside this script
library(cctu)
library(magrittr)
n <- 10
df <- data.frame(
  arm = rep(c("A", "B"), rep(n, 2)),
  age = rnorm(2 * n, mean = 40, sd = 10) %>% round(1),
  gender = sample(c("Male", "Female"), 2 * n, replace = TRUE)
)

pdf(file = fig_file)
sumby(gender, arm, data = df)
dev.off()
