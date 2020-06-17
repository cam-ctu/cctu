context("Test attach_pop errors")

test_that("call all warnings",
          {
            expect_warning(attach_pop("99.99.99.99"), "No population was attached")
            meta_table <- cctu::meta_table_example
            set_meta_table(meta_table)
            old_meta <- get_meta_table()
            meta_table <- meta_table[, -c(4,5)]
            assign("meta_table",meta_table, envir=cctu_env)
            expect_warning(attach_pop("99.99.99.99"), "Need to have 'number' column in meta_table")
            expect_warning(attach_pop("99.99.99.99"), "Need to have 'population' column in meta_table")
            set_meta_table(old_meta)
          }
)
