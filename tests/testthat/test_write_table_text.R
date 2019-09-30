context("testing write_text and write_table")


test_that("alternative dimension for table",
          {.old_meta <- get_meta_table()
            set_meta_table(cctu::meta_table_example)
            X <- data.frame(x=1,y=1)
            fig <- ggplot(X, aes(x=x,y=y))+geom_point()
            .parent <- cctu_env$parent
            assign("parent", NULL, envir=cctu_env)
            expect_warning(
              write_table(X,number="1.10", na_to_empty=TRUE),
              "Unable to identify the code file that created table"
            )


            expect_warning(
              write_text("Hello World!",number="1.10"),
              "Unable to identify the code file that created table"
            )

            assign("parent", .parent, envir=cctu_env)
            set_meta_table(.old_meta)
            rm(.old_meta, .parent)
            }
)
