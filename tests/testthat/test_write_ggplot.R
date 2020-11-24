context("testing write_ggplot")


test_that("alternative dimension",
          {.old_meta <- get_meta_table()
            set_meta_table(cctu::meta_table_example)
            X <- data.frame(x=1,y=1)
            fig <- ggplot(X, aes(x=x,y=y))+geom_point()
            .parent <- cctu_env$parent
            assign("parent", NULL, envir=cctu_env)
            expect_warning(
              write_ggplot(fig,number="1.10", directory=".", units="inches"),
              "Unable to identify the code file that created figure"
            )
            assign("parent", .parent, envir=cctu_env)
            X <- data.frame(x=1,y=1)
            fig <- ggplot(X, aes(x=x,y=y))+geom_point()
            expect_error(  write_ggplot(fig,number="1.10", directory=".", units="pt", clean_up = FALSE),
                           "units must be ''cm'' or ''inches''"
                           )
            file.remove("fig_1.10.eps")
            write_ggplot(fig,number="1.10", directory=".", format="postscript", clean_up = FALSE)
            expect_equal(file.exists("fig_1.10.eps"), TRUE)
            set_meta_table(.old_meta)
            rm(.old_meta, .parent)
            }
)
