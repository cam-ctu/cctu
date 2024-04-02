context("testing write_ggplot")


test_that("alternative dimension",
          {.old_meta <- set_meta_table(cctu::meta_table_example)
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
            write_ggplot(fig,number="1.10", directory=".",
                         format="postscript",
                         clean_up = FALSE,
                         footnote = "I am custom footnote")
            mt_tab <- get_meta_table()
            expect_equal(mt_tab[mt_tab$number == "1.10", "footnote2"],
                         "I am custom footnote")
            expect_equal(file.exists("fig_1.10.eps"), TRUE)
            set_meta_table(.old_meta)
            rm(.old_meta, .parent)
            }
)


test_that("test write_plot",{
  .old_meta <- set_meta_table(cctu::meta_table_example)
  .parent <- cctu_env$parent

  new_plot <- function(x, y, h, v) {
    par(pty = "s", cex = 0.7) # adjust plot style
    plot(x, y)
    abline(h = h,v = v, lty=2) # add some lines
  }

  assign("parent", NULL, envir = cctu_env)

  write_plot(plot_fn = new_plot,
             number = "1.10",
             plot_args = list(x = iris[,1], y = iris[,2],
                              h = 2.5, v = 6.0),
             clean_up = FALSE)

  write_plot(x = iris[,1], y = iris[,2],
             plot_fn = new_plot,
             number = "1.10",
             plot_args = list(h = 2.5, v = 6.0),
             clean_up = FALSE)

  write_plot(x = iris[,1], y = iris[,2],
             h = 2.5, v = 6.0,
             plot_fn = new_plot,
             number = "1.10",
             clean_up = FALSE)

  X <- data.frame(x=1,y=1)
  fig <- ggplot(X, aes(x=x,y=y))+geom_point()
  write_plot(fig, number = "1.10")

})

