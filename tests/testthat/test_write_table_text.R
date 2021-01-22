context("testing write_text and write_table")


test_that("alternative dimension for table",
          {.old_meta <- get_meta_table()
            set_meta_table(cctu::meta_table_example)
            X <- data.frame(x=1,y=1)
            fig <- ggplot(X, aes(x=x,y=y))+geom_point()
            .parent <- cctu_env$parent
            assign("parent", NULL, envir=cctu_env)
            expect_warning(
              write_table(X,number="1.10", na_to_empty=TRUE,
                          clean_up=FALSE),
              "Unable to identify the code file that created table"
            )
            expect_warning(
              write_table(X,number="1.10", na_to_empty=TRUE),
              "will be removed by clean_up()"
            )

            expect_warning(
              write_text("Hello World!",number="1.10"),
              "Unable to identify the code file that created table"
            )
          # Test of tibble typ objects

            X <- data.frame(gender=rep(1:2,c(10,20)))
            X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
              dplyr::group_by(sex) %>%
              dplyr::summarise(n=dplyr::n(),.groups="drop") %>%
              write_table(number="1.10", directory=".")
            tab <- xml2::read_xml("table_1.10.xml")
            second_row <- xml2::xml_text(xml2::xml_find_all(tab, "//tr")[[2]])
            expect_equal(second_row," Male  10 ")
            assign("parent", .parent, envir=cctu_env)
            set_meta_table(.old_meta)
            rm(.old_meta, .parent)
            }
)

test_that("clean up behaviour standard code evaluation",
          { X <- data.frame(gender=rep(1:2,c(10,20)))
          .reserved <- "Y"
          Y <- X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
            dplyr::group_by(sex) %>%
            dplyr::summarise(n=dplyr::n(),.groups="drop")
            write_table(Y,number="1.10", directory=".")
          expect_false("X" %in% ls())
          expect_true("Y" %in% ls())
          }
)




test_that("clean up behaviour when piping",
          { X <- data.frame(gender=rep(1:2,c(10,20)))
          X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
            dplyr::group_by(sex) %>%
            dplyr::summarise(n=dplyr::n(),.groups="drop") %>%
            write_table(number="1.10", directory=".")
            expect_false("X" %in% ls())

          }
          )

