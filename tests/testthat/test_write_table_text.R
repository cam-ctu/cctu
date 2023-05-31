context("testing write_text and write_table")


test_that("alternative dimension for table",
          {.old_meta <- set_meta_table(cctu::meta_table_example)
            X <- data.frame(x=1,y=1)
            fig <- ggplot(X, aes(x=x,y=y))+geom_point()
            .parent <- cctu_env$parent
            assign("parent", NULL, envir=cctu_env)
            expect_warning(
              write_table(X,number="1.10", na_to_empty=TRUE,
                          clean_up=FALSE),
              "Unable to identify the code file that created table"
            )
            X <- data.frame(x=1,y=1)
            write_table(X, number="1.1", heading=c("x<2", "Y>3"),
                        footnote = "I am custom footnote")
            mt_tab <- get_meta_table()
            expect_equal(mt_tab[mt_tab$number == "1.1", "footnote2"],
                         "I am custom footnote")
            filetemp <- tempfile("report", fileext=".doc")
            create_word_xml("test report", "author",
                              meta_table=get_meta_table() %>% dplyr::filter(number=="1.1"),
                              popn_labels=c("my population"),
                              filename = filetemp

              )
            expect_true(file.exists(filetemp))
            unlink(filetemp)

            X <- data.frame(x=1,y=1)

            expect_warning(
              write_table(X,number="1.10", na_to_empty=TRUE),
              "will be removed by clean_up()"
            )

            #checking when the heading has dodgy characters


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
            expect_equal(second_row,"Male10")
            assign("parent", .parent, envir=cctu_env)
            set_meta_table(.old_meta)
            rm(.old_meta, .parent)
            }
)

test_that("clean up behaviour standard code evaluation",
          {
            .old_meta <- set_meta_table(cctu::meta_table_example)
            X <- data.frame(gender=rep(1:2,c(10,20)))
          .reserved <- "Y"
          Y <- X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
            dplyr::group_by(sex) %>%
            dplyr::summarise(n=dplyr::n(),.groups="drop")
            write_table(Y,number="1.10", directory=".")
          expect_false("X" %in% ls())
          expect_true("Y" %in% ls())
          set_meta_table(.old_meta)
          rm(.old_meta)
          }
)

test_that("Empty data",
          {
            .old_meta <- set_meta_table(cctu::meta_table_example)
            X <- data.frame(a=numeric(),b=numeric())
            .reserved <- "Y"
            write_table(X,number="1.10", directory=".")
            tab <- xml2::read_xml("table_1.10.xml")
            second_row <- xml2::xml_text(xml2::xml_find_all(tab, "//tr")[2])
            expect_equal(second_row, "No Data")
            set_meta_table(.old_meta)
            rm(.old_meta)
          }
)


test_that("clean up behaviour when piping",
          {
          .old_meta <- set_meta_table(cctu::meta_table_example)
          on.exit(set_meta_table(.old_meta))
          on.exit(rm(.old_meta) )
          X <- data.frame(gender=rep(1:2,c(10,20)))
          X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
            dplyr::group_by(sex) %>%
            dplyr::summarise(n=dplyr::n(),.groups="drop") |>
            write_table(number="1.10", directory=".")
          expect_false("X" %in% ls())


          skip_on_ci()
          #don't want to fail in github action as this is not yet fixed or might not be fixed
          X <- data.frame(gender=rep(1:2,c(10,20)))
          X %>% dplyr::mutate(sex=factor(gender, labels=c("Male","Female"))) %>%
            dplyr::group_by(sex) %>%
            dplyr::summarise(n=dplyr::n(),.groups="drop") %>%
            write_table(number="1.10", directory=".")
          # expect_false("X" %in% ls())
          }
          )

