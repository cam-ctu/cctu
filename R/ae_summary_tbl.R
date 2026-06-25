#' Summarise subject-level events by class and term
#'
#' Generic counter for AE-style tables: counts unique subjects at the
#' class level, term level, and overall, optionally stratified by treatment
#' and grade/severity. Works for adverse events, medical history, prior/
#' concomitant medications, or anything with a class/term hierarchy.
#'
#' @param data     Event-level dataset (one row per event).
#' @param adsl     Subject-level dataset providing denominators.
#' @param id_var   Subject ID column name (string). Must exist in both
#'                 `data` and `adsl`.
#' @param term_var Preferred term column name in `data` (string).
#' @param class_var Class/SOC column name in `data` (string), or `NULL`.
#'                  When `NULL`, the table is flat: a top "any event" row
#'                  followed by term-level rows, with no class grouping.
#' @param trt_var  Optional treatment column in both `adsl` and `data`.
#' @param overall  Add a Total column when `trt_var` is given.
#' @param grade_var Optional grade/severity column in `data`.
#' @param grades   Grade levels to report; defaults to "Any grade" + observed.
#'                  When supplied, the order is taken as ascending severity
#'                  (worst last), which also drives worst-grade counting.
#' @param grade_layout When `grade_var` is given, "vertical" (default) nests
#'                  grades as rows beneath each term: the term row shows the
#'                  any-grade count and one indented sub-row per grade group
#'                  gives the subject count for that group. "horizontal" instead
#'                  puts each grade group in its own column. Class/overall rows
#'                  stay any-grade. No effect when `grade_var` is `NULL`.
#' @param grade_count How a subject is counted across grades for a given term
#'                  (or class). "worst" (default) counts each subject once, at
#'                  their maximum grade, so grade rows are mutually exclusive and
#'                  sum to the any-grade count. "any" counts a subject at every
#'                  grade they experienced (rows may sum past the any-grade
#'                  count). Worst-grade is taken independently at each level.
#' @param grade_report Which grade groups to show. "all" (default) reports each
#'                  grade. "high" reports a single collapsed "Grade >=k" group
#'                  (k = `grade_high`) alongside the any-grade row. Requires
#'                  numeric grade labels (e.g. "Grade 3", "3").
#' @param grade_high Threshold k for `grade_report = "high"`; default 3 gives a
#'                  "Grade >=3" group.
#' @param min_pct  Term-level frequency filter (percent); 0 disables.
#' @param class_sort "freq" (default) or "alpha".
#' @param any_label Label for the top "any event" row. Defaults to
#'                  "Subjects with any event"; pass e.g. "Patients with any AE"
#'                  or "Subjects with any medical history" to match context.
#'
#' @return A \code{data.frame} of class \code{cttab}: a \code{label} column,
#'   one column per arm (or arm x grade group), and a \code{row_style}
#'   attribute (via \code{\link{format_table}}) that bolds the any-event and
#'   class rows - and, in the vertical grade layout, the term rows that carry
#'   nested grade sub-rows - and indents the term / grade rows for
#'   \code{\link{write_table}}.
#' @seealso \code{\link{format_table}}, \code{\link{shift_table}},
#'   \code{\link{write_table}}
#' @import data.table
#' @export
#' @examples
#' adsl <- data.frame(id = sprintf("S%02d", 1:10),
#'                    arm = rep(c("Active", "Placebo"), each = 5))
#' ae <- data.frame(
#'   id  = c("S01", "S01", "S02", "S03", "S06", "S07"),
#'   soc = c("Skin", "GI", "Skin", "Skin", "GI", "Skin"),
#'   pt  = c("Rash", "Nausea", "Rash", "Pruritus", "Nausea", "Rash"),
#'   gr  = c("Grade 1", "Grade 3", "Grade 2", "Grade 1", "Grade 2", "Grade 1")
#' )
#' # Class -> term hierarchy with worst-grade sub-rows and a Total column.
#' ae_summary(ae, adsl, id_var = "id", class_var = "soc", term_var = "pt",
#'            trt_var = "arm", grade_var = "gr")
#'
#' # Flat table (no class): a single bold any-event row over indented terms.
#' ae_summary(ae, adsl, id_var = "id", term_var = "pt", trt_var = "arm")
ae_summary <- function(data, adsl,
                       id_var = cctu_opt("subjid_string"),
                       term_var,
                       class_var = NULL, 
                       trt_var    = NULL,
                       overall    = TRUE,
                       grade_var  = NULL,
                       grades     = NULL,
                       grade_layout = c("vertical", "horizontal"),
                       grade_count  = c("worst", "any"),
                       grade_report = c("all", "high"),
                       grade_high   = 3L,
                       min_pct    = 0,
                       class_sort = c("freq", "alpha"),
                       any_label  = "Participants with any event") {

  class_sort   <- match.arg(class_sort)
  grade_layout <- match.arg(grade_layout)
  grade_count  <- match.arg(grade_count)
  grade_report <- match.arg(grade_report)
  has_class    <- !is.null(class_var)
  has_grade    <- !is.null(grade_var)
  # Worst-grade collapsing only applies when there is a grade variable.
  worst        <- grade_count == "worst" && has_grade
  # Vertical grade rows only make sense when an actual grade variable is given.
  vertical     <- grade_layout == "vertical" && has_grade
  if (grade_report == "high")
    stopifnot(is.numeric(grade_high), length(grade_high) == 1, grade_high >= 1)

  # data.table NSE column names (internal `_`-prefixed columns and `i.`-prefixed
  # join columns), bound locally so R CMD check / the object_usage linter don't
  # flag them as undefined globals. Lexical scope makes them visible to the
  # nested build_level() helper too.
  # nolint start: object_name_linter.
  `_id` <- `_class` <- `_term` <- `_arm` <- `_trtvar` <- `_gradevar` <-
    `_grade` <- `_grank` <- N <- np <- rn <- label <-
    max_pct <- class_order <- term_order <- grade_order <-
    i.class_order <- i.term_order <- NULL
  # nolint end

  # ---- 1. Validate and copy to internal names ------------------------------
  stopifnot(
    is.character(id_var), length(id_var) == 1,
    is.character(term_var), length(term_var) == 1,
    id_var    %in% names(data), id_var %in% names(adsl),
    term_var  %in% names(data)
  )
  if (has_class) stopifnot(
    is.character(class_var), length(class_var) == 1,
    class_var %in% names(data)
  )
  if (!is.null(trt_var))   stopifnot(trt_var %in% names(adsl))
  if (!is.null(grade_var)) stopifnot(grade_var %in% names(data))

  # Work on copies so we don't mutate the caller's data, and rename to
  # stable internal names. This keeps the rest of the function readable
  # and sidesteps quasi-quotation gymnastics inside data.table calls.
  d <- as.data.table(data)
  a <- as.data.table(adsl)
  d <- copy(d)
  a <- copy(a)

  if (has_class) {
    setnames(d, c(id_var, class_var, term_var), c("_id", "_class", "_term"))
  } else {
    setnames(d, c(id_var, term_var), c("_id", "_term"))
    # A single constant class keeps the class-aware logic below working as a
    # no-op (one group); the column is dropped before returning.
    d[, `_class` := ""]
  }
  setnames(a, id_var, "_id")
  if (!is.null(grade_var)) setnames(d, grade_var, "_gradevar")
  if (!is.null(trt_var))   setnames(a, trt_var,   "_trtvar")

  d <- d[`_id` %in% a$`_id`, ]

  # ---- 2. Arms and denominators from ADSL ----------------------------------
  if (is.null(trt_var)) {
    a[, `_arm` := "Total"]
    arms <- "Total"
  } else {
    a[, `_arm` := as.character(`_trtvar`)]
    arms <- sort(unique(a$`_arm`))
  }

  # Bring arm into the event data from ADSL, never from the events themselves
  d <- merge(d, a[, .(`_id`, `_arm`)], by = "_id", all.x = TRUE)

  denom <- a[, .(N = uniqueN(`_id`)), by = `_arm`]
  if (overall && !is.null(trt_var)) {
    denom <- rbind(denom,
                   data.table(`_arm` = "Total", N = uniqueN(a$`_id`)))
    arms  <- c(arms, "Total")
  }

  # ---- 3. Grade strata -----------------------------------------------------
  # `grade_levels` is the list of reported groups: "Any grade" plus either every
  # grade ("all") or a single "Grade >= k" group ("high"). `ggroup_fun` maps an
  # event/collapsed grade to its reported group; `_grank` gives severity order
  # for worst-grade collapsing.
  if (!has_grade) {
    d[, `_grade` := "Any grade"]
    grade_levels <- "Any grade"
    ggroup_fun   <- function(x) x
  } else {
    d[, `_grade` := as.character(`_gradevar`)]
    observed <- sort(unique(d$`_grade`))

    # Integer embedded in a grade label, e.g. "Grade 3" -> 3, "3" -> 3.
    parse_gnum <- function(x) {
      suppressWarnings(as.integer(sub("\\D*([0-9]+).*$", "\\1", x)))
    }

    # Severity rank (ascending; worst = highest). Prefer caller-supplied order,
    # then numeric grade, else alphabetical with a warning for worst-grade use.
    if (!is.null(grades)) {
      sev_levels <- setdiff(grades, "Any grade")
      rank_of <- function(x) match(x, sev_levels)
    } else {
      sev_levels <- observed
      gn <- parse_gnum(observed)
      if (!anyNA(gn)) {
        rank_of <- function(x) parse_gnum(x)
      } else {
        rank_of <- function(x) match(x, observed)
        if (worst)
          warning("Grade severity order assumed alphabetical; pass `grades` ",
                  "in ascending severity order for correct worst-grade counts.")
      }
    }

    if (grade_report == "high") {
      if (anyNA(parse_gnum(observed)))
        stop("grade_report = \"high\" needs numeric grade labels ",
             "(e.g. \"Grade 3\"); none could be parsed. Use ",
             "grade_report = \"all\" or supply numeric grades.")
      high_label    <- sprintf("Grade >=%d", as.integer(grade_high))
      ggroup_fun    <- function(x) {
        data.table::fifelse(parse_gnum(x) >= grade_high, high_label,
                            NA_character_)
      }
      report_groups <- high_label
    } else {
      report_groups <- sev_levels[order(rank_of(sev_levels))]
      ggroup_fun    <- function(x) x
    }

    grade_levels <- c("Any grade", report_groups)
    if (worst) d[, `_grank` := rank_of(`_grade`)]
  }

  # ---- 4. Builder for each level -------------------------------------------
  build_level <- function(by_cols, label_fn, level) {
    # Worst-grade: collapse to one row per subject x key at the maximum grade,
    # taken at THIS level's key, so grade groups partition subjects. NA ranks
    # (grades outside `grades`) sort lowest so such subjects are never dropped
    # from the any-grade count.
    base <- if (worst) {
      ord <- d[, .I[which.max(fifelse(is.na(`_grank`), -Inf,
                                      as.numeric(`_grank`)))],
               by = c("_id", by_cols)]$V1
      d[ord]
    } else {
      d
    }
    grp <- if (has_grade) ggroup_fun(base[["_grade"]]) else NULL

    parts <- list()
    for (g in grade_levels) {
      sub <- if (g == "Any grade") base else base[!is.na(grp) & grp == g]
      for (arm in arms) {
        s <- if (arm == "Total" && overall && !is.null(trt_var)) sub
        else sub[`_arm` == arm]

        if (nrow(s) == 0) {
          empty <- if (length(by_cols) == 0) data.table(n = 0L)
          else cbind(unique(d[, .SD, .SDcols = by_cols]), n = 0L)
          empty[, `:=`(`_arm` = arm, `_grade` = g)]
          parts[[length(parts) + 1L]] <- empty
        } else {
          cnt <- s[, .(n = uniqueN(`_id`)), by = by_cols]
          cnt[, `:=`(`_arm` = arm, `_grade` = g)]
          parts[[length(parts) + 1L]] <- cnt
        }
      }
    }
    out <- rbindlist(parts, fill = TRUE)
    out[, level := level]
    out[, label := label_fn(out)]
    out
  }

  any_ev <- build_level(character(0),
                        function(x) any_label, level = 0L)
  any_ev[, `:=`(`_class` = NA_character_, `_term` = NA_character_)]

  if (has_class) {
    cls <- build_level("_class",
                       function(x) x$`_class`, level = 1L)
    cls[, `_term` := NA_character_]
  }

  trm <- build_level(c("_class", "_term"),
                     function(x) x$`_term`,
                     level = if (has_class) 2L else 1L)

  # ---- 5. Term-level frequency filter --------------------------------------
  if (min_pct > 0) {
    any_grade_lab <- grade_levels[1]
    pct <- merge(trm[`_grade` == any_grade_lab], denom, by = "_arm")
    pct[, pct := 100 * n / N]
    keep <- pct[, .(max_pct = max(pct, na.rm = TRUE)),
                by = .(`_class`, `_term`)][max_pct >= min_pct]
    trm <- trm[keep, on = c("_class", "_term")]
    if (has_class) cls <- cls[`_class` %in% unique(keep$`_class`)]
  }

  # ---- 6. Class and term ordering ------------------------------------------
  if (class_sort == "freq") {
    class_rank <- d[, .(rn = uniqueN(`_id`)), by = `_class`]
    setorder(class_rank, -rn)
  } else {
    class_rank <- data.table(`_class` = sort(unique(d$`_class`)))
  }
  class_rank[, class_order := .I]

  if (has_class) cls[class_rank, class_order := i.class_order, on = "_class"]
  trm   [class_rank, class_order := i.class_order, on = "_class"]
  any_ev[, class_order := 0L]

  any_grade_lab <- grade_levels[1]
  term_rank <- trm[`_grade` == any_grade_lab,
                   .(rn = sum(n)), by = .(`_class`, `_term`)]
  setorder(term_rank, `_class`, -rn, `_term`)
  term_rank[, term_order := seq_len(.N), by = `_class`]
  trm[term_rank, term_order := i.term_order, on = c("_class", "_term")]
  if (has_class) cls[, term_order := 0L]
  any_ev[, term_order := 0L]

  # Grade ordering within a term (used only by the vertical layout). "Any grade"
  # maps to 0 so the term/class/overall rows always sort ahead of grade rows.
  trm   [, grade_order := match(`_grade`, grade_levels) - 1L]
  if (has_class) cls[, grade_order := match(`_grade`, grade_levels) - 1L]
  any_ev[, grade_order := match(`_grade`, grade_levels) - 1L]

  # ---- 7. Format and pivot wide --------------------------------------------
  # Vertical layout: term/class/overall rows show the any-grade count; the
  # per-grade rows are pulled out of `trm` and re-levelled as indented sub-rows
  # beneath each term, labelled by grade. Columns are arms only.
  if (vertical) {
    term_level  <- if (has_class) 2L else 1L
    grade_level <- term_level + 1L

    grd <- trm[`_grade` != "Any grade"]
    grd[, `:=`(level = grade_level, label = `_grade`)]

    any_ev <- any_ev[`_grade` == "Any grade"]
    if (has_class) cls <- cls[`_grade` == "Any grade"]
    trm <- trm[`_grade` == "Any grade"]
  }

  parts <- list(any_ev,
                if (has_class) cls,
                trm,
                if (vertical) grd)
  tbl <- rbindlist(Filter(Negate(is.null), parts), fill = TRUE)

  tbl <- merge(tbl, denom, by = "_arm", all.x = TRUE)
  tbl[, np := ifelse(is.na(N) | N == 0, "0",
                     sprintf("%d (%.1f)", n, 100 * n / N))]
  # Grade goes into columns only for the horizontal layout.
  tbl[, col := if (length(grade_levels) == 1 || vertical) `_arm`
      else paste(`_arm`, `_grade`, sep = " | ")]

  lhs  <- c("class_order", "term_order", "level", "label",
            if (vertical) "grade_order",
            if (has_class) "_class", "_term")
  form <- as.formula(paste(paste0("`", lhs, "`", collapse = " + "), "~ col"))
  wide <- dcast(tbl, form, value.var = "np", fill = "0")
  # term_order before level so each term is immediately followed by its grades.
  sort_cols <- c("class_order", "term_order", "level",
                 if (vertical) "grade_order")
  setorderv(wide, sort_cols)

  arm_order <- arms
  col_order <- if (length(grade_levels) > 1 && !vertical)
    as.vector(outer(arm_order, grade_levels, paste, sep = " | "))
  else arm_order
  col_order <- intersect(col_order, names(wide))

  # Rename the internal class/term columns back to the user's names for
  # downstream use (rendering, joins, QC). With no class_var, the placeholder
  # class column is dropped entirely.
  if (has_class) {
    setnames(wide, c("_class", "_term"), c(class_var, term_var))
    setcolorder(wide, c("label", "level", class_var, term_var, col_order))
  } else {
    setnames(wide, "_term", term_var)
    setcolorder(wide, c("label", "level", term_var, col_order))
  }
  drop_cols <- intersect(c("class_order", "term_order", "grade_order"),
                         names(wide))
  wide[, (drop_cols) := NULL]

  # ---- 8. Style: bold top levels, indent inner levels ----------------------
  # `level` indexes the hierarchy (0 any-event, 1 class, 2 term, 3 grade; with
  # no class_var the class level is absent and everything shifts up by one).
  # Rows at or above the top level are bold; deeper rows are indented one step
  # (three spaces) per level below the top, both as visible label text and as
  # a `row_style` token consumed by `write_table()`. The internal class/term
  # and helper columns are dropped so only `label` + the data columns remain.
  top_bold <- if (has_class) 1L else 0L
  lvl      <- wide$level
  depth    <- pmax(0L, lvl - top_bold)

  lab <- as.character(wide$label)
  ind <- depth > 0L
  lab[ind] <- paste0(strrep("   ", depth[ind]), lab[ind])

  out <- data.frame(label = lab, stringsAsFactors = FALSE)
  for (cc in col_order) out[[cc]] <- as.character(wide[[cc]])

  # Top rows (any-event, class) are bold. When the vertical layout nests grade
  # sub-rows beneath each term, the term row also gets bolded so it stands out
  # as the header above its (indented) grade rows; the term stays indented too.
  bold_lvl <- which(lvl <= top_bold)
  if (vertical) {
    term_level <- if (has_class) 2L else 1L
    bold_lvl   <- union(bold_lvl, which(lvl == term_level))
  }

  format_table(out,
               bold   = sort(bold_lvl),
               indent = which(lvl >  top_bold))
}
