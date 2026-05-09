#' Get or set cctu package options
#'
#' Centralised setter / getter for the package's tunables (output directory,
#' default rendering, p-value digits, etc). Until now these have been read at
#' each call site via \code{getOption("cctu_<name>", default = <D>)}, with the
#' default literal duplicated in every formal-argument list. \code{cctu_options()}
#' replaces those scattered defaults with one table.
#'
#' \strong{Precedence.} \code{\link[cctu]{cctu_opt}}\code{(name)} resolves a
#' value by checking, in order: (1) the base R option \code{cctu_<name>} if
#' set via \code{base::options()}; (2) the value stashed by \code{cctu_options()}
#' in the package's internal environment; (3) the packaged default in
#' \code{.cctu_default_opts}. Setting an option via \code{base::options()}
#' therefore still wins, which preserves the existing workflow for users who
#' set things in \code{.Rprofile}.
#'
#' @section Available options:
#' \describe{
#'   \item{\code{digits}}{Significant digits for numeric summaries (default 3).}
#'   \item{\code{digits_pct}}{Decimal places for percentages (default 0).}
#'   \item{\code{subjid_string}}{Column name identifying a subject (default \code{"subjid"}).}
#'    \item{\code{print_plot}}{Whether \code{cttab()} prints a summary plot (default \code{TRUE}).}
#'   \item{\code{nest}}{How to nest variables in the output (default \code{"split"}) of \code{cttab()}.}
#'   \item{\code{render_num}}{Numeric statistic spec used by \code{cttab()} (default \code{"Median [Min, Max]"}).}
#'   \item{\code{blinded}}{Suppress the grouping variable in \code{cttab()} output (default \code{FALSE}).}
#'   \item{\code{output}}{Top-level output directory (default \code{"Output"}).}
#'   \item{\code{p_digits}}{Digits for p-values (default 4).}
#'   \item{\code{rm_empty}}{\code{apply_macro_dict()}'s empty-row/column policy (default \code{"both"}).}
#'   \item{\code{na_to_empty}}{Render \code{NA} as empty in \code{write_table()} (default \code{FALSE}).}
#'   \item{\code{source_local}}{\code{cctu::source()} default for \code{local} (default \code{FALSE}).}
#'   \item{\code{fig_format}}{Figure formats for \code{write_ggplot()} (default \code{c("png", "eps")}).}
#' }
#' @param ... Named values to set. Names must match the table above.
#' @param reset If \code{TRUE}, restore packaged defaults. Combined with named
#'   \code{...} arguments, the reset happens first and the new values are then
#'   applied on top.
#'
#' @return When called with no arguments and \code{reset = FALSE}, a named list
#'   of currently effective option values (after applying the precedence rule).
#'   When setting, returns the \emph{previous} values of the affected options
#'   invisibly, in the style of \code{base::options()}.
#'
#' @examples
#' old <- cctu_options(digits = 4, p_digits = 3)
#' cctu_options()$digits          # 4
#' cctu_options(reset = TRUE)     # restore packaged defaults
#' do.call(cctu_options, old)     # or restore manually
#'
#' @export
cctu_options <- function(..., reset = FALSE) {
  args <- list(...)
  has_args <- length(args) > 0L

  if (!has_args && !isTRUE(reset)) {
    nms <- names(.cctu_default_opts)
    return(setNames(lapply(nms, cctu_opt), nms))
  }

  if (isTRUE(reset)) {
    prev_reset <- cctu_env$options
    cctu_env$options <- .cctu_default_opts
    if (!has_args) return(invisible(prev_reset))
  }

  nms <- names(args)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("All arguments to `cctu_options()` must be named.", call. = FALSE)
  }
  unknown <- setdiff(nms, names(.cctu_default_opts))
  if (length(unknown)) {
    stop(
      "Unknown cctu option",
      if (length(unknown) > 1L) "s" else "", ": ",
      paste(unknown, collapse = ", "),
      ". Run `cctu_options()` to see the full list.",
      call. = FALSE
    )
  }

  shadowed <- vapply(nms,
    function(nm) !is.null(getOption(paste0("cctu_", nm))),
    logical(1L)
  )
  if (any(shadowed)) {
    warning(
      "Set via `cctu_options()` but also live in `base::options()` (which ",
      "takes precedence): ",
      paste(nms[shadowed], collapse = ", "),
      ". Clear with `options(cctu_<name> = NULL)` if you want ",
      "`cctu_options()` to take effect.",
      call. = FALSE
    )
  }

  prev <- setNames(lapply(nms, cctu_opt), nms)
  for (nm in nms) cctu_env$options[[nm]] <- args[[nm]]
  invisible(prev)
}


#' Internal accessor for a single cctu option
#'
#' Resolves \code{name} against, in order: \code{base::getOption("cctu_<name>")},
#' \code{cctu_env$options[[name]]}, then \code{.cctu_default_opts[[name]]}.
#' Used inside formal-argument defaults so call sites don't have to repeat the
#' packaged default literal.
#'
#' @param name Option name (without the \code{"cctu_"} prefix).
#' @return The resolved value, or an error if \code{name} is not a known option.
#' @keywords internal
#' @export
cctu_opt <- function(name) {
  if (!name %in% names(.cctu_default_opts)) {
    stop("`", name, "` is not a known cctu option.", call. = FALSE)
  }
  base_val <- getOption(paste0("cctu_", name))
  if (!is.null(base_val)) return(base_val)
  env_val <- cctu_env$options[[name]]
  if (!is.null(env_val)) return(env_val)
  .cctu_default_opts[[name]]
}


# Packaged defaults - single source of truth. Keep alphabetised to make
# diffs readable when someone adds a new option.
.cctu_default_opts <- list(
  blinded       = FALSE,
  digits        = 3,
  digits_pct    = 0,
  fig_format    = c("png", "eps"),
  na_to_empty   = FALSE,
  output        = "Output",
  p_digits      = 4,
  print_plot    = TRUE,
  nest          = "split",
  render_num    = "Median [Min, Max]",
  rm_empty      = "both",
  source_local  = FALSE,
  subjid_string = "subjid"
)


.onLoad <- function(libname, pkgname) {
  # cctu_env is created at top level in R/globalvariables.R. Initialise the
  # options bag here so downstream code can rely on it being present.
  cctu_env$options <- .cctu_default_opts
}
