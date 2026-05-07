#' Create Nested Group Header Rows in a data.table
#'
#' @description
#' Transforms a \code{data.table} by inserting header rows for each level of
#' the specified grouping variables. The grouping variables are moved to the
#' front of the dataset. The nesting level is returned as an
#' \code{is_header} attribute (1 to N for headers, 0 for data rows).
#'
#' @param data A \code{data.table}. If a \code{data.frame} is provided, it will be converted.
#' @param groups A character vector of column names to group by (ordered high to low).
#' @param shift Logical. If \code{TRUE}, moves labels into the first non-grouping column.
#' @param indent Logical. If \code{TRUE}, indents nested headers and data rows.
#' @param keep_groups Logical. If \code{TRUE} (and \code{shift} is \code{TRUE}),
#'   preserves the original grouping columns in the output. Default is \code{FALSE}.
#' @param passthrough Character vector of column names whose values should be
#'   carried through unchanged on data rows. The header rows that
#'   \code{group_data} inserts get \code{NA} in these columns (their type is
#'   preserved from \code{data}). This is intended for downstream renderers
#'   (e.g. cttab's \code{Row_Style}) that want to attach per-row metadata
#'   that survives the nesting transformation.
#' @param skip_na_label Logical. If \code{TRUE} (default), header rows whose
#'   grouping value is \code{NA} are silently dropped rather than emitted.
#' @param header_label_fn A function \code{function(level_idx, value,
#'   group_name)} that returns the visible string for a header row. When
#'   \code{NULL} (default) the grouping value is used verbatim (possibly after
#'   indentation if \code{indent = TRUE}).
#' @param skip_header_fn A function \code{function(level_idx, value,
#'   group_name, sub_data)} returning \code{TRUE} to drop a header row. The
#'   \code{sub_data} argument is the slice of \code{data} that matches the
#'   group path leading up to this header, letting callers decide based on
#'   row-level metadata (e.g. suppress a Variable header when its sub-data is
#'   a single bold row). When \code{NULL} (default) no headers are dropped
#'   beyond what \code{skip_na_label} already removes.
#' @param preserve_attrs Logical. If \code{TRUE} (default), non-internal
#'   attributes of \code{data} (i.e. everything except \code{names},
#'   \code{class}, \code{row.names}, \code{.internal.selfref}, \code{sorted},
#'   and \code{index}) are copied to the output so that downstream consumers
#'   can read metadata attached to the original table.
#'
#' @return A \code{data.table} with nested header rows. The nesting level is
#'   stored in \code{attr(., "is_header")}.
#'
#' @import data.table
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(Region = c("North", "North", "South"),
#'                  Dept = c("Sales", "Sales", "HR"),
#'                  Staff = c("A", "B", "C"),
#'                  Sales = c(10, 20, 30))
#'
#' # Standard headers
#' group_data(dt, groups = "Region")
#'
#' # Shifted and indented headers for reporting
#' group_data(dt, groups = "Region", shift = TRUE, indent = TRUE)
#' # Shifted and indented headers for reporting with multiple grouping levels
#' group_data(dt, groups = c("Region", "Dept"), shift = TRUE, indent = TRUE)
#'
#' # Carry an extra metadata column (e.g. a per-row style flag) through to
#' # the final layout; header rows get NA in that column.
#' dt$style <- c("bold", "", "")
#' group_data(dt, groups = "Region", shift = TRUE, indent = TRUE,
#'            passthrough = "style")
#'
#' # Skip NA group values
#' dt2 <- data.table(G = c("A", NA, "B"), V = 1:3)
#' group_data(dt2, groups = "G", skip_na_label = TRUE)
#'
#' # Custom header label function
#' group_data(dt, groups = "Region",
#'            header_label_fn = function(lvl, val, grp) paste0("[", val, "]"))
#'
group_data <- function(data, groups, shift = FALSE, indent = FALSE,
                       keep_groups = FALSE, passthrough = character(),
                       skip_na_label = TRUE, header_label_fn = NULL,
                       skip_header_fn = NULL,
                       preserve_attrs = TRUE) {

  # Capture user-set attributes before any mutation
  .internal_attrs <- c("names", "class", "row.names", ".internal.selfref",
                       "sorted", "index")
  if (preserve_attrs) {
    input_attrs <- attributes(data)
    user_attrs  <- input_attrs[setdiff(names(input_attrs), .internal_attrs)]
  }

  # Shallow copy and ensure grouping variables are at the start
  temp_dt <- copy(as.data.table(data))

  # Ensure groups exist
  missing_cols <- setdiff(groups, names(temp_dt))
  if (length(missing_cols) > 0) {
    stop(paste("Grouping columns not found:", paste(missing_cols, collapse = ", ")))
  }

  # 1. Establish Column Order: Groups first, then everything else
  all_cols <- names(temp_dt)
  other_vars <- setdiff(all_cols, groups)

  if (length(other_vars) == 0) {
    stop("At least one non-grouping variable is required to hold data or shifted labels.")
  }

  final_col_order <- c(groups, other_vars)
  setcolorder(temp_dt, final_col_order)

  # 2. Create Headers for each nesting level
  header_list <- list()

  for (i in seq_along(groups)) {
    current_path <- groups[1:i]
    # Extract unique headers for this specific level
    h <- unique(temp_dt[, ..current_path])

    # Drop NA-valued headers when requested
    if (skip_na_label) {
      h <- h[!is.na(h[[groups[i]]]), ]
      if (nrow(h) == 0L) next
    }

    # Caller-driven header drop based on sub-data (lets cttab suppress the
    # Variable header for single-row bold variables, where the data row is
    # styled bold and stands in for the header).
    if (!is.null(skip_header_fn) && nrow(h) > 0L) {
      # Build the level-specific row index once, then look up each header's
      # subset by join; avoids re-scanning the full table per header row.
      idx_map <- temp_dt[, .(.rows = list(.I)), by = current_path]
      row_idx <- idx_map[h, on = current_path][[".rows"]]
      keep <- vapply(seq_len(nrow(h)), function(r) {
        rows <- row_idx[[r]]
        if (is.null(rows)) rows <- integer(0L)
        rows <- rows[!is.na(rows)]
        sub <- if (length(rows)) temp_dt[rows] else temp_dt[0L]
        !isTRUE(skip_header_fn(i, h[[groups[i]]][r], groups[i], sub))
      }, logical(1L))
      h <- h[keep, ]
      if (nrow(h) == 0L) next
    }

    # Internal sort index: headers are 1..N, data is N+1
    h[, ._sort_idx := i]

    # Indication attribute: 1 for top level, 2 for second, etc.
    h[, is_header := i]

    if (shift) {
      target_col <- other_vars[1]
      current_label_col <- groups[i]

      # Progressive indentation for headers
      prefix <- if (indent) strrep("   ", i - 1) else ""

      # Compute visible label
      raw_val <- as.character(h[[current_label_col]])
      if (!is.null(header_label_fn)) {
        label_val <- vapply(raw_val, function(v)
          header_label_fn(i, v, current_label_col), character(1L))
        label_val <- paste0(prefix, label_val)
      } else {
        label_val <- paste0(prefix, raw_val)
      }

      # Insert label into the target column
      set(h, j = target_col, value = label_val)

      # Ensure other non-grouping variables are NA in headers, but skip
      # passthrough columns (they pick up NA via rbindlist fill below,
      # preserving the source type).
      remaining_vars <- setdiff(other_vars, c(target_col, passthrough))
      for (col in remaining_vars) {
        set(h, j = col, value = NA)
      }
    } else {
      if (!is.null(header_label_fn)) {
        current_label_col <- groups[i]
        raw_val <- as.character(h[[current_label_col]])
        label_val <- vapply(raw_val, function(v)
          header_label_fn(i, v, current_label_col), character(1L))
        set(h, j = current_label_col, value = label_val)
      }
      # Standard header: all non-grouping vars are NA except passthrough
      for (col in setdiff(other_vars, passthrough)) {
        set(h, j = col, value = NA)
      }
    }
    header_list[[i]] <- h
  }

  # 3. Prepare Data Rows
  temp_dt[, ._sort_idx := length(groups) + 1]
  temp_dt[, is_header := 0] # Non-header rows

  if (shift) {
    target_col <- other_vars[1]
    if (indent) {
      # Data rows are indented further than the deepest header
      data_indent <- strrep("   ", length(groups))
      set(temp_dt, j = target_col, value = paste0(data_indent, as.character(temp_dt[[target_col]])))
    } else {
      set(temp_dt, j = target_col, value = as.character(temp_dt[[target_col]]))
    }
  }

  # 4. Combine and Interleave
  res <- rbindlist(c(header_list, list(temp_dt)), use.names = TRUE, fill = TRUE)

  # Sort by groups then by the internal index to keep hierarchy intact
  setorderv(res, c(groups, "._sort_idx"), na.last = FALSE)

  # 5. Final Cleanup
  res[, ._sort_idx := NULL]

  if (shift && !keep_groups) {
    # Remove grouping columns if shifting and user didn't request to keep them
    res[, (groups) := NULL]
    setcolorder(res, c(other_vars))
  } else {
    # Ensure groups are the first variables, then others, then is_header
    setcolorder(res, c(groups, other_vars))
  }

  attr(res, "is_header") <- res$is_header
  res[, is_header := NULL]

  # Re-attach user attributes (after all data.table mutation is done)
  if (preserve_attrs && length(user_attrs) > 0L) {
    for (nm in names(user_attrs)) {
      setattr(res, nm, user_attrs[[nm]])
    }
  }

  return(res[])
}
