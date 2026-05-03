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
group_data <- function(data, groups, shift = FALSE, indent = FALSE,
                       keep_groups = FALSE, passthrough = character()) {

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
    
    # Internal sort index: headers are 1..N, data is N+1
    h[, ._sort_idx := i]
    
    # Indication attribute: 1 for top level, 2 for second, etc.
    h[, is_header := i]
    
    if (shift) {
      target_col <- other_vars[1]
      current_label_col <- groups[i]

      # Progressive indentation for headers
      prefix <- if (indent) strrep("   ", i - 1) else ""

      # Insert label into the target column
      set(h, j = target_col, value = paste0(prefix, as.character(h[[current_label_col]])))

      # Ensure other non-grouping variables are NA in headers, but skip
      # passthrough columns (they pick up NA via rbindlist fill below,
      # preserving the source type).
      remaining_vars <- setdiff(other_vars, c(target_col, passthrough))
      for (col in remaining_vars) {
        set(h, j = col, value = NA)
      }
    } else {
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
  
  return(res[])
}

