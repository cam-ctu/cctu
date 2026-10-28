#' Create Nested Group Header Rows in a data.table
#'
#' @description
#' Transforms a \code{data.table} by inserting header rows for each level of
#' the specified grouping variables. The grouping variables are moved to the
#' front of the dataset.
#'
#' @param data A \code{data.table}. If a \code{data.frame} is provided, it will be converted.
#' @param groups A character vector of column names to group by (ordered high to low).
#' @param shift_to Character. The name of the column into which group labels
#'   are shifted. When \code{NULL} (default) no shifting is performed.
#' @param indent Logical. If \code{TRUE}, indents nested headers and data rows.
#' @param carry Character vector of column names whose values should be carried
#'   into header rows rather than set to \code{NA}. Each named column must have
#'   a unique value within its group at every nesting level - an error is thrown
#'   if that is not the case.
#'
#' @return A \code{data.table} with nested header rows.
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
#' group_data(dt, groups = "Region", shift_to = "Staff", indent = TRUE)
#' # Shifted and indented headers for reporting with multiple grouping levels
#' group_data(dt, groups = c("Region", "Dept"), shift_to = "Staff", indent = TRUE)
#'
group_data <- function(data, groups, shift_to = NULL, indent = FALSE,
                       carry = NULL) {

  # data.table NSE sort/order helper columns (incl. the `i.`-prefixed join
  # column), bound locally so the object_usage linter / R CMD check don't treat
  # them as undefined globals.
  `._sort_idx` <- `._row_idx` <- `._first_occ` <- `i.._first_occ` <- NULL # nolint: object_name_linter.

  # Shallow copy and ensure grouping variables are at the start
  temp_dt <- copy(as.data.table(data))
  orig_attrs <- attributes(data)
  # `sorted` / `index` are data.table's internal key and secondary indices.
  # They describe the INPUT's row order and columns, both of which this
  # function changes (rows are rebuilt and reordered, group columns dropped),
  # so restoring them would leave the result claiming a key it does not have --
  # sometimes naming a column that no longer exists. dcast() returns a keyed
  # data.table, so cttab feeds keyed tables in here routinely.
  orig_attrs <- orig_attrs[!names(orig_attrs) %in%
                             c("names", "class", "row.names",
                               ".internal.selfref", "sorted", "index")]
  
  # Ensure groups exist
  missing_cols <- setdiff(groups, names(temp_dt))
  if (length(missing_cols) > 0) {
    stop(paste("Grouping columns not found:", paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(shift_to)) {
    if (!shift_to %in% names(temp_dt)) {
      stop(paste("shift_to column not found:", shift_to))
    }
    if (shift_to %in% groups) {
      stop("shift_to column cannot be a grouping column.")
    }
  }

  if (!is.null(carry)) {
    missing_carry <- setdiff(carry, names(temp_dt))
    if (length(missing_carry) > 0) {
      stop(paste("carry columns not found:", paste(missing_carry, collapse = ", ")))
    }
    for (i in seq_along(groups)) {
      current_path <- groups[1:i]
      for (col in carry) {
        n_unique <- temp_dt[, .(n = uniqueN(get(col))), by = current_path]
        bad <- n_unique[n > 1]
        if (nrow(bad) > 0) {
          stop(paste0(
            "carry column '", col, "' has non-unique values within group [",
            paste(current_path, collapse = ", "), "] - cannot carry through."
          ))
        }
      }
    }
  }
  
  # 1. Establish Column Order: Groups first, then everything else
  all_cols <- names(temp_dt)
  other_vars <- setdiff(all_cols, groups)
  
  if (length(other_vars) == 0) {
    stop("At least one non-grouping variable is required to hold data or shifted labels.")
  }
  
  final_col_order <- c(groups, other_vars)
  setcolorder(temp_dt, final_col_order)
  temp_dt[, ._row_idx := .I]
  
  # 2. Create Headers for each nesting level
  header_list <- list()
  
  for (i in seq_along(groups)) {
    current_path <- groups[1:i]
    # Extract unique headers with first-occurrence index to preserve original order
    h <- temp_dt[, .(._first_occ = min(._row_idx)), by = current_path]
    
    # Internal sort index: headers are 1..N, data is N+1
    h[, ._sort_idx := i]
    
    if (!is.null(shift_to)) {
      current_label_col <- groups[i]

      # Progressive indentation for headers
      prefix <- if (indent) strrep("   ", i - 1) else ""

      # Insert label into the target column
      set(h, j = shift_to, value = paste0(prefix, as.character(h[[current_label_col]])))

      remaining_vars <- setdiff(other_vars, c(shift_to, carry))
      for (col in remaining_vars) {
        set(h, j = col, value = NA)
      }
    } else {
      for (col in setdiff(other_vars, carry)) {
        set(h, j = col, value = NA)
      }
    }
    # Carry values: join unique value per group level onto header rows
    if (!is.null(carry)) {
      current_path <- groups[1:i]
      carry_vals <- unique(temp_dt[, c(current_path, carry), with = FALSE])
      h[carry_vals, (carry) := mget(paste0("i.", carry)), on = current_path]
    }
    header_list[[i]] <- h
  }
  
  # 3. Prepare Data Rows
  temp_dt[, ._sort_idx := length(groups) + 1]
  # First occurrence per group combination (for ordering headers and data consistently)
  group_first_occ <- temp_dt[, .(._first_occ = min(._row_idx)), by = groups]
  temp_dt[group_first_occ, ._first_occ := i.._first_occ, on = groups]
  
  if (!is.null(shift_to)) {
    if (indent) {
      # Data rows are indented further than the deepest header
      data_indent <- strrep("   ", length(groups))
      set(temp_dt, j = shift_to, value = paste0(data_indent, as.character(temp_dt[[shift_to]])))
    } else {
      set(temp_dt, j = shift_to, value = as.character(temp_dt[[shift_to]]))
    }
  }
  
  # 4. Combine and Interleave
  res <- rbindlist(c(header_list, list(temp_dt)), use.names = TRUE, fill = TRUE)
  
  # Sort by first occurrence (preserves original group order), then by level, then by row
  setorderv(res, c("._first_occ", "._sort_idx", "._row_idx"), na.last = FALSE)
  
  # 5. Final Cleanup
  res[, ._sort_idx := NULL]
  res[, ._row_idx := NULL]
  res[, ._first_occ := NULL]

  if (!is.null(shift_to)) {
    res[, (groups) := NULL]
    setcolorder(res, c(other_vars))
  } else {
    setcolorder(res, c(groups, other_vars))
  }

  for (a in names(orig_attrs)) attr(res, a) <- orig_attrs[[a]]
  
  return(res[])
}
