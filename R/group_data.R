#' Create Group Header Rows in a data.table
#'
#' @description
#' Transforms a \code{data.table} by inserting a new row above each group. 
#' By default, other variables in this header row are \code{NA}. If \code{shift} 
#' is TRUE, the grouping variable label is moved into the first non-grouping 
#' column and the original grouping columns are removed.
#'
#' @param data A \code{data.table}. If a \code{data.frame} is provided, it will be converted.
#' @param groups A character vector of column names to group by.
#' @param shift Logical. If \code{TRUE}, moves the value of the first grouping 
#'   variable into the first non-grouping column and drops the original group columns.
#' @param indent Logical. If \code{TRUE} (and \code{shift} is \code{TRUE}), 
#'   adds three leading spaces to the first non-grouping column in the data rows.
#'
#' @return A \code{data.table} with additional rows for each group.
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
group_data <- function(data, groups, shift = FALSE, indent = FALSE) {

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
      
      # Ensure other non-grouping variables are NA in headers
      remaining_vars <- setdiff(other_vars, target_col)
      for (col in remaining_vars) {
        set(h, j = col, value = NA)
      }
    } else {
      # Standard header: all non-grouping vars are NA
      for (col in other_vars) {
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
  attr(res, "is_header") <- res$is_header
  res[, is_header := NULL]
  res[, ._sort_idx := NULL]
  
  return(res[])
}
