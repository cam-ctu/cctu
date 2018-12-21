#' Function to convert text to propercase
#'
#' @param text a character vector
#' @return a character vector converted to "proper" case, where each word starts with a capital,
#' and all subsequent letters are lower caser
#'
#' @export



propercase = function(text){
  gsub("(\\w)([\\w|']*)", "\\U\\1\\L\\2", text, perl = TRUE)
}
