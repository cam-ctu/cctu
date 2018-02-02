#' function to provide the environment in which an object is located
#'
#' @param name character string the object name to be located
#' @param env the environment in which to search.
#'
#' @return an environment if the object is found, or NULL
#' @details  the search works upwards from the environment specified in env.


where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    warning(paste("Can't find", name))
    NULL

  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    env

  } else {
    # Recursive case
    where(name, parent.env(env))

  }
}
