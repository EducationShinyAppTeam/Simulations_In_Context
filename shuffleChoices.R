#' Shuffling Answer Choices
#'
#' @param choices A vector containing the potential choices (strings)
#' @param prompt Logical; should a 'Select an answer' option be listed first. Will add an option
#'
#' @return A vector containing the shuffled choices
shuffleChoices <- function(choices, prompt = TRUE) {
  set.seed(NULL)
  choices <- sample(x = choices, size = length(choices), replace = FALSE)

  if (prompt) {
    choices <- c("Select an answer", choices)
  }

  return(choices)
}
#'