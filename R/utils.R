#'Returns an object suitable for the Datasets argument of the bulk export endpoints
#'@export
Datasets <- function(Identifier, Calculation = NULL, Unit = NULL){
  if (!is.null(Calculation)){
    choices <- c("Instantaneous", "Maximum", "Minimum", "Aggregate",
      "Integrate", "Differentiate")
    Calculation <- match.arg(Calculation, choices, several.ok = TRUE)
  }
  tibble(Identifier, Calculation, Unit)
}

process_pars <- function(...){
  pars <- list2(...)
  pars <- Filter(\(x) !is.null(x), pars)
  pars <- pars_flatten(pars)
  pars
}

pars_flatten <- function(x){
  lx <- sapply(x, length)
  i  <- lx > 1
  nms <- names(x)
  x[i] <- lapply(x[i], as.list)
  setNames(
    object = c(x, recursive = TRUE),
    nm     = rep(nms, lx)
  )
}
