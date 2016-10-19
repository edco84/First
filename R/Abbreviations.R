#' Abbreviated functions that usually take too much space.
#'
an <- function(x, ...) as.numeric(x, ...)
ac <- function(x, ...) as.character(x, ...)
ai <- function(x, ...) as.integer(x, ...)
p0 <- function(..., collapse=NULL) paste0(..., collapse=collapse)
p <- function(..., sep=' ', collapse=NULL) paste(..., sep=sep, collapse=collapse)
