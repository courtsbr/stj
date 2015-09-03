#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Arrumar nomes
#'
#' @export
arrumar_nomes <- function(x) {
  nm <- names(x)
  names(x) <- gsub("\\(a\\)", "", gsub(" +", "_", tolower(desacentuar(nm))))
  x
}

#' Desacentuar
#'
#' @export
desacentuar <- function (x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}

#' @export
post_chrome <- function(x) {
  x <- unlist(strsplit(x, '\n'))
  x_split <- stringr::str_split_fixed(x, "\\:", 2)
  x_unite <- sprintf("'%s'='%s'", x_split[, 1], x_split[, 2])
  x_unite <- paste(x_unite, collapse = ",\n")
  x_unite <- paste0("list(", x_unite, ")")
  cat(x_unite)
  invisible(x_unite)
}
