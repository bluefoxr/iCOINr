add_dset_name <- function(base_string, dset){
  paste0(base_string, " (", dset, ")")
}


#' Truncate a character vector
#'
#' For any entries in `x` that are longer than `maxtot`, individual words in
#' the string will be truncated to `maxword`, and the whole string will be
#' truncated to have total length `maxtot` or less. Also removes small words like
#' "in", "and", etc.
#'
#' @param x Character vector
#' @param maxtot Max total string length
#' @param maxword Max word length before truncation
#' @param at_end Suffix to indicate string has been truncated
#'
#' @return Truncated character vector
#' @export
#'
truncate_strings <- function(x, maxtot = 15, maxword = 4, at_end = ""){
  stopifnot(is.character(x))

  # find strings that need truncation
  need_trunc <- nchar(x) > maxtot
  xtrunc <- x[need_trunc]

  # truncate
  xtrunc <- sapply(xtrunc, truncate_string, maxword = maxword)

  # truncate overall length if still needed
  xtrunc[nchar(xtrunc) > maxtot] <-
    paste0(substr(xtrunc[nchar(xtrunc) > maxtot], 1, maxtot-nchar(at_end)), at_end)

  x[need_trunc] <- xtrunc

  x
}

truncate_string <- function(x, maxword = 4){
  stopifnot(is.character(x),
            length(x) == 1)

  xsp <- strsplit(x, " ") |>
    unlist()

  xsp[nchar(xsp) > maxword] <- paste0(substr(xsp[nchar(xsp) > maxword], 1, maxword), ".")

  # remove joining words
  xsp <- xsp[!(xsp %in% c("Of", "of", "And", "and", "The", "the"))]

  # put back to one string
  xsp <- paste0(xsp, collapse = " ")

  xsp
}
