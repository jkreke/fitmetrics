##############################################################################################################################
#
# simple function to capitalize first letters of words for use in titles
#
#
#' Capitalize a string
#'
#' Capitalizes each word in a character string
#'
#' @param x a character string
#'
#' @return a character string
#'
#' @examples
#' cap1("this is a character string")
#'
#' @export
#' cap1()
cap1 <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s,1,1)), substring(s, 2),
          sep="", collapse=" ")
}
