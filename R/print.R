#' @name print
#'
#' @title Print methods for \pkg{schoenberg}
#'
#' @description
#' Print methods for \pkg{schoenberg} output objects with classes exported from \pkg{schoenberg}.
#'
#' @param x Object to be printed (object is used to select a method).
#' @param ... Additional arguments.
NULL


#' @export
#' @keywords internal
#' @exportClass schoenberg
#' @method print schoenberg
print.schoenberg <- function(x, ...){
     rnames <- rownames(x)
     cnames <- colnames(x)

     rnames.retro <- gsub(x = rnames, pattern = "P", replacement = "R")
     cnames.retro <- paste0("R", cnames)

     x <- cbind(x, " " = rnames.retro)
     x <- data.frame(apply(x, 2, as.character), stringsAsFactors = FALSE)
     x <- rbind(x, " " = c(cnames.retro, ""))

     dimnames(x) <- list(c(rnames, " "), c(cnames, " "))

     print.data.frame(x)
}
