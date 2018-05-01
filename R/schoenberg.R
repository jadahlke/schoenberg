#' Generate a 12-tone matrix using Arnold Schoenberg's serialism technique.
#'
#' @param prime0 \emph{Optional}: Vector of notes or numeric note indices to use in forming the matrix.
#' If the vector is numeric, the values must span from 0 - 11, where 0 is the lead tone (unless \code{tone0} is specified, note 0 will be treated as "C").
#' If supplying note names, use capital letters for the note names, use "#" to indicate sharps, and use "b" to indicate flats.
#' @param tone0 \emph{Optional}: Name of the note to use as the lead tone of the matrix.
#' @param accidentals \emph{Optional}: Character scalar that determines whether accidentals should be represented as sharps (\code{accidentals} = "sharps") or flats (\code{accidentals} = "flats"); default value is \code{NULL}.
#' \code{accidentals} can also be set to "integers" when one wishes to obtain a 12-tone matrix of numeric indices rather than notes.
#' When \code{accidentals} is \code{NULL}, matrices created from pre-specified vectors of notes will use the original set of accidentals, whereas
#' random matrices and matrices created from vectors of numeric indices will default to sharp notation.
#' @param seed \emph{Optional}: Seed value to use in generating random matrices. Set this to a numeric value when matrices need to be reproducible.
#'
#' @return A 12-tone matrix of the "schoenberg" class with prime series on the rows and inverted series on the columns.
#' @export
#'
#' @references
#' Schoenberg, A. (1923). \emph{Fünf klavierstücke [Five piano pieces], Op. 23, Movement 5: Walzer}.
#' Copenhagen, Denmark: Wilhelm Hansen.
#'
#' @examples
#' #### Generating Random 12-Tone Matrices ####
#' # The schoenberg() function can generate completely random 12-tone matrices:
#' schoenberg()
#'
#' # Or you can specify a seed value so that your matrices are reproducible:
#' schoenberg(seed = 42)
#'
#'
#' #### Generating 12-Tone Matrices From a Specified Vector of Notes ####
#' # For illustration, let's create two equivalent vectors of note information
#' # for Schoenberg's first 12-tone serialist work: Walzer from Opus 23.
#'
#' # First, let's create one vector with note labels:
#' prime01 <- c("C#", "A", "B", "G", "Ab", "F#", "A#", "D", "E", "Eb", "C", "F")
#'
#' # Next, let's create an equivalent vector using numeric indices instead of notes:
#' prime02 <- c(1, 9, 11, 7, 8, 6, 10, 2, 4, 3, 0, 5)
#'
#'
#' # Now, let's generate a 12-tone matrix from our note-based vector:
#' schoenberg(prime0 = prime01)
#'
#' # And let's generate a matrix from our number-based vector:
#' schoenberg(prime0 = prime02)
#'
#' # Schoenberg used a mix of sharps and flats in his notation, wich lost in translation with the
#' # numeric-index approach. Let's re-create our note-based matrix using only sharps:
#' schoenberg(prime0 = prime01, accidentals = "sharps")
#'
#' # These two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01, accidentals = "sharps") == schoenberg(prime0 = prime02))
#'
#'
#' # Matrices can also be generated with flat notation by setting accidentals to "flats":
#' schoenberg(prime0 = prime01, accidentals = "flats")
#' schoenberg(prime0 = prime02, accidentals = "flats")
#'
#' # As before, these two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01, accidentals = "flats") ==
#'          schoenberg(prime0 = prime02, accidentals = "flats"))
#'
#'
#' # We can also manipulate the output of the schoenberg() function
#' # so that the lead tone of the matrix is a particular note.
#' # This works with either note-based or number-based input vectors:
#' schoenberg(prime0 = prime01, tone0 = "C", accidentals = "sharps")
#' schoenberg(prime0 = prime02, tone0 = "C")
#'
#' # And, as before, these two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01, tone0 = "C", accidentals = "sharps") ==
#'          schoenberg(prime0 = prime02, tone0 = "C"))
schoenberg <- function(prime0 = NULL, tone0 = NULL, accidentals = NULL, seed = NULL){
     if(!is.null(accidentals)){
          if(!is.character(accidentals))
               stop("When 'accidentals' is not NULL, it must be a character scalar", call. = FALSE)

          if(length(accidentals) > 1){
               accidentals <- accidentals[1]
               warning("When 'accidentals' is not NULL, it must be a scalar - only the first value was used", call. = FALSE)
          }

          if(!(accidentals %in% c("sharps", "flats", "integers")))
               stop("When 'accidentals' is not NULL, it must be 'sharps', 'flats', or 'integers'")

     }

     set.seed(seed)
     if(is.null(prime0)) prime0 <- sample(0:11, 12)

     note_list <- list("0" = c("B#", "C", "Dbb"),
                       "1" = c("C#", "Db"),
                       "2" = c("D", "C##", "Ebb"),
                       "3" = c("D#", "Eb"),
                       "4" = c("E", "Fb", "D##"),
                       "5" = c("F", "E#", "Gbb"),
                       "6" = c("F#", "Gb"),
                       "7" = c("G", "F##", "Abb"),
                       "8" = c("G#", "Ab"),
                       "9" = c("A", "G##", "Bbb"),
                       "10" = c("A#", "Bb"),
                       "11" = c("B", "Cb"))
     note_vec <- unlist(note_list)

     if(!is.numeric(prime0)){
          .prime0 <- prime0 <- as.character(prime0)

          if(!all(prime0 %in% note_vec))
               stop("Notes in 'prime0' are limited to the following notation: \n", paste(note_vec, collapse = ", "), call. = FALSE)

          for(i in as.character(0:11)) prime0[prime0 %in% note_list[[i]]] <- i

          prime0 <- as.numeric(prime0)
          .key <- NULL
     }else{
          if(is.null(accidentals))
               accidentals <- "sharps"
     }

     if(!all(prime0 <= 11 & prime0 >= 0))
          stop("Values in 'prime0' must range from 0 to 11", call. = FALSE)

     if(!all(round(prime0) == zapsmall(prime0)))
          stop("Values in 'prime0' must be integers", call. = FALSE)

     if(any(duplicated(prime0)))
          stop("Notes in 'prime0' must be unique", call. = FALSE)

     if(!is.null(tone0)){
          .key <- as.numeric(names(which(unlist(lapply(note_list, function(x) tone0 %in% x)))))
     }else{
          .key <- NULL
     }

     key <- prime0[1]
     prime0 <- prime0 - key
     prime0[prime0 < 0] <- prime0[prime0 < 0] + 12

     if(!is.null(.key)) key <- .key

     inv <- prime0[1]
     for(i in 2:12){
          add <- prime0[i - 1] - prime0[i]
          inv[i] <- inv[i - 1] + add
     }
     inv[inv < 0] <- inv[inv < 0] + 12

     mat <- matrix(NA, 12, 12)
     mat[1,] <- prime0
     mat[,1] <- inv

     colnames(mat) <- paste0("I", prime0)
     rownames(mat) <- paste0("P", inv)

     for(i in 2:12){
          mat[-1,2:12] <- matrix(mat[-1,1], 11, 11) + matrix(mat[1,-1], 11, 11, byrow = T)
          mat[mat > 11] <- mat[mat > 11] - 12
     }

     out <- mat + key
     out[out > 11] <- out[out > 11] - 12
     out[1:length(out)] <- as.character(out)

     if(is.null(accidentals)){
          for(i in as.character(0:11))
               out[out == i] <- .prime0[.prime0 %in% note_list[[i]]]
     }else if(accidentals == "flats"){
          out[out == "0"] <- "C"
          out[out == "1"] <- "Db"
          out[out == "2"] <- "D"
          out[out == "3"] <- "Eb"
          out[out == "4"] <- "E"
          out[out == "5"] <- "F"
          out[out == "6"] <- "Gb"
          out[out == "7"] <- "G"
          out[out == "8"] <- "Ab"
          out[out == "9"] <- "A"
          out[out == "10"] <- "Bb"
          out[out == "11"] <- "B"
     }else if(accidentals == "sharps"){
          out[out == "0"] <- "C"
          out[out == "1"] <- "C#"
          out[out == "2"] <- "D"
          out[out == "3"] <- "D#"
          out[out == "4"] <- "E"
          out[out == "5"] <- "F"
          out[out == "6"] <- "F#"
          out[out == "7"] <- "G"
          out[out == "8"] <- "G#"
          out[out == "9"] <- "A"
          out[out == "10"] <- "A#"
          out[out == "11"] <- "B"
     }

     out <- data.frame(out)
     class(out) <- c("schoenberg", "data.frame")
     out
}



#' Re-express a "schoenberg" class object with a different lead tone or different notation of accidentals.
#'
#' @param tone_mat Object of the class "schoenberg" produced by the \code{schoenberg()} function.
#' @param tone0 \emph{Optional}: Name of the note to use as the lead tone of the matrix.
#' @param accidentals \emph{Optional}: Character scalar that determines whether accidentals should be represented as sharps (\code{accidentals} = "sharps") or flats (\code{accidentals} = "flats"); default value is \code{NULL}.
#' \code{accidentals} can also be set to "integers" when one wishes to obtain a 12-tone matrix of numeric indices rather than notes.
#' When \code{accidentals} is \code{NULL}, matrices created from pre-specified vectors of notes will use the original set of accidentals, whereas
#' random matrices and matrices created from vectors of numeric indices will default to sharp notation.
#'
#' @return A 12-tone matrix of the "schoenberg" class with prime series on the rows and inverted series on the columns.
#' @export
#'
#' @examples
#' # Let's create a vector of notes to use in creating our inital 'tone_mat' matrix based
#' # on Schoenberg's Walzer from Opus 23
#' prime01 <- c("C#", "A", "B", "G", "Ab", "F#", "A#", "D", "E", "Eb", "C", "F")
#' tone_mat <- schoenberg(prime0 = prime01)
#'
#' # Now, let's change the lead tone to "C":
#' rekey(tone_mat = tone_mat, tone0 = "C")
#'
#' # And let's also change the accidentals to flats:
#' rekey(tone_mat = tone_mat, tone0 = "C", accidentals = "flats")
rekey <- function(tone_mat, tone0 = NULL, accidentals = NULL){
     if(!("schoenberg" %in% class(tone_mat) ))
          stop("'tone_mat' must be of class 'schoenberg'", call. = FALSE)
     schoenberg(prime0 = as.matrix(tone_mat)[1,], tone0 = tone0, accidentals = accidentals)
}


#' print method for objects of the "schoenberg" class.
#'
#' @param x Object to be printed.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Printed results from objects of the "schoenberg" class.
#' @export
print.schoenberg <- function(x, ...){
     print.data.frame(x)
}
