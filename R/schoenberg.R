#' Generate a 12-tone (dodecaphonic) serialist matrix using Arnold Schoenberg's technique.
#'
#' @param prime0 Optional vector of notes or numeric note indices to use in forming the matrix.
#' If the vector is numeric, the values must span from 0 - 11 (unless \code{lead_tone} is specified, note 0 will be treated as "C").
#' @param lead_tone Name of the note to use as the leading tone of the matrix.
#' @param use_sharps Logical scalar that determines whether accidentals should be represented as sharps (\code{TRUE}; default) or flats (\code{FALSE}).
#' @param seed Optional seed value to use in generating random matrices. Set this to a numeric value when matrices need to be reproducible.
#'
#' @return A 12-tone matrix of the "schoenberg" class.
#' @export
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
#' # For illustration, let's create two equivalent vectors of note information...
#' # one vector with note labels:
#' prime01 <- c("F#", "F", "D", "E", "D#", "C", "A", "C#", "G#", "B", "A#", "G")
#' # and another vector with numeric indices of notes:
#' prime02 <- c(6, 5, 2, 4, 3, 0, 9, 1, 8, 11, 10, 7)
#'
#'
#' # Now, let's generate a 12-tone matrix from our note-based vector:
#' schoenberg(prime0 = prime01)
#'
#' # And let's generate a matrix from our number-based vector:
#' schoenberg(prime0 = prime02)
#'
#' # These two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01) == schoenberg(prime0 = prime02))
#'
#'
#' # schoenberg() uses sharp notation by default, but matrices can be generated
#' # with flat notation by setting use_sharps to FALSE:
#' schoenberg(prime0 = prime01, use_sharps = FALSE)
#' schoenberg(prime0 = prime02, use_sharps = FALSE)
#'
#' # As before, these two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01, use_sharps = FALSE) ==
#'          schoenberg(prime0 = prime02, use_sharps = FALSE))
#'
#'
#' # We can also manipulate the output of the schoenberg() function
#' # so that the leading tone of the matrix is a particular note.
#' # This works with either note-based or number-based input vectors:
#' schoenberg(prime0 = prime01, lead_tone = "C")
#' schoenberg(prime0 = prime02, lead_tone = "C")
#'
#' # And, as before, these two approaches produce identical outputs:
#' all(schoenberg(prime0 = prime01, lead_tone = "C") ==
#'          schoenberg(prime0 = prime02, lead_tone = "C"))
schoenberg <- function(prime0 = NULL, lead_tone = NULL, use_sharps = TRUE, seed = NULL){
     if(!is.logical(use_sharps))
          stop("'use_sharps' must be logical", call. = FALSE)

     if(length(use_sharps) > 1){
          use_sharps <- use_sharps[1]
          warning("'use_sharps' must be a scalar - only the first value was used", call. = FALSE)
     }

     set.seed(seed)
     if(is.null(prime0)) prime0 <- sample(0:11, 12)

     if(!is.numeric(prime0)){
          prime0 <- as.character(prime0)

          note_vec <- c("B#", "C",
                        "C#", "Db",
                        "D",
                        "D#", "Eb",
                        "E", "Fb",
                        "F", "E#",
                        "F#", "Gb",
                        "G",
                        "G#", "Ab",
                        "A",
                        "A#", "Bb",
                        "B", "Cb")

          if(!all(prime0 %in% note_vec))
               stop("Notes in 'prime0' are limited to the following notation: \n", paste(note_vec, collapse = ", "), call. = FALSE)

          prime0[prime0 == "B#"] <- "0"
          prime0[prime0 == "C"] <- "0"

          prime0[prime0 == "C#"] <- "1"
          prime0[prime0 == "Db"] <- "1"

          prime0[prime0 == "D"]  <- "2"

          prime0[prime0 == "D#"] <- "3"
          prime0[prime0 == "Eb"] <- "3"

          prime0[prime0 == "E"]  <- "4"
          prime0[prime0 == "Fb"]  <- "4"

          prime0[prime0 == "E#"]  <- "5"
          prime0[prime0 == "F"]  <- "5"

          prime0[prime0 == "F#"] <- "6"
          prime0[prime0 == "Gb"] <- "6"

          prime0[prime0 == "G"]  <- "7"

          prime0[prime0 == "G#"] <- "8"
          prime0[prime0 == "Ab"] <- "8"

          prime0[prime0 == "A"]  <- "9"

          prime0[prime0 == "A#"] <- "10"
          prime0[prime0 == "Bb"] <- "10"

          prime0[prime0 == "B"]  <- "11"
          prime0[prime0 == "Cb"]  <- "11"

          prime0 <- as.numeric(prime0)
          .key <- NULL
     }

     if(!all(prime0 <= 11 & prime0 >= 0))
          stop("Values in 'prime0' must range from 0 to 11", call. = FALSE)

     if(!all(round(prime0) == zapsmall(prime0)))
          stop("Values in 'prime0' must be integers", call. = FALSE)

     if(any(duplicated(prime0)))
          stop("Notes in 'prime0' must be unique", call. = FALSE)

     if(!is.null(lead_tone)){
          if(lead_tone == "C") .key <- 0
          if(lead_tone == "C#") .key <- 1
          if(lead_tone == "D") .key <- 2
          if(lead_tone == "D#") .key <- 3
          if(lead_tone == "E") .key <- 4
          if(lead_tone == "F") .key <- 5
          if(lead_tone == "F#") .key <- 6
          if(lead_tone == "G") .key <- 7
          if(lead_tone == "G#") .key <- 8
          if(lead_tone == "A") .key <- 9
          if(lead_tone == "A#") .key <- 10
          if(lead_tone == "B") .key <- 11

          if(lead_tone == "B#") .key <- 0
          if(lead_tone == "C") .key <- 0

          if(lead_tone == "C#") .key <- 1
          if(lead_tone == "Db") .key <- 1

          if(lead_tone == "D") .key  <- 2

          if(lead_tone == "D#") .key <- 3
          if(lead_tone == "Eb") .key <- 3

          if(lead_tone == "E") .key  <- 4
          if(lead_tone == "Fb") .key  <- 4

          if(lead_tone == "E#") .key  <- 5
          if(lead_tone == "F") .key  <- 5

          if(lead_tone == "F#") .key <- 6
          if(lead_tone == "Gb") .key <- 6

          if(lead_tone == "G") .key  <- 7

          if(lead_tone == "G#") .key <- 8
          if(lead_tone == "Ab") .key <- 8

          if(lead_tone == "A") .key  <- 9

          if(lead_tone == "A#") .key <- 10
          if(lead_tone == "Bb") .key <- 10

          if(lead_tone == "B") .key  <- 11
          if(lead_tone == "Cb") .key  <- 11
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

     if(use_sharps){
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
     }else{
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
     }

     out <- data.frame(out)
     class(out) <- c("schoenberg", "data.frame")
     out
}



#' Re-express a "schoenberg" class object with a different leading tone or accidental.
#'
#' @param tone_mat Object of the class "schoenberg" produced by the \code{schoenberg()} function.
#' @param lead_tone Name of the note to use as the leading tone of the matrix.
#' @param use_sharps Logical scalar that determines whether accidentals should be represented as sharps (\code{TRUE}; default) or flats (\code{FALSE}).
#'
#' @return A 12-tone matrix of the "schoenberg" class.
#' @export
#'
#' @examples
#' ## Let's create a vector of notes to use in creating our inital 'tone_mat' matrix:
#' prime01 <- c("F#", "F", "D", "E", "D#", "C", "A", "C#", "G#", "B", "A#", "G")
#' tone_mat <- schoenberg(prime0 = prime01)
#'
#' # Now, let's change the leading tone to "C":
#' rekey(tone_mat = tone_mat, lead_tone = "C")
#'
#' # And let's also change the accidentals to flats:
#' rekey(tone_mat = tone_mat, lead_tone = "C", use_sharps = FALSE)
rekey <- function(tone_mat, lead_tone = NULL, use_sharps = TRUE){
     if(!("schoenberg" %in% class(tone_mat) ))
          stop("'tone_mat' must be of class 'schoenberg'", call. = FALSE)
     schoenberg(prime0 = as.matrix(tone_mat)[1,], lead_tone = lead_tone, use_sharps = use_sharps)
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
