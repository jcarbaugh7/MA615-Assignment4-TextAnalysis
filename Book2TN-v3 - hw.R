


library(stringr)

nDigitString <- function(num,digs){
  st <- as.character(num)
  zeros <- digs - str_length(st)
  for(i in 1:zeros){
    st <- paste0("0",st)
  }
  return(st)
}

#' Title Truenumbers from a book (vector of character strings)
#'
#' @param theBook, book structure as found in janeaustenR
#' @param subjectRoot, name of the book
#'
#' @description Because these gutenberg library texts have no explicit structure,
#' we make the assumption that lines in the array preceded and followed by two
#' blank lines are heading material, and that successive non-blank lines
#' constitute paragraphs separated by single blank lines.  Thus, content is a sequence 
#' of header lines and paragraphs of sentences. 
#' 
#' The position of each header or sentence in this sequence is called its _ordinal_.
#'  
#' Heading line TNs have subject: subjectRoot/heading:<heading-number>
#' 
#' Sentences have subject: subjectRoot/section:<heading-number>/paragraph:<number>/sentence:<number>
#' 
#' Since we don't know structurally whether headings denote chapters, volumes, etc.
#' we give each sentence a section number equal to the number of the last heading line
#' that preceded the sentence.
#' 
#' Sentences will have a TN for their, text, word count, and ordinal.  Headings will only have their
#' text and ordinal.
#'
#' @export
#'
tnBooksFromLines <- function(theBook, subjectRoot) {

  sentence <- ""    # this variable accumulates a sentence across line boundaries
  sentencenum <- 0  # keeps track of the sentence number within a paragraph
  ordinal <- 0      # the sequence number in the stream of headers and sentences
  headingnum <- 0   # keeps track of the last heading line number to use as the section of running text

  bklen <- length(theBook)
  tn <- list()
  for (j in 1:bklen) {  # loop through lines in theBook
    line <- theBook[j]
    
    if (line == "") {
      #new section or paragraph
      sentence <- ""
      sentencenum <- 1
      
    } else {
      # non-blank line - could be a heading or running text 
      # NOTE: hack in detecting headings, because by-lines don't have 2 blank lines before them
      
      if (j == 1
          || ((j < bklen-2 && j >2)
               && (theBook[j+1] == "" && theBook[j+2] == "" && theBook[j-1] == "" )
                    || (theBook[j-1] == "" && theBook[j-2] == "" && theBook[j+1] == ""))
          ) {
        # some kind of break in the flow of paragraphs
        headingnum <- headingnum+1  # increment the heading number
        tn[[length(tn) + 1]] <-
          tnum.makeObject(paste0(subjectRoot, "/heading:", nDigitString(headingnum,4)), "text", line)
        tn[[length(tn) + 1]] <-
          tnum.makeObject(paste0(subjectRoot, "/heading:", nDigitString(headingnum,4)), "ordinal", ordinal+1)
        
        ordinal <- ordinal + 1
        paragraph <- 0           # number paragraphs within sections
        
      } else {
        # running text
        if ((j > 1 && theBook[j-1] == "")){ #must be start of paragraph
          paragraph <- paragraph+1
          sentencenum <- 1
        }

        ln <- str_replace_all(line, "Mrs.", "Mrs") #HACK to make period only sentence delimiters
        ln <- str_replace_all(ln, "Mr.", "Mr")
        ln <- str_replace_all(ln, "Esq.", "Esq")
        
        # accumulate sentences across line boundaries, and make TNs for each
        if (stringr::str_detect(ln, "\\.|\\?")) {
          matchs <- stringr::str_locate_all(pattern = '\\.|\\?', ln)
          beg <- 0
          for (i in matchs[[1]][, 1]) {
            sentence <- paste0(sentence, substr(ln, beg, i), collapse = "")
            beg <- i
            subj <-
              paste0(
                tolower(subjectRoot),
                "/section:",
                nDigitString(headingnum,4),
                "/paragraph:",
                nDigitString(paragraph,4),
                "/sentence:",
                nDigitString(sentencenum,4),
                collapse = ''
              )
            sentence <- str_replace_all(sentence, "\"", "/iQ/") # fix quotes 
            
            # make 3 TNs
            tn[[length(tn) + 1]] <-
              tnum.makeObject(subj, "text", trimws(sentence))
            tn[[length(tn) + 1]] <-
              tnum.makeObject(subj, "ordinal", ordinal + 1)
            tn[[length(tn) + 1]] <-
              tnum.makeObject(subj,
                              "count:word",
                              stringr::str_count(trimws(sentence)) + 1)
            
            if (length(tn) >= 50) {
              res <- tnum.postObjects(tn)
              tn <- list()
            }
            sentencenum <- sentencenum + 1
            ordinal <- ordinal + 1
          }
          sentence <- paste0(substr(ln, beg + 1, nchar(ln)), " ")
        } else {
          sentence <- paste0(sentence, ln, " ", collapse = "")
        }
      }
    }
  }
}
