#' Basic day to day functions
#'
#' @param vec character() A character vector of unspecified length with many
#' delimiters.
#'
#' @param delim character(1) A character delimiter
#'
#' @param sel numeric() A numeric vector with the order which the output is to
#' be returned.
#'
#' @param out character(1) 'sep' means that the selected field should be returned
#' as a vector individually. 'coll' means that the selected fields should
#' collased in single string separated by the delimiter.
#'
#' @return character() A character vector per entry.
#'
#' @examples
#' thestrsplit('Hello_there_3', '-')
#' hi(c('01;Second;aadf', '02;Column;dasfweq', '03;Message;awfds'), ';', 2)
#'
#' @export
thestrsplit <- function(vec, delim, sel=NULL, outsep=NULL){
    if(is.null(sel))
        if(is.null(outsep)){
            out <- t(sapply(strsplit(vec, delim), function(x) x))
            if(nrow(out)>1){
                out
            }else{
                as.vector(out)
            }
        }
        else if(outsep=='delim'){  ## Collapsed option outputs only vectors
            sapply(strsplit(vec, delim), function(x) paste(x, collapse=delim))
        }else{
            sapply(strsplit(vec, delim), function(x) paste(x, collapse=outsep))
        }
    else{ ## Selected entries
        if(is.null(outsep)){
            out <- t(sapply(strsplit(vec, delim), function(x) x[sel]))
            if(nrow(out)>1)
                out
            else
                as.vector(out)
        }else if(outsep=='delim'){  ## Collapsed option outputs only vectors
            sapply(strsplit(vec, delim), function(x) paste(x[sel], collapse=delim))
        }else{
            sapply(strsplit(vec, delim), function(x) paste(x[sel], collapse=outsep))
        }
    }
}
