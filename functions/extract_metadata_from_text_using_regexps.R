#' extract metadata from text using regexps
#' @param text text 
#' @param regexps vector of regexps; first match to a regexp will be used --> input of regexps order matters
#' @return character containing the found metadata
#' @examples
#' extract_metadata_from_text_using_regexps(text = "Die Fördersumme liegt bei 10 Millionen €.", regexps<-c("[0-9]+[Millionen]?(DM|€)"))
extract_metadata_from_text_using_regexps <- function(text, regexps){
  meta<-NULL
  for( regexp in regexps){
    meta<-c(meta,stringr::str_extract(string = text, pattern = stringr::regex(regexp)))
    if(all(is.na(meta))){
      meta<-NULL
    }
    if(length(meta)==1){
      break
    }
  }
  return(meta)
}