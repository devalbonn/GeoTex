#' condense whitelist either by location name or coordiantes
#' @param whitelist whitelist data.frame with columns:location, frequency, lat, lon, class, type, importance, country, continent, confidence
#' @param mode ccharacter either "name" or "coords"
#' @return condensed whitelist
condense_whitelist <- function(whitelist,mode){
  if(mode=="name"){
    condensed_whitelist<-aggregate(as.numeric(whitelist[,"frequency"]),by=list(whitelist[,"location"]),FUN=sum)
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"lat"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"lon"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"class"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"type"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"importance"],by=list(whitelist[,"location"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"country"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"continent"],by=list(whitelist[,"location"]),FUN=function(x){return(x[1])})[,2])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"confidence"],by=list(whitelist[,"location"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})[,2])
  }
  if(mode=="coords"){
    # use most frequent location name with identical coordiantes
    condensed_whitelist<-aggregate((whitelist[,"location"]),by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(names(sort(table(as.character(x)),decreasing=T))[1])})[,3]
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(as.numeric(whitelist[,"frequency"]),by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=sum)[,c(3,1,2)])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"class"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"type"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"importance"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"country"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"continent"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"confidence"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})[,3])
  }
  colnames(condensed_whitelist)<-colnames(whitelist)
  return(condensed_whitelist)
}