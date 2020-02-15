#' apply clustering algorithms to detect outlier locations as possible false positive locations
#' 
#' @param locations_with_meta data.frame containing the 4 variables: name, frequency, lat, long
#' @return confidence for every location; either 1(if inside correct cluster) or 0(if inside wrong cluster)
#' @examples
#' apply_clustering_algorithm_on_locations_to_get_confidence_scores(names=c("Hamburg","Bremen","Lübeck","München"),frequencies=c(10,2,2,1),
#'                 lat=c(53.551086, 53.073635, 53.869720, 48.137154), lon=c(9.993682, 8.806422, 10.686389, 11.576124))
apply_clustering_algorithm_on_locations_to_get_confidence_scores <- function(names,frequencies,lat,lon){
  N<-length(names)
  # @confidence confidence scores for locations
  confidence<-rep(0,length(names))
  names(confidence)<-names
  if(N>0){
    # calculate centroid for all locations taking the frequencies in consideration
    centroid <-
      c(
        weighted.mean(x = as.numeric(lat), w = as.numeric(frequencies)),
        weighted.mean(x = as.numeric(lon), w = as.numeric(frequencies))
      )
    # calculate the distances to centroid for all locations;
    # divided by 1000 to get distance in kilometers
    distances_to_centroid <- matrix(c(0), N, 1)
    rownames(distances_to_centroid)<-names
    for (j in 1:N) {
      # distm needs input in form (lon, lat) --> rev(centroid)
      distances_to_centroid[j, 1] <-geosphere::distm(x=as.numeric(c(lon[j],lat[j])),y =  rev(centroid), fun = geosphere::distHaversine) /1000
    }
    # start clustering based on distances to centroid
    # ensure at least 3 different distances are present
    if(length(unique(distances_to_centroid))>2){
      # find correct number of clusters
      # start k means clustering with 2 centers
      currently_best_clustering_setting <- stats::kmeans(x = distances_to_centroid, centers = 2)
      # test k means with more than 2 clusters; if betweeness does not significantly (factor 1.15) increase stop for loop;
      # test with maximum number of 5 cluster centers
      if(nrow(distances_to_centroid)>3){
        for (k in 3:min(5,(nrow(distances_to_centroid)-1))) {
          custering_result <- stats::kmeans(x = distances_to_centroid,centers =  k)
          # if not significantly better stop
          if (custering_result$betweenss < (1.15 * currently_best_clustering_setting$betweenss)) {
            break
          }
          # else set this replace @currently_best_clustering_setting with custering_result
          else{
            currently_best_clustering_setting <- custering_result
          }
        }
      }
      # determine the correct cluster, as the one whos elements have the least distance to the centroid on average
      clusters <- currently_best_clustering_setting$cluster
      number_of_clusters <- length(unique(clusters))
      # set cluster #1 as the default correct cluster
      currently_best_cluster<-1
      avg_distance_for_currently_best_cluster <- mean(distances_to_centroid[which(clusters == 1)])
      for (k in 2:number_of_clusters) {
        # if mean distance of k'th cluster is better than currently best cluster: replace @currently_best_cluster with k and @avg_distance_for_currently_best_cluster with mean idtsance of elements in cluster k 
        if (avg_distance_for_currently_best_cluster > mean(distances_to_centroid[which(clusters == k)])) {
          currently_best_cluster = k
          avg_distance_for_currently_best_cluster <- mean(distances_to_centroid[which(clusters == k)])
        }
      }
      
      # after for loop @currently_best_cluster has the value of the correct cluster
      # all locations, that belong to this cluster get confidence value of 1(secure); all locations belonging to other clusters get value 0 (unsecure)
      confidence[which(clusters==currently_best_cluster)]<-1
    }
    # if no more than 2 different distnaces are present, set all to secure
    else{
      confidence[1:length(confidence)]<-1
    }
  }
  #if only a single location is present, set it to secure
  else{
    confidence[1:length(confidence)]<-1
  }
  return(confidence)
}