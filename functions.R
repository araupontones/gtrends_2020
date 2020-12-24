
#' Function to download terms
#' @param term the term to search
#' @param geo location of the search

get_trend = function(term, geo, ...){
  
  trends = gtrends(
    keyword = term,
    geo = geo,
    time = "today 12-m",
    gprop = "web"
    
  )
  
  data_trends = trends$interest_over_time %>%
    mutate(hits = if_else(hits =="<1", 0, as.numeric(hits)),
           date = ymd(date),
           term = term)
  
  return(data_trends)
  
}