#' Graphs NBA award winners over time
#'
#' @param award Type of award (MVP, DPOY, ROY, etc.)
#' @param stat What stat do you want to look at
#'
#' @import tidyverse
#' @import rvest
#' @export



read_mvps <- function(mvps){
mvpurl<- "https://www.basketball-reference.com/awards/mvp.html"
mvps <- mvpurl %>%
  read_html() %>%
  html_nodes(css = "#mvp_NBA") %>%
  html_table() %>%
  .[[1]] %>%
  as.data.frame()

names <- as.vector(mvps[1,])
colnames(mvps) <- names
mvps <- mvps[-1,]
return(mvps)
}
