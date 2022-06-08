#' Graphs NBA award winners over time
#'
#' @param award Type of award (MVP, DPOY, ROY, etc.)
#' @param stat What stat do you want to look at (PTS, TRB, AST, STL, BLK, FG%, 3P%, FT%, WS, MP)
#'
#' @import tidyverse
#' @import rvest
#' @import ggplot2
#' @import gganimate
#' @export


award_winners <- function(award,stat){
if(award == "MVP"){
  dat <- read_mvps(mvp)
}


  anim <- dat %>%
    ggplot(aes(x=Season, y = stat)) +
    geom_point() +
    transition_time(Season) +
    shadow_mark() +
    xlab("Season") +
    ylab("{stat}")
}



read_mvps <- function(mvp){
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
mvps[,1] <- substr(mvps[,1],1,4)
mvps[,1] <- lapply(mvps[,1],as.numeric)
mvps[,5] <- lapply(mvps[,5],as.numeric)
mvps[,7:18] <- lapply(mvps[,7:18],as.numeric)
return(mvps)
}
