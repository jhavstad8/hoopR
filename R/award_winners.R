#' Graphs NBA award winners over time
#'
#' @param award Type of award (MVP, DPOY, ROY, SMOY, MIP)
#' @param stat What stat do you want to look at (PTS, TRB, AST, STL, BLK, FG%, 3P%, FT%, WS, MP)
#'
#' @import tidyverse
#' @import rvest
#' @import ggplot2
#' @import gganimate
#' @import rlang
#' @export


award_winners <- function(award,stat,start = NULL, end = NULL){
  # specify which dataset to work with
if(award == "MVP"){
  dat <- read_mvps(mvp)
}
else if(award == "DPOY"){
  dat <- read_dpoy(dpoy)
}
else if(award == "SMOY"){
    dat <- read_smoy(smoy)
}
else if(award == "ROY"){
  dat <- read_roys(roy)
}
else if(award == "MIP"){
  dat <- read_mip(mip)
}


  # filter data if there is start and end year
  s <- 1
  e <- nrow(dat)
  if(!is.null(start)){
    s <- 2022 - start
  }
  if(!is.null(end)){
    e <- 2022 - end
  }
 dat2 <- dat[e:s,]

 # create animated plot for the function to return
  anim <- dat2 %>%
    ggplot(aes_string(x="Season", y = stat)) +
    geom_line(colour = "red") +
    geom_point() +
    transition_reveal(Season) +
    xlab("Season") +
    ylab("{stat}") +
    ggtitle("{stat} for {award} Winner each Season") +
    theme_bw()

  return(anim)
}



