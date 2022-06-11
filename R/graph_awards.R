#' Graphs a stat of an NBA award's winners over time
#'
#' @param award Type of award (MVP, DPOY, ROY, SMOY, MIP) (quoted)
#' @param stat What stat do you want to look at -- PTS, TRB, AST, STL, BLK, FG%, 3P%, FT%, WS, MP (quoted)
#' @param start The season to start at
#' @param end The season to end at
#' @param anim If you wish to output an animated line plot, make anim = T
#' @import tidyverse
#' @import ggplot2
#' @import gganimate
#' @import rlang
#' @return An animated plot
#' @examples award_stat("MVP", "PTS", start = 2000)
#' @export


award_stat <- function(award,stat,start = NULL, end = NULL, anim = F){
  # specify which data set to work with
  dat <- read_award(award)

  # edit data for start and end
  dat <- edit_dat(dat,start,end)

 # create animated plot for the function to return
  if(anim == T){animation <- dat %>%
    ggplot(aes_string(x="Season", y = stat)) +
    geom_line(colour = "red") +
    geom_point() +
    transition_reveal(Season) +
    xlab("Season") +
    ylab("{stat}") +
    ggtitle("{stat} for {award} Winner each Season") +
    theme_bw()}
 else{
   animation <- dat %>%
     ggplot(aes_string(x="Season", y = stat)) +
     geom_line(colour = "red") +
     geom_point() +
     xlab("Season") +
     ylab(stat) +
     ggtitle(paste(stat, "for", award, "Winner each Season")) +
     theme_bw()
 }


  return(animation)
}




#' Compares multiple stats at a time for an award over time
#'
#' @param award The award you want to see
#' @param stats The stats you want to compare in a vector
#' @param start The season to start at
#' @param end The season to end at
#' @param anim If you want an animated plot, set anim = T
#' @import ggplot2
#' @import gganimate
#' @import reshape2
#' @import tidyverse
#'
#' @return an animated plot
#' @examples award_stats("DPOY", c("PTS", "AST"))
#' @export


award_stats <- function(award, stats, start = NULL, end = NULL, anim = F){

# pull data
  dat <- read_award(award)

# edit data for start and end
  dat <- edit_dat(dat,start,end)

# create new data frame to work with with the certain stats
  df <- data.frame(Season = dat$Season)

  for(i in 1:length(stats)){
    df <- df %>% cbind(dat[,stats[i]])
  }

  colnames(df) <- c("Season", stats)
  df <- melt(df, id.vars = 'Season', variable.name = 'Stats')

  # create plot with new data frame
  if(anim == T){
    p <- ggplot(df, aes(Season, value)) +
      geom_line(aes(colour = Stats)) +
      geom_point(aes(color = Stats)) +
      transition_reveal(Season) +
      ylab("Value") +
      ggtitle(paste("Comparing Stats for", award, "Winners Over Time")) +
      theme_bw()
  }
  else{p <- ggplot(df, aes(Season, value)) +
    geom_line(aes(colour = Stats)) +
    geom_point(aes(color = Stats)) +
    ylab("Value") +
    ggtitle(paste("Comparing Stats for", award, "Winners Over Time")) +
    theme_bw()
}
  return(p)
}
