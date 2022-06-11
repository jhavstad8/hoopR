#' Compares teams in terms of number of awards won
#'
#' @param award The award to compare
#' @param top Enter a number (x) to show the top x players in terms of the stat
#' @param bot Enter a number (y) to show the bottom y players in terms of the stat
#' @param start The season to start at
#' @param end The season to end at
#' @param plot Set plot = T to plot results
#' @import tidyverse
#' @import ggplot2
#' @return a data frame with the number of awards for each team
#' @examples teams("SMOY", top = 5, end=2000)
#' @export

teams <- function(award, top = NULL, bot = NULL, start = NULL, end = NULL, plot = F){
  # read in data based on award
  dat <- read_award(award)

  # edit data for start and end
  dat <- edit_dat(dat,start,end)

  tb <- table(dat$Tm)
  df <- as.data.frame(tb)
  df <- df[order(df$Freq, decreasing = T),]
  names(df) <- c("Team", "Awards")

  # top and bot options
  if(!is.null(top)){
    df <- df[1:top,]
  }
  else if(!is.null(bot)){
    df <- df[(nrow(df) - bot + 1):nrow(df),]
  }

  if(plot == T){
    df <- ggplot(df) +
      geom_col(aes(x=Team, y = Awards, fill = Team)) +
      xlab("Team") +
      ylab("Awards Won") +
      ggtitle(paste("Comparing Teams for the",award, "Award")) +
      theme_bw()
  }

  return(df)
}
