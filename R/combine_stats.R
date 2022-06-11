#' Combine stats and compare players
#'
#' @param award The award to compare players for (auoted)
#' @param stats Vector of stats to combine (quoted)
#' @param top Enter a number (x) to show the top x players in terms of the stat
#' @param bot Enter a number (y) to show the bottom y players in terms of the stat
#' @param start Season to start at
#' @param end Season to end at
#' @import tidyverse
#' @return A data frame with rankings of players
#' @examples add_stats("MIP", c("PTS", "TRB", "AST"), start = 1990)
#' @export


add_stats <- function(award, stats, top = NULL, bot = NULL, start = NULL, end = NULL){
  # pull data
  dat <- read_award(award)

  # edit data for start and end
  dat <- edit_dat(dat,start,end)

  # create data frame
  df <- data.frame(Season = dat$Season, Player = dat$Player)

  for(i in 1:length(stats)){
    df <- df %>% cbind(dat[,stats[i]])
  }

  # combine stats
  df[,3] <- apply(df[,3:ncol(df)], 1, sum)
  df <- df[,1:3]

  colnames(df) <- c("Season", "Player", "Stats")

  # order df
  df <- df[order(-df[,3]),]

  # top and bot options
  if(!is.null(top)){
    df <- df[1:top,]
  }
  else if(!is.null(bot)){
    df <- df[(nrow(df) - bot + 1):nrow(df),]
  }

  return(df)
}
