#' Find how many times a player has won an award
#'
#' @param award Type of award (MVP, DPOY, ROY, SMOY, MIP) (quoted)
#' @param max Set max = T to only output the player(s) with the most of the specific award
#' @param start The season to start at
#' @param end The season to end at
#' @import tidyverse
#' @import rlang
#' @return A data frame with award outcomes
#' @export

award <- function(award, max = F, start = NULL, end = NULL){
  # read in data based on award
  dat <- read_award(award)

  # edit data for start and end
  dat <- edit_dat(dat,start,end)

  tb <- table(dat$Player)
  df <- as.data.frame(tb)
  df <- df[order(df$Freq, decreasing = T),]
  names(df) <- c("Player", "Times Won")

  if(max == T){
    val <- max(df$`Times Won`)
    df <- df[df$`Times Won` == val,]
  }
  return(df)
}



#' Find out which award winner averaged the most of a statistic
#'
#' @param award The award you want to look at (quoted)
#' @param stat The stat you want to look at (quoted)
#' @param top Enter a number (x) to show the top x players in terms of the stat
#' @param bot Enter a number (y) to show the bottom y players in terms of the stat
#' @param start The season to start at
#' @param end The season to end at
#' @import tidyverse
#'
#' @return a data frame with the stat breakdown
#' @export


player_stats <- function(award, stat, top = NULL, bot = NULL, start = NULL, end = NULL){
  # read in award
  dat <- read_award(award)

  # edit data for start and end
  dat <- edit_dat(dat,start,end)

  # create new data frame with the players and the stat
  c1 <- dat$Season
  c2 <- dat$Player
  c3 <- dat[stat]
  df <- cbind(c1,c2,c3) %>% data.frame()
  names(df) <- c("Season", "Player", stat)
  df <- df[order(-df[,3]),]

  # max and min options
  if(!is.null(top)){
    df <- df[1:top,]
  }
  else if(!is.null(bot)){
    df <- df[(nrow(df) - bot + 1):nrow(df),]
  }

  return(df)
}



#' Find out which player has won the most of multiple awards combined
#'
#' @param awards A vector of awards (quoted)
#' @param top  Enter a number (x) to show the top x players in terms of awards won
#' @param start Season to start at
#' @param end Season to end at
#' @import tidyverse
#' @return A data frame with players and times they won


awards <- function(awards, top, start, end){
  # create first data frame
    dat <- read_award(awards[1])
    dat <- edit_dat(dat,start,end)
    tb <- table(dat$Player)
    df1 <- as.data.frame(tb)
    names(df1) <- c("Player", "Times Won")

  # loop through and merge data frames
    for(i in 2:length(awards)){
      dat <- read_award(awards[i])
      dat <- edit_dat(dat,start,end)
      tb <- table(dat$Player)
      df <- as.data.frame(tb)
      names(df) <- c("Player", "Times Won")

      df1 <- merge(df1, df, by = 'Player', all = T)
    }

  # add up columns
    df1[,2] <- apply(df1[,2:ncol(df1)],1,sum)
    df1 <- df1[,1:2]
    df1 <- df1[order(-df1[,2]),]
    names(df1) <- c("Player", "Awards Won")

    if(!is.null(top)){
      df1 <- df1[1:top,]
    }

    return(df1)

}
