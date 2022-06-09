#' Find how many times a player has won an award
#'
#' @param award Type of award (MVP, DPOY, ROY, SMOY, MIP) (quoted)
#' @param max Set max = T to only output the player(s) with the most of the speicific award
#' @param start The season to start at
#' @param end The season to end at
#' @import tidyverse
#' @import rlang
#' @return A data frame with award outcomes
#' @export

player_freq <- function(award, max = F, start = NULL, end = NULL){
  # read in data based on award
  dat <- read_award(award)
  # filter data if there is start and end year
  s <- 1
  e <- nrow(dat)
  if(!is.null(start)){
    s <- 2022 - start
  }
  if(!is.null(end)){
    e <- 2022 - end
  }
  dat <- dat[e:s,]

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
  # manipulate data for certain seasons
  s <- 1
  e <- nrow(dat)
  if(!is.null(start)){
    s <- 2022 - start
  }
  if(!is.null(end)){
    e <- 2022 - end
  }
  dat <- dat[e:s,]

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
