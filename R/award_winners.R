#' Find how many times a player has won an award
#'
#' @param award Type of award (MVP, DPOY, ROY, SMOY, MIP) (quoted)
#' @param max Set max = T to only output the player(s) with the most of the speicific award
#' @param start The season to start at
#' @param end The season to end at
#' @import tidyverse
#' @import rvest
#' @import ggplot2
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
#' @param award The award you want to look at
#' @param stat The stat you want to look at
#' @param start The season to start at
#' @param end The season to end at
#' @import tidyverse
#'
#' @return a data frame with the stat breakdown
#' @export
