#' Pulls award data
#' @param award Which type of award you want data on (quoted)
#' @import rvest
#' @import tidyverse
#' @return a data frame of award data
#' @export


read_award <- function(award){
  if(award == "MVP"){
    mvpurl<- "https://www.basketball-reference.com/awards/mvp.html"
    dat <- mvpurl %>%
      read_html() %>%
      html_nodes(css = "#mvp_NBA") %>%
      html_table() %>%
      .[[1]] %>%
      as.data.frame()
  }
  else if(award == "ROY"){
    royurl<- "https://www.basketball-reference.com/awards/roy.html"
    dat <- royurl %>%
      read_html() %>%
      html_nodes(css = "#roy_NBA") %>%
      html_table() %>%
      .[[1]] %>%
      as.data.frame()
  }
  else if(award == "DPOY"){
    dpoyurl<- "https://www.basketball-reference.com/awards/dpoy.html"
    dat <- dpoyurl %>%
      read_html() %>%
      html_nodes(css = "#dpoy_NBA") %>%
      html_table() %>%
      .[[1]] %>%
      as.data.frame()
  }
  else if(award == "SMOY"){
    smoyurl<- "https://www.basketball-reference.com/awards/smoy.html"
    dat <- smoyurl %>%
      read_html() %>%
      html_nodes(css = "#smoy_NBA") %>%
      html_table() %>%
      .[[1]] %>%
      as.data.frame()
  }
  else if(award == "MIP"){
    mipurl<- "https://www.basketball-reference.com/awards/mip.html"
    dat <- mipurl %>%
      read_html() %>%
      html_nodes(css = "#mip_NBA") %>%
      html_table() %>%
      .[[1]] %>%
      as.data.frame()
  }
  names <- as.vector(dat[1,])
  colnames(dat) <- names
  dat <- dat[-1,]
  dat[,1] <- substr(dat[,1],1,4)
  dat[,1] <- as.numeric(dat[,1])
  dat[,5] <- as.numeric(dat[,5])
  dat[,7:18] <- lapply(dat[,7:18],as.numeric)
  dat[is.na(dat)] <- 0
  dat[,1] <- dat[,1] + 1
  return(dat)
}


#' Edits data based on season restrictions
#' @param dat the data frame to modify
#' @param start season to start at (use end of season year)
#' @param end season to end at (use end of season year)
#'
#' @return data frame with season restrictions
#' @export

edit_dat <- function(dat,start=NULL,end=NULL){

  year <- dat[1,1] + 1

  if(!is.null(start)){
    s <- year - start
      if(!is.null(end)){
        e <- year - end
        dat <- dat[e:s,]
      }
      else{
        e <- 1
        dat <- dat[e:s,]
      }
  }
  else if(!is.null(end)){
    e <- year - end
    dat <- dat[e:nrow(dat),]
  }
  else{
    dat <- dat
  }

  return(dat)

}

