#' Pulls MVP data
#' @import rvest
#' @import tidyverse
#' @return a dataframe of MVP data
#' @export

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
  mvps[,1] <- as.numeric(mvps[,1])
  mvps[,5] <- as.numeric(mvps[,5])
  mvps[,7:18] <- lapply(mvps[,7:18],as.numeric)
  mvps[is.na(mvps)] <- 0
  return(mvps)
}

#' Pulls ROY data
#' @import rvest
#' @import tidyverse
#' @return a dataframe of ROY data
#' @export

read_roys <- function(roy){
  royurl<- "https://www.basketball-reference.com/awards/roy.html"
  roys <- royurl %>%
    read_html() %>%
    html_nodes(css = "#roy_NBA") %>%
    html_table() %>%
    .[[1]] %>%
    as.data.frame()

  names <- as.vector(roys[1,])
  colnames(roys) <- names
  roys <- roys[-1,]
  roys[,1] <- substr(roys[,1],1,4)
  roys[,1] <- as.numeric(roys[,1])
  roys[,5] <- as.numeric(roys[,5])
  roys[,7:18] <- lapply(roys[,7:18],as.numeric)
  roys[is.na(roys)] <- 0
  return(roys)
}

#' Pulls DPOY data
#' @import rvest
#' @import tidyverse
#' @return a dataframe of DPOY data
#' @export

read_dpoy <- function(dpoy){
  dpoyurl<- "https://www.basketball-reference.com/awards/dpoy.html"
  dpoy <- dpoyurl %>%
    read_html() %>%
    html_nodes(css = "#dpoy_NBA") %>%
    html_table() %>%
    .[[1]] %>%
    as.data.frame()

  names <- as.vector(dpoy[1,])
  colnames(dpoy) <- names
  dpoy <- dpoy[-1,]
  dpoy[,1] <- substr(dpoy[,1],1,4)
  dpoy[,1] <- as.numeric(dpoy[,1])
  dpoy[,5] <- as.numeric(dpoy[,5])
  dpoy[,7:18] <- lapply(dpoy[,7:18],as.numeric)
  dpoy[is.na(dpoy)] <- 0
  return(dpoy)
}


#' Pulls SMOY data
#' @import rvest
#' @import tidyverse
#' @return a dataframe of SMOY data
#' @export

read_smoy <- function(smoy){
  smoyurl<- "https://www.basketball-reference.com/awards/smoy.html"
  smoy <- smoyurl %>%
    read_html() %>%
    html_nodes(css = "#smoy_NBA") %>%
    html_table() %>%
    .[[1]] %>%
    as.data.frame()

  names <- as.vector(smoy[1,])
  colnames(smoy) <- names
  smoy <- smoy[-1,]
  smoy[,1] <- substr(smoy[,1],1,4)
  smoy[,1] <- as.numeric(smoy[,1])
  smoy[,5] <- as.numeric(smoy[,5])
  smoy[,7:18] <- lapply(smoy[,7:18],as.numeric)
  smoy[is.na(smoy)] <- 0
  return(smoy)
}


#' Pulls MIP data
#' @import rvest
#' @import tidyverse
#' @return a dataframe of MIP data
#' @export

read_mip <- function(mip){
  mipurl<- "https://www.basketball-reference.com/awards/mip.html"
  mip <- mipurl %>%
    read_html() %>%
    html_nodes(css = "#mip_NBA") %>%
    html_table() %>%
    .[[1]] %>%
    as.data.frame()

  names <- as.vector(mip[1,])
  colnames(mip) <- names
  mip <- mip[-1,]
  mip[,1] <- substr(mip[,1],1,4)
  mip[,1] <- as.numeric(mip[,1])
  mip[,5] <- as.numeric(mip[,5])
  mip[,7:18] <- lapply(mip[,7:18],as.numeric)
  mip[is.na(mip)] <- 0
  return(mip)
}
