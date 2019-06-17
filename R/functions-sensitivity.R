## ###################################################################################################################
## >> nepva.calcs.localsens
## Version 2.2: created as a separate function
## ###################################################################################################################

nepva.calcs.localsens <- function(inputs, demomods){

  ## ##############################################################
  ## Set of parameters to consider in sensitivity analysis
    
  senspars <- c("inipop.vals", "demobase.prod.mean", "demobase.survadult.mean",
                "impact.prod.mean", "impact.survadult.mean")
  
  ## ##############################################################
  ## Specify sets of input parameters to run
  ## Version 2.8: restructured code (cosmetic changes only) to make logic easier to follow
  
  npv <- inputs$sens.npvlocal
  
  nsp <- length(senspars)
  
  zmat <- regshiftmat(nsp = nsp, npv = npv)
  
  parname <- data.frame(parname = c("standard", rep(senspars, 1, each = npv * 2)))
  
  npe <- nrow(zmat)
  
  pcchange <- data.frame(inipop.vals = inputs$sens.pcr[1] * zmat[,1],
                         demobase.prod.mean = inputs$sens.pcr[2] * zmat[,2],
                         demobase.survadult.mean = inputs$sens.pcr[3] * zmat[,3],
                         impact.prod.mean = inputs$sens.pcr[4] * zmat[,4],
                         impact.survadult.mean = inputs$sens.pcr[5] * zmat[,5])
  
  standard.vals <- data.frame(inipop.vals = rep(inputs$inipop.vals, npe),
                              demobase.prod.mean = rep(inputs$demobase.prod$mean, npe),
                              demobase.survadult.mean = rep(inputs$demobase.survadult$mean, npe),
                              impact.prod.mean = rep(inputs$impacts.prod.mean, npe),
                              impact.survadult.mean = rep(inputs$impacts.survadult.mean, npe))
  
  parvals <- standard.vals * (1 + (pcchange/100)) ## bug fix: Version 1.9
  
  colnames(pcchange) <- paste("pcchange", colnames(pcchange), sep=".")
  
  pars <- cbind(parname, pcchange, parvals)
  
  ## ##############################################################
  ## Run PVAs
  
  out <- NULL
  
  for(k in 1:npe){
    
    ## #############################################
    
    inputs.tmp <- inputs
    
    inputs.tmp$demobase.prod <- data.frame(mean = parvals$demobase.prod.mean[k], 
                                           sd = inputs$demobase.prod$sd)
    
    inputs.tmp$demobase.survadult <- data.frame(mean = parvals$demobase.survadult.mean[k], 
                                                sd = inputs$demobase.survadult$sd)
    
    inputs.tmp$inipop.vals <- parvals$inipop.vals[k]
    
    inputs.tmp$impacts.prod.mean <- parvals$impact.prod.mean[k] ## Added Version 1.9
    
    inputs.tmp$impacts.survadult.mean <- parvals$impact.survadult.mean[k] ## Added Version 1.9
    
    inputs.tmp$output.year.start <- inputs$output.year.end ## Added Version 2.1
    
    ## #############################################
    
    tmp <- nepva.calcs.main(inputs.tmp, demomods = demomods)
    
    ## #############################################
    
    out <- rbind(out, tmp)
  }
  
  ## #############################################
  ## Names of outputs
  
  out <- out[out$Scenario != "baseline",]
  
  out <- cbind(pars, out) ## Version 2.4: bug fix
  
  ## out <- list(parvals = parvals, out = out) ## Version 2.4: simplified
  
  ## ##############################################################
  
  out
}

## ###################################################################################################################
## >> nepva.calcs.globalsens
## Version 2.2: created as a separate function
## ###################################################################################################################

nepva.calcs.globalsens <- function(inputs, demomods = demomods){
  
  ## ##############################################################
  ## Ranges of input parameters
  
  simranges <- data.frame(inipop.vals = inputs$inipop.vals * (1 + c(-1,1) * inputs$sens.pcr[1] / 100))
  
  simranges$demobase.prod.mean <- inputs$demobase.prod$mean * (1 + c(-1,1) * inputs$sens.pcr[2] / 100)
  
  simranges$demobase.survadult.mean <- inputs$demobase.survadult$mean * (1 + c(-1,1) * inputs$sens.pcr[3] / 100)
  
  simranges$impact.prod.mean <- inputs$impacts.prod.mean * (1 + c(-1,1) * inputs$sens.pcr[4] / 100)
  
  simranges$impact.survadult.mean <- inputs$impacts.survadult.mean * (1 + c(-1,1) * inputs$sens.pcr[5] / 100)
  
  ## ##############################################################
  ## Generate input parameter combinations to use
  
  inputmats <- vbgs.siminputs(simranges, n = inputs$sens.npvglobal)
  
  ## ##############################################################
  ## Run PVAs
  
  nom <- 25
  
  ok <- 6+(1:nom)
  
  outmats <- array(dim=c(inputs$sens.npvglobal, 7, nom))
  
  for(i in 1:inputs$sens.npvglobal){
    
    for(j in 1:7){
      
      ## #############################################
      
      inputs.tmp <- inputs
      
      inputs.tmp$inipop.vals <- inputmats[i,j,1]
      
      inputs.tmp$demobase.prod <- data.frame(mean = inputmats[i,j,2], sd = inputs$demobase.prod$sd)
      
      inputs.tmp$demobase.survadult <- data.frame(mean = inputmats[i,j,3], sd = inputs$demobase.survadult$sd)
      
      inputs.tmp$impacts.prod.mean <- inputmats[i,j,4]
      
      inputs.tmp$impacts.survadult.mean <- inputmats[i,j,5]
      
      inputs.tmp$output.year.start <- inputs$output.year.end ## Added Version 2.1
      
      ## ###########################  

      out.tmp <- nepva.calcs.main(inputs.tmp, demomods = demomods)  
      
      out.tmp <- out.tmp[out.tmp$Scenario != "baseline",]
      
      ## ###########################
      
      outnames <- names(out.tmp[ok])
      
      outmats[i,j,] <- as.numeric(out.tmp[ok])
    }
  }
  
  ## ##############################################################
  ## Summarize results of sensitivity analysis
  
  out <- list(inputs.common = inputs)
  
  out$tab <- vbgs.calctable(inputmats = inputmats, outmats = outmats, 
                            inputnames = names(simranges), outnames = outnames)
  
  out$decomposition <- vbgs.decomposition(inputmats = inputmats, outmats = outmats, 
                                          inputnames = names(simranges), outnames = outnames)
  
  ## ##############################################################
  
  out
}

## ###################################################################################################################
## >> vbgs.siminputs
## ############################################################################
#' @title Simulate parameter values to use a global variance-based sensitivty analysis
#' @description Simulation is currently based on independent simulations using a uniform
#'   distribution
## ########################################################################################
## Global sensitivity analysis - added 3 January 2018, last modified 13 January 2018
## ########################################################################################
#' @param simranges A matrix or data frame of dimension [p,2] containing the lower and upper
#'   ranges to use in simulating from each of the "p" input parameters to be considered in
#'   the sensitivity analysis
#' @param n Number of simulations per input parameter
## ########################################################################################
#' @return An array of dimension [n,p+2,p]; elements [n,1,p] correspond to the "A" matrix,
#'   [n,1+j,p] to the "Aj" matrix, and [n,p+2,p] to be "B" matrix
## ########################################################################################
#' @export

vbgs.siminputs <- function(simranges, n){
  
  ## ############################
  
  p <- ncol(simranges)
  
  ## ############################
  ## Set up "A" and "B" matrices
  
  Amat <- array(dim=c(n,p), data = runif(n*p))
  
  Bmat <- array(dim=c(n,p), data = runif(n*p))
  
  for(k in 1:p){
    
    Amat[,k] <- simranges[1,k] + ((simranges[2,k] - simranges[1,k]) * Amat[,k])
    
    Bmat[,k] <- simranges[1,k] + ((simranges[2,k] - simranges[1,k]) * Bmat[,k])
  }
  
  ## ############################
  ## Set up all "input" matrices to run
  
  inputmats <- array(dim=c(n,p+2,p))
  
  inputmats[,1,] <- Amat
  
  for(k in 1:p){
    
    inputmats[,k+1,-k] <- Amat[,-k]
    
    inputmats[,k+1,k] <- Bmat[,k]
  }
  
  inputmats[,p+2,] <- Bmat
  
  ## ############################
  
  inputmats
}

## ###################################################################################################################
## >> vbgs.decomposition
## ############################################################################
#' @title Perform a global variance-based sensitivity analysis for a model, in
#'   relation to one or more model output variables
#' @description See https://en.wikipedia.org/wiki/Variance-based_sensitivity_analysis
## ############################################################################
#' Global sensitivity analysis - added 3 January 2018, last modified 13 January 2018
## ############################################################################
#' @param inputmats An array of dimension [n, p+2, p] containing the input parameter
#'   values to use in running the global sensitivity analysis, where: "n" is the number
#'   of simulations per parameter and "p" is the number of input parameters  
#' @param outmats An array of dimension [n, p+2, m] containing the model outputs for each
#'   run of the n * (p+2) runs of the model, where the model produces "m" different outputs
#' @param inputnames The names of the "p" input parameters; a vector of length "p" containing
#'   character values
#' @param outnames The names of the "m" model output variables; a vector of length "m" containing
#'   character values
## ############################################################################
#' @return A data frame with "p" rows (one for each input parameter) and "2*m" columns (one for each
#'     model output variable), containing the amount of variation in the model output variable that
#'     is explained by each input parameter. Two summaries of variation are presented: the 
#'     First-Order Index (FOI) and the Total Effect Index (TEI)
## ############################################################################
#' @export

vbgs.decomposition <- function(inputmats, outmats, inputnames, outnames){
  
  ## ###########################################
  ## ###########################################
  ## inputmats : dimension [n, p+2, p]
  ## outmats : dimension [n, p+2, m]
  ## ###########################################
  
  n <- dim(inputmats)[1]
  
  p <- dim(inputmats)[3]
  
  m <- dim(outmats)[3]
  
  ## ###########################################
  ## Create output object
  
  out.foi <- array(dim = c(p, m)) ; colnames(out.foi) <- paste("FOI", outnames, sep=".")
  
  out.tei <- array(dim = c(p, m)) ; colnames(out.tei) <- paste("TEI", outnames, sep=".")
  
  ## ###########################################
  
  for(j in 1:p){
    
    for(k in 1:m){
      
      zd <- (outmats[,j+1,k] - outmats[,1,k])
      
      ## ###########################
      ## First-order index
      
      out.foi[j,k] <- mean(outmats[,p+2,k] * zd)
      
      ## ###########################
      ## Total-effect index
      
      out.tei[j,k] <- (mean(zd^2)) / 2   
      
      ## ###########################
    }
  }
  
  ## ###########################################
  
  out <- data.frame(cbind(out.foi, out.tei))
  
  row.names(out) <- inputnames
  
  out
}

# ###################################################################################################################
## >> vbgs.calctable
## ############################################################################
#' @title Generate raw outputs from a global variance-based sensitivity analysis 
## ############################################################################
#' @inheritParams vbgs.decomposition
## ############################################################################
#' @return A data frame with n*(p+2) rows, with one row corresponding to one 
#'   run of the model, containing columns that give values for each of the
#'   "p" input parameters and each of the "m" model output variables 
## ############################################################################
#' @export

vbgs.calctable <- function(inputmats, outmats, inputnames, outnames){
  
  n <- dim(inputmats)[1]
  
  p <- dim(inputmats)[3]
  
  m <- dim(outmats)[3]
  
  out <- NULL
  
  for(k in 1:(p+2)){
    
    tmp.in <- inputmats[,k,]
    
    colnames(tmp.in) <- inputnames
    
    tmp.out <- outmats[,k,]
    
    colnames(tmp.out) <- outnames
    
    tmp <- cbind(tmp.in, tmp.out)
    
    out <- rbind(out, tmp) ## Bug fix, Version 1.7: changed from "cbind" to "rbind"
  }
  
  tmp
}  

## ###################################################################################################################
## Created as a separate function Version 2.8:

regshiftmat <- function(nsp,npv){
  
  npp <- npv * 2
  
  npe <- 1 + (npp * nsp)
  
  zseq <- (c(-(npv:1),1:npv)) / npv
  
  zmat <- array(dim=c(npe,nsp), data = 0)
  
  vp <- 1:npp
  
  zmat[1+vp,1] <- zseq
  zmat[(npp+1)+vp,2] <- zseq
  zmat[(npp*2+1)+vp,3] <- zseq
  zmat[(npp*3+1)+vp,4] <- zseq
  zmat[(npp*4+1)+vp,5] <- zseq
  
  zmat
}

## ###################################################################################################################
