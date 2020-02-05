## ###################################################################################################################
## This file contains functions to pre-process the inputs, prior to running the main PVA
## Last modified Version 1.9 on 21 Jan 2019
## ###################################################################################################################

## ###################################################################################################################
## >> nepva.preprocessing
## ############################################################################
#' @title Internal function to run pre-processing, prior to running the PVA simulation
## ############################################################################
## [nepva.sim ::] Version 0.5: completely rewritten, to: (1) absorb previous internal functions,
##  (2) make use of the more general specification of the underlying parametric
##  models and (3) take random numbers that have already been generated as input
##
## Version 1.3: completely rewritten, so that simulation of scenario impacts
##   are performed *first* (to allow automatic allocation of numbers killed to 
##   subpopulations and ages, a key part of the new "absolute harvest" option)
##
## Version 1.7: renamed "nepva.calcs", and modified to include parts from "nepva.preprocess"
##
## Version 1.8: separated "nepva.preprocess" and "nepva.sim" back out again
##            : completely rewritten, mainly to fix specification of "impacts"
## Version 1.9: documentation improved
## ############################################################################
#' @param inputs A list containing the inputs to use in running the PVA; the contents of this list
#'   match the arguments to \code{\link{nepva.fullrun}}
#' @param demomods A list of model specifications, formed by running "source(models.R)"; 
#'   this is part of the internal structure of the R package,
#'   and does not need to be modified by the user   
## ############################################################################
#' @return A list, which is a modified version of "inputs", containing elements with
#'   names, formats and contents equal to the arguments for \code{\link{nepva.sim}}  
## ############################################################################
#' @seealso An internal function. Calls the additional internal functions
#'   \code{\link{getpars.demorates}}, 
#'   \code{\link{mean.demorates}} and \code{\link{make.leslie.matrix}}.
#' @export

nepva.preprocess <- function(inputs, demomods){

  ## #################################################################
  ## Step 1. Basic dimensions
  
  afb <- inputs$afb ## age at first breeding
  
  ## Number of ages; ages are: 0 [chicks], 1,...,(afb-1) [immatures], afb+ [adults]
  na <- afb + 1 
  
  ## Number of demographic rates: productivity, immature survival ages 0-afb, adult survival
  nd <- afb + 2 

  ## Number of subpopulations
  npop <- inputs$npop
  
  ## #################################################################
  ## Stage 2.
  ## Find parametric models to use for simulation, using "demomods"
  ## Added Version 0.5, tidied Version 1.2, further tidied Version 1.8
  ## Updated Version 2.5
  ## ############################################
  
  if(inputs$trans.dd){
    
    esname <- paste(inputs$model.envstoch, "ddt", sep=".")
  }
  else{
    
    esname <- paste(inputs$model.envstoch, "ddu", sep=".")
  }
  
  zi <- which(names(demomods$es) == esname)
  zj <- which(names(demomods$dd) == inputs$model.dd)
  
  modelfns <- demomods$es[[zi]]
   
  modelfns$npes <- NULL
  
  modelfns$np <- demomods$es[[zi]]$npes + demomods$dd[[zj]]$npdd

  modelfns$ddfn <- demomods$dd[[zj]]$fn
  
  ## #################################################################
  ## Step 3. Determine parameter estimates for baseline demographics
  ## Rewritten Version 1.8 (20 January 2018)
  ## #########################################

  ## #########################################
  ## Number of "parameters": 2 if the inputs are specified as mean and SD 
  ##   (demobase.specify.as.params = FALSE), otherwise (demobase.specify.as.params = TRUE) 
  ##   determine automatically using the information in "demomods"

  ## Changed Version 2.5
  
  npar.adj <- modelfns$np
  
  ## npar.adj <- (2 * (! inputs$demobase.specify.as.params)) + (modelfns$np * inputs$demobase.specify.as.params)

  ## #########################################
  ## Create an object containing all baseline demographic rates
  
  demobase.vals <- array(dim=c(npop, nd, npar.adj)) ## Blank object

  for(i in 1:npop){ ## Loop over subpopulations
  
      ## ##################
      ## Specify productivity rates
  
      if(inputs$demobase.splitpops){
    
        demobase.vals[i,1,] <- as.numeric(c(inputs$demobase.prod[i,]))
      }
      else{
        
        demobase.vals[i,1,] <- as.numeric(c(inputs$demobase.prod))   
      }
    
      ## ##################
      ## Specify immature survival rates
    
      for(j in 1:afb){
  
         if(inputs$demobase.splitimmat){
        
           ## ####
           ## If "demobase.splitimmat = TRUE" uses the immature survival rates specified by the user
            
            if(inputs$demobase.splitpops){
              
              demobase.vals[i,j+1,] <- as.numeric(c(inputs$demobase.survimmat[i,j,]))
            }
            else{
              
              demobase.vals[i,j+1,] <- as.numeric(c(inputs$demobase.survimmat[j,]))
            }
         }
         else{
        
           ## ####
           ## If "demobase.splitimmat = FALSE" use the adult survival rates
           
           if(inputs$demobase.splitpops){
             
              demobase.vals[i,j+1,] <- as.numeric(c(inputs$demobase.survadult[i,]))
           }
           else{
             
             demobase.vals[i,j+1,] <- as.numeric(c(inputs$demobase.survadult))
           }
         }
      }
   
      ## ##################
      ## Specify adult survival rates
 
      if(inputs$demobase.splitpops){
      
        demobase.vals[i,nd,] <- as.numeric(c(inputs$demobase.survadult[i,]))
      }
      else{
        
        demobase.vals[i,nd,] <- as.numeric(c(inputs$demobase.survadult))
      }
    
      ## ##################
  }
 
  ## #########################################
  ## Convert summaries (mean + SD) to parameter estimates, 
  ##  via moment matching, where relevant

  demobase.ests <- array(dim=c(npop, nd, modelfns$np))

  if(! inputs$demobase.specify.as.params){

    for(i in 1:npop){
      
      demobase.ests[i,,] <- getpars.demorates(demorates.smy = demobase.vals[i,,], 
                                            modelfns = modelfns,
                                            model.prodmax = inputs$model.prodmax, 
                                            mbs = inputs$mbs)
    }
  }
  else{
  
    demobase.ests <- demobase.vals
  }

  ## if(any(demobase.ests[,,2] < 0)){ print("NOOO!") ; browser() }
  
  inputs$demobase.prod <- inputs$demobase.survimmat <- inputs$demobase.survadult <- NULL
  
  inputs$demobase.splitimmat <- NULL
  
  inputs$demobase.ests <- demobase.ests
  
  ## ############################################
  ## Step 4.   ## Initial values - i.e. calculations for initial time point
  ##           determine initial deterministic Leslie matrix
  ##           determine vector of initial values, using stable age distribution
  ##
  ## Version 0.5: specifying of "mean.demorates" amended
  ## Changed Version 0.8 to use "t = tt.ini" rather than "t = 1"
  ## ############################################

  inipop.counts <- array(dim=c(na, npop)) ## 29 Oct 2018: changed from "np" to "np - 1"
  
  if(inputs$inipop.splitimmat){
    
    ## #################################################
    ## If initial values are provided by the user broken down by age
    
    inipop.counts[1:afb,] <- inputs$inipop.immatvals
    
    if(inputs$inipop.inputformat == "breeding.adults"){
      
      inipop.counts[na,] <- inputs$inipop.vals
    }
    
    if(inputs$inipop.inputformat == "breeding.pairs"){
      
      inipop.counts[na,] <- inputs$inipop.vals * 2 ## !!! Bug fix Version 2.6
    }
    
    if(inputs$inipop.inputformat == "all.individuals"){
      
      inipop.counts[na,] <- inputs$inipop.vals - apply(inipop.counts[1:afb,], 2, sum)
    }
    
    ## #################################################
  }
  else{
    
    ## #################################################
    ## If initial values are provided by the user just as
    ##   total number of breeding adults/pairs, or total
    ##   number of all individuals
    
    for(i in 1:npop){
      
      ## ##############################
      ## Construct the Leslie matrix associated with the deterministic
      ##  version of the model
      
      demorates.deterministic <- mean.demorates(ests = demobase.ests[i,,], 
                                                modelfns = modelfns, 
                                                model.prodmax = inputs$model.prodmax,
                                                popsize.prev = inputs$inipop.vals[i], mbs = inputs$mbs)
      
      demorates.deterministic[1] <- demorates.deterministic[1] / 2 ## Version 3.2: bug fix: indiv-to-pairs conversion for productivity
      
      mmat <- make.leslie.matrix(demorates.deterministic, afb = afb)
      
      ## Version 3.2: added
      
      agr.analytic <- as.numeric(eigen(mmat)$values[1])
    
      if(! inputs$silent){ ## Added Version 3.3.1  
    
        print(paste("Analytic AGR:", round(agr.analytic, 6)))
      }
      
      ## ##############################
      ## Find the stable age structure
      
      ssv <- stable.stage(mmat) 
      
      ## ##############################
      ## Find amounts to rescale initial values by
      
      ## Added Version 1.1, modified Version 1.8
      is.ba <- (inputs$inipop.inputformat == "breeding.adults") 
      
      is.bp <- (inputs$inipop.inputformat == "breeding.pairs")
    
      wts <- ssv * (2 ^ is.bp) / ((ssv[na])^(is.ba|is.bp)) ## Version 2.6: bug fix: changed "1/2" to 2
   
      ## ##############################
      ## Find initial values for each age

      inipop.counts[,i] <- round(wts * inputs$inipop.vals[i])
      
      ## ##############################
    }
  }
  
  inputs$inipop.counts <- inipop.counts
  
  inputs$inipop.inputformat <- inputs$inipop.splitimmat <- NULL
  
  inputs$demobase.specify.as.params <- NULL
  
  ## ############################################
  ## Step 5. Scenarios of impact
 
  ## ##############################
  ## Add baseline scenario

  inputs$nscen <- inputs$nscen + inputs$include.baseline

  if(inputs$include.baseline){
  
    inputs$impacts.scennames <- c("baseline", inputs$impacts.scennames)
  }
  
  ## ##############################
  ## Expand "impacts" to have a standardized format

  ids <- (1 + inputs$include.baseline):inputs$nscen

  idp <- 1:(npop^(inputs$impacts.splitpops|(inputs$impacts.relative)))

  inputs$baseonly <- (inputs$include.baseline) & (inputs$nscen == 1) ## Added Version 1.9

  ## if(length(inputs$impacts.prod.mean) > (length(ids) * length(idp))){ browser() }
    
  inputs$impacts.demochange.mean <- create.impactmat(prod = inputs$impacts.prod.mean,
                                              survimmat = inputs$impacts.survimmat.mean, 
                                              survadult = inputs$impacts.survadult.mean,
                                              npop = npop, afb = afb, ids = ids, idp = idp,
                                              nullonly = (! inputs$impacts.relative) | inputs$baseonly, 
                                              impacts.relative = TRUE) ## inputs$impacts.relative)
  
  inputs$impacts.demochange.se <- create.impactmat(prod = inputs$impacts.prod.se,
                                              survimmat = inputs$impacts.survimmat.se, 
                                              survadult = inputs$impacts.survadult.se,
                                              npop = npop, afb = afb, ids = ids, idp = idp,
                                              nullonly = ((! inputs$impacts.relative) | (! inputs$impacts.provideses)  | inputs$baseonly), 
                                              impacts.relative = TRUE) ## inputs$impacts.relative)

  inputs$impacts.abschange.mean <- create.impactmat(prod = inputs$impacts.prod.mean,
                                              survimmat = inputs$impacts.survimmat.mean, 
                                              survadult = inputs$impacts.survadult.mean,
                                              npop = npop, afb = afb, ids = ids, idp = idp,
                                              nullonly = inputs$impacts.relative  | inputs$baseonly, 
                                             impacts.relative = FALSE) ## inputs$impacts.relative)

  inputs$impacts.abschange.se <- create.impactmat(prod = inputs$impacts.prod.se,
                                            survimmat = inputs$impacts.survimmat.se, 
                                            survadult = inputs$impacts.survadult.se,
                                            npop = npop, afb = afb, ids = ids, idp = idp,
                                            nullonly = (inputs$impacts.relative | (! inputs$impacts.provideses)  | inputs$baseonly), 
                                            impacts.relative = FALSE) ## inputs$impacts.relative)

  inputs$impacts.infillpops <- (! inputs$impacts.relative) & (! inputs$impacts.splitpops)

  inputs$impacts.infillages <- (! inputs$impacts.relative) & (! inputs$impacts.splitimmat)

  inputs$impacts.splitpops <- inputs$impacts.splitimmat <- inputs$impacts.provideses <- NULL
  inputs$impacts.relative <- NULL
  
  inputs$impacts.prod.mean <- impacts.prod.se <- NULL
  inputs$impacts.survimmat.mean <- impacts.survimmat.se <- NULL
  inputs$impacts.survadult.mean <- impacts.survadult.se <- NULL
  
  ## ############################################
  ## Step 6. Models
  
  inputs$model.dd <- NULL
  inputs$model.envstoch <- NULL
  
  inputs$ddfn <- modelfns$ddfn
  inputs$fn.sim.con <- modelfns$sim.con
  inputs$fn.sim.unc <- modelfns$sim.unc
  
  ## ############################################
  
  inputs
}
  
## ###################################################################################################################
## >> create.impactmat
## ###################################################################################################################
#' @title Convert impacts into a standardized format
#' @details Converts impacts into a matrix of dimension [nscen, npop, amax+1] 
#'   (if "impacts.relative = TRUE") or [nscen, npop, amax+1] (if "impacts.relative = FALSE")
## ##############################################################################
#' @inheritParams nepva.fullrun
#' @param prod Change in productivity values as a result of the impact 
#'   (if "impacts.relative = TRUE") or number of chicks
#'   killed (if "impacts.relative = FALSE"). A matrix/vector of dimension [length(ids),length(idp)].
#' @param survimmat Change in immature survival values as a result of the impact 
#'   (if "impacts.relative = TRUE") or number of immatures killed (if "impacts.relative = FALSE"). 
#' @param survadult Change in adult survival values as a result of the impact 
#'   (if "impacts.relative = TRUE") or number of adults killed (if "impacts.relative = FALSE").     
#' @param ids Positions in "output.scennames" 
#'   corresponding to non-baseline scenarios specified by the user
#'   either (1:nscen) [if there is no baseline]
#'   or (2:nscen) [if there is a baseline]
#' @param idp The set of populations to which values should
#'    be written; a vector of integers. Either "1" (if all outputs are written to the first
#'    subpopulation) or "1:npop" (if outputs are written to each subpopulation separately)
#' @param nullonly Should the resulting matrix be populated only with zero values (nullonly = TRUE)
#'    or with actual values (nullonly = FALSE)?
## ##############################################################################
#' @return A matrix of dimension [nscen,npop,afb+2] containing impacts on demographic rates
#'   for each scenario and subpopulation (if impacts.relative = TRUE), or a matrix dimension
#'   [nscen,npop,afb+1] containing actual number of birds killed associated with the impact
#'   (if impacts.relative = FALSE). 
#'   
#'   In the latter case: (a) if "impacts.splitpops = FALSE" then
#'   all birds are allocated (at this stage) to the first subpopulation, so numbers are zero
#'   for all other subpopulations; (b) if "impacts.splitimmats = FALSE" then all birds are
#'   allocated to adults or chicks (at this stage), so numbers are zero for immature ages.
#'   Note that in these situations the splitting of individuals to subpopulations and ages is
#'   performed within the main PVA, by the function "split.deaths", based on the current
#'   allocation of birds to subpopulations and ages in each year.
## ##############################################################################
#' @export

create.impactmat <- function(prod, survimmat, survadult, 
                             npop, afb, ids, idp, nullonly = FALSE, impacts.relative){
    
  ## ##########################################
  ## Number of scenarios
  
  ns <- max(ids)
  
  ## ##########################################
  ## Number of ages in output: (afb+2) if impacts.relative = TRUE, (afb+1) otherwise

  nc <- afb + 1 + impacts.relative
    
  ## ##########################################
  ## Set of populations to which output should be written
  
  idpa <- idp ^ impacts.relative
    
  ## ##########################################
  ## Set up a blank array containing the output
  
  out <- array(dim=c(ns, npop, nc), data = 0)
    
  ## ##########################################
    
  if(! nullonly){
      
    ## ####################
    ## Productivity
    
    ## if(length(prod) != (length(ids)*length(idp))){ browser() }
    
    out[ids,idp,1] <- prod
      
    ## ####################
    ## Immature survival
    
    for(j in 2:(nc-1)){
        
      ## If immature survival is specified by the user, then used this:
      
      if(! is.null(survimmat)){ out[ids,idpa,j] <- survimmat }
      
      ## Otherwise, if (impacts.relative = TRUE), assume adult survival rates
      ##  can also be applied to immatures:
      
      ## VERSION 4.14 - bug fix - "! impacts.relative" changed to "impacts.relative"
      
      if(is.null(survimmat) & (impacts.relative)){ out[ids,idpa,j] <- survadult } 
    }

    ## ####################
    ## Adult survival
    
    out[ids,idpa,nc] <- survadult
    
    ## ####################
  }
    
  ## ##########################################
    
  out
}
  
## ############################################################################
## ############################################################################
## >> make.leslie.matrix
#' @title Create the Leslie matrix associated with particular demographic rates
## ############################################################################
## Created 24 October 2018, last modified 24 October 2018
## ############################################################################
#' @inheritParams nepva.fullrun
#' @param demorates A numeric vector of length (amax+2) containing the demographic rates:
#'    the first entry is the productivity rate, and the remaining entries are the annual 
#'    survival rates 
#'    for ages 0 to 1,...,(amax-1) to amax, and amax onwards
#' @return A matrix of dimension (amax + 1) by (amax + 1) containing the Leslie matrix
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.run}}.
#' @export
  
make.leslie.matrix <- function(demorates, afb){
    
    ## ###################################
    ## Version 1.8: fixed to use "afb" rather than "amax"
    
    svmat <- array(dim=c(afb,afb), data = 0)
    
    diag(svmat) <- demorates[1+(1:afb)]
    
    ## ###################################
    
    out <- array(dim=c(afb+1,afb+1),data=0)
    
    out[1+(1:afb),1:afb] <- svmat
    
    out[1,afb+1] <- demorates[1]  
    
    out[afb+1,afb+1] <- demorates[afb+2]
    
    ## ###################################
    
    out
}
  
## ####################################################################
## ####################################################################
#' >> getpars.demorates
#' @title Calculate parameter estimates associated with each demographic rate, given a mean and
#'   standard deviation
## ####################################################################
## Created 24 October 2018, last modified 20 Jan 2019
## Version 0.5: completely rewritten to use general specification of model
## Version 1.8: modified so that model function are provided directly ("modelfns"),
##  and not derived from "model.dd" and "model.envstoch"
## ####################################################################
#' @details Note that this function is only used when "model.dd = "nodd"" - i.e. when there is 
#'   assumed to be no density dependence.
## ####################################################################
#' @inheritParams nepva.fullrun
#' @param demorates.smy Summaries (mean and SD) for each demographic rate; a matrix of dimensions
#'   [amax+2, 2]. The first row corresponds to productivity, the next [amax] rows to survival rates
#'   for each age from (0-1) up to (amax-1) to amax, and the final row to annual survival rates
#'   for individuals older than age "amax". The first column is the mean, and the second the standard
#'   deviation. 
#' @param modelfns A set of model functions; in practice, these are always derived from the relevant 
#'   elements of "demomods", and do not need to be directly specified from the user.  
#' @return Parameter estimates associated with each demographic rate. A matrix of dimension [amax+2,np],
#'   where "np" depends upon the parametric model chosen. 
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.preprocess}}.
#' @export
  
getpars.demorates <- function(demorates.smy, modelfns, model.prodmax, mbs = NULL){
    
    ## ###################################
    ## Number of demographic rates: equal to "amax + 2"
    
    nd <- nrow(demorates.smy)
    
    ## ###################################
    ## Extract mean and SDs of demographic rates from "demorates.smy"
    
    prod.mn <- demorates.smy[1,1]
    prod.sd <- demorates.smy[1,2]
    
    surv.mn <- demorates.smy[-1,1]
    surv.sd <- demorates.smy[-1,2]
    
    ## ###################################
    ## Number of parameters in the parametric model; changed Version 1.8    

    np <- modelfns$np
    
    ## ###################################
    ## Extract functions for performing moment matching
    
    fn.mom.con <- modelfns$mom.con
    
    fn.mom.unc <- modelfns$mom.unc
    
    ## ###################################
    ## If functions for moment matching do not exist, 
    ##  perform it numerically, via simulation
    
    if(is.null(fn.mom.con)){
      
      fn.mom.con <- function(smy){ mom.numeric(smy, inits = rep(1,np), simfn = modelfns$sim.con, popsize = NULL) }
    }
    
    if(is.null(fn.mom.unc)){
      
      fn.mom.unc <- function(smy){ mom.numeric(smy, inits = rep(1,np), simfn = modelfns$sim.unc, popsize = NULL) }
    }
    
    ## ###################################
    ## Set up a blank object for the estimates
    
    ests <- demorates.smy ## array(dim=c(nd,np)) ## Changed Version 2.5: no longer blank, now pre-filled (so DD estimates are kept)
    
    ## ###################################
    ## Create estimates by moment matching - productivity
    
    if(model.prodmax){ 
      
      ## Version 2.5: Added "1:2", as "ests" may now also contain dd columns
      ests[1,1:2] <- fn.mom.con(c(prod.mn / mbs, prod.sd / mbs))
    }
    else{
      
      ## Version 2.5: Added "1:2", as "ests" may now also contain dd columns
      ests[1,1:2] <- fn.mom.unc(c(prod.mn, prod.sd))
    }
    
    ## ###################################
    ## Create estimates by moment matching - survival  
    
    for(k in 2:nd){
      
      ## Version 2.5: Added "1:2", as "ests" may now also contain dd columns
      ests[k,1:2] <- fn.mom.con(c(surv.mn[k-1], surv.sd[k-1]))
    }
    
    ## ###################################
    
    ests
}
  
## ####################################################################
#' @title Calculate mean demographic rates, based on estimates associated
#'   with a parametric model for each demographic rate
## ####################################################################
## Created 25 October 2018, last modified 20 Jan 2019
## Version 0.5, 6 Nov 2018: completely rewritten, to use new format for specifying models,
##  and to include "model.dd" argument
## Version 1.8: changed so that model functions are specified directly
## ####################################################################
#' @inheritParams nepva.fullrun
#' @param ests Parameter estimates associated with each demographic rate. A matrix of dimension [amax+2,np],
#'   where "np" depends upon the parametric model chosen. 
#' @param popsize Previous population size; a non-negative integer. If "model.dd = 'nodd'" then "popsize"
#'   can be NULL.
#' @return A vector of length equal to 'nrow(ests)', containing the mean value for each demographic rate.
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.preprocess}}.
#' @export
  
mean.demorates <- function(ests, modelfns, model.prodmax, popsize.prev = NULL, mbs = NULL){
    
  ## #####################
  
  nd <- nrow(ests) ## Number of demographic rates
    
  out <- rep(NA, nd) ## Set up a blank object for outputs

  ## #####################
  ## Extract functions for prediction
  
  ddfn <- modelfns$ddfn ## Added Version 2.5
  
  fn.pred.con <- modelfns$pred.con
  
  fn.pred.unc <- modelfns$pred.unc
    
  ## #####################
  ## If no functions for prediction exist, perform
  ##  prediction numerically via simulation
  
  if(is.null(fn.pred.con)){
      
    simfn <- modelfns$sim.con
      
    fn.pred.con <- function(pars, popsize,...){ pred.numeric(pars = pars, popsize = popsize, 
                                                           simfn = simfn, ddfn = ddfn) }
  }
    
  if(is.null(fn.pred.unc)){
      
    fn.pred.unc <- function(pars, popsize,...){ pred.numeric(pars = pars, popsize = popsize, 
                                                           simfn = modelfns$sim.unc, ddfn = ddfn) }
  }

  ## #####################
  ## Generate predictions - productivity
    
  if(model.prodmax){
      
     out[1] <- mbs * fn.pred.con(pars = as.numeric(c(ests[1,])), popsize = popsize.prev, ddfn = ddfn)
  }
  else{
      
      out[1] <- fn.pred.unc(pars = as.numeric(c(ests[1,])), popsize = popsize.prev, ddfn = ddfn)
  }
   
  ## #####################
  ## Generate predictions - survival
  
  for(k in 2:nd){
      
    out[k] <- fn.pred.con(pars = as.numeric(c(ests[k,])), popsize = popsize.prev, ddfn = ddfn)
  }

  ## #####################
  
  as.numeric(c(out))
}
  
## ########################################################################################
## Created V0.5 on 5 November 2018:
## ########################################################################################
  
pred.numeric <- function(pars, popsize, simfn, sim.big = 100000,...){
    
    ru <- runif(sim.big)
    
    out <- simfn(ru = ru, pars = as.numeric(c(pars)), popsize = popsize,...)
    
    mean(out)
}
  
## ########################################################################################
## Created V0.5 on 5 November 2018
## Method of moments via numerical optimization
## ########################################################################################
  
mom.numeric <- function(smy, inits, simfn, sim.big = 100000,...){
    
  ## ###################################################
    
  mom.objfn <- function(pars, smy, ru, simfn,...){ 
      
    out <- simfn(ru = ru, pars = pars,...)
      
    smy.sim <- c(mean(out), sd(out))
      
    sum((smy.sim - smy)^2)
  }
    
  ## ###################################################
    
  ru <- runif(sim.big)
    
  ## ###################################################
    
  fit <- optim(par = inits, fn = mom.objfn, smy = smy, ru = ru, simfn = simfn,...)
    
  if(fit$convergence > 0){ 
      
    warning("Lack of convergence in 'mom.numeric'...")  
  }
    
  ## ###################################################
    
  fit$par
}
  
## #################################################################