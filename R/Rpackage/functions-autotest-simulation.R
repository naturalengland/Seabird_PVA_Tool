## ###############################################################################################
## Functions to simulate random sets of inputs for the NE PVA tool
## Simulate a set of valid inputs for the NE PVA tool
## Created 18 September 2019, last modified 28 September 2019
## ###############################################################################################

## ############################################################
## Randomly simulate a set of valid inputs for the tool
## Version 4.7: add "sim.n" argument
## ############################################################

nepva.siminputs.valid <- function(runtype, seed.meta, sim.n = NULL){

  ## #################################
  
  inobj <- as.list(NULL)
    
  ## #################################
  
  set.seed(seed.meta)
  
  r0 <- (runtype == "simplefull")
  
  r1 <- (runtype == "simplescenarios")
  
  r2 <- (runtype == "validation")
  
  r3 <- (runtype == "sensitivity.local")
  
  r4 <- (runtype == "sensitivity.global")
  
  inputs <- as.list(NULL)
  
  ## #################################
  ## 1. Structural - checked
  ## #################################
  ## "model.envstoch"
  
  if(r0){ inputs$model.envstoch <- "betagamma" }
  
  if(! r0){ inputs$model.envstoch <- c("deterministic", "betagamma")[ri(2)] }
  
  ## ####################
  ## "model.demostoch"
  
  inputs$model.demostoch <- rl()
  
  ## ####################
  ## "model.dd" - fixed to be "nodd" for r3, r4
  
  if(r0){ inputs$model.dd <- "dduloglin" }
  
  if(r1|r2){ inputs$model.dd <- c("nodd", "dduloglin")[ri(2)] }
  
  ## ####################
  
  inputs$model.prodmax <- rl()
  
  ## ####################
  
  inputs$mbs <- ri(4)
  
  ## ####################
  
  inputs$afb <- ri(6)
  
  ## ####################
  ## "npop" - fixed to be 1 for r2,r3,r4
  
  if(r0){ inputs$npop <- ri(4) + 1 }
  if(r1){ inputs$npop <- ri(5) }
  
  ## ####################
  ## "nscen" - fixed to be 0 for r2, fixed to be 1 for r3,r4
  
  if(r0){ inputs$nscen <- ri(2) + 1 }
  if(r1){ inputs$nscen <- ri(3) }
  
  ## ####################
  
  inputs$nburn <- ri(11) - 1
  
  ## ####################
  
  if(is.null(sim.n)){
  
    inputs$sim.n <- ri(10)
  }
  else{
    
    inputs$sim.n <- sim.n ## added Version 4.7
  }
  
  ## ####################
  
  inputs$sim.seed <- ri(10000)
  
  ## #################################
  ## Dimensions
  ## #################################
  
  if(r0|r1){ inobj$npop <- inputs$npop }
  if(r2|r3|r4){ inobj$npop <- 1 }
  
  if(r0|r1){ inobj$nscen <- inputs$nscen } ## bug fix Version 4.6 - corrected from "npop" to "nscen"
  if(r2){ inobj$nscen <- 0 }
  if(r3|r4){ inobj$nscen <- 1 }  
  
  if(r0){ inobj$spop <- 1 }
  if(! r0){ inobj$spop <- inobj$npop }
  
  if(r0){ inobj$bob <- inobj$nscen * inobj$spop}
  if(! r0){ inobj$bob <- inobj$nscen * inobj$npop}
  
  inobj$nep <- 2
  
  if(r0|r1|r2){ if(inputs$model.dd == "dduloglin"){ inobj$nep <- inobj$nep + 1} }
  
  ## #################################
  ## 2. Baseline demography
  ## #################################
  
  inputs$demobase.specify.as.params <- FALSE
  
  ## #################################
  ## "demobase.splitpops": FALSE if r2,r3,r4
  
  if(r0){ inputs$demobase.splitpops <- TRUE }
  
  if(r1){ inputs$demobase.splitpops <- rl() }
  
  inobj$demobase.splitpops <- FALSE
  
  if(r0|r1){ if(inputs$demobase.splitpops){ inobj$demobase.splitpops <- TRUE} }
  
  ## #################################
  ## "demobase.splitimmat": FALSE if r3,r4
  
  if(r0){ inputs$demobase.splitimmat <- TRUE }
  
  if(r1|r2){ inputs$demobase.splitimmat <- rl() }
  
  inobj$demobase.splitimmat <- FALSE
  
  if(! is.null(inputs$demobase.splitimmat)){ 
    
    inobj$demobase.splitimmat <- inputs$demobase.splitimmat
  }
  else{
    
    inobj$demobase.splitimmat <- FALSE
  }
 
  ## #################################
  ## "demobase.prod"
  
  inputs$demobase.prod <- array(dim=c(inobj$npop^inobj$demobase.splitpops, inobj$nep))
  
  inputs$demobase.prod[,1] <- inputs$mbs * runif(inobj$spop^inobj$demobase.splitpops, 0.2, 0.9)
  
  ## SD range reduced, Version 4.7
  inputs$demobase.prod[,2] <- inputs$demobase.prod[,1] * runif(inobj$spop^inobj$demobase.splitpops, 0.01, 0.2)
  
  if(inobj$nep > 2){ 
  
    if(r0){ 
      
      inputs$demobase.prod[,3] <- 0 ## added Version 4.7 - BUG FIX!
    }
    else{
      
      inputs$demobase.prod[,3] <- rnorm(inobj$spop^inobj$demobase.splitpops, 0, 0.1) 
    }
  }
  
  ## #################################
  ## "demobase.survadult"
  
  inputs$demobase.survadult <- array(dim=c(inobj$npop^inobj$demobase.splitpops, inobj$nep))
  
  inputs$demobase.survadult[,1] <- runif(inobj$spop^inobj$demobase.splitpops, 0.5, 0.99)
  
  ## SD range reduced, Version 4.7
  inputs$demobase.survadult[,2] <- inputs$demobase.survadult[,1] * runif(inobj$spop^inobj$demobase.splitpops, 0.01, 0.2)
  
  if(inobj$nep > 2){ 
    
    if(r0){ 
      
      inputs$demobase.survadult[,3] <- 0 ## added Version 4.7 - BUG FIX!
    }
    else{
    
      inputs$demobase.survadult[,3] <- rnorm(inobj$spop^inobj$demobase.splitpops, 0, 0.1) 
    }
  }
   
  ## #################################
  ## "demobase.survimmat"
  
  naf <- inputs$afb ## + 1 ## Version 4.12: changed from "afb + 1" to "afb"
  
  if(r0|r1|r2){ 
    
    if(inobj$demobase.splitimmat){
      
      inputs$demobase.survimmat <- array(dim=c(inobj$npop^inobj$demobase.splitpops, naf, inobj$nep))
      
      if(r0){
        
        for(k in 1:(inputs$afb)){ inputs$demobase.survimmat[,k,] <- inputs$demobase.survadult }
      }
      else{
        
        inputs$demobase.survimmat[,,1] <- runif(naf * inobj$npop^inobj$demobase.splitpops, 0.5, 0.99)
        
        inputs$demobase.survimmat[,,2] <- inputs$demobase.survadult[,1] * runif(naf * inobj$npop^inobj$demobase.splitpops, 0.5, 0.99)
        
        if(inobj$nep > 2){ inputs$demobase.survimmat[,,3] <- rnorm(naf * inobj$spop^inobj$demobase.splitpops, 0, 0.1) }
      }
      
      if(! inobj$demobase.splitpops){ inputs$demobase.survimmat <- apply(inputs$demobase.survimmat[1,,,drop=FALSE],c(2,3),identity) } ## Added Version 4.6:
    } 
  }
  
  ## #################################
  ## 3. Initial population size(s)
  ## #################################
  
  inputs$inipop.years <- sample(2000:2015, inobj$npop, replace = TRUE)
  
  ## #################################
  
  if(r0|r1|r2){ inputs$inipop.inputformat <- c("breeding.adults", "breeding.pairs", "all.individuals")[ri(3)] }
  
  ## #################################
  
  inputs$inipop.vals <- round(10^runif(inobj$npop,1,4))
  
  ## #################################
  ## 4. Impacts
  ## #################################
  
  if(!r2){ inputs$impacts.relative <- rl() }
  
  ## #################################
  
  if(r0){ inputs$impacts.splitpops <- TRUE}
  if(r1){ inputs$impacts.splitpops <- rl() }
  
  if(! is.null(inputs$impacts.splitpops)){
    
    inobj$impacts.splitpops <- inputs$impacts.splitpops
  }
  else{
    
    inobj$impacts.splitpops <- FALSE
  }
  
  ## #################################
  
  if(r0){ inputs$impacts.splitimmat <- TRUE}
  if(r1){ inputs$impacts.splitimmat <- rl() }
  
  if(! is.null(inputs$impacts.splitimmat)){
    
    inobj$impacts.splitimmat <- inputs$impacts.splitimmat
  }
  else{
    
    inobj$impacts.splitimmat <- FALSE
  }
  
  ## #################################
  
  if(r0){ inputs$impacts.provideses <- TRUE}
  if(r1){ inputs$impacts.provideses <- rl() }
  
  if(! is.null(inputs$impacts.provideses)){
    
    inobj$impacts.provideses <- inputs$impacts.provideses
  }
  else{
    
    inobj$impacts.provideses <- FALSE
  }

  ## #################################
  
  if(! r2){ inputs$impacts.year.start <- sample(max(inputs$inipop.years)+(1:20), 1) }
  
  ## #################################
  
  if(! r2){ inputs$impacts.year.end <- sample(inputs$impacts.year.start+(1:30), 1) }
  
  ## #################################
  
  if(r0|r1){ inputs$impacts.scennames <- paste0("scen", 1:inputs$nscen) }
  
  ## #################################
  
  if(r0|r1){ inputs$impacts.matchscens <- rl() }
  
  ## #################################
  
  if(! r2){
    
    if((inobj$npop > 1) & (inobj$impacts.splitpops)){
      
      inputs$impacts.prod.mean <- array(dim = c(inobj$nscen, inobj$npop), data = inputs$mbs * runif(inobj$nscen * inobj$spop, -0.05, 0.05))
      
      inputs$impacts.survadult.mean <- array(dim = c(inobj$nscen, inobj$npop), data = runif(inobj$nscen * inobj$spop, -0.05, 0.05))
      
      if(inobj$impacts.splitimmat){
        
        inputs$impacts.survimmat.mean <- array(dim = c(inobj$nscen, inobj$npop), data = runif(inobj$nscen * inobj$spop, -0.05, 0.05))
      }
    }
    else{
      
      inputs$impacts.prod.mean <- inputs$mbs * runif(inobj$nscen, -0.05, 0.05)
      
      inputs$impacts.survadult.mean <- runif(inobj$nscen, -0.05, 0.05)
      
      if(inobj$impacts.splitimmat){
        
        if(r0){
          
          inputs$impacts.survimmat.mean <- inputs$impacts.survadult.mean
        }
        else{
          
          inputs$impacts.survimmat.mean <- runif(inobj$nscen, -0.05, 0.05)
        }
      }
    }
  }
  
  ## #################################
  
  if(r0|r1){ 
    
    if(inobj$impacts.provideses){ 
      
      if((inobj$npop > 1) & (inobj$impacts.splitpops)){
        
        rsm.prod <- array(dim = c(inobj$nscen, inobj$npop), data = runif(inobj$nscen * inobj$npop, 0.01, 0.05))
      
        rsm.survadult <- array(dim = c(inobj$nscen, inobj$npop), data = runif(inobj$nscen * inobj$npop, 0.01, 0.05))
        
        rsm.survimmat <- array(dim = c(inobj$nscen, inobj$npop), data = runif(inobj$nscen * inobj$npop, 0.01, 0.05))
      }
      else{
        
        rsm.prod <- runif(inobj$nscen, 0.01, 0.05)
        
        rsm.survadult <- runif(inobj$nscen, 0.01, 0.05)

        rsm.survimmat <- runif(inobj$nscen, 0.01, 0.05)
      }
      
      inputs$impacts.prod.se <- inputs$impacts.prod.mean * rsm.prod
      
      inputs$impacts.survadult.se <- inputs$impacts.survadult.mean * rsm.survadult
      
      if(inobj$impacts.splitimmat){
        
        if(r0){
          
          inputs$impacts.survimmat.se <- inputs$impacts.survadult.se
        }
        else{
          
          inputs$impacts.survimmat.se <- inputs$impacts.survimmat.mean * rsm.survimmat
        }
      }
    }
  }
  
  ## #################################
  ## 5. Outputs
  ## #################################
  
  ## Fixed Version 4.8 to include "whole.population" & fix "age.separated"
  if(r0|r1|r2){ inputs$output.agetype <- c("breeding.adults", "breeding.pairs", "age.separated", "whole.population")[ri(4)] }
  
  ## #################################
  
  inputs$output.year.end <- sample(max(inputs$inipop.years)+(30:60), 1)
  
  ## #################################
  
  if(r0|r1){ inputs$output.year.start <- inputs$output.year.end - sample(20:30, 1) }
  
  ## #################################
  
  inputs$output.popsize.target <- round(10^runif(1,1,4))
  
  ## #################################
  
  inputs$output.popsize.qe <- round(10^runif(1,0,1.5))
  
  ## #################################
  
  if(r2){ inputs$output.validation.years <- sample(max(inputs$inipop.years) + 1:20, 
                                                   sample(1:20, ri(10)), replace = FALSE) }
  
  ## #################################
  
  if(r2){   inputs$output.validation.counts <- round(10^runif(length(inputs$output.validation.years),1,4)) }
  
  ## #################################
  ## 6. Sensitivity analysis
  ## #################################
  
  if(r3|r4){ inputs$sens.pcr <- runif(5,0,50) }
  
  ## #################################
  
  if(r3){ inputs$sens.npvlocal <- ri(10) }
  
  ## #################################
  
  if(r4){ inputs$sens.npvglobal <- 1 + ri(9) } ## Version 4.11 - changed to avoid ever being one
  
  ## #################################

  inputs$output.raw <- TRUE
  
  ## if(r4){ browser() }
  
  ## if(is.null(inputs$inipop.vals)){ browser() }
  
  ## print(paste(inputs$npop, inputs$nscen, nrow(inputs$impacts.prod.mean), ncol(inputs$impacts.prod.mean)))
  
  inputs
}

## ##################################################################################################
## Simulate multiple sets of invalid inputs for the NE PVA tool, by adding errors to a valid set of
##   inputs
## Created 24 September 2019, last modified 24 September 2019
##
## NOTE: these errors are designed to be ones that could be relevant for any value of "runtype"
## ##################################################################################################

nepva.siminputs.invalid <- function(validinputs){
  
  ## ########################################
  
  netypes <- 20
  
  invalidinputs <- NULL
  
  for(etype in 1:netypes){ ## loop over possible error types
 
    ## #####################################################
    ## Start off with valid inputs...
       
    new <- validinputs
    
    ## #####################################################
    ## Introduce one or more errors into the list of inputs
    
    if(etype == 1 | etype == 18 | etype == 19 | etype == 20){ names(new)[names(new) == "mbs"] <- "mns" }
    
    if(etype == 2 | etype == 20){ new$demobase.survadult <- array(dim=c(11,17),data=0.5) }
    
    if(etype == 3){ new$model.envstoch <- "bob" }
    
    if(etype == 4 | etype == 16 | etype == 20){ new$model.demostoch <- "fred" }
    
    if(etype == 5){ new$model.envstoch <- FALSE }
    
    if(etype == 6 | etype == 19){ new$model.envstoch <- 3 }
    
    if(etype == 7 | etype == 20){ new$model.envstoch <- 7.4467 }
    
    if(etype == 8 | etype == 18 | etype == 19 | etype == 20){ new$nburn <- -1 }
    
    if(etype == 9 | etype == 17 | etype == 20){ new$demobase.survadult <- new$demobase.survadult - 1 }
    
    if(etype == 10 | etype == 20){ if(is.null(new$npop)){ 
      
      new$npop <- 2
    }
      else{
        
        new$npop <- new$npop + 1
      }
    }
    
    if(etype == 11 | etype == 20){ new$output.year.end <- min(new$inipop.years) - 1 }
    
    if(etype == 12 | etype == 16){ new$inipop.vals[1] <- NA }
    
    if(etype == 13 | etype == 17 | etype == 19){ new$inipop.vals[1] <- NaN }
    
    if(etype == 14 | etype == 20){ new$inipop.vals[1] <- Inf }
    
    if(etype == 15){ new$inipop.years <- new$inipop.years[-1] }
    
    ## #####################################################
    
    invalidinputs[[etype]] <- new
  }
  
  invalidinputs
}

## ##################################################################################################
## Generate 128 = (2^7) set of inputs which are (a) valid and (b) specify exactly the same PVA inputs, but
##   each express these using direct syntax
##
## The 7 differences are:
##   1. whether there is specified as being environmental stochasticity or not (in a situation where 
##        the level of environmental stochasticity is zero)
##   2. whether there is specified as being density dependence or not (in a situation where the 
##        the level of density dependence is zero)
##   3. whether baseline demographic rates are split between subpopulations (in a situation where the
##        rates are actually common)
##   4. whether baseline demographic rates are specified separately for immatures (in a situations 
##        where the rates for immatures are the same as those for adults)
##   5, 6: as 3,4, but for impacts rather than baseline demographic rates
##   7. whether impact standard errors are specifed or not (in a situation where they are actually zero)
## ##################################################################################################

## Version 4.7: added "sim.n" argument
## Version 4.8: added "full" argument

nepva.siminputs.consistent <- function(seed.meta, sim.n, full=TRUE){
  
  ## ######################################
  ## Randomly simulate an appropriate set of inputs - e.g. with everything specified separately,
  ##  but with actual values common
  
  fullinputs <- nepva.siminputs.valid(runtype = "simplefull", seed.meta = seed.meta, sim.n = sim.n)
  
  ## ######################################
  ## Specify the different possible forms of syntax
  
  if(full){
  
    sopts <- c("model.envstoch", "model.dd", "demobase.splitpops", "demobase.splitimmat", 
               "impacts.splitpops", "impacts.splitimmat", "impacts.provideses")  
  
    mstruc <- data.frame((expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1) == 1))
  }
  else{
  
    sopts <- c("model.envstoch", "impacts.splitpops", "impacts.provideses")  
    
    mstruc <- data.frame((expand.grid(0:1,0:1,0:1) == 1))
  }
  
  ## ######################################
  ## Set up the inputs associated with each of these, and store into a list
  
  colnames(mstruc) <- sopts
  
  inputlist <- as.list(NULL)
  
  nstruc <- nrow(mstruc)
  
  for(k in 1:nstruc){  
    
    ms <- c(mstruc[k,])
    
    new <- fullinputs
    
    ## ######################################
    
    if(ms[[1]]){ new$model.envstoch <- "deterministic"} ## Version 4.7: bug fix - "deterministic", not "none"
    
    ## ######################################
  
    if(full){
      
      if(ms[[2]]){ 
        
      new$model.dd <- "nodd"

      new$demobase.prod <- new$demobase.prod[,-3,drop=FALSE] ## Added Version 4.7
      
      new$demobase.survadult <- new$demobase.survadult[,-3,drop=FALSE] ## Added Version 4.7
      
      new$demobase.survimmat <- new$demobase.survimmat[,,-3,drop=FALSE] ## Added Version 4.7
      }
    }
    
    ## ######################################
    
    if(full){
      
      if(ms[[3]]){ 
      
         new$demobase.splitpops <- FALSE
      
         new$demobase.prod <- new$demobase.prod[1,] ##,drop=FALSE] 

         new$demobase.survadult <- new$demobase.survadult[1,] ## drop=FALSE] ## Added Version 4.7
      
         new$demobase.survimmat <- new$demobase.survimmat[1,,] ## ,drop=FALSE]
      }
    }
    
    ## ######################################
    
    if(full){
      
      if(ms[[4]]){ 
      
        new$demobase.splitimmat <- FALSE
      
        new$demobase.survimmat <- NULL
      }
    }
    
    ## ######################################
    
    bob <- FALSE
    
    if(full){ if(ms[[5]]){ bob <- TRUE }}
    
    if(! full){ if(ms[[2]]){ bob <- TRUE }}
        
    if(bob){ 
      
      new$impacts.splitpops <- FALSE
      
      new$impacts.prod.mean <- new$impacts.prod.mean[,1]
      
      new$impacts.prod.se <- new$impacts.prod.se[,1]
      
      new$impacts.survimmat.mean <- new$impacts.survimmat.mean[,1]
      
      new$impacts.survimmat.se <- new$impacts.survimmat.se[,1]
      
      new$impacts.survadult.mean <- new$impacts.survadult.mean[,1]
      
      new$impacts.survadult.se <- new$impacts.survadult.se[,1]
    }
    
    ## ######################################
   
    if(full){
      
      if(ms[[6]]){ 
        
      new$impacts.splitimmat <- FALSE
      
      new$impacts.survimmat.mean <- NULL
      
      new$impacts.survimmat.se <- NULL
      }
    }
    
    ## ######################################
    
    bob <- FALSE
    
    if(full){ if(ms[[7]]){ bob <- TRUE }}
    
    if(! full){ if(ms[[3]]){ bob <- TRUE }}
    
    if(bob){ 
        
      new$impacts.provideses <- FALSE
      
      new$impacts.prod.se <- NULL
      
      new$impacts.survimmat.se <- NULL
      
      new$impacts.survadult.se <- NULL
    }
    
    ## ######################################
    
    inputlist[[k]] <- new
  }
  
  ## ######################################
  
  out <- list(mstruc = mstruc, inputlist = inputlist)
}

## ##################################################################################################

rb <- function(){rbinom(1,1,0.5)}

rl <- function(){(rb() == 1)}

ri <- function(n){sample(1:n,1)}

ru <- function(a,b){runif(1,a,b)}

## ###############################################################################################
