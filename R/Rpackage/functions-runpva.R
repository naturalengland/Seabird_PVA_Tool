## ###################################################################################################################
## Technical functions used for calculations within the NE PVA tool
##    Last edited Version 4.13
## ###################################################################################################################
## --- Contents (updated v4.3) ---
##
## nepva.sim                     --- function to run PVA calculations
##    leslie.calcs               --- core function to run Leslie matrix updating
##         leslie.update         --- update the population sizes over one year using the Leslie matrix model
##         inits.burned          --- calculate age-specific initial population sizes using results from burn-in
##         split.deaths          --- simulate the (absolute or relative) impacts associated with an impact
##    make.impactmetrics.table   --- summarize PVA metrics and other outputs
##
## Version 4.7: updated to deal with changes requested by PSG to the calculation of metrics
## Version 4.13: bug fix
## Version 4.15: bug fix
## ###################################################################################################################

## ##############################################################################
## >> nepva.sim
## ##############################################################################
#' @title Run NE PVA simulations - this is the main internal function within the 
#'   NEPVA package
## ##############################################################################
#' @inheritParams nepva.fullrun
#' @param fn.sim.con Function to simulate from a constrained distribution
#' @param fn.sim.unc Function to simulate from an unconstrained distribution
#' @param demobase.est Parameter estimates associated with baseline demographic rates; 
#'   an array of dimension [npop, nd, npars], where "npop" is the number of subpopulations,
#'   "nd = afb + 2" is the number of demographic rates, and "npars" is the number of 
#'   parameters associated with the model for each demograpic rate
#' @param inipop.counts Initial counts; a matrix of dimension [na, npop], where "na = afb + 1"
#'   is the number of ages
#' @param impacts.demochange.mean Mean of the change in demographic rates associated with each
#'   scenario; an array of dimension [nscen, npop, nd]
#' @param impacts.demochange.se Standard error associated with "impacts.demochange.mean"
#' @param impacts.abschange.mean Mean of absolute harvest associated with each
#'   scenario; an array of dimension [nscen, npop, na]
#' @param impacts.abschange.se Standard error associated with "impacts.abschange.mean"  
#' @param impacts.infillpops Should the absolute harvest be split between subpopulations,
#'    automatically using "split.deaths" 
#' @param impacts.infillages Should the absolute harvest be split between ages,
#'    automatically using "split.deaths" 
## ############################################################################
#' @return A data frame with one row for each combination of Scenario, Year and Age. 
#' 
#'   Outputs are provided for all scenarios (there are "inputs$nscen" scenarios, with names equal to "inputs$impacts.scennames", 
#'   if  "inputs$include.baseline = FALSE"; if "inputs$include.baseline = TRUE" there are "nscen + 1" scenarios,
#'   with the first scenario bein "baseline"). 
#'   
#'   Outputs are provided for all ages (with ages above "afb"
#'   lumped together) if "output.byage = TRUE" and for breeding adults only if "output.byage = FALSE".
#' 
#'   Outputs are provided for all years inbetween "inputs$output.year.start" and "inputs$output.year.end", inclusive.
#'   
#'   The data frame contains columns:
#'   "Scenario": scenario name; "Year": year; "Age": age; 
#'   "Baseyear": year that impacts are calculated relative to
#'     (equal to "inputs$impacts.year.start - 1" if "nscen > 0", and to "NA" if "nscen = 0");
#'   "Impacted": is the impact operating in this year? (TRUE or FALSE);
#'   "Impact.year": years since start of the impact (equal to NA for years prior to "inputs$impact.year.start" or if "nscen = 0");
#'   "popsize.mean": mean projected population size;
#'   "popsize.sd": standard deviation of projected population size;
#'   "popsize.median": median projected population size;
#'   "popsize.qX%": quantiles of projected population size, for a range of different values of X,
#'   "m1.median", "m1.mean", "m1.sd", "m2.median", "m2.mean", "m2.sd", "m3", "m4", "m5", "m6":
#'    impact metrics; the values in these columns will be missing (NA) if "Impact.year" is missing (NA).
## ############################################################################
## Version 4.2: added "nburn" as an additional argument, and revised code to 
##   include "burn-in" period
## ############################################################################
#' @seealso A function that is directly called by the user. Calls the internal functions
#'   \code{\link{leslie.calcs}}, \code{\link{inits.burned}} and \code{\link{make.impactmetrics.table}}
#' @export

nepva.sim <- function(fn.sim.con, fn.sim.unc, ddfn,
                      model.demostoch, model.prodmax, afb, mbs, 
                      npop, nscen, sim.n, sim.seed, 
                      year.first, nburn,
                      inipop.years, inipop.counts, 
                      demobase.ests, demobase.cormat, demobase.bskippc,
                      impacts.year.start, impacts.year.end,
                      impacts.scennames, impacts.matchscens, 
                      impacts.demochange.mean, impacts.demochange.se,
                      impacts.abschange.mean, impacts.abschange.se,
                      impacts.infillpops, impacts.infillages,
                      output.year.start, output.year.end,
                      output.agetype, output.popsize.qe, output.popsize.target,
                      silent, output.raw, noround){
       
  ## #########################################
  ## Print out
  ## Version 4.2: added "output.raw" argument
  
  if(! silent){
  
    print.noquote("-------------------------------------------------")
  
    print.noquote("Running the NE PVA tool, Version 4.17")
  
    print.noquote("-------------------------------------------------")
  
    print.noquote(paste("Checking and pre-processing...     ", date()))
  }
  
  ## ############################################
  ## Stage 1. Set of years to run PVA for
  ## Completely rewritten for Version 0.8, revisions Version 1.8
  ## ############################################
  
  year.first <- min(inipop.years) ## begin PVA in year within first initial count
  
  years <- year.first : output.year.end ## run PVA through until "output.year.end"
  
  nyears <- length(years)
  
  ## ############################################
  ## Added Version 1.8: reference "baseline" year against which impact metrics are calculated
  ##   -- fixed to be the year before the impact begins (NOTE: a key assumption of the package
  ##      is that impacts apply to the same set of years for all scenarios and subpopulations, 
  ##      which means that it is also reasonable to use the same "baseref.year" for all scenarios
  ##      and subpopulations
  
  ## baseref.year <- impacts.year.start - 1 ## v4.15: this is now set later, at stage 9c
  
  ## #################################################################
  ## Stage 2. Relative years within PVA
  ## ############################################
  
  inipop.relyears <- inipop.years - year.first + 1
  
  impacts.relyears <- (impacts.year.start:impacts.year.end) - year.first + 1
  
  ## baseref.relyear <- baseref.year - year.first + 1 ## v4.15: this is now set later, at stage 9c

  output.relyears <- (output.year.start:output.year.end) - year.first + 1

  ## #################################################################
  ## Step 3. Number of ages and demographic rates
  
  ## ############################################
  ## Number of age classes:
  ## 0 (chicks), 1,...(afb-1) (immatures), afb+ (adults)
  ## #########################################
  
  na <- afb + 1
  
  ## #########################################
  ## Number of demographic rates to specify
  ##   Demographic rates are organized as:
  ##   [1] Productivity
  ##   [2:na] age-specific immature survival (0 to 1,1 to 2,(afb-1) to afb)
  ##   [nd] adult survival (afb+)
  ## Note: key assumption here is that adult survival (above age "afb") is independent of age
  ## #########################################
  
  nd <- afb + 2 ## note: could equivalently define as "nd = na + 1"

  ## #################################################################
  ## Step 4. Set seed
  
  set.seed(sim.seed)
  
  ## #################################################################
  ## Step 5. Random number generation for baseline simulations
  ## Added Version 0.4, 2 Nov 2018
  ## Note: generating these in advance helps to ensure that matching works correctly
  ## ############################################
  
  if(is.null(demobase.cormat)){
    
    ## Version 0.6: changed from "runif" to "nnorm":
    
    ru.base <- array(dim=c(npop, nyears, sim.n, nd), 
                     data = rnorm(npop * nyears * sim.n * nd))
  }
  else{
    
    ru.base <- array(dim=c(npop, nyears, sim.n, nd))
    
    for(i in 1:npop){
      
      for(j in 1:nyears){
        
        ## Version 0.6: removed "pnorm"
        
        ru.base[i,j,,] <- rmvnorm(sim.n, sigma=demobase.cormat)
      }
    }
  }
  
  ## #################################################################
  ## Step 6. Simulate skipped breeding
  ## Version 1.1: added "ru.skip"; modified Version 1.8 
  
  if(is.null(demobase.bskippc)){ demobase.bskippc <- c(0,0) }
  
  ## (note: running it this way ensures that matching is preserved even
  ##   if demobase.bskippc is zero in some runs and non-zero in others)
  
  sim.bskippc <- array(dim=c(nyears, sim.n), 
                       data = rnorm(nyears * sim.n, demobase.bskippc[1], demobase.bskippc[2]))
  
  ## #################################################################
  ## Step 7. Simulate magnitudes of impacts
  ## Revised Version 1.8
  ## ############################################
  
  if(impacts.matchscens){
    
    ru.scen <- array(dim=c(nscen, npop, sim.n, nd))
    
    ## note: "rnorm" is run BEFORE looping over scenarios, to ensure matching
    
    ro <- rnorm(npop * sim.n * nd) 
    
    for(i in 1:nscen){
      
      ru.scen[i,,,] <- ro
    }
    
  }
  else{
    
    ru.scen <- array(dim=c(nscen, npop, sim.n, nd), data = rnorm(nscen * npop * sim.n * nd))
  }
  
  impacts.demochange <- array(dim=c(nscen, npop, sim.n, nd))

  impacts.abschange <- array(dim=c(nscen, npop, sim.n, na)) ## NOTE: final dimension differs from "impacts.demochange"
  
  for(i in 1:nscen){
  
    for(j in 1:npop){
      
      for(k in 1:nd){
        
        impacts.demochange[i,j,,k] <- impacts.demochange.mean[i,j,k] + impacts.demochange.se[i,j,k] * ru.scen[i,j,,k]
        
        ## NOTE: a restriction is needed (k < nb) because relative impacts relate to rates (of which there 
        ##  are "nd"), whereas absolute impacts relate to age classes (of which there are "na = nd - 1");
        ##  important to double check that this is correct - i.e. that it is the final class that is not used
        
        if(k < nd){ 
     
          impacts.abschange[i,j,,k] <- impacts.abschange.mean[i,j,k] + impacts.abschange.se[i,j,k] * ru.scen[i,j,,k]
        }
      }
    }
  }
  
  impacts.abschange <- round(impacts.abschange) ## to ensure the number of birds killed in each category is an integer

  ## #########################################
  ## Step 9a. Running burn-in - added Version 4.2
  
  ## !! Note: the order of dimensions here is different from that in "nbyage"
  ##      - this is because the order in "inipop.counts" was already different
   
  inipop.origcounts <- array(dim=c(na, sim.n, npop))
  
  for(z in 1:sim.n){ inipop.origcounts[,z,] <- inipop.counts }
  
  ## ####################
  
  if(nburn < 1){ ## no burn-in in this case
    
    inipop.newcounts <- inipop.origcounts
    
    nbyage.burned <- NULL
  }
  else{ ## do use a burn-in, of length "nburn" years

    if(! silent){
    
      print.noquote("-------------------------------------------------")
    
      print.noquote(paste("Running burn-in...     ", date()))
      print.noquote("        ")
    }
  
    ## Number of years to use for burn-in: this is the value of "nburn" specified by the user,
    ##  plus the number of years needed for all subpopulations to have been initialized:
    
    nburn.adj <- max(inipop.relyears) + nburn
    
    ## Run burn-in: a baseline-only run of the Leslie matrix models, for "nburn.adj" years
  
    nbyage.burned <- leslie.calcs(npop = npop, nyears = nburn.adj, na = na, 
                               inipop.counts = inipop.origcounts, inipop.relyears = inipop.relyears, 
                               demobase.ests = demobase.ests,
                               ddfn = ddfn, fn.sim.con = fn.sim.con, fn.sim.unc = fn.sim.unc,
                               model.demostoch = model.demostoch, model.prodmax = model.prodmax,
                               ru.base = ru.base, 
                               sim.bskippc = sim.bskippc, 
                               sim.n = sim.n, afb = afb, mbs = mbs, noround = noround,
                               nscen = 1, impacts.relyears = 10000:10001,
                               impacts.demochange = NULL, impacts.abschange = NULL,
                               impacts.infillpops = NULL, impacts.infillages = NULL,
                               silent = silent)
    
    ## Total initial population size for each subpopulation:
    
    inipop.totals <- apply(inipop.counts, 2, sum)
    
    ## Use output from burn-in to create age-specific initial counts:
    
    inipop.newcounts <- inits.burned(nbyage.burned = nbyage.burned, inipop.totals = inipop.totals)
  }  
  
  ## #########################################
  ## Step 9b. Run main PVA calculations
  ## Loop over scenarios, and colonies, and run "nepva.sim"
  
  ## Version 0.4: change so that random uniform numbers are already generated,
  ##   and are inputted here
  ## - means that "demobase.cormat", "sim.n" and "sim.seed" no longer need to be
  ##    inputs to "nepva.sim"
  ##
  ## Version 4.2: revised to: 
  ##    a) use "leslie.calcs" function
  ##    b) use "inipop.newcounts"
  ## #########################################
  
  if(! silent){
    
    print.noquote("-------------------------------------------------")
  
    print.noquote(paste("Running main PVA calculations...     ", date()))
    print.noquote("        ")
  }
  
  nbyage <- leslie.calcs(npop = npop, nyears = nyears, na = na, 
                         inipop.counts = inipop.newcounts, inipop.relyears = inipop.relyears, 
                         demobase.ests = demobase.ests,
                         ddfn = ddfn, fn.sim.con = fn.sim.con, fn.sim.unc = fn.sim.unc,
                         model.demostoch = model.demostoch, model.prodmax = model.prodmax,
                         ru.base = ru.base, 
                         sim.bskippc = sim.bskippc, 
                         sim.n = sim.n, afb = afb, mbs = mbs, noround = noround,
                         nscen = nscen, impacts.relyears = impacts.relyears,
                         impacts.demochange = impacts.demochange,
                         impacts.abschange = impacts.abschange,
                         impacts.infillpops = impacts.infillpops,
                         impacts.infillages = impacts.infillages, silent = silent)

  ## Plot convergence of burn-in - added Version 4.2: 
  
  if(! silent){
  
    par(ask = TRUE)
    plot.burnconv(nbyage, nbyage.burned = nbyage.burned, year.first = min(years), afb = afb)
    par(ask = FALSE)
  }
  
  ## ####################################################
  ## Stage 9c. Baseline reference year
  ## Added Version 4.7: fixed "baserefyear" to try to work correctly when doing baseline-only runs
  
  if(all(impacts.scennames == "baseline")){ ## Added Version 4.7
    
    baserefyear <- max(inipop.years) ## v4.15: bug fix: changed "inipop.relyears" to "inipop.years"
  }
  else{
    
    baserefyear <- (impacts.year.start - 1)
  }
  
  ## ####################################################
  ## Stage 10: Aggregate output - sum across populations
  ## Extract number of birds for relevant age classes, and sum across subpopulations:
  ## Rewritten Version 4.7 to a) add option for "whole population";
  ##                          b) remove M5-M7 calculations for individual age classes
  ##                          c) add "whole population" results when running individual age classes
  
  ## print(output.agetype)
  
  if(output.agetype == "age.separated"){
 
    if(afb == 1){ ## Version 4.8 - clause added to deal with "afb = 1"
      
      immat.names <- NULL
    }
    else{
      
      immat.names <- paste0("immatures.age", 1:(afb - 1))
    }
      
    age.names <- c("chicks", immat.names, "breeding.adults", "whole.population") ## names of age classes

    popsize.qe <- popsize.target <- rep(NA, na + 1)
    
    if(! is.null(output.popsize.qe)){ popsize.qe[na+1] <- output.popsize.qe }
    
    if(! is.null(output.popsize.target)){ popsize.target[na+1] <- output.popsize.target }
 
    aggvals <- array(dim=c(dim(nbyage)[c(1,3,4)],na+1))
    
    aggvals[,,,1:na] <- apply(nbyage[,,,,1:na,drop=FALSE], c(1,3,4,5), sum)
    
    aggvals[,,,na+1] <- apply(nbyage[,,,,,drop=FALSE], c(1,3,4), sum)
  }
  else{

    age.names <- output.agetype
   
    popsize.qe <- popsize.target <- NA
    
    if(! is.null(output.popsize.qe)){ popsize.qe <- output.popsize.qe }
    
    if(! is.null(output.popsize.target)){ popsize.target <- output.popsize.target }
    
    if(output.agetype == "whole.population"){
    
      aggvals <- array(dim=c(dim(nbyage)[c(1,3,4)],1))
    
      aggvals[,,,1] <- apply(nbyage[,,,,,drop=FALSE], c(1,3,4), sum)
    }
    
    if(output.agetype == "breeding.pairs" | output.agetype == "breeding.adults" ){
      
      aggvals <- apply(nbyage[,,,,na,drop=FALSE], c(1,3,4,5), sum)
    }
    
    if(output.agetype == "breeding.pairs"){
      
      aggvals <- aggvals * (1/2) ## conversion factor that needs to be applied: ## Version 2.6: changed "2" to "1/2"
    } 
  }

  ## ####################################################
  ## Stage 11. Create impact metrics table
  
  ## Version 0.5: added "popsize.qe" and "popsize.target" arguments
  ## Version 0.8: changed to use "baseref.relyear", and "nyears"
  ## Version 4.7: rewritten to use "popsize.qe" rather than "output.popsize.qe", and ditto for ".target"
 
  if(is.null(aggvals)){ browser() }
  
  out <- make.impactmetrics.table(ntot = aggvals, scen.names = impacts.scennames, 
                                  age.names = age.names, 
                                  year.first = year.first, impacts.year.start = impacts.year.start,
                                  impacts.year.end = impacts.year.end,
                                  baserefyear = baserefyear,
                                  output.year.start = output.year.start,
                                  output.year.end = output.year.end,
                                  popsize.qe = popsize.qe, 
                                  popsize.target = popsize.target)  

  ## browser()
  
  ## ####################################################
  ## Stage 12. Raw outputs - added Version 3.1
  
  if(output.raw){
  
    out <- list(tab = out)
    
    ## 'nbyage' is an array: [nscen, npop, nyears, sim.n, na]

    out$raw <- list(impact.scennames = impacts.scennames, npop = npop, years = years,
                   sim.n = sim.n, afb = afb, nbyage = nbyage)
  }
  
  ## ####################################################
  
  out
}

## ###################################################################################################################
## >> leslie.calcs
## ############################################################################
#' @title Update Leslie matrix model for all years, scenarios and subpopulations
## ############################################################################
## Version 0.8: changed to use "inipop.relyears+1" rather than "2"
## Version 1.2: changed order to loop over time first...
## Version 1.8: changed order of loops: now: scenario > time > population
## Version 4.2: changed so that code is now within new "leslie.calcs" function
##   (previous was all just in "nepva.sim")
## Version 4.2: also changed so that counts are initialized to be zero, NOT "NA"
## ############################################################################
#' @inheritParams nepva.sim
#' @param demobase.ests Parameter estimates for the model associated with each demographic rate;
#'   a matrix of dimension equal to [npop, afb+2, npars], where "npars" denotes the number of parameters
#'   associated with the parametric model selected using "model.dd", "model.envstoch" and "model.prodmax".
#'   The first row corresponds to "productivity", the next "afb" rows to survival rates for each
#'   age from 0 up to afb, and the final row to annual survival rates for individuals beyond age "afb"
#' @param impacts.demochange Impacts of each scenario on demographic rates for each subpopulation. 
#'   A four-dimensional array of numeric values of dimension [nscen, npop, sim.n, afb+2].
#' @param impacts.abschange Impacts of the scenario on numbers in each age class, as an absolute harvest.
#'   A four-dimensional array of numeric values of dimension [nscen, npop, sim.n, afb+1]..
#' @param impacts.relyears Years, relative to the year associated with the first initial count, 
#'   for which the PVA should be run. A vector of integer values.
## ############################################################################
#' @return A five-dimensional array, of dimension [nscen, npop, nyears, sim.n, afb+1],
#' containing the simulated number of individuals of
#'  each age (0,1,...,afb+), post-breeding (i.e. at the end of the current breeding season), for
#'  each of "sim.n" simulation runs, for each year, subpopulation and scenario
## ############################################################################
#' @export

leslie.calcs <- function(npop, nyears, na, inipop.counts, inipop.relyears, 
                         demobase.ests, ddfn, fn.sim.con, fn.sim.unc,
                         model.demostoch, model.prodmax, ru.base, 
                         sim.bskippc, sim.n, afb, mbs, noround, nscen, 
                         impacts.relyears, impacts.demochange, impacts.abschange,
                         impacts.infillpops, impacts.infillages, silent){
  
  ## ############################################
  ## Initialize values - NOTE: changed Version 4.2 so values are initialized to zero,
  ##   not NA. This avoids some technical complexities in dealing with missing values,
  ##   but means resulting values need to be treated carefully, as colony sizes are 
  ##   assumed to be zero prior to initialization (which is clearly not plausible - 
  ##   it is only valid to do this because these values are never actually used in calculations)
  ##
  ## v4.17: changed so that outputs become NA after failure to split occurs
  ## ############################################
  
  nbyage <- array(dim=c(nscen, npop, nyears, sim.n, na), data = 0) 
  
  for(i in 1:nscen){
  
    tt.fail <- FALSE ## added v4.17
    
    ## #########################################
    ## Set up output, and initialize
    ## ############################################
    
    for(j in 1:npop){
      
      for(a in 1:na){ 
        
        ## Version 3.3: bug fix: "inipop.relyears" > "inipop.relyears[j]"
        ## Version 4.2: "inipop.counts[a,j]" changed to "inipop.counts[a,,j]"
        ##    (since now also includes "sim.n" as a dimension)
        
        nbyage[i,j,inipop.relyears[j],,a] <- inipop.counts[a,,j] ## 30 Oct 2018: changed from "a" to "a+1"
      }
    }
    
    ## ############################################
    
    for(tt in 2:nyears){

      if(tt.fail){ ## added v4.17
  
        nbyage[i,,tt,,] <- NA
      }
      else{
        
        ## ######################################################
      
        if(! silent){
        
          print.noquote(paste0("Scenario:", i, " Year: (", tt, "/",nyears,")    ", date()))
        }
      
        ## ######################################################
        ## Is the impact active in this year
      
        impacted <- (tt >= min(impacts.relyears)) & (tt <= max(impacts.relyears))
        
        ## ######################################################
        ## Number of birds from previous year: array of [subpopulations, years, ages]
      
        nprev <- d(nbyage[i,,tt-1,,,drop=FALSE], c(2,4,5))
      
        ## print(paste(nprev[1,1,], collapse=" "))
        
        ## ######################################################
        ## Simulate scenarios
      
        if(impacted){
        
          impacts.demochange.tt <- d(impacts.demochange[i,,,,drop=FALSE], 2:4)
        
          impacts.abschange.tt <- d(impacts.abschange[i,,,,drop=FALSE], 2:4)
        
          if(any(nprev < 0)){print("ZIT"); browser()}
        
          impacts.abschange.tt <- try(split.deaths(impacts.abschange = impacts.abschange.tt,
                                                   nprev = nprev, 
                                                   impacts.infillpops = impacts.infillpops,
                                                   impacts.infillages = impacts.infillages), silent = TRUE)
        }
        else{
        
          impacts.demochange.tt <- NULL
        
          impacts.abschange.tt <- NULL
        }
      
        ## ##################################################
        ## Loop through subpopulations and update size of each
      
        if(inherits(impacts.abschange.tt, "try-error")){
        
          print.noquote("Leslie matrix calculations truncated prematurely, due to failure to split absolute harvest!")
        
          nbyage[i,,tt,,] <- NA ## added v4.17
          
          tt.fail <- TRUE ## added v4.17
        }
        else{ ## "else" added v4.17
        
          for(j in 1:npop){
        
           ## ##################################################
           ## Leslie matrix updating for each subpopulation
          
           if(tt > inipop.relyears[j]){
          
           ## if(any(is.na(mean(apply(nbyage[i,j,tt-1,,,drop=FALSE],2,sum))))){browser()} ## Version 2.6: Added drop=FALSE
          
           ## print(paste(i,"  ", j, "   ", tt,"         ",mean(apply(nbyage[i,j,tt-1,,],1,sum))))

           nbyage.prev <- apply(nbyage[i,j,tt-1,,,drop=FALSE], 4:5, identity) ## Added Version 2.6, to get right # dimensions
          
           ## if(any(is.na(nbyage.prev)|is.nan(nbyage.prev))){ browser() }
          
           ruij.base <- apply(ru.base[j,tt,,,drop=FALSE], 3:4, identity) ## Added Version 2.6, to get right # dimensions
          
           ret3minus1 <- function(y,m){ out <- y ;  if(! is.null(y)){ out <- apply(y[m,,,drop=FALSE], 2:3, identity) } ; out }
          
            ret2minus1 <- function(y,m){ out <- y ;  if(! is.null(y)){ out <- apply(y[m,,drop=FALSE], 2, unique) } ; out }
          
            impacts.demochange.ij <- ret3minus1(impacts.demochange.tt, m = j) ## Tidied Version 4.2
          
            impacts.abschange.ij <- ret3minus1(impacts.abschange.tt, m = j) ## Tidied Version 4.2
          
            simbsk <- ret2minus1(sim.bskippc, m = tt) ## Version 2.6: bug fix; tidied Version 4.2; bug fix Version 4.3
 
            nbyage[i,j,tt,,] <- leslie.update(demobase.ests = demobase.ests[j,,],
                                            nbyage.prev = nbyage.prev,
                                            ddfn = ddfn,
                                            fn.sim.con = fn.sim.con,
                                            fn.sim.unc = fn.sim.unc,
                                            model.demostoch = model.demostoch, 
                                            model.prodmax = model.prodmax,
                                            ru.base = ruij.base, 
                                            impacts.demochange = impacts.demochange.ij,
                                            impacts.abschange = impacts.abschange.ij,
                                            sim.bskippc = simbsk, impacted = impacted,
                                            sim.n = sim.n, afb = afb, mbs = mbs, noround = noround)
           }
             
           ## ##################################################
          }
        }
      }
    }
    
    ## ##################################################
  }
  
  ## ##################################################
  
  nbyage
}

## ###################################################################################################################
## >> inits.burned
## ############################################################################
#' @title Calculate initial population sizes for a "full" run of the Leslie matrix
#'   PVA, based on outputs from a burn-in PVA
## ############################################################################
## Created 12 August 2019, last modified 12 August 2019
## ############################################################################
#' @param nbyage.burned Number of birds in a baseline simulation for a burn-in period.
#'   An array of dimension [nscen, npop, nburn.adj, sim.n, na], where 
#'     'nscen' = number of simulations (must be one for this function to make sense);
#'     'npop' = number of subpopulations
#'     'nburn.adj' = number of years in burn-in period
#'     'sim.n' = number of simulation runs
#'     'na' = number of age classes within Leslie matrix model
#' @param inipop.totals Total initial population size (summed across all ages);
#'     a vector of length 'npop', containing one (integer) value for each subpopulation 
## ############################################################################
#' @return An array of dimension [na, sim.n, npop], containing the initial population
#'     sizes for each age class, in each simulation, in each subpopulation
## ############################################################################
#' @export

inits.burned <- function(nbyage.burned, inipop.totals){
  
  ## ########################################
  ## Extract dimensions
  
  npop <- dim(nbyage.burned)[2]
  
  nburn.adj <- dim(nbyage.burned)[3]
  
  sim.n <- dim(nbyage.burned)[4]
  
  na <- dim(nbyage.burned)[5]
  
  ## ########################################
  ## Simulated number of birds in each combination age class, subpopulation and simulation run,
  ##   in the final year of the burn-in period
  
  nbyage.burn <- apply(nbyage.burned[1,,nburn.adj,,,drop=FALSE], c(2,4,5), unique)
  
  ## ########################################
  ## If there are zero values during the burn-in then it cannot be used to fix the age structure
  
  if(any(nbyage.burn == 0)){ stop("Error! Zero values during burn-in...") } ## Version 4.4: error message updated
  
  ## ########################################
  ## Calculate proportion of birds in each age class, for each subpopulation and simulation run
  
  pbyage.burn <- apply(nbyage.burn, 1:2, function(x){x/sum(x)})
  
  ## ########################################
  ## Multiply these by total initial counts for each subpopulation, in order to obtain age-specific
  ##  initial counts
  
  out <- array(dim = c(na, sim.n, npop))
  
  for(i in 1:npop){ for(z in 1:sim.n){ 
    
    out[,z,i] <- round(inipop.totals[i] * pbyage.burn[,i,z])
  }}
 
  ## ########################################
  
  out
}

## ###################################################################################################################
## >> leslie.update
## ############################################################################
#' @title Update Leslie matrix model for a single year and subpopulation
## ############################################################################
#' @inheritParams nepva.sim
#' @param demobase.ests Parameter estimates for the model associated with each demographic rate;
#'   a matrix of dimension equal to [afb+2, npars], where "npars" denotes the number of parameters
#'   associated with the parametric model selected using "model.dd", "model.envstoch" and "model.prodmax".
#'   The first row corresponds to "productivity", the next "afb" rows to survival rates for each
#'   age from 0 up to afb, and the final row to annual survival rates for individuals beyond age "afb"
#' @param nbyage.prev Number of birds of each age in each simulation in the previous year. 
#'   An array of numeric values dimension [sim.n, afb+2].
#' @param ru.base Random numbers simulated from a standard normal distribution,
#'   associated with baseline simulations. An array of numeric values dimension [sim.n, afb+2].
#' @param impacts.demochange Impacts of the scenario on demographic rates. 
#'   An array of numeric values dimension [sim.n, afb+2].
#' @param impacts.abschange Impacts of the scenario on numbers, as an absolute harvest.
#'   An array of integers of dimension [sim.n, afb+2].
#' @param sim.bskippc Simulated sercentage of birds that skip breeding this year: a vector of 
#' length "sim.n"
#' @param sim.n Number of simulations. An integer.
#' @param impacted Is the scenario current active? A logical value: TRUE/FALSE.  
## ############################################################################
#' @return A matrix, of dimension [sim.n, afb+1], containing the simulated number of individuals of
#'  each age (0,1,...,afb+), post-breeding (i.e. at the end of the current breeding season), for
#'  each of "sim.n" simulation runs
## ############################################################################
#' @export

leslie.update <- function(demobase.ests, nbyage.prev, 
                          ddfn, fn.sim.con, fn.sim.unc,
                          model.demostoch, model.prodmax, 
                          ru.base, impacts.abschange, impacts.demochange,
                          sim.bskippc, impacted, afb, mbs, sim.n, noround){

  ## if(any(demobase.ests[,,2] < 0)){ print("TIM!") ; browser() ; stop("Missing population sizes in output!") } ## error message added Version 4.4
  
  ## #################################################################
  ## "Maybe round": added Version 3.2 to allow option to run without rounding
  
  if(noround){
    
    mayberound <- identity
  }
  else{
    
    mayberound <- round
  }
  
  ## #################################################################
  ## Dimensions
  
  na <- afb + 1 ## number of ages [0,...,afb-1, afb+], where "afb+" is "breeding adults of age afb or more"
  nd <- afb + 2 ## number of demographic rates
  
  npars <- ncol(demobase.ests) ## number of model parameters

  ## #####################
  ## Create a blank object to read outputs into - population size for each
  ##  simulation, of each age
  
  nbyage <- array(dim=c(sim.n, na), data = NA)
    
  ## #####################################################################
  ## STAGE 1. Annual over-winter survival of adults and chicks since previous winter
  
  ## #####################
  ## Step 1a. Survival in the absence of any impacts
  ##' QUERY: "popsize" for density dependence is now number of breeding adults - is that correct?
    
  ## Number of breeding adults surviving from the previous winter
  
  nadult.prev <- nbyage.prev[,na] ## Added Version 1.8
  
  p.surv <- array(dim=c(sim.n, na)) ## Probabilities of survival
    
  for(k in 1:na){
      
    ## "Simulate" survival probabilities:
    
    psim <- fn.sim.con(ru = ru.base[,k+1], pars = as.numeric(c(demobase.ests[k+1,])), 
                       popsize = nadult.prev, ddfn = ddfn) 
      
    ## Version 4.6: added error message:
    if(any(is.nan(psim)|is.na(psim))){ stop("Invalid survival probabilities simulated!")}
    
    ## If there are no adults alive, automatically set 
    ##   all survival probabilities to zero:
    
    psim[nadult.prev == 0] <- 0 ## Added Version 0.7
      
    p.surv[,k] <- psim
  }
  
  ## #####################
  ## Step 1b. Modify survival rate to account for impact scenario
  ## Modified Version 1.2 (4 Jan 2019) to account for absolute harvest
  ## The "-1" is because "impacts.demochange" containing impacts on productivity
  ##  in the first column, so it is only the other (nd-1) columns that are needed here
  
  if(impacted){ ## only apply if the year is within the period of impact
    
    p.surv <- p.surv - impacts.demochange[,-1] ## Version 1.8: added "-1"
  }   
    
  ## #####################
  ## Step 1c. Fix invalid survival rates
    
  p.surv[p.surv < 0] <- 0  ## Added Version 0.5 ## Changed Version 0.7
    
  p.surv[p.surv > 1] <- 1 ## Changed Version 0.7
 
  ## #####################
  ## Step 1d. Simulate number of individuals surviving from previous winter
    
  ## Individuals that were previously chicks (age 0) or immatures ages 1,...,(afb-1):
  
  for(a in 1:afb){
      
    if(! model.demostoch){
        
        nbyage[,a+1] <- mayberound(nbyage.prev[,a] * p.surv[,a]) 
    }
    else{
        
        nbyage[,a+1] <- rbinom(sim.n, nbyage.prev[,a], p.surv[,a])
    }
  }
    
  ## if(any(is.na(nbyage[,-1]))){ print("NOOOOOOOOOO") ; browser() }
  
  ## Individuals that were previously adults (age "afb" or higher):
  
  if(! model.demostoch){
      
    nextra <- mayberound(nbyage.prev[,na] * p.surv[,na]) 
  }
  else{
      
    nextra <- rbinom(sim.n, nbyage.prev[,na], p.surv[,na])
  }
  
  ## Sum together existing adults and newly recruited adults:
  
  frot <- nbyage[,na,drop=FALSE]
  
  ## Version 4.6: added error message
  if(max(frot) > 1e+08){ stop("Population size explosion - will lead to numerical overflow")}
  
  nbyage[,na] <- frot + nextra ## 4 Jan 2019: added " '- nremoved' term"
  
  ## #####################
  ## Stage 1e. Absolute harvest of adults and immatures
  ## Note that the absolute harvest is specified as a *reduction* in numbers,
  ##    unlike the change in demographic rates
  ##
  ## The "-1" is because chicks are not yet born, so the absolute harvest
  ##  of chicks is not yet applied
    
  if(impacted){ ## only apply if the year is within the period of impact
      
    nbyage[,-1] <- nbyage[,-1] - impacts.abschange[,-1]
  }
    
  ## #####################################################################
  ## STAGE 2. NUMBER OF BREEDING PAIRS
  
  ## #####################
  ## Step 2a. Calculate number of potential breeding birds
    
  ntotal.breedpairs <- mayberound(nbyage[,na] / 2) ## Version 1.8: bug fix 
    
  ## #####################
  ## Step 2b. Simulate actual number of breeding pairs,
  ##   based on number that skip breeding
  ## Added Version 1.1, on 4 Jan 2019
  ## Version 2.7: moved
  ## Version 4.1: added "floor"
  
  ntotal.actualbreedpairs <- floor(ntotal.breedpairs * (1 - (sim.bskippc/100)))
  
  ## #####################
  ## Step 2c. Number of adults
  ## Version 2.7: changed from "ntotal.breedpairs" to "ntotal.actualbreedpairs"
  ## QUERY: should this be based on "ntotal.breedpairs" or "ntotal.actualbreedpairs"?
  
  ntotal.adults <- ntotal.actualbreedpairs * 2 

  ## ntotal.all <- apply(nbyage[,-1,drop=FALSE], 1, sum) ## total number of all individuals
 
  ## #####################################################################
  ## STAGE 3. NUMBER OF CHICKS BORN
  
  ## #####################
  ## Stage 3a. Productivity  rates in the absence of impacts
  ## Version 1.9: density dependence effect changes from "ntotal.all" to "ntotal.adults"
    
  if(model.prodmax){
      
    p.prod <- fn.sim.con(ru = ru.base[,1], pars = as.numeric(c(demobase.ests[1,])), popsize = ntotal.adults, ddfn = ddfn)
  }
  else{
    
    ## Bug fix Version 3.2
    p.prod <- fn.sim.unc(ru = ru.base[,1], pars = as.numeric(c(demobase.ests[1,])), popsize = ntotal.adults, ddfn = ddfn)
  }
    
  ## Version 4.6: added error message:
  if(any(is.nan(p.prod)|is.na(p.prod))){ stop("Invalid productivity rates simulated!")}

  p.prod[ntotal.adults == 0] <- 0 ## Added Version 0.7
    
  ## #####################
  ## Stage 3b. Modify productivity rate to account for scenario
  ## Version 2.7: bug fix: "impacts.demochange" not "impacts.abschange"
    
  if(impacted){
      
    p.prod <- p.prod - impacts.demochange[,1]
  }
    
  ## #####################
  ## Stage 3c. Fix invalid productivity rates
    
  p.prod[p.prod < 0] <- 0 ## Added Version 0.5, changed Version 0.7
    
  if(model.prodmax){ ## Added Version 2.7
  
    p.prod[p.prod > 1] <- 1 ## Changed Version 0.7
  }
  
  ## print.noquote(paste(round(mean(p.surv),3), "...", round(mean(p.prod),3)))
 
  ## #####################
  ## Stage 3d. Simulate number of individuals born
  ## Version 1.1: modified to use "actualbreedpairs" rather than "breedpairs"
    
  if((! model.demostoch) & (model.prodmax)){
     
    nbyage[,1] <- ntotal.actualbreedpairs * p.prod * mbs ## Version 2.6: bug fix: added "* mbs"
  }
  
  if((! model.demostoch) & (! model.prodmax)){ ## Bug fix: added Version 3.2
    
    nbyage[,1] <- ntotal.actualbreedpairs * p.prod ## Version 2.6: bug fix: added "* mbs"
  }
  
  if(model.demostoch & model.prodmax){
      
    nbyage[,1] <- rbinom(sim.n, ntotal.actualbreedpairs * mbs, p.prod)
  }
    
  if(model.demostoch & ! model.prodmax){
      
    ## Version 2.7: fix bug: "actualbreedpairs" not "breedpairs"
    
    nbyage[,1] <- rpois(sim.n, ntotal.actualbreedpairs * p.prod)
  }  
  
  ## #####################
  ## Stage 3e. Absolute harvest of chicks - added Version 2.7
  
  if(impacted){ ## only apply if the year is within the period of impact
    
    nbyage[,1] <- nbyage[,1] - impacts.abschange[,1]
  }
  
  ## #####################
  ## Calculate total population size (at end of breeding season)
  ## Version 1.9: no longer required
  
  ## ntotal.all <- apply(nbyage[,,drop=FALSE], 2, sum)
    
  ## if(any(is.na(ntotal.all) | is.nan(ntotal.all))){browser()}
  
  ## #####################
  
  ## print(paste(mean(p.surv), mean(p.prod)))
  
  if(any(is.na(nbyage))){ stop("Missing population sizes in output!") } ## error message added Version 4.4
  
  ## #####################
  ## Stage 3f. Get rid of negative values - added Version 4.6
  
  nbyage <- nbyage * (nbyage > 0)
  
  ## #####################
  
  nbyage
}

## ####################################################################
## >> split.deaths
## ####################################################################
#' @title Split the total number of bird killed into separate subpopulations and/or ages
#' @details The split is based upon the current number of birds in each subpopulation
#'   and/or age within this simulation run, scenario and year.
## ####################################################################
## Created Version 1.3, on 10 January 2019, 
##  rewritten Version 1.8 on 20 January 2019
## #################################################################
#' @inheritParams nepva.fullrun
#' @inheritParams nepva.sim
#' @param impacts.abschange Absolute harvest under a particular population
#'    a matrix of dimension [npop, sim.n, na] containing the number 
#'    of individuals in each combination of subpopulation and simulation run, for each age,
#'    at the previous time step. If "impacts.infillpops" the number killed will
#'    currently all be in subpopulation 1, with zeroes for all other subpopulations.
#'    If "impacts.infillages" the number killed will currently all be in ages 1 and (afb+1) 
#'    (chicks and adults), with zeroes for the intervening (immature) ages.
#' @param nprev A matrix of dimension [npop, sim.n, na] containing the number 
#'    of individuals in each combination of subpopulation and simulation run, for each age,
#'    at the previous time step
#' @param impacts.in    
## #################################################################
#' @return A matrix of dimension [nsim, npop, na], containing the simulated 
#'   impact of a particular scenario for each of "nsim" simulations 
#'   within each of "npop" subpopulations, for each of "na" ages. The structure
#'   and format is the same as the input "impacts.abschange", but the values have
#'   now been split by subpopulation and ages.
## #################################################################
#' @export

split.deaths <- function(impacts.abschange, nprev, impacts.infillpops, impacts.infillages){

  ## ##################################################
  ## ##################################################
  ## Extract dimensions
  
  npop <- dim(impacts.abschange)[1] ## Version 1.9: bug fix
  
  sim.n <- dim(impacts.abschange)[2] ## Version 1.9: bug fix
  
  na <- dim(impacts.abschange)[3] ## Version 1.9: bug fix

  ## ##################################################
  
  afb <- na - 1 ## Version 2.1: bug fix
  
  ## ##################################################
  ## Absolute harvest: split impact according to the relative 
  ##   size of the different subpopulations
  
  tmp <- impacts.abschange

  if(impacts.infillpops){

    ## ##################################
    
    for(j in 1:na){ ## loop over ages
      
      ## ###############
      ## Total number in each age class (matrix: [npop, sim.n])
      
      nprevs <- d(nprev[,,j,drop=FALSE], 1:2)
    
      sum.overpops <- c(apply(nprevs, 2, sum)) ## Total (summed across populations) in each simulation
       
      ## Added Version 4.6:
      if(any(sum.overpops == 0)){ stop("Cannot split absolute harvest - empty column sums...")}
      
      ## ###############
      ## Proportion of individuals in each simulation, within this age class,
      ##   that arise from each population i=1,...,npop:
      
      ppop <- array(dim=c(npop,sim.n))
      
      for(i in 1:npop){
      
        ppop[i,] <- nprev[i,,j] / sum.overpops
      }
      
      ## ###############
      ## Total number of birds killed
      
      nkilled <- impacts.abschange[1,,j]
      
      ## ###############
      ## Simulate number killed in each population
      
      for(k in 1:sim.n){
      
        if(nkilled[k] > 0){
        
          if(any(is.na(ppop[,k]) | ppop[,k] < 0)){ print("A") ; browser() }
          
          tmp[,k,j] <- c(rmultinom(1, nkilled[k], ppop[,k]))
        }
      }
      
      ## ###############
    }
  }

  impacts.abschange <- tmp
 
  ## ##################################################
  ## Absolute harvest: split impact according to the relative distribution of ages
  
  tmp <- impacts.abschange
  
  if(impacts.infillages){
    
    ## ##################################
    ## Splitting of immatures: Allocate immatures to individual 
    ##   ages based on prevalence in the subpopulation
    
    for(i in 1:npop){ ## loop over subpopulations
      
      ## ###############
      ## Total number in each population, summed across all individuals except chicks (hence "-1")
      
      nprevs <- d(nprev[i,,-1,drop=FALSE], 2:3)
      
      sum.overages <- c(apply(nprevs, 1, sum))

      ## Added Version 4.6:
      if(any(sum.overages == 0)){ stop("Cannot split absolute harvest - empty column sums...")}
      
      ## ###############
      ## Proportion of individuals in each simulation, within the current subpopulation,
      ##   that arise from each age class
      
      page <- array(dim=c(sim.n,afb))
      
      for(j in 1:afb){
        
        page[,j] <- nprev[i,,j+1] / sum.overages
      }
      
      ## ###############
      ## Total number of birds killed
      
      nkilled <- impacts.abschange[i,,afb+1]
      
      ## ###############
      ## Total number of birds killed (note: # chicks in unchanged)
      
      for(k in 1:sim.n){
        
        if(nkilled[k] > 0){
        
          if(any(page[k,] < 0 | is.na(page[k,]))){ print("B") ; browser() }
          
          tmp[i,k,-1] <- c(rmultinom(1, nkilled[k], page[k,]))
        }
      }
    }
  }
  
  impacts.abschange <- tmp
  
  ## ##################################################
  
  impacts.abschange
}
  
## ####################################################################
## ####################################################################
## >> make.impactmetrics.table
#' @title Produce a table of impact metrics and summaries of projected population sizes 
#' @description Make a table of impact metrics, and summaries of projected population size, 
#' associated with the outputs from a PVA
## ####################################################################
#' @details The function summarizes the PVA results for each combination of scenario, 
#'   year, and age; the set of years and ages to output are specified by the user
#' 
#' reports the mean, median and SD of projc
#' The metrics that are reported in the table are:
#' (m1) the ratio of impacted to unimpacted final population size in each future year - mean, median and SD across the set of simulations
#' (m2) the ratio of impacted to unimpacted growth rate - mean, median and SD across the set of simulations
#' (m3) quantile for unimpacted population which matches the 50th centile for the impacted population
## (-) the ratio of impacted to unimpacted trend
#' (m4) the probability of decline in population size, over specified future time intervals
#' (m5) the risk of extinction or quasi-extinction, over a specified future time interval
#' (m6) the probability that the population will have recovered to this level over any particular number of years
## (-) the number of years required for the population to recover a user specified target level, with a specified probability
## #############################################################
## Change log:
## - Created v0.2 on 25 October 2018, last modified 6 Nov 2018
## Based on "calc.pvam" from "ForthTay-SIPM-PVA-Protocol-v6-functions.R"
##
## - Version 0.5: added metrics M4-M7
##            : added arguments "popsize.qe", "popsize.target" 
## - Version 1.3: 1) changed to calculate for *all* years, not just some...
##   Changed "metric.period" to "year.basefinal" and "year.outfinal"
##              2) changed so that answers could be separated by ages, if requested
##              3) changed to add in mean and SD, as well as median, for metrics M1 and M2 
##              4) changed to add in outputs for additional quantiles
## - Version 1.3: ntot has dimension [nscen, nsim, nages]
## - Version 2.8: bug fixes
## - Version 4.7: added explicit "baserefyear" argument
##              : changed so that "popsize.qe" and "popsize.target" are vectors, and never NULL
## #############################################################
## ####################################################################
#' @inheritParams make.summary.table
#' @return A data frame with one row per combination of scenario, 
#'   year and age, so that there are a total of 
#'   length(scen.names)*length(age.names)*(output.year.end-year.first+1) rows.
#'   The data frame has 31 columns, which record (a) the scenario name,
#'   year and age, (b) whether the impact is active in the year being reported
#'   ("Impacted", a logical value), (c) the number of years since the start of the
#'   impact ("Impact.year", an integer; NA is the scenario corresponds to the baseline,
#'   or if the year being reported is prior to the start of the impact), (d) the
#'   "baseline" year against which metrics are calculated ("Baseyear", an integer),
#'   (e) the values of metrics M1-M6; for metrics M1 and M2 median, mean and SDs of
#'   the values are reported; (f) mean, median and SD of population size (e.g. "popsize.mean", etc.),
#'   and (g) a range of different quantiles of population size ("popsize.Q2.5%", etc.)
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.run}}.
#' @export

make.impactmetrics.table <- function(ntot, scen.names, age.names, 
                                     year.first, impacts.year.start, impacts.year.end,
                                     baserefyear,
                                     output.year.start, output.year.end,
                                     popsize.qe, popsize.target){
  
  ## #############################################################
  ## Output years - added Version 1.3, reordered Version 2.8
  
  output.years <- output.year.start:output.year.end ## Changed Version 1.9
  
  nout <- length(output.years)
  
  ## #############################################################
  ## Calculate relative years - added Version 1.3, reordered Version 2.8
  
  impacts.relyear.start <- impacts.year.start - year.first + 1
  
  impacts.relyear.end <- impacts.year.end - year.first + 1
  
  output.relyears <- output.years - year.first + 1 ## Changed Version 1.9
  
  ## #############################################################
  ## Sort out scenarios - which of the scenarios corresponds to a baseline
  
  ns <- length(scen.names) ## Number of scenarios
  
  kb <- which(scen.names == "baseline")
  
  ## #############################################################
  ## "Baseline reference - relative year" (year against which population growth
  ##   rate calculations are based)
  
  if(! is.null(kb)){
  
    brry <- baserefyear - year.first + 1 ## Changed Version 4.7
    
    ## brry <- impacts.relyear.start - 1 ## changed Version 0.8, renamed from "ib" in Version 1.3
  }

  ## #############################################################
  ## Sort out ages
  
  nages <- length(age.names) # Added Version 1.3
  
  ## Version 3.1: changed to add "..." statement (needed to extend to quantiles)
  fnrat <- function(x,y,fn,...){
    
    if(any(is.na(x) | is.na(y) | is.nan(x) | is.nan(y) | is.infinite(x) | is.infinite(y))){ ## Version 4.8 - added clause
      
      out <- NA
    }
    else{
    
      out <- fn((x/y)[y > 0],...) ## Version 1.3: generalized beyond "median"
    } 
    
    out
  }
  
  ## #############################################################
  
  out <- NULL
  
  for(j in 1:nages){ ## loop over ages

    for(i in 1:nout){ ## loop over years

      ## #############################################################
      ## Current year
      
      ic <- output.relyears[i]
              
      ## #############################################################
      ## Meta-information
      ## #########################################
      
      tmp <- data.frame(Year = rep(output.years[i], ns))
      
      tmp$Age <- age.names[j]
      
      tmp$Scenario <- scen.names
      
      ## ###################
      ## "Reference" year
      
      tmp$Baseyear <- baserefyear ## (output.years[i] == (impacts.year.start - 1)) ## Changed Version 4.7
      
      ## ###################
      ## Is the impact currently operating? (TRUE/FALSE)
      
      tmp$Currently.Impacted <- (output.years[i] >= impacts.year.start) & (output.years[i] <= impacts.year.end)
      
      tmp$Currently.Impacted[tmp$Scenario == "baseline"] <- FALSE ## added v4.15
      
      ## ###################
      ## Years since start of impact (defined to be 1 in first year of impact)
      
      xx <- output.years[i] - impacts.year.start ## + 1 ## Version 2.8: bug fix - removed "+ 1"
      
      if(xx < 0){ 
        
        tmp$Impact.year <- NA ## Set to be missing if impact has not yet begun...
      }
      else{
      
        tmp$Impact.year <- xx
      }
      
      tmp$Impact.year[tmp$Scenario == "baseline"] <- NA ## added v4.15
      
      ## #############################################################
      ## Summaries of actual population sizes 
      ## #########################################
      
      ## Which quantiles to output:
      ## Version 3.1: added 25% and 75% quantiles
      qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
      
      ## Functions for extracting each summary:
      getsumy <- function(x){c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE),
                               median(x,na.rm=TRUE),quantile(x,qs/100,na.rm=TRUE))}
      
      ## Name of summaries being outputted:
      sumynames <- paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep=".")
      
      ## Projected population sizes for this age and year; this
      ##  lines yield a matrix of [scenarios, simulations]
      
      ## print("-----------------------")
      ## print(paste(dim(ntot), collapse = ";"))
      ## print(paste(ic, collapse=";"))
      ## print(j)
      ## if((j > (dim(ntot)[4])) | any(ic > dim(ntot)[2])){ browser() }
      
      ntots <- d(ntot[,ic,,j,drop=FALSE], c(1,3)) 
      
      ## Produce summary across simulations for each summary:
      
      sumy <- t(apply(ntots, 1, getsumy))
      
      colnames(sumy) <- sumynames
 
      tmp <- cbind(tmp, sumy)      

      ## #############################################################
      ## Moved
      
      tmp$pgr.median <- rep(NA,ns) ; tmp$pgr.mean <- rep(NA,ns) ; tmp$pgr.sd <- rep(NA,ns) ; tmp$pgr.cilo <- rep(NA,ns) ; tmp$pgr.cihi <- rep(NA,ns)
      tmp$agr.median <- rep(NA,ns) ; tmp$agr.mean <- rep(NA,ns) ; tmp$agr.sd <- rep(NA,ns) ; tmp$agr.cilo <- rep(NA,ns) ; tmp$agr.cihi <- rep(NA,ns)
      tmp$ppc.median <- rep(NA,ns) ; tmp$ppc.mean <- rep(NA,ns) ; tmp$ppc.sd <- rep(NA,ns) ; tmp$ppc.cilo <- rep(NA,ns) ; tmp$ppc.cihi <- rep(NA,ns)
      
      tmp$m1.median <- rep(NA,ns) ; tmp$m1.mean <- rep(NA,ns) ; tmp$m1.sd <- rep(NA,ns) ; tmp$m1.cilo <- rep(NA,ns) ; tmp$m1.cihi <- rep(NA,ns)
      tmp$m2.median <- rep(NA,ns) ; tmp$m2.mean <- rep(NA,ns) ; tmp$m2.sd <- rep(NA,ns) ; tmp$m2.cilo <- rep(NA,ns) ; tmp$m2.cihi <- rep(NA,ns)
      tmp$m3 <- rep(NA,ns) ; tmp$m4 <- rep(NA,ns) ; tmp$m5 <- rep(NA,ns) ; tmp$m6 <- rep(NA,ns)
      
      ## #############################################################

      ## print(kb)
            
      if((! is.null(kb))){ ## changed version 4.15 - moved clause relating to impact year to further down
        
        ## #############################################################
        ## Calculate annualized growth rate under baseline
        ## #########################################
      
        ntot.base.cur <- ntot[kb,ic,,j] ## current population size in baseline
        ntot.base.ref <- ntot[kb,brry,,j] ## population in reference year in baseline
        
        ## Version 3.2: bug fix!! Was wrongly 'brry - ic'
        pgy <- ic - brry ## Version 2.8: added: gap between current year and baseline year
      
        ## annualized growth rate under baseline
        ## Version 2.8: bug fix: change (1/i) to (1/pgy)
        pgr.baseline <- (ntot.base.cur / ntot.base.ref)
          
        agr.baseline <- pgr.baseline^(1/pgy) 

        for(ks in 1:ns){
      
          if(tmp$Year[ks] > tmp$Baseyear[ks]){ ## Clause added Version 4.16
            
            ## #############################################################
            ## Calculate annualized growth rate under scenario
            ## #########################################
        
            ntot.scen.cur <- ntot[ks,ic,,j] ## current population size in this impact scenario
            ntot.scen.ref <- ntot[ks,brry,,j] ## population in reference year in this impact scenario
          
            ## #############################################################
            ## annualized growth rate under impact scenario
            ## Version 2.8: bug fix: change (1/i) to (1/pgy)
          
            pgr.scenario <- (ntot.scen.cur / ntot.scen.ref)
          
            agr.scenario <- pgr.scenario^(1/pgy)
 
            ## #############################################################
            ## Population growth rate summaries - added Version 3.1
            ## NOTE: this is overall growth, not annual growth rate
          
            if(all(! (is.na(pgr.scenario) | is.nan(pgr.scenario)))){ ## Version 4.8 - added clause
          
              tmp$pgr.median[ks] <- median(pgr.scenario) 
          
              tmp$pgr.mean[ks] <- mean(pgr.scenario) 
          
              tmp$pgr.sd[ks] <- sd(pgr.scenario)
          
              tmp$pgr.cilo[ks] <- quantile(pgr.scenario, 0.025)
          
              tmp$pgr.cihi[ks] <- quantile(pgr.scenario, 0.975) ## added 3 Dec 2019
            }
  
            ## #############################################################
            ## Annualized growth rate summaries - added Version 3.2

            if(all(! (is.na(agr.scenario) | is.nan(agr.scenario)))){ ## Version 4.8 - added clause
            
              tmp$agr.median[ks] <- median(agr.scenario)
            
              tmp$agr.mean[ks] <- mean(agr.scenario)
            
              tmp$agr.sd[ks] <- sd(agr.scenario)
          
              tmp$agr.cilo[ks] <- quantile(agr.scenario, 0.025)
          
              tmp$agr.cihi[ks] <- quantile(agr.scenario, 0.975)
            }
          
            ## #############################################################
            ## Percentage population change - added Version 3.1
            
            ppc <- 100 * (ntot.scen.cur - ntot.scen.ref) / ntot.scen.ref
            
            if(all(! (is.na(ppc) | is.nan(ppc)))){ # Version 4.8 - added clause
          
              tmp$ppc.median[ks] <- median(ppc)

              tmp$ppc.mean[ks] <- median(ppc)
             
              tmp$ppc.sd[ks] <- sd(ppc)
          
              tmp$ppc.cilo[ks] <- quantile(ppc, 0.025)
          
              tmp$ppc.cihi[ks] <- quantile(ppc, 0.975)
            }
          }
      
          ## #############################################################
          ## Calculate metrics
          ## Version 1.3: changed to use "mean" and "SD" as well as "median" for M1 and M2
          
          if(! is.na(tmp$Impact.year[ks])){ ## changed version 4.15
            
            ## ##################################
            ## Metric M1. The ratio of impacted to unimpacted final population size 
            ## in each future year - mean, median and SD across the set of simulations
            ## Version 3.1: added confidence interval limits (".cilo" and ".cihi")
            ## ##################################
        
            tmp$m1.median[ks] <- fnrat(agr.scenario, agr.baseline, fn = median)
          
            tmp$m1.mean[ks] <- fnrat(agr.scenario, agr.baseline, fn = mean) ## Version 1.3: added
          
            tmp$m1.sd[ks] <- fnrat(agr.scenario, agr.baseline, fn = sd) ## Version 1.3: added
          
            tmp$m1.cilo[ks] <- fnrat(agr.scenario, agr.baseline, fn = quantile, probs = 0.025) ## Version 3.1: added

            tmp$m1.cihi[ks] <- fnrat(agr.scenario, agr.baseline, fn = quantile, probs = 0.975) ## Version 3.1: added
          
            ## ##################################
            ## Metric M2. The ratio of impacted to unimpacted growth rate - mean, median and 
            ## SD across the set of simulations
            ## Version 3.1: added confidence interval limits (".cilo" and ".cihi")
            ## ##################################
          
            tmp$m2.median[ks] <- fnrat(ntot.scen.cur, ntot.base.cur, fn = median)  ## Version 0.6: fixed
        
            tmp$m2.mean[ks] <- fnrat(ntot.scen.cur, ntot.base.cur, fn = mean)  ## Version 0.6: fixed
        
            tmp$m2.sd[ks] <- fnrat(ntot.scen.cur, ntot.base.cur, fn = sd)  ## Version 0.6: fixed
         
            tmp$m2.cilo[ks] <- fnrat(ntot.scen.cur, ntot.base.cur, fn = quantile, probs = 0.025) ## Version 3.1: added
          
            tmp$m2.cihi[ks] <- fnrat(ntot.scen.cur, ntot.base.cur, fn = quantile, probs = 0.975) ## Version 3.1: added
          
            ## ##################################
            ## Metric M3. Quantile for unimpacted population which matches the 50th 
            ##  centile for the impacted population
            ## ##################################
          
            tmp$m3[ks] <- 100 * mean(ntot.base.cur <= median(ntot.scen.cur))
        
            ## ##################################
            ## Metric M4. The probability of decline in population size (expressed as a %), 
            ##  over specified future time intervals
            
            ## Version 4.9: ** changed **
          
            tmp$m4[ks] <- 100 * mean(ntot.scen.cur <= median(ntot.base.cur)) ## Version 0.6: fixed
          
            ## ##################################
            ## Metric M5. The risk of extinction or quasi-extinction, over a 
            ##  specified future time interval
            ## ##################################
          
            ## Version 4.7: changed so that "popsize.qe" is now a vector, and NA if not used
            if(is.na(popsize.qe[j])){ ## Version 1.3: code reordered to improve internal logic
            
              tmp$m5[ks] <- NA
            }
            else{
          
              tmp$m5[ks] <- 100 * mean(ntot.scen.cur < popsize.qe[j]) ## Version 4.8: added "[j]" subscript
            }
          
            ## ##################################
            ## Metric M6. The probability that the population will have recovered 
            ##  to this level over any particular number of years
            ## ##################################
          
            ## Version 3.1: bug fix: "popsize.target" was wrongly "popsize.qe" in "if" clause
            ## Version 4.7: changed so that "popsize.qe" is now a vector, and NA if not used
          
            if(is.na(popsize.target[j])){ ## Version 1.3: code reordered to improve internal logic  
          
              tmp$m6[ks] <- NA
            }
            else{
          
              tmp$m6[ks] <- 100 * mean(ntot.scen.cur > popsize.target[j]) ## Version 4.8: added "[j]" subscript
            }
          }
          
          ## ##################################
        }
      }
      else{
        
       }
      
      out <- rbind(out, tmp)
    }
  }
  
  out
}

## ############################################################################
## Added Version 1.9, on 22 Jan 2018: to retain dimensions in an object, when used after
##   with [,,,drop=FALSE]
##
## "x": an array
## "cp": set of dimensions of "x" to retain, even if the dimension has a length of one; 
##    a vector of integers

d <- function(x,cp){ apply(x, cp, unique) }

## ##############################################################################
## >> plot.burnconv
## ##############################################################################
#' @title Plot convergence of the age structure, during and beyond the burn-in period
## ##############################################################################
## Added Version 4.2, on 12 August 2019
## ##############################################################################
#' @inheritParams nepva.fullrun
#' @param nbyage A five-dimensional array, of dimension [nscen, npop, nyears, sim.n, na],
#'   containing the simulated number of birds in each combination of scenario, subpopulation, year,
#'   simulation run and age class, in the "main" PVA run - i.e. after the burn-in
#' @param nbyage.burned A five-dimensional array, of dimension [nscen, npop, nburn.adj, sim.n, na],
#'   containing the simulated number of birds in each combination of scenario, subpopulation, year,
#'   simulation run and age class, in the "burn-in" run. Note that "nscen = 1" always for the
#'   burn-in run. The number of years, "nburn.adj", is equal to the value of "nburn" specified by the
#'   user, plus the number of years needed to ensure all subpopulations are initialized.
#' @param year.first Year associated with the start of the main run; an integer   
## ############################################################################
#' @return The value returned is NULL; the function is run for the side-effect that
#'  it produces a plot
#' @export

plot.burnconv <- function(nbyage, nbyage.burned, year.first, afb){
  
  ## ########################################
  ## Calculate age structure for each year 
  ##  (summed across subpopulations, and then averaged across simulation runs)
  
  pa <- baseagestruc(nbyage)
  
  nyears <- dim(nbyage)[3]
  
  if(! is.null(nbyage.burned)){
    
    pa <- cbind(baseagestruc(nbyage.burned), pa)
    
    nburn.adj <- dim(nbyage.burned)[3]
  }
  else{
    
    nburn.adj <- 0
  }

  ## ########################################
  
  years <- year.first + 0:(nyears-1)
  
  yo <- seq(ceiling(years[1]/5)*5, years[nyears], 5)
  
  xo <- nburn.adj + match(yo, years)
  
  {if(nburn.adj > 10){
    
    xo <- c(nburn.adj/2, xo)
    yo <- c("Burn-in", yo)
  }}
  
  ## ########################################
  
  cols <- rainbow(round((afb+1)*1.2))[1:(afb+1)]
  
  barplot(pa, space=0, col=cols, axes=FALSE, border = gray(0.8),
          xlab = "Year", ylab="Age structure (%)", cex.axis=1.22, cex.lab=1.22)
  
  axis(1, at = xo, labels = yo)
  
  axis(2, at = seq(0,1,0.2), labels = seq(0,100,20), cex.axis = 1.22, cex.lab = 1.22)
  
  lines(rep(nburn.adj+0.5,2),c(0,1.1),lwd=2.5,lty=3)
  lines(rep(nburn.adj,2),c(0,1.1),lwd=2.5,lty=3,col="white")
  
  legend(0.6 * nyears, 0.9, c(0:(afb-1), paste0(afb,"+")), 
         bg="white", fill=cols, title="Age class") ## bug fix Version 4.13
  
  ## ########################################
  
  NULL
}

## ############################################################################
## Calculate age structure for each year 
##  (summed across subpopulations, and then averaged across simulation runs)
## 
## Added Version 4

baseagestruc <- function(y){ 
  
  z <- apply(apply(y[1,,,,,drop=FALSE], 3:5, sum), c(1,3), mean)
  
  apply(z, 1, function(x){x/sum(x)})
}

## ############################################################################

## ############################################################################
