## ###################################################################################################################
## Technical functions used for calculations within the NE PVA tool, Version 3.3
## ###################################################################################################################
## --- Contents ---
## nepva.calcs                       --- function to run PVA calculations
##      getpars.demorates            --- use moment matching to calculate the parameters associated with mean and SD
##          mom.numeric              --- perform moment matching using numeric optimisation
##      mean.demorates               --- find the mean demographic rates associated with a statistical model for rates
##          pred.numeric             --- find mean values via simulation
##      make.leslie.matrix           --- construct the Leslie matrix associated with a particular set of demographic rates
##      split.deathss              --- simulate the (absolute or relative) impacts associated with an impact
##          split.deaths      --- split the absolute number of birds killed into separate subpopulations
##      leslie.update                --- update the population sizes over one year using the Leslie matrix model
##
## vbgs.siminputs                    --- global sensitivty analysis: simulate sets of inputs to use
## vbgs.calctable                    --- global sensitivty analysis: produce table to summarise output
## vbgs.decomposition                --- global sensitivty analysis: produce variance decomposition
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
#' @seealso A function that is directly called by the user. Calls the internal functions
#'   \code{\link{getpars.demorates}}, 
#'   \code{\link{mean.demorates}}, \code{\link{make.leslie.matrix}}, 
#'   \code{\link{split.deathss}}, \code{\link{leslie.update}} and
#'   \code{\link{make.impactmetrics.table}}
#' @export

nepva.sim <- function(fn.sim.con, fn.sim.unc, ddfn,
                      model.demostoch, model.prodmax, afb, mbs, 
                      npop, nscen, sim.n, sim.seed, 
                      year.first, 
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
  ## Version 3.1: added "output.raw" argument
  
  if(! silent){
  
    print.noquote("-------------------------------------------------")
  
    print.noquote("Running the NE PVA tool, Version 3.1")
  
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
  
  ## Added Version 1.8: reference "baseline" year against which impact metrics are calculated
  ##   -- fixed to be the year before the impact begins (NOTE: a key assumption of the package
  ##      is that impacts apply to the same set of years for all scenarios and subpopulations, 
  ##      which means that it is also reasonable to use the same "baseref.year" for all scenarios
  ##      and subpopulations
  
  baseref.year <- impacts.year.start - 1 
  
  ## #################################################################
  ## Stage 2. Relative years within PVA
  ## ############################################
  
  inipop.relyears <- inipop.years - year.first + 1
  
  impacts.relyears <- (impacts.year.start:impacts.year.end) - year.first + 1
  
  baseref.relyear <- baseref.year - year.first + 1

  output.relyears <- (output.year.start:output.year.end) - year.first + 1

  ## #################################################################
  ## Step 3. Number of ages and demographic rates
  
  ## ############################################
  ## Number of ages: 
  ## 0 (chicks), 1,...(afb-1) (immatures), afb+ (adults)
  
  na <- afb + 1
  
  ## #########################################
  ## Number of demographic rates to specify
  ##   Demographic rates are organized as:
  ##   [1] Productivity
  ##   [2:na] age-specific immature survival (0 to 1,1 to 2,(afb-1) to afb)
  ##   [nd] adult survival (afb+)
  
  nd <- afb + 2 

  ## #################################################################
  ## Step 4. Set seed
  
  set.seed(sim.seed)
  
  ## #################################################################
  ## Step 5. Random number generation for baseline simulations
  ## Added Version 0.4, 2 Nov 2018
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
  
  sim.bskippc <- array(dim=c(nyears, sim.n), 
                       data = rnorm(nyears * sim.n, demobase.bskippc[1], demobase.bskippc[2]))
  
  ## #################################################################
  ## Step 7. Simulate magnitudes of impacts
  ## Revised Version 1.8
  ## ############################################
  
  if(impacts.matchscens){
    
    ru <- array(dim=c(nscen, npop, sim.n, nd))
    
    ro <- rnorm(npop * sim.n * nd)
    
    for(i in 1:nscen){
      
      ru[i,,,] <- ro
    }
    
  }
  else{
    
    ru <- array(dim=c(nscen, npop, sim.n, nd), data = rnorm(nscen * npop * sim.n * nd))
  }
  
  impacts.demochange <- array(dim=c(nscen, npop, sim.n, nd))

  impacts.abschange <- array(dim=c(nscen, npop, sim.n, na)) ## NOTE: final dimension differs from "impacts.demochange"
  
  for(i in 1:nscen){
  
    for(j in 1:npop){
      
      for(k in 1:nd){
        
        impacts.demochange[i,j,,k] <- impacts.demochange.mean[i,j,k] + impacts.demochange.se[i,j,k] * ru[i,j,,k]
        
        if(k < nd){
     
          impacts.abschange[i,j,,k] <- impacts.abschange.mean[i,j,k] + impacts.abschange.se[i,j,k] * ru[i,j,,k]
        }
      }
    }
  }
  
  impacts.abschange <- round(impacts.abschange)
  
  ## #########################################
  ## Step 9. Run main PVA calculations
  ## Loop over scenarios, and colonies, and run "nepva.sim"
  
  ## Version 0.4: change so that random uniform numbers are already generated,
  ##   and are inputted here
  ## - means that "demobase.cormat", "sim.n" and "sim.seed" no longer need to be
  ##    inputs to "nepva.sim"
  ## #########################################
  
  if(! silent){
    
    print.noquote("-------------------------------------------------")
  
    print.noquote(paste("Running main PVA calculations...     ", date()))
    print.noquote("        ")
  }
  
  ## ###########################################
  ## Version 1.2: changed order to loop over time first...
  
  ## #####################
  ## Each time point  
  ## Version 0.8: changed to use "inipop.relyears+1" rather than "2"
  ## Version 1.8: changed order of loops: now: scenario > time > population
  
  out <- NULL
  
  nbyage <- array(dim=c(nscen, npop, nyears, sim.n, na), data = NA)
  
  for(i in 1:nscen){

    ## #########################################
    ## Set up output, and initialize
    ## ############################################
  
    for(j in 1:npop){
        
      for(a in 1:na){ 
          
        ## Version 3.3: bug fix: "inipop.relyears" > "inipop.relyears[j]"
        nbyage[i,j,inipop.relyears[j],,a] <- inipop.counts[a,j] ## 30 Oct 2018: changed from "a" to "a+1"
      }
    }
  
    ## ############################################
    
    for(tt in 2:nyears){

      ## if(tt == 53){ browser() }
      
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
  
        impacts.abschange.tt <- split.deaths(impacts.abschange = impacts.abschange.tt,
                                             nprev = nprev, 
                                             impacts.infillpops = impacts.infillpops,
                                             impacts.infillages = impacts.infillages)
      }
      else{
        
        impacts.demochange.tt <- NULL
        
        impacts.abschange.tt <- NULL
      }
      
      ## ##################################################
      ## Loop through subpopulations and update size of each
      
      for(j in 1:npop){
        
        ## ##################################################
        ## Leslie matrix updating for each subpopulation
        
        if(tt > inipop.relyears[j]){
        
          if(any(is.na(mean(apply(nbyage[i,j,tt-1,,,drop=FALSE],2,sum))))){browser()} ## Version 2.6: Added drop=FALSE
        
          ## print(paste(i,"  ", j, "   ", tt,"         ",mean(apply(nbyage[i,j,tt-1,,],1,sum))))
          
          nbyage.prev <- apply(nbyage[i,j,tt-1,,,drop=FALSE], 4:5, identity) ## Added Version 2.6, to get right # dimensions
          
          ruij.base <- apply(ru.base[j,tt,,,drop=FALSE], 3:4, identity) ## Added Version 2.6, to get right # dimensions
          
          if(is.null(impacts.demochange.tt)){
            
            impacts.demochange.ij <- NULL
          }
          else{
          
            impacts.demochange.ij <- apply(impacts.demochange.tt[j,,,drop=FALSE], 2:3, identity)
          }
  
          if(is.null(impacts.abschange.tt)){
            
            impacts.abschange.ij <- NULL
          }
          else{
            
            impacts.abschange.ij <- apply(impacts.abschange.tt[j,,,drop=FALSE], 2:3, identity)
          }
         
          simbsk <- apply(sim.bskippc[tt,,drop=FALSE], 2, unique) ## Version 2.6: bug fix
           
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
    
    ## ##################################################
  }
  
  ## ####################################################
  ## Stage 8: Aggregate output - sum across populations
  
  age.names <- output.agetype
  
  if(output.agetype == "age.separated"){
    
    age.names <- c("chicks", paste0("immatures.age", 1:(afb - 1)), "breeding.adults")
    
    ia <- 1:na
    
    mf <- 1
  }
  else{
    
    age.names <- output.agetype
    
    ia <- na
    
    mf <- (1/2)^(output.agetype == "breeding.pairs") ## Version 2.6: changed "2" to "1/2"
  }
  
  aggvals <- mf * apply(nbyage[,,,,ia,drop=FALSE], c(1,3,4,5), sum)
    
  ## ####################################################
  ## Stage 9. Create impact metrics table
  
  ## Version 0.5: added "popsize.qe" and "popsize.target" arguments
  ## Version 0.8: changed to use "baseref.relyear", and "nyears"
  
  out <- make.impactmetrics.table(ntot= aggvals, scen.names = impacts.scennames, 
                                  age.names = age.names, 
                                  year.first = year.first, impacts.year.start = impacts.year.start,
                                  impacts.year.end, 
                                  output.year.start = output.year.start,
                                  output.year.end = output.year.end,
                                  popsize.qe = output.popsize.qe, 
                                  popsize.target = output.popsize.target)  

  ## ####################################################
  ## Stage 10. Raw outputs - added Version 3.1
  
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
      
    ## Simulate survival probabilities:
    
    psim <- fn.sim.con(ru = ru.base[,k+1], pars = as.numeric(c(demobase.ests[k+1,])), 
                       popsize = nadult.prev, ddfn = ddfn) 
      
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
    
  ## Individuals that were previously adults (age "afb" or higher):
  
  if(! model.demostoch){
      
    nextra <- mayberound(nbyage.prev[,na] * p.surv[,na]) 
  }
  else{
      
    nextra <- rbinom(sim.n, nbyage.prev[,na], p.surv[,na])
  }
  
  ## Sum together existing adults and newly recruited adults:
    
  nbyage[,na] <- nbyage[,na,drop=FALSE] + nextra ## 4 Jan 2019: added " '- nremoved' term"
    
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
  
  ntotal.actualbreedpairs <- ntotal.breedpairs * (1 - (sim.bskippc/100))
  
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
                                     output.year.start, output.year.end,
                                     popsize.qe = NULL, popsize.target = NULL){
  
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
  
    brry <- impacts.relyear.start - 1 ## changed Version 0.8, renamed from "ib" in Version 1.3
  }

  ## #############################################################
  ## Sort out ages
  
  nages <- length(age.names) # Added Version 1.3
  
  ## Version 3.1: changed to add "..." statement (needed to extend to quantiles)
  fnrat <- function(x,y,fn,...){fn((x/y)[y > 0],...)} ## Version 1.3: generalized beyond "median"
  
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
      
      tmp$Baseyear <- (output.years[i] == (impacts.year.start - 1))
      
      ## ###################
      ## Is the impact currently operating? (TRUE/FALSE)
      
      tmp$Currently.Impacted <- (output.years[i] >= impacts.year.start) & (output.years[i] <= impacts.year.end)
      
      ## ###################
      ## Years since start of impact (defined to be 1 in first year of impact)
      
      xx <- output.years[i] - impacts.year.start ## + 1 ## Version 2.8: bug fix - removed "+ 1"
      
      if(xx < 0){ 
        
        tmp$Impact.year <- NA ## Set to be missing if impact has not yet begun...
      }
      else{
      
        tmp$Impact.year <- xx
      }
      
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
      
      ntots <- d(ntot[,ic,,j,drop=FALSE], c(1,3)) 
      
      ## Produce summary across simulations for each summary:
      
      sumy <- t(apply(ntots, 1, getsumy))
      
      colnames(sumy) <- sumynames
 
      tmp <- cbind(tmp, sumy)      
      
      ## #############################################################

      ## print(kb)
            
      if((! is.null(kb)) & ! is.na(tmp$Impact.year[1])){
          
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
          
          tmp$pgr.median[ks] <- median(pgr.scenario)
          
          tmp$pgr.mean[ks] <- mean(pgr.scenario)
          
          tmp$pgr.sd[ks] <- sd(pgr.scenario)
          
          tmp$pgr.cilo[ks] <- quantile(pgr.scenario, 0.025)
          
          tmp$pgr.cihi[ks] <- quantile(pgr.scenario, 0.975)

          ## #############################################################
          ## Annualized growth rate summaries - added Version 3.2

          tmp$agr.median[ks] <- median(agr.scenario)
          
          tmp$agr.mean[ks] <- mean(agr.scenario)
          
          tmp$agr.sd[ks] <- sd(agr.scenario)
          
          tmp$agr.cilo[ks] <- quantile(agr.scenario, 0.025)
          
          tmp$agr.cihi[ks] <- quantile(agr.scenario, 0.975)
          
          ## #############################################################
          ## Percentage population change - added Version 3.1
          
          ppc <- 100 * (ntot.scen.cur - ntot.scen.ref) / ntot.scen.ref
          
          tmp$ppc.median[ks] <- median(ppc)

          tmp$ppc.mean[ks] <- median(ppc)
          
          tmp$ppc.sd[ks] <- sd(ppc)
          
          tmp$ppc.cilo[ks] <- quantile(ppc, 0.025)
          
          tmp$ppc.cihi[ks] <- quantile(ppc, 0.975)
          
          ## #############################################################
          ## Calculate metrics
          ## Version 1.3: changed to use "mean" and "SD" as well as "median" for M1 and M2
        
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
          
          tmp$m4[ks] <- 100 * mean(ntot.scen.cur < ntot.base.cur) ## Version 0.6: fixed
          
          ## ##################################
          ## Metric M5. The risk of extinction or quasi-extinction, over a 
          ##  specified future time interval
          ## ##################################
          
          if(is.null(popsize.qe)){ ## Version 1.3: code reordered to improve internal logic
            
            tmp$m5[ks] <- NA
          }
          else{
          
            tmp$m5[ks] <- 100 * mean(ntot.scen.cur < popsize.qe)
          }
          
          ## ##################################
          ## Metric M6. The probability that the population will have recovered 
          ##  to this level over any particular number of years
          ## ##################################
          
          ## Version 3.1: bug fix: "popsize.target" was wrongly "popsize.qe" in "if" clause
          
          if(is.null(popsize.target)){ ## Version 1.3: code reordered to improve internal logic  
          
            tmp$m6[ks] <- NA
          }
          else{
          
            tmp$m6[ks] <- 100 * mean(ntot.scen.cur > popsize.target)
          }
          
          ## ##################################
        }
      }
      else{
        
        tmp$pgr.median <- NA ; tmp$pgr.mean <- NA ; tmp$pgr.sd <- NA ; tmp$pgr.cilo <- NA ; tmp$pgr.cihi <- NA
        tmp$agr.median <- NA ; tmp$agr.mean <- NA ; tmp$agr.sd <- NA ; tmp$agr.cilo <- NA ; tmp$agr.cihi <- NA
        tmp$ppc.median <- NA ; tmp$ppc.mean <- NA ; tmp$ppc.sd <- NA ; tmp$ppc.cilo <- NA ; tmp$ppc.cihi <- NA
      
        tmp$m1.median <- NA ; tmp$m1.mean <- NA ; tmp$m1.sd <- NA ; tmp$m1.cilo <- NA ; tmp$m1.cihi <- NA
        tmp$m2.median <- NA ; tmp$m2.mean <- NA ; tmp$m2.sd <- NA ; tmp$m2.cilo <- NA ; tmp$m2.cihi <- NA
        tmp$m3 <- NA ; tmp$m4 <- NA ; tmp$m5 <- NA ; tmp$m6 <- NA
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

## ############################################################################
