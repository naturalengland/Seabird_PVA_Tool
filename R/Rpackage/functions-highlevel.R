## ########################################################################################
## Functions for NE PVA R tool
## (AB, Biomathematics and Statistics Scotland)
##
## This file contains the high-level functions called by the user, and the functions involved
##  in re-formatting inputs and visualizing outputs
## ########################################################################################

## ########################################################################################
## * Note: this file is documented using the Roxygen2 system, which enables the documentation to automatically
##  be compiled into the help files for the associated R package - if editing the documentation within this file, 
##  please consider doing so using only valid Roxygen2 syntax *
## ########################################################################################

## ########################################################################################
## >> nepva.calcdefaults
#' @title Calculate default input values for the NE PVA tool 
#' @description Calculate default input values for baseline demographic rates and initial population
#'   sizes for the NE PVA tool, using existing data
## ########################################################################################
#' @details The underlying data on abundance and breeding success are taken from the Seabird 
#'   Monitoring Programme (SMP), augmented with additional data on gannets provided by CEH
#' 
#' Default values for survival are based upon the species-level values given in 
#'   Horswill and Robinson (2015), with modifications as detailed in **REF-TBA
#'
#' Default values for productivity are derived from the augmented SMP dataset. The mean and inter-annual 
#'   standard deviation of breeding success are calculated. A default value is only provided if the 
#'   breeding success is the same for all sites specified within "site.ids".
#'
#' Default values for age at first breeding are based on **REF
#'
#' Default values for maximum brood size are based on **REF
## ########################################################################################
## Created 23 October 2018, last modified 3 January 2019
##
## v0.2 on 25 October 2018: 
##  Change 1: modified so that it saves the outputs with the same names/formats
##  as the inputs to "nepva.run"; specifically:
##  [demobase.specify.as.params, demobase.vals, afb, mbs, 
##    inipop.mergedages, npop, inipop.vals, inipop.years, 
##    output.validation.counts, output.validation.years]
## 
##  Change 2: modified to include a "for.validation" argument
##
## V1 on 3 January 2019: modified to:
##   - use new values for survival and breeding success
##   - change inputs to: poolregtype.BS, poolregion.BS, sourcepop.surv
##   - use "lookup" (an R object), rather than "lookup.file" (a filename)
##   - remove inputs for initial values and validation
##
## Version 1.5 on 15 January 2019: modified to split by demo type
## ########################################################################################
#' @param Species Species; a single character string; possible options are given by 'levels(lookup$Spmeta$Species)'
#' @param poolregtype.BS Regional classification to use in pooling breeding success data spatially; 
#'         a single character string; possible options are given by 'unique(lookup$BS$Regclass)'. If NULL
#'         then a default value for breeding success is not produced.
#' @param poolregion.BS Region to use for pooling breeding success data spatially; a single character string;
#'         possible options are given by "unique(lookup$BS$Region[lookup$BS$Regclass == poolregtype.BS])". If NULL
#'         then a default value for breeding success is not produced.
#' @param sourcepop.surv Population to use for determining survival rates. A single character string;
#'         possible options are given by "unique(lookup$Surv$Source[lookup$Surv$Species == Species])". 
#'         If NULL then a default value for survival is not produced.
#' @param lookup.dir A character string, specifying the path to the directory that contains the lookup table files,
#'     "lookup-spmeta.csv", "lookup-BS.csv" and "lookup-surv.csv"
## ########################################################################################
#' @return A list, with elements 'basespecify.as.params' (logical), 'amax' (single integer),
#'     'mbs' (single integer), 'afb' (single integer) and 'demobase.vals' (data frame)
## ########################################################################################
#' @seealso A function that is directly called by the user 
#' @export
#' 

## #################################################################
## #################################################################

nepva.calcdefaults <- function(Species, poolregtype.BS = NULL, poolregion.BS = NULL, sourcepop.surv = NULL, lookup.dir){
  
  ## ############################################
  ## Version 3.4: (1) added code to reformat survival data, and
  ##  remove rows with blank entries for "Source"
  ## -- this was previously done in the lookup table
  ##
  ## Change is so that the lookup table for "survival"
  ##  can now be directly edited, making updates much easier
  ##
  ## (2) renamed "BS.max" to "MBS", for consistency
  ##
  ## Version 3.5: rewritten and restructured to fix bugs
  ##  introduced in Version 3.4...
  ## #############################################
  
  sourcepop.ref <- "National (H&R; 2015)"
  
  svdat <- read.csv(paste0(lookup.dir, "lookup-surv.csv"))
  
  svdat <- svdat[svdat$Source != "",]
  
  lookup <- list(Spmeta = read.csv(paste0(lookup.dir, "lookup-spmeta.csv")),
                 BS = read.csv(paste0(lookup.dir, "lookup-BS.csv")),
                 Surv = svdat)
  
  ## ############################################
  
  ii <- (lookup$Spmeta$Species == Species)
  
  out <- list(demobase.specify.as.params = FALSE) ## Version 1.5 ::: removed "amax"
  
  ## ############################################
  
  if(any(ii)){
    
    ## ######################
    ## Species-level metadata

    out$afb <- lookup$Spmeta$AFB[ii]
    out$mbs <- lookup$Spmeta$MBS[ii] ## Version 3.4: "BS.max" changed to "MBS"
    
    ## ######################
    ## Breeding success
    
    if(is.null(poolregtype.BS) | is.null(poolregion.BS)){
      
      warning("Either 'poolregtype.BS' or 'poolregion.BS' is empty, so no defaults will be provided for breeding success...")
      
      out$demobase.prod <- data.frame(mean = NA, sd = NA) ## Changed Version 1.5
    }
    else{
      
      jj <- (lookup$BS$Species == Species) & (lookup$BS$Regclass == poolregtype.BS) & (lookup$BS$Region == poolregion.BS)
      
      if(any(jj)){
        
        out$demobase.prod <- data.frame(mean = lookup$BS$BS.mean[jj], sd = lookup$BS$BS.sd[jj]) ## Changed Version 1.5
      }
      else{
        
        stop("Invalid choice for either 'poolregtype.BS' or 'poolregion.BS'...")
      }
    }
    
    ## ######################
    ## Survival
    
    if(is.null(sourcepop.surv)){
      
      warning("'sourcepop.surv' is empty, so no defaults will be provided for survival...")
      
      out$demobase.survadult <- NA
      out$demobase.survimmat <- NULL
    }
    else{
      
      ## Completely rewritten Version 3.5:

      lookup$Surv$Age.hi[is.na(lookup$Surv$Age.hi)] <- Inf
      
      dat <- lookup$Surv[(lookup$Surv$Species == Species) & (as.character(lookup$Surv$Source) == sourcepop.surv),]
      ref <- lookup$Surv[(lookup$Surv$Species == Species) & (as.character(lookup$Surv$Source) == sourcepop.ref),]
      
      out$demobase.survadult <- getsurvdefaults(dat = dat, ref = ref,
                                    age.lo = out$afb, age.hi = NULL)
      
      out$demobase.survimmat <- data.frame(mean = rep(NA, out$afb), sd = rep(NA, out$afb))
      
      for(k in 1:out$afb){
        
        out$demobase.survimmat[k,] <- getsurvdefaults(dat = dat, ref = ref, 
                                          age.lo = (k - 1), age.hi = k)
      }
    }   

    ## ######################
  }
  else{
    
    stop("Error! 'Species' not found...")
  }
  
  ## ############################################
  
  out
} 

## ###################################################################################################################
## getsurvdefaults: added Version 3.5
## ###################################################################################################################

getsurvdefaults <- function(dat, ref, age.lo, age.hi){
    
  out <- data.frame(mean = NA, sd = NA)
    
  if(is.null(age.hi)){
    
    test.dat <- is.infinite(dat$Age.hi)
    test.ref <- is.infinite(ref$Age.hi)
  }
  else{
      
    test.dat <- ((dat$Age.lo <= age.lo) & (dat$Age.hi >= age.hi))
    test.ref <- ((ref$Age.lo <= age.lo) & (ref$Age.hi >= age.hi))
  }
    
  if(any(test.dat)){ 
      
    out <- dat[test.dat,c("SV.mean", "SV.SD")] 
  }
  else{
      
    if(any(test.ref)){
        
      out <- ref[test.ref,c("SV.mean", "SV.SD")] 
    }
  }
    
  out
}
  
## ###################################################################################################################
## >> nepva.fullrun
## Version 3.1: added "output.run" argument
## Version 4.18: added "noround" argument
## ############################################################################
#' @title NE PVA tool: Generate PVAs from a Leslie matrix model under one or more
#'   different scenarios
#' @description NE PVA tool: Generate PVAs from a stochastic 
#'    or deterministic Leslie matrix model under one or more different scenarios 
#'    of impact, and compare against an unimpacted "baseline" PVA
## ############################################################################
#' @details Generates projections with and without scenarios of anthropogenic 
#'    impacts using a Leslie matrix model, and then produces tabular and graphic
#'    summaries to quantify the magnitude of the anthropogenic impact under each
#'    scenario     
#'    
#'    The function allows for one or more of three variants on the standard, deterministic, Leslie 
#'    matrix model to be used: (a) inclusion of demographic stochasticity,
#'    (b) inclusion of environmental stochasticity, and (c) inclusion of density dependence.
#'    For productivity the function allows the user to either specify the productivity as 
#'    being constrained to be less than or equal to a user-specified maximum brood size, or to 
#'    be unconstrained.
#'    
#'    Users may specify baseline demographic rates either as mean and SD values, or as estimates from 
#'      a parametric model, and may either specify the survival rates to either be common to all age 
#'      classes or to be specified separately for each age.
#'    
#'    Parametric distributions that are currently implemented for environmental stochasticity are:
#'      beta (survival and productivity), gamma (productivity only), logit-normal (survival and productivity)
#'      and log-normal (productivity only). Parametric distributions that are currently implemented for
#'      demographic stochasticity are binomial (survival and productivity) and Poisson (productivity only).
#'      
#'    Parametric models that are currently implemented for various different forms of density dependence.
#'    
#'    Users may use the function to run the PVAs for more than one subpopulation - in this case the projections
#'     for the subpopulations will be summed together before outputs are produced.
#'     
#'    Users may use the function to run more than one impact scenario simultaneously. 
## ############################################################################
## Steps 2-5. Run calculations to convert inputs into format needed for calculation
## ############################################################################
#' @param run.type Type of run to be performed: a character value. Possible options are 
#'    "fullrun", "simplescenario", "local.sensitivity", "global.sensitivity" or "validation" 
#' @param model.envstoch Model to use for environmental stochasticity; 
#'    a character string. Options are currently: "none" (deterministic), 
#'    "betagamma" (implying: beta for survival; gamma for productivity if "model.prodmax = FALSE", 
#'    beta for producitvity if "model.prodmax = TRUE"),
#'    "logitnlogn" (implying: logitnormal for survival; lognormal for productivity if "model.prodmax = TRUE",
#'    logitnormal for productivity if "model.prodmax = FALSE")
#' @param model.demostoch Should demographic stochasticity be accounted for? A logical value (TRUE/FALSE).
#' @param trans.dd Is density dependence applied to transformed or untransformed rates? A logical value (TRUE/FALSE).
#' @param model.dd Model to use for density dependence; a character string. 
#'    Options are currently: "none" (density independent model), "linear" or "loglinear". NEEDS TO BE UPDATED.
#' @param model.prodmax Should productivity be values be constrained to be less than or equal to 
#'    "max.brood.size"? A logical value (TRUE/FALSE).
#' @param mbs Maximum brood size. A single numeric value (positive integer).
#' @param afb Age at first breeding. A single numeric value (positive integer). Individuals are assumed to begin
#'    breeding at age "afb". Adult survival rates are also assumed to apply for ages above "afb", 
#'    and immature survival rates for ages from 0 to "afb".
#' @param npop Number of population to simulate; a postive integer. The default is one;
#'   if "npop > 1" then separate projections are produced for each population, and the
#'   projected abundances of the subpopulations are summed together; metrics and other outputs are
#'   then based upon the aggregate (summed) projections.
#' @param nscen Number of scenarios, *other than the baseline*, to consider; zero (in which case
#'   only the baseline is considered) or a positive integer. Outputs are produced for each
#'   scenario.
#' @param include.baseline Should a baseline run be included? A logical value.
#' @param nburn Number of years to use for burn-in; a non-negative integer. If zero no burn-in period is used.
#' @param sim.n Number of simulations to use. A positive integer. Defaults to 1000. If all inputs are
#'         specified to be deterministic (e.g. "model.envstoch = "none"" and "model.demostoch = FALSE"
#'         and "scen.sds = NULL") then the value of "sim.n" is ignored, and a single simulation is used. 
#' @param sim.seed Seed to use in generating random numbers. A positive integer. If this is specified to
#'         be NULL (the default) then the seed is selected automatically.
#' @param demobase.specify.as.params Should baseline demographic rates be specified as estimated parameters 
#'    from a fitted model ('demobase.specify.as.params = TRUE'), or as means and standard deviations
#'    of demographic rates ('demobase.specify.as.params = FALSE'). A logical value.
#' @param demobase.splitpops Should baseline demographic rates be specified separately for each population 
#'    ('demobase.splitpops = TRUE') or should a single set of rates be used for
#'    all subpopulations ('demobase.splitpops = FALSE')?
#' @param demobase.splitimmat Should baseline survival rates be specified for each immature ages
#'    ('demobase.splitimmat = TRUE') or should immature survival rates be assumed to be equal to adult survival rates?
#' @param inipop.splitimmat Should separate initial populations size be specified for each immature age?
#' If "inipop.splitimmat = FALSE" then the allocation of individuals from "inipop.vals" to individual ages is determined
#' using the stable age structure.
#' @param include.baseline Should a baseline scenario be added or not? A logical value (TRUE/FALSE).
#' @param impacts.splitpops Should impacts be specified separately for each population 
#'    ('impacts.splitpops = TRUE') or should a single set of impacts be used for
#'    all subpopulations ('impacts.splitpops = FALSE')?
#' @param impacts.splitimmat Should impacts be specified for each immature age?
#' If "impacts.splitimmat = FALSE" then the allocation of impacts to individual ages is determined
#'  based on the current allocation of individuals to ages. 
#' @param impacts.provideses Should standard errors be provided to specify the 
#'  uncertainty associated with the impacts? A logical value.  
#' @param demobase.prod Baseline productivity rates: either parameter estimates, or summaries, depending on 
#'   "demobase.specify.as.params". A matrix or data frame, with dimension equal to [npop*, npar*], where
#'   "npar*" is 2 if "demobase.specify.as.params = FALSE" and depends upon the number of unknown parameters
#'   in the model if "demobase.specify.as.params = TRUE", and where "npop*" is 
#'   equal to "npop" if "demobase.splitpops = TRUE" and equal to 1 if "
#'   demobase.splitpops = FALSE"
#' @param demobase.survimmat Baseline immature survival rates, for each age up to "afb". The will be NULL (empty)
#'   if "demobase.splitimmat = FALSE", since in that case immature survival rates are derived from the adult survival rate.
#'   If "demobase.splitimmat" this will be a matrix or data frame with dimension equal to [npop*, amax, npar*], where
#'   "npar*" is 2 if "demobase.specify.as.params = FALSE" and depends upon the number of unknown parameters
#'   in the model if "demobase.specify.as.params = TRUE", and where "npop*" is 
#'   equal to "npop" if "demobase.splitpops = TRUE" and equal to 1 if "
#'   demobase.splitpops = FALSE".
#' @param demobase.survadult Baseline adult survival rates: : either parameter estimates, or summaries, depending on 
#'   "demobase.specify.as.params". A matrix or data frame, with dimension equal to [npop*, npar*], where
#'   "npar*" is 2 if "demobase.specify.as.params = FALSE" and depends upon the number of unknown parameters
#'   in the model if "demobase.specify.as.params = TRUE", and where "npop*" is 
#'   equal to "npop" if "demobase.splitpops = TRUE" and equal to 1 if demobase.splitpops = FALSE"    
#' @param demobase.cormat Correlation between baseline demographic rates. 
#'   A matrix, of dimension [nrow(demobase.ests),nrow(demobase.ests)].
#'   The demographic rates are ordered as follows: productivity, immature survival (ordered by ascending age, from 1 to amax), 
#'   annual adult survival (above "amax"). The correlation matrix is assumed to be 
#'   common to all subpopulations.
#' @param demobase.bskippc Percentage of adults of breeding age that skip breeding in any particular year: a vector of
#'   length two, specifying the mean and SD for this percentage. The value is NULL, in which case subpopulation is
#'   assumed to be always be equal to zero.
#' @param inipop.inputformat Will the user specify the initial overall population sizes as number of breeding adults
#'   ("inipop.inputformat = 'nbadults'), number of breeding pairs ("inipop.inputformat = 'nbpairs'") or
#'   total number of individuals of all ages ("inipop.inputformat = 'allindivs'")?
#' @param inipop.years Years associated with the initial populations sizes, for each population to be
#'   considered. A vector of positive integers, of length equal to the value of "npop".
#' @param inipop.vals Initial overall population size, of type given by "inipop.inputformat", 
#' for each subpopulation be considered. A vector of numeric values, of length equal to the value of "npop".
#' @param inipop.immatvals Initial population sizes for each immature age class. NULL, if "inipop.splitimmat = FALSE";
#'  if "inipop.splitimmat = TRUE" this is a matrix or data frame of dimension [afb,npop].
#' @param impacts.year.start The first year is which the impact associated with scenario is assumed to operate.
#'   A postive integer. Must be greater than "max(inipop.years)".
#' @param impacts.year.start The final year is which the impact associated with scenario is assumed to operate.
#'   A postive integer. Must be greater than "impacts.year.start".
#' @param impacts.scennames Names/labels to use in referring to each "scenario" (e.g. each hypothesised set of effects 
#'         of an anthropogenic impact). A vector of character values, of length equal to the number
#'         of scenarios to be considered.
#' @param impacts.matchscens Should the random seeds associated with the simulation of impact effects for different
#'   scenarios be matched, or not? A logical value.         
#' @param impacts.relative Should the impacts be specified as a relative, rather than absolute, harvest? i.e. should be impact be
#'   relative to population size (impacts.relative = TRUE) or independent of population size (impacts.relative = FALSE).          
#' @param impacts.prod.mean Mean impacts of each scenario upon productivity: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in productivity rate (impacts.relative = TRUE) as
#'   a result of the impact. An array of dimension [nscen, npop] if "impacts.splitpops = TRUE",
#'      a vector of length [nscen] otherwise. 
#' @param impacts.prod.se Standard deviation impacts of each scenario upon productivity: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in productivity rate (impacts.relative = TRUE) as
#'   a result of the impact. If "impacts.provideses": an array of dimension [nscen, npop] if "impacts.splitpops = TRUE",
#'      a vector of length [nscen] if "impacts.splitpops = FALSE"; if "impacts.provideses" then NULL.
#' @param impacts.survimmat.mean Mean impacts of each scenario upon immature productivity: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in immature survival rate (impacts.relative = TRUE) as
#'   a result of the impact. If "impacts.splitimmat = TRUE": an array of dimension [nscen, npop] if "impacts.splitpops = TRUE",
#'      a vector of length [nscen] if "impacts.splitpops = FALSE"; if "impacts.splitimmat = FALSE" then NULL.        
#' @param impacts.survimmat.se Standard deviation impacts of each scenario upon immature survival: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in immature survival rate (impacts.relative = TRUE) as
#'   a result of the impact. If "impacts.provideses = TRUE" and "impacts.splitimmat = TRUE" then their  
#'   an array of dimension [nscen, npop] (if "impacts.splitpops = TRUE") or
#'      a vector of length [nscen] (if "impacts.splitpops = FALSE"). Otherwise, NULL.    
#' @param impacts.survadult.mean Mean impacts of each scenario upon adult survival: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in adult survival rate (impacts.relative = TRUE) as
#'   a result of the impact. An array of dimension [nscen, npop] if "impacts.splitpops = TRUE",
#'      a vector of length [nscen] otherwise. 
#' @param impacts.survadult.se Standard deviation impacts of each scenario upon adult survival: either the absolute number of
#'   birds killed (impacts.relative = FALSE), or the change in adult survival rate (impacts.relative = TRUE) as
#'   a result of the impact. If "impacts.provideses": an array of dimension [nscen, npop] if "impacts.splitpops = TRUE",
#'      a vector of length [nscen] if "impacts.splitpops = FALSE"; if "impacts.provideses" then NULL.
## # @param output.type Type of output to be produced. A character string. Available options are currently 
## #          "summary.table", "timeseries.plot" or "impactmetrics.table".
#' @param output.agetype How should output be produced, in terms of ages? A single categorical value.
#'   Options are: "breeding.adults", "breeding.pairs" or "ages.separately".
#' @param output.year.start First year for which outputs should be generated. A positive whole number. 
#' @param output.year.end Final year for which outputs should be generated. A positive whole number. Must be grater than "impacts.year.start".
## @param output.finalyearonly Should output be generated for all years, or only for the 
#'  final year of impact? 
#' @param output.popsize.target User-specified target population size. Can be NULL if "output.type" 
#'          is not equal to "impactmetrics.table".
#' @param output.popsize.qe Quasi-extinction level; a positive integer. Can be NULL if "output.type" 
#'          is not equal to "impactmetrics.table".
#' @param output.validation.counts Counts to use for validation. Either NULL (if validation is not to be performed)
#'         or a vector of non-negative integers. If "npop > 1" or "nscen > 0" then "output.validation.counts" is automatically
#'         set to NULL, regardless of the value supplied by the user.
#' @param output.validation.year Years associated with the validation counts. Either a vector of integers, of length 
#'         equal to the length of "output.validation.counts", or NULL. "output.validation.year" should be NULL if and only if
#'         "output.validation.counts" is also NULL.
#' @param sens.npvlocal Number of points per variable to use for local sensitivity analysis; a whole number.
#'   Defaults to one.
#' @param sens.npvglobal Number of simulations to use for global sensitivity analysis; a whole number.
#'   Defaults to one.
#' @param sens.pcr Maximum variation (as a percentage of standard value) to use for each of the
#'   five key parameters, in the following order: initial population size,
#'   mean productivity, mean adult survival, mean impact on productivity,
#'   mean impact on adult survival. A vector of numeric values of length 5.        
## ############################################################################
#' @return A data frame...
## ############################################################################
#' @seealso A function that is directly called by the user. Calls the internal function \code{\link{nepva.calcs.dummy}}.
## ############################################################################
## Version 4.2: added "nburn" argument
## Version 4.11: added "changetablenames" argument
## ############################################################################
#' @export

nepva.fullrun <- function(model.envstoch = "deterministic", model.demostoch = FALSE, 
                      trans.dd = FALSE, model.dd = "nodd", model.prodmax = TRUE, mbs = NULL, afb,
                      npop = 1, nscen = 0, include.baseline = TRUE, nburn = 0, 
                      sim.n = 1000, sim.seed = NULL,
                      demobase.specify.as.params = TRUE,
                      demobase.splitpops = FALSE, demobase.splitimmat = FALSE, 
                      demobase.prod, demobase.survimmat = NULL,
                      demobase.survadult, demobase.cormat = NULL,
                      demobase.bskippc = NULL,
                      inipop.years, inipop.inputformat = "breeding.pairs", inipop.vals, 
                      inipop.splitimmat = FALSE, inipop.immatvals = NULL,
                      impacts.relative = TRUE, impacts.splitpops = FALSE,
                      impacts.splitimmat = FALSE, impacts.provideses = FALSE,
                      impacts.year.start, impacts.year.end = NULL,
                      impacts.scennames, impacts.matchscens = FALSE,
                      impacts.prod.mean = NULL, impacts.prod.se = NULL, 
                      impacts.survimmat.mean = NULL, impacts.survimmat.se = NULL, 
                      impacts.survadult.mean = NULL, impacts.survadult.se = NULL,
                      output.agetype = "breeding.pairs", 
                      output.year.start, output.year.end,
                      output.popsize.target = NULL,
                      output.popsize.qe = 10, 
                      output.validation.counts = NULL,
                      output.validation.years = NULL, 
                      sens.npvlocal = 1, sens.npvglobal = 10,
                      sens.pcr = rep(20,5),
                      silent = FALSE, output.raw = FALSE, changetablenames = FALSE, noround = TRUE){

  ## ######################################################
  ## Added Version 2.1, updated Version 2.2
  
  run.type <- "fullrun"
  
  ## ######################################################
  
  tmp <- nepva.setinputs(run.type = run.type, modeoptions = modeoptions) 

  inputs <- tmp$inputs

  for(k in 1:length(tmp$autonames)){
    
    inputs[[tmp$autonames[k]]] <- eval(parse(text = tmp$autonames[k]))
  }

  inputs.null <- unlist(lapply(inputs, function(x){is.null(unlist(x))}))
  
  ## print(paste(names(inputs)[inputs.null], collapse=" "))
  
  inputs <- inputs[! inputs.null]
  
  ## ######################################################
  
  out <- nepva.calcs(inputs = inputs, demomods = demomods, run.type = run.type)
  
  ## #############################################
 
  out
}

## ###################################################################################################################
## >> nepva.simplescenarios
## ############################################################################
#' @title NE PVA tool: Generate PVAs from a Leslie matrix model under one or more
#'   different scenarios: simplified version designed for compatability with Shiny tool
#' @details This is essentially the same as \code{\link{nepva.fullrun}}, but with
#'   options that are omitted from the Shiny tool set to be NULL.
## ############################################################################
#' @inheritParams nepva.fullrun
## ############################################################################
#' @export

nepva.simplescenarios <- function(model.envstoch = "deterministic", model.demostoch = FALSE, 
                      model.dd = "nodd", model.prodmax = TRUE, mbs = NULL, afb,
                      npop = 1, nscen = 0, sim.n = 1000, nburn, sim.seed = NULL,
                      demobase.specify.as.params,
                      demobase.splitpops = FALSE, demobase.splitimmat = FALSE, 
                      demobase.prod, demobase.survimmat = NULL,
                      demobase.survadult, 
                      inipop.years, inipop.inputformat = "breeding.pairs", inipop.vals,
                      impacts.relative = TRUE, impacts.splitpops = FALSE,
                      impacts.splitimmat = FALSE, impacts.provideses = FALSE,
                      impacts.year.start, impacts.year.end = NULL,
                      impacts.scennames, impacts.matchscens = FALSE,
                      impacts.prod.mean = NULL, impacts.prod.se = NULL, 
                      impacts.survimmat.mean = NULL, impacts.survimmat.se = NULL, 
                      impacts.survadult.mean = NULL, impacts.survadult.se = NULL,
                      output.agetype = "breeding.pairs", 
                      output.year.start, output.year.end,
                      output.popsize.target = NULL,
                      output.popsize.qe = 10, silent = FALSE, output.raw = FALSE, changetablenames = FALSE){
  
  ## ######################################################

  ## ######################################################
  ## Added Version 2.1, updated Version 2.2
  
  run.type <- "simplescenarios"
  
  ## ######################################################
  
  tmp <- nepva.setinputs(run.type = run.type, modeoptions = modeoptions) 
  
  inputs <- tmp$inputs
  
  for(k in 1:length(tmp$autonames)){
    
    inputs[[tmp$autonames[k]]] <- eval(parse(text = tmp$autonames[k]))
  }
  
  inputs.null <- unlist(lapply(inputs, function(x){is.null(unlist(x))}))
  
  ## print(paste(names(inputs)[inputs.null], collapse=" "))
  
  inputs <- inputs[! inputs.null]
  
  ## ######################################################
  
  out <- nepva.calcs(inputs = inputs, demomods = demomods, run.type = run.type)
  
  ## ######################################################
  
  if(! inputs$silent){
    
    if(output.raw){
      
      bob <- out$tab
    }
    else{
      
      bob <- out
    }
    
    nepva.plot.timeseries(bob, changetablenames = inputs$changetablenames)
  }
  
  ## ######################################################
  
  out
}

## ########################################################################################
## ###################################################################################################################
## >> nepva.validation
## ############################################################################
#' @title NE PVA tool: perform validation of a PVA over a historical period
#' @details This is essentially the same as \code{\link{nepva.fullrun}}, but with
#'   options that are not relevant to validation fixed as follows:
#' (npop = 1, nscen = 0, demobase.splitpops = FALSE, impacts.splitpops = FALSE,
#' impacts.splitimmat = FALSE, impacts.provideses = FALSE,
#' impacts.year.start = NULL, impacts.year.end = NULL,
#' impacts.scennames = NULL, impacts.matchscens = FALSE,
#' impacts.relative = TRUE, 
#' impacts.prod.mean = NULL, impacts.prod.se = NULL, 
#' impacts.survimmat.mean = NULL, impacts.survimmat.se = NULL, 
#' impacts.survadult.mean = NULL, impacts.survadult.se = NULL and
#' output.finalyearonly = FALSE)
#'
#' The function also produces a plot against the validation counts
## ############################################################################
#' @inheritParams nepva.fullrun
## ############################################################################
#' @export

nepva.validation <- function(model.envstoch = "deterministic", model.demostoch = FALSE, 
                               model.dd = "nodd", model.prodmax = TRUE, mbs = NULL, afb,
                               sim.n = 1000, sim.seed = NULL, nburn, demobase.specify.as.params = TRUE,
                               demobase.splitimmat = FALSE, demobase.prod, 
                               demobase.survimmat = NULL, demobase.survadult, 
                               inipop.years, inipop.inputformat = "breeding.pairs",
                               inipop.vals,
                               output.agetype = "breeding.pairs", 
                               output.year.end,
                               output.popsize.target = NULL,
                               output.popsize.qe = 10, output.validation.counts = NULL,
                               output.validation.years = NULL, silent = FALSE, changetablenames = FALSE){
  
  ## ######################################################
  ## Check errors for "output.validation.*" 
  
  if(! is.null(output.validation.counts)){
    
    erra <- ! ((is.numeric(output.validation.counts) & is.vector(output.validation.counts)))
    
    if(erra){ stop("The input 'output.validation.years' has an incorrect format/dimension")}
    
    errb <- ! ((is.numeric(output.validation.counts) & is.vector(output.validation.counts)))
    
    if(errb){ stop("The input 'output.validation.counts' has an incorrect format/dimension")}
    
    errc <- length(output.validation.counts) != length(output.validation.years)
    
    if(errc){ stop("Lengths of 'output.validation.counts' and 'output.validation.years' do not match")}
  }
  
  if(is.null(output.validation.counts) & ! is.null(output.validation.years)){
    
    stop("Lengths of 'output.validation.counts' and 'output.validation.years' do not match")
  }

  ## ######################################################
  ## Added Version 2.1, updated Version 2.2
  
  run.type <- "validation"
  
  ## ######################################################
  
  tmp <- nepva.setinputs(run.type = run.type, modeoptions = modeoptions) 
  
  inputs <- tmp$inputs
  
  for(k in 1:length(tmp$autonames)){
    
    inputs[[tmp$autonames[k]]] <- eval(parse(text = tmp$autonames[k]))
  }
  
  inputs.null <- unlist(lapply(inputs, function(x){is.null(unlist(x))}))
  
  ## print(paste(names(inputs)[inputs.null], collapse=" "))
  
  inputs <- inputs[! inputs.null]
  
  ## ######################################################
  
  out <- nepva.calcs(inputs = inputs, demomods = demomods, run.type = run.type)
   
  ## ######################################################
  
  if(! inputs$silent){
    
    nepva.plot.validation(out, output.validation.counts = output.validation.counts, 
                          output.validation.years = output.validation.years,
                          inipop.years = inipop.years, inipop.vals = inipop.vals,
                          changetablenames = inputs$changetablenames)
  }
  
  ## ######################################################
  
  out
}

## ########################################################################################
## ###################################################################################################################
## >> nepva.sensitivity.local
## ############################################################################
#' @title NE PVA tool: Perform a local sensitivity analysis
#' #' @title NE PVA tool: Perform a global sensitivity analysis
#' @details This function runs a local sensitivity analysis using a simple "one-at-a-time" (OAT) approach,
#'   which looks as how various outputs (impact metrics and summaries of projected population size) change
#'   as the inputs are varied.
#'   
#'   Outputs are summarized in terms of the change in the output relative to a change in each input 
#'   ("sensitivity"), and the change in output relative to the relative (i.e. percentage) change in each input 
#'   ("elasticity")
#'   
#'   The sensitivity analysis is run on a specific version of the PVA in which there is a single
#'    population (npop = 1), there is a single scenario other than the baseline (nscen = 1) and 
#'    immature survival rates are assumed to be the same as adult survival rates (demobase.splitimmat = FALSE,
#'    inipop.splitimmat = FALSE and impact.splitimmat = FALSE).
#'  
#'  The sensitivity analysis then focuses on five input parameters: (1) initial population size,
#'    (2) mean baseline productivity rate, (3) mean baseline adult survival rate,
#'    (4) impact of scenario on baseline productivity and (5) impact of scenario on baseline adult survival.
#' @inheritParams nepva.fullrun
## ############################################################################
#' @param demobase.prod.mean Mean baseline productivity rate. A single numeric value.
#' @param demobase.prod.sd Standard deviation of baseline productivity. A single numeric value.
#' @param demobase.prod.mean Mean Adult survival rate. A single numeric value.
#' @param demobase.prod.sd Standard deviation of adult survival. A single numeric value.
#' @param inipop.year Year associated with initial population size. A single whole number.
#' @param impact.prod.mean Mean impact of scenario upon productivity. A single numeric value.
#' @param impact.survadult.mean Mean impact of scenario upon adult survival. A single numeric value.
#' @returnIf "output.type = "impactmetrics.table"" or "output.type = "summary.table""  the output is a data frame. If
#'         "output.type = " the output is NULL, and the function is run for the by-product of producing a plot.
## ############################################################################
#' @seealso A function that is directly called by the user. Called the internal function \code{\link{nepva.calcs}}
#' @export

nepva.sensitivity.local <- function(model.envstoch = "deterministic",
                                    model.demostoch = FALSE, model.prodmax = TRUE, 
                                    mbs, afb, sim.n, sim.seed, nburn, 
                                    demobase.specify.as.params = TRUE,
                                    demobase.prod,
                                    demobase.survadult,
                                    inipop.years, inipop.vals,
                                    impacts.relative = TRUE, impacts.year.start, impacts.year.end, 
                                    impacts.prod.mean, impacts.survadult.mean, 
                                    output.popsize.qe = 10, output.popsize.target,
                                    output.year.end, 
                                    sens.npvlocal = 1,
                                    sens.pcr = c(20,20,20,20,20), 
                                    silent = FALSE, changetablenames = FALSE){

  ## ##############################################################
  # Error checking
  
  ## errcheck(c("model.demostoch", "model.prodmax", "impacts.relative"), errfn1)
  
  ## errcheck("model.envstoch", errfn2)
  
  ## errcheck(c("afb", "output.year.end", 
  ##           "demobase.prod.mean", "demobase.prod.sd", 
  ##             "demobase.survadult.mean", "demobase.survadult.sd", "sens.npv"), errfn3a)
  
  ## errcheck(c("impact.prod.mean", "impact.survadult.mean"), errfn3b)
  
  ## errcheck(c("mbs", "sim.n", "sim.seed", "impacts.year.start"), errfn4)

  ## ######################################################
  ## Added Version 2.1, updated Version 2.2
  
  run.type <- "sensitivity.local"
  
  ## ######################################################
  
  tmp <- nepva.setinputs(run.type = run.type, modeoptions = modeoptions) 
  
  inputs <- tmp$inputs
  
  for(k in 1:length(tmp$autonames)){
    
    inputs[[tmp$autonames[k]]] <- eval(parse(text = tmp$autonames[k]))
  }
  
  inputs.null <- unlist(lapply(inputs, function(x){is.null(unlist(x))}))
  
  ## print(paste(names(inputs)[inputs.null], collapse=" "))
  
  inputs <- inputs[! inputs.null]
  
  ## ######################################################
  
  out <- nepva.calcs(inputs = inputs, demomods = demomods, run.type = run.type)
  
  ## ##############################################################
  ## Produce plot
  
  nepva.plot.localsens(out, changetablenames = inputs$changetablenames) ## Bug fix: Version 2.4
  
  ## ##############################################################
  
  out
}

## ########################################################################################
## ###################################################################################################################
## >> nepva.global.sensitivity
## ############################################################################
#' @title NE PVA tool: Perform a global sensitivity analysis
#' @details This function runs a global sensitivty analysis using a variance decomposition
#'   approach: it calculates the percentage of variation associated with each of the input parameters using
#'   a Monte Carlo simulation approach.
#'   
#'   The sensitivity analysis is run on a specific version of the PVA in which there is a single
#'    population (npop = 1), there is a single scenario other than the baseline (nscen = 1) and 
#'    immature survival rates are assumed to be the same as adult survival rates (demobase.splitimmat = FALSE,
#'    inipop.splitimmat = FALSE and impact.splitimmat = FALSE).
#'  
#'  The sensitivity analysis then focuses on five input parameters: (1) initial population size ("inipop.size"),
#'    (2) mean baseline productivity rate ("demobase.prod"), (3) mean baseline adult survival rate ("demobase.survadult"),
#'    (4) impact of scenario on baseline productivity ("impacts.prod") and 
#'    (5) impact of scenario on baseline adult survival ("impacts.survadult")
#' @inheritParams nepva.sensitivity.local
## ############################################################################
#' @returnIf "output.type = "impactmetrics.table"" or "output.type = "summary.table""  the output is a data frame. If
#'         "output.type = " the output is NULL, and the function is run for the by-product of producing a plot.
## ############################################################################
#' @seealso A function that is directly called by the user. Called the internal functions 
#'   \code{\link{nepva.sim}}, \code{\link{getpars.demorates}}, 
#'   \code{\link{mean.demorates}}, \code{\link{make.leslie.matrix}}, 
#'   \code{\link{make.demo.cormat}},  \code{\link{make.summary.table}},
#'   \code{\link{make.impactmetrics.table}} and \code{\link{make.timeseries.plot}} 
#' @export

nepva.sensitivity.global <- function(demobase.specify.as.params = TRUE,
                                    model.envstoch = "deterministic",
                                    model.demostoch = FALSE, model.prodmax = TRUE, 
                                    mbs, afb, sim.n, sim.seed, nburn,
                                    demobase.prod, 
                                    demobase.survadult, 
                                    inipop.years, inipop.vals,
                                    impacts.year.start, impacts.year.end, impacts.relative = TRUE,
                                    impacts.prod.mean, impacts.survadult.mean, 
                                    output.popsize.qe = 10, output.popsize.target,
                                    output.year.end, 
                                    sens.npvglobal = 1,
                                    sens.pcr = c(20,20,20,20,20), silent = TRUE, changetablenames = FALSE){

  ## ##############################################################
  # Error checking
  
  ## errcheck(c("model.demostoch", "model.prodmax", "impacts.relative"), errfn1)
  
  ## errcheck("model.envstoch", errfn2)
  
  ## errcheck(c("afb", "output.year.end", "inipop.year",
  ##           "demobase.prod.mean", "demobase.prod.sd", "sens.npv"), errfn3a)
  
  ## errcheck(c("impact.prod.mean", "impact.survadult.mean"), errfn3b)
  
  ## errcheck(c("impacts.year.start", "mbs", "sim.n", "sim.seed"), errfn4)

  ## ######################################################
  ## Added Version 2.1, updated Version 2.2
  
  run.type <- "sensitivity.global"
  
  ## ######################################################
  
  tmp <- nepva.setinputs(run.type = run.type, modeoptions = modeoptions) 
  
  inputs <- tmp$inputs
  
  for(k in 1:length(tmp$autonames)){
    
    inputs[[tmp$autonames[k]]] <- eval(parse(text = tmp$autonames[k]))
  }
  
  inputs.null <- unlist(lapply(inputs, function(x){is.null(unlist(x))}))
  
  ## print(paste(names(inputs)[inputs.null], collapse=" "))
  
  inputs <- inputs[! inputs.null]
  
  ## ######################################################
  
  out <- nepva.calcs(inputs = inputs, demomods = demomods, run.type = run.type)
  
  ## ######################################################
  
  out
}

## ###################################################################################################################
## >> nepva.plot.timeseries
#' @title Create a time series plot of the model outputs
## ####################################################################
## Created v0.2 on 25 October 2018, last modified v1.5 on 16 January 2019
## ####################################################################
#' @param out A data frame, with the format as outputted by \code{\link{nepva.calcs}}
#' @return NULL: the function is run for the side effect of creating a plot
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.run}}.
#' @export

nepva.plot.timeseries <- function(out, changetablenames){

  par(mfrow=c(1,1))
  
  ## ###################################################
  ## Extract adults only - bug fix Version 3.3
  ## bug fix v4.18 to add "| out$Age == "whole.population""
  
  out <- out[(out$Age == "breeding.adults" | out$Age == "breeding.pairs" | out$Age == "whole.population"),]
  
  ## ###################################################
  
  scen.names <- unique(out$Scenario)
  
  ns <- length(scen.names)
  
  cols <- c("black", rainbow(ceiling(ns*1.2))[(ns-1):1])
  
  if(changetablenames){
    
    ylim <- range(c(out[,c("Popsize_2.5%_quantile", "Popsize_97.5%_quantile")]))
  }
  else{
    
    ylim <- range(c(out[,c("popsize.q2.5%", "popsize.q97.5%")]), na.rm=TRUE)
  }
  
  ## ###################################################
  
  years <- sort(unique(out$Year))
  
  plot(years, rep(0,length(years)), col="white", ylim=ylim,
       xlab="Year", ylab="Projected population size")
  
  {for(k in 1:ns){
    
    tmp <- out[out$Scenario == scen.names[k],]
    
    if(changetablenames){
      
      ym <- tmp[,"Popsize_Median"]
      yl <- tmp[,"Popsize_2.5%_quantile"] 
      yu <- tmp[,"Popsize_97.5%_quantile"]
    }
    else{
    
      ym <- tmp$popsize.median
      yl <- tmp[,"popsize.q2.5%"]
      yu <- tmp[,"popsize.q97.5%"]
    }
    
    lines(tmp$Year, ym, lwd=1.8, col=cols[k])
    lines(tmp$Year, yl, lty=3, lwd=1.8, col=cols[k])
    lines(tmp$Year, yu, lty=3, lwd=1.8, col=cols[k])
  }}
  
  legend(min(years), ylim[1] + (ylim[2] - ylim[1])*0.27, scen.names, fill = cols,
         title = "Scenarios", bg="white") ## added Version 0.5
  
  ## ###################################################
  
  NULL 
}

## ###################################################################################################################
## >> nepva.plot.validation
#' @title Validate a PVA applied to a historical period
## ####################################################################
## Added as part of v1.5 on 16 January 2019
## ####################################################################
#' @param out A data frame, with the format as outputted by \code{\link{nepva.calcs}}
#' @return NULL: the function is run for the side effect of creating a plot
## ####################################################################
#' @seealso An internal function that is called by \code{\link{nepva.run}}.
#' @export
#' 

nepva.plot.validation <- function(out, output.validation.counts, output.validation.years,
                                  inipop.vals, inipop.years, changetablenames){
  
  par(mfrow=c(1,1))
  
  years <- sort(unique(out$Year))

  if(changetablenames){
   
    pvnames <- c("Popsize_2.5%_quantile", "Popsize_Median", "Popsize_97.5%_quantile")
  }
  else{
    
    pvnames <- c("popsize.q2.5%", "popsize.median", "popsize.q97.5%")
  }
  
  pv <- out[,pvnames]
            
  ylim <- range(c(pv[,c(1,3)], output.validation.counts), na.rm=TRUE)
  
  plot(years, rep(0,length(years)), col="white", ylim=ylim,
       xlab="Year", ylab="Projected population size")

  lines(out$Year, pv[,2], lwd=1.8, col=gray(0.3))
  lines(out$Year, pv[,1], lty=3, lwd=1.8, col=gray(0.3))
  lines(out$Year, pv[,3], lty=3, lwd=1.8, col=gray(0.3))
  
  points(output.validation.years, output.validation.counts, pch=20)
  
  points(inipop.years, inipop.vals, pch=20, col="red")
  points(inipop.years, inipop.vals)
  
  NULL
}

## ###################################################################################################################
## Run an error check on inputs
## ###################################################################################################################

nepva.errorcheck <- function(inputs){

  ## #########################################
  ## Attach "inputs" object
  
  attach(inputs)  

  ## ##############################################
  ## Step 1. Check format for objects of length 1

  errcheck(c("model.demostoch", "model.prodmax", "demobase.splitpops", "demobase.specify.as.params",
             "demobase.splitimmat", "inipop.splitimmat", "impact.splitpops", 
             "impacts.splitimmat", "impacts.provideses",
             "impacts.relative"), errfn1)

  errcheck(c("model.envstoch", "model.dd", "inipop.inputformat", "output.agetype"), errfn2)

  errcheck(c("afb", "npop", "output.year.end"), errfn3a)

  errcheck("nscen", errfn3b)

  errcheck(c("mbs", "sim.n", "sim.seed", "impacts.year.start", "impacts.year.end", "output.popsize.qe", "output.popsize.target"), errfn4)

  ## ##############
  ## Step 2. Check contents for objects for object of length 1
  
  envmodels <- c("deterministic", "betagamma", "logitnlogn")

  iniagetypes <- c("breeding.adults", "breeding.pairs", "all.individuals")

  outagetypes <- c("breeding.adults", "breeding.pairs", "age.separated")

  if(is.na(match(model.envstoch, envmodels))){ stop("An invalid option has been selected for 'model.envstoch'...") }

  if(is.na(match(inipop.inputformat, iniagetypes))){ stop("An invalid option has been selected for 'inipop.inputformat'...") }

  if(is.na(match(output.agetype, outagetypes))){ stop("An invalid option has been selected for 'output.agetype'...") }

  ## ##############
  ## Step 3. Check initial population sizes
  ## [inipop.years, inipop.vals, inipop.immatvals]
 
  if(errfn5(inipop.years, npop)){ stop("The input 'inipop.years' has an incorrect format/dimension")}

  if(errfn5(inipop.vals, npop)){ stop("The input 'inipop.years' has an incorrect format/dimension")}

  if(inipop.splitimmat){
  
    err <- errfn6(inipop.immatvals, nr = afb, nc = npop)
    
    if(err){ stop("The input 'inipop.immatvals' has an incorrect format/dimension") }
  }
  else{
    
    if(! is.null(inipop.immatvals)){ stop("The input 'inipop.immatvals' has an incorrect format/dimension") }
  }

  ## ##############
  ## Step 4. Check baseline demography   
  ## [::: demobase.prod, demobase.survimmat, demobase.survadult :::]
  ## NEED TO ADD a check for "demobase.survimmat"
  
  if(demobase.specify.as.params){
  
    npa <- demomods[[which((names(demomods) == paste(model.envstoch, model.dd, sep=".")))]]$np
  }
  else{
    
    npa <- 2
  }

  if(demobase.splitpops){
    
    if(errfn6(demobase.prod, npop, npa)){stop("The input 'demobase.prod' has an incorrect format/dimension")}
    if(errfn6(demobase.survadult, npop, npa)){stop("The input 'demobase.survadult' has an incorrect format/dimension")}
  }
  else{
    
    if(errfn5(demobase.prod, npa)){stop(errmess)}
    if(errfn5(demobase.survadult, npa)){stop(errmess)}
  }
 
  ## ##############
  ## Stage 5. Check baseline demography: correlation]
  ## [::: demobase.cormat :::] 

  if(! is.null(demobase.cormat)){ ## Changed Version 1.1
  
    erra <- ! (is.matrix(demobase.cormat))
  
    if(erra){ stop("The input 'demobase.cormat' has an incorrect format")}
  
    errb <- any(dim(demobase.cormat) != (amax + 2))
  
    if(errb){ stop("Dimension of 'demobase.cormat' is incorrect")}
  }

  ## ##############################################
  ## Stage 6. Check baseline demography: skipped breeding
  ## [: demobase.skippc]

  if(errfn7(demobase.bskippc, 2)){stop("The input 'demobase.bskippc' has an incorrect format")}
  
  ## ##############################################
  ## Stage 7. Check impacts 
  
  ## [:: impacts.scennames, impacts.survimmat.mean, impacts.survimmat.se, 
  ##     impacts.survimmat.mean, impacts.survimmat.se, 
  ##     impacts.survimmat.mean, impacts.survimmat.se ::]

  if(nscen > 0){

    err <- ! (is.character(impacts.scennames) & is.vector(impacts.scennames))

    if(err){ stop("The input 'impacts.scennames' has an incorrect format/dimension")}
  }
  
  ## ##################
  
  if(nscen > 0){
  
    if(impacts.splitpops){
    
      if(errfn6(impacts.prod.mean, nscen, npop)){stop("The input 'impacts.prod.mean' has an incorrect format/dimension")}
    
      if(errfn6(impacts.survadult.mean, nscen, npop)){stop("The input 'impacts.survadult.mean' has an incorrect format/dimension")}
    }
    else{
    
      if(errfn5(impacts.prod.mean, nscen)){stop("The input 'impacts.prod.mean' has an incorrect format/dimension")}
    
      if(errfn5(impacts.survadult.mean, nscen)){stop("The input 'impacts.survadult.mean' has an incorrect format/dimension")}
    }

  ## ##################

  if(impacts.provideses){
  
    if(impacts.splitpops){
    
      if(errfn6(impacts.prod.se, nscen, npop)){stop("The input 'impacts.prod.se' has an incorrect format/dimension")}
    
      if(errfn6(impacts.survadult.se, nscen, npop)){stop("The input 'impacts.survadult.se' has an incorrect format/dimension")}
    }
    else{
    
      if(errfn5(impacts.prod.se, nscen)){stop("The input 'impacts.prod.se' has an incorrect format/dimension")}
    
      if(errfn5(impacts.survadult.se, nscen)){stop("The input 'impacts.survadult.se' has an incorrect format/dimension")}
    }
  }
  else{
    
    if(! is.null(impacts.prod.se)){stop("The input 'impacts.prod.se' has an incorrect format/dimension")}
    
    if(! is.null(impacts.survadult.se)){stop("The input 'impacts.survadult.se' has an incorrect format/dimension")}
  }
  
  ## ##################
  
  if(impacts.splitimmat){
    
    if(impacts.splitpops){
      
      if(errfn6(impacts.survimmat.mean, nscen, npop)){stop("The input 'impacts.survimmat.mean' has an incorrect format/dimension")}
    }
    else{
      
      if(errfn5(impacts.survimmat.mean, nscen)){stop("The input 'impacts.survimmat.mean' has an incorrect format/dimension")}
    }
  }
  else{
    
    if(! is.null(impacts.survimmat.mean)){stop("The input 'impacts.survimmat.mean' has an incorrect format/dimension")}
  }
  
  ## ##################
  
  if(impacts.splitimmat & impacts.provideses){
    
    if(impacts.splitpops){
      
      if(errfn6(impacts.survimmat.se, nscen, npop)){stop("The input 'impacts.survimmat.se' has an incorrect format/dimension")}
    }
    else{
      
      if(errfn5(impacts.survimmat.se, nscen)){stop("The input 'impacts.survimmat.se' has an incorrect format/dimension")}
    }
  }
  else{
    
    if(! is.null(impacts.survimmat.se)){stop("The input 'impacts.survimmat.se' has an incorrect format/dimension")}
  }
  }
  
  ## #############################
  ## Step 8. Is this a stochastic or deterministic run?
  ## #######################

  is.determ <- (model.envstoch == "deterministic") & (! model.demostoch) & (! impacts.provideses)

  ## #############################
  ## Step 9. Assign values where inputs are NULL
  ## #######################

  if(is.determ){ 
  
    warning("Deterministic run! Over-riding value of 'sim.n', using 'sim.n = 1'") 
  
    sim.n <- 1
  
    warning("Deterministic run! Over-riding value of 'sim.seed'")
  
    sim.seed <- 1
  }

  if(is.null(sim.n)){
  
    warning("Value of 'sim.n' not provided; setting 'sim.n = 1000'")
  }

  if(is.null(sim.seed)){
  
    ru <- round(runif(1,0.5,10000.5))
  
    warning(paste("Value of 'sim.seed' not provided; selected to be automatically be", ru))
  }

  ## #################

  if(is.null(output.year.start)){
  
    output.year.start <- max(inipop.years) + 1
  
    warning(paste("Value of 'output.year.start' not provided; automatically setting is equal to", output.year.start))
  }

  ## #################

  if(is.null(impacts.year.end)){
  
    impacts.year.end <- output.year.end
  
    warning(paste("Value of 'impacts.year.end' not provided; automatically setting is equal to", impacts.year.end))
  }

  ## #############################
  ## Step 10. Check ranges and option combinations are valid
  ## #######################

  if( ! any((names(demomods) == paste(model.envstoch, model.dd, sep=".")))){
  
    stop("The choices for 'model.envstoch' and 'model.dd' do not correspond to a 
         model that is currently implemented (see 'names(depomods)' for a list
         of models that are currently implemented")
  }

  if((demobase.specify.as.params == FALSE) & (model.dd != "nodd")){
  
    stop("Cannot perform moment matching for models containing density dependence! 
         If 'demobase.specify.as.params = TRUE' then 'model.dd' must equal 'nodd'")
  }

  if(is.null(mbs) & (model.prodmax)){ stop("If 'model.prodmax = TRUE' then the value of 'mbs' cannot be NULL")}

  if(nscen > 0){
  
    if(impacts.year.start <= max(inipop.years)){ stop("The value of 'impacts.year.start' must be greater than 'max(inipop.years)")}

    if(impacts.year.end <= impacts.year.start){ stop("The value of 'impacts.year.end' must be greater than 'impacts.year.start")}

    if(output.year.end <= output.year.start){ stop("The value of 'output.year.end' must be greater than 'output.year.start")}
  }
  
  if(sim.n < 1000){ warning("Simulations based on values of 'sim.n' lower than 1000 are unlikely to lead to stable results, and should be used for testing/exploratory purposes only!") }

  if(sim.n > 1000000){ warning("Simulations based on values of 'sim.n' higher than 1000000 are likely to be highly computer intensive to run...") }

  ## ###################################################################################################################

  detach(inputs)  

  ## ###################################################################################################################

  NULL
}

## ############################################################################
## Created version 1.4:

errcheck <- function(x, errfn){ 
  
  for(k in 1:length(x)){
    
    print(x[k])
    
    bob <- get(x[k])
    
    if(errfn(get(x[k]))){ stop(paste0("The input '", x[k], "' has an incorrect format...")) }
  }
  
  NULL
}

## ############################################################################

errfn1 <- function(x){! (is.logical(x) & is.vector(x) & length(x == 1)) }

errfn2 <- function(x){! (is.character(x) & is.vector(x) & length(x == 1)) }

errfn3a <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & (x > 0)) }

errfn3b <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & (x >= 0)) }

errfn4 <- function(x){  if(is.null(x)){ out <- FALSE  }
  else{ out <- ! ((is.numeric(x) & is.vector(x) & length(x == 1) & (x > 0)) | is.null(x)) }
  out
}

errfn5 <- function(x,n){ ! (is.numeric(x) & is.vector(x) & (length(x) == n) & (all(x > 0))) }

errfn6 <- function(x,nr,nc){ err <- ! ((is.data.frame(x) | is.matrix(x)) & (ncol(x) == nc) & (nrow(x) == nr)) }

errfn7 <- function(x,n){ ! ((is.numeric(x) & is.vector(x) & (length(x) == n)) | is.null(x)) }

## ############################################################################
## Added 16 Jan 2019
## ############################################################################

nepva.plot.localsens <- function(obj, changetablenames){
  
  pars <- c("inipop.vals", "demobase.prod.mean", "demobase.survadult.mean",
            "impact.prod.mean", "impact.survadult.mean")
  
  cols <- c("blue", "orange", "red", "green", "darkgreen")
    
  if(changetablenames){
    
    cnames <- c("CGR_Median", "CPS_Median", "QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS")
      
    mnames <- c("CGR (median)", "CPS (median)", "QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS")
  }
  else{
  
    cnames <- c("m1.median", "m2.median", "m3", "m4", "m5", "m6")
  
    mnames <- c("M1 (median)", "M2 (median)", "M3", "M4", "M5", "M6")
  }
  
  npp <- length(pars)
  
  par(mfrow=c(3,2))
  
  for(i in 1:length(cnames)){
    
    xlim <- range(obj[,substr(colnames(obj),1,8) == "pcchange"])
    
    ylim <- range(obj[,cnames[i]])
  
    if(all(! is.na(ylim))){
    
      plot(0,0,xlim=xlim,ylim=ylim,cex.axis=1.3,cex.lab=1.3,xlab="% change in input parameter",
           ylab="Metric", main=mnames[i])
    
      for(j in 1:length(pars)){
      
       mm <- which(! is.na(match(obj$parname, c("standard", pars[j]))))
      
        x <- obj[mm, paste0("pcchange.", pars[j])]
      
        y <- obj[mm, cnames[i]]
      
        y <- y[order(x)]
      
        x <- x[order(x)]
      
        points(x,y,col=cols[j], pch=20)
      
        lines(x,y,col=cols[j],lwd=1.3)
      }
    }
  }
  
  NULL
}

## ############################################################################
## Created 14 February 2019:

asla <- function(obj, tests, val){ 
  
  mm <- which(tests) 
  
  for(k in 1:length(mm)){ obj[[mm[k]]] <- val } 
  
  obj 
}

## ############################################################################
## Created 14 February 2019:

nepva.setinputs <- function(run.type, modeoptions){
  
  ## #############################################
  
  inputs <- as.list(rep(NA, nrow(modeoptions)))
  
  names(inputs) <- modeoptions$Argument
  
  ## #############################################
  
  type <- as.character(modeoptions[,run.type])
  
  ## #############################################
 
  if(all(is.na(type))){ ## bug fix v4.18: added clause needed for 'nepva.fullrun'
    
    type <- rep("", length(type)) ## 4.18 - this line added
  }
  else{ ## v4.18: this part unchanged:
  
  if(any(type == "Calculate")){
   
     inputs <- asla(inputs, type == "Calculate", "To be calculated !!")
  }
  ## #############################################
  
  if(any(type == "Fix::TRUE")){
    
     inputs <- asla(inputs, type == "Fix::TRUE", TRUE)
  }
  
  if(any(type == "Fix::FALSE")){
    
     inputs <- asla(inputs, type == "Fix::FALSE", FALSE)
  }
  
  ## #############################################
  
  tests <- (substr(type, 1, 9) == "Fix::Val:")
  
  if(any(tests)){
    
    mm <- which(tests)
    vals <- as.numeric(substr(type, 10, 100)[mm])
    
    for(k in 1:length(mm)){
      
      inputs[[mm[k]]] <- vals[k]
    }
  }
  
  ## #############################################
  
  tests <- (substr(type, 1, 10) == "Fix::Char:")
  
  if(any(tests)){
    
    mm <- which(tests)
    
    vals <- as.character(substr(type, 11, 100)[mm])
    
    for(k in 1:length(mm)){
      
      inputs[[mm[k]]] <- vals[k]
    }
  }
  
  ## #############################################
  ## Note: set to "list(NULL)" so this is not dropped from list
  ##  (as that would mess up indexing)
  
  inputs <- asla(inputs, type == "Fix::NULL", list(NULL)) 
  } ## close extra clause added for v4.18
  
  ## #############################################

  autonames <- as.character(modeoptions$Argument[type == ""])
  
  ## #############################################
  
  list(inputs = inputs, autonames = autonames)
}

## ###################################################################################################################
## >> nepva.calcs
## Version 4.11 on 13 Jan 2020: added "changetablenames" option

nepva.calcs <- function(inputs, demomods, run.type){
  
  ## ######################################################################
  
  out <- NULL
  
  ## ######################################################################
  
  if(run.type == "sensitivity.local" | run.type == "sensitivity.global"){
    
    if(run.type == "sensitivity.local"){
      
      out <- nepva.calcs.localsens(inputs = inputs, demomods = demomods)
    }
    else{
      
      out <- nepva.calcs.globalsens(inputs = inputs, demomods = demomods)
    }
  }
  
  if(run.type == "fullrun" | run.type == "simplescenarios" | run.type == "validation"){
    
    if(run.type == "validation"){
      
      inputs$output.year.start <- inputs$inipop.years
    }
    
    out <- nepva.calcs.main(inputs = inputs, demomods = demomods)
  }
  
  ## ######################################################################
  
  if(is.null(out)){ stop("Invalid choice for 'run.type'...")}
  
  ## ######################################################################
  ## Added Version 4.11, revised Version 4.13
  
  if(inputs$changetablenames){
    
    if(run.type == "sensitivity.global"){
      
      out$tab <- change.table.names.globalsens(out$tab)
    }
    
    if(run.type == "sensitivity.local"){
      
      out <- change.table.names.sen(out)
    }
    
    if(run.type == "simplescenarios" | run.type == "validation"){
    
      if(inputs$output.raw){ 
      
        out$tab <- change.table.names(out$tab)
      }
      else{
      
        out <- change.table.names(out)
      }
    }
  }
 
  ## ######################################################################
  
  out
}

## ###################################################################################################################
## Added Version 4.11:

change.table.names <- function(tab){
  
  qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
  
  old.names <- c("Year", "Age", "Scenario", "Baseyear", "Currently.Impacted", "Impact.year",
                 paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep="."),
                 paste(rep(c("pgr", "agr", "ppc", "m1", "m2"), 1, each = 5),
                       rep(c("median", "mean", "sd", "cilo", "cihi"), 5), sep="."),
                 paste0("m", 3:6))
  
  new.names <- c("Year", "Age_Class", "Scenario", "Base_Year", "Currently_Impacted", "YRS_From_First_Imp",
                 paste("Popsize", c("Mean", "SD", "Median", paste0(qs, "%_quantile")), sep="_"),
                 paste(rep(c("pgr", "Annual_GR", "pc_Pop_Change", "CGR", "CPS"), 1, each = 5),
                       rep(c("Median", "Mean", "SD", "LCI", "UCI"), 5), sep="_"),
                 "QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS")
  
  colnames(tab)[match(colnames(tab), old.names)] <- new.names
  
  tab
}

## ###################################################################################################################
## Added Version 4.13:

change.table.names.sen <- function(tab){
  
  qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
  
  old.names <- c("parname","pcchange.inipop.vals", "pcchange.demobase.prod.mean",
                 "pcchange.demobase.survadult.mean", "pcchange.impact.prod.mean",
                 "pcchange.impact.survadult.mean", "inipop.vals", "demobase.prod.mean",
                 "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean",
                 "Year", "Age", "Scenario", "Baseyear", "Currently.Impacted", "Impact.year",
                 paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep="."),
                 paste(rep(c("pgr", "agr", "ppc", "m1", "m2"), 1, each = 5),
                       rep(c("median", "mean", "sd", "cilo", "cihi"), 5), sep="."),
                 paste0("m", 3:6))
  
  new.names <- c("parname","pcchange.inipop.vals", "pcchange.demobase.prod.mean",
                 "pcchange.demobase.survadult.mean", "pcchange.impact.prod.mean",
                 "pcchange.impact.survadult.mean", "inipop.vals", "demobase.prod.mean",
                 "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean",
                 "Year", "Age_Class", "Scenario", "Base_Year", "Currently_Impacted", "YRS_From_First_Imp",
                 paste("Popsize", c("Mean", "SD", "Median", paste0(qs, "%_quantile")), sep="_"),
                 paste(rep(c("pgr", "Annual_GR", "pc_Pop_Change", "CGR", "CPS"), 1, each = 5),
                       rep(c("Median", "Mean", "SD", "LCI", "UCI"), 5), sep="_"),
                 "QuantileUNIMP50pcIMP", "QuantileIMP50pcUNIMP", "Quasi_Extinction", "pc_ImpSims_above_TPS")
  
  colnames(tab)[match(colnames(tab), old.names)] <- new.names
  
  tab
}

## ############################################################################

change.table.names.globalsens <- function(tab){
  
  qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
  
  old.names <- c(paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep="."),
                 paste(rep(c("pgr", "agr", "ppc"), 1, each = 5),
                       rep(c("median", "mean", "sd", "cilo", "cihi"), 3), sep="."))

  new.names <- c(paste("Popsize", c("Mean", "SD", "Median", paste0(qs, "%_quantile")), sep="_"),
                 paste(rep(c("pgr", "Annual_GR", "pc_Pop_Change"), 1, each = 5),
                       rep(c("Median", "Mean", "SD", "LCI", "UCI"), 3), sep="_"))

  colnames(tab)[5+(match(colnames(tab)[-(1:5)], old.names))] <- new.names
  
  tab
}
