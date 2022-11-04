## ###############################################################################
## TEMPORARY BUG FIX - TO DEAL WITH SITUATIONS WHERE SD = 0 AND THE TOOL WAS
##  PRODUCING INCORRECT OUTPUT - NOW PRODUCES AN ERROR MESSAGE IN THESE SITUATIONS
## ###############################################################################

## ###############################################################################
## Specification of parametric models for the inter-annual distribution of 
##   demographic rates - modelling both
##   environmental stochasticity and density dependence 
##
## Added in this format in Version 0.5, on 5 Nov 2018, in order to make it as simple as possible for 
##  new parametric models to be added
##
## Also set up now so that model simulation is based on applying a deterministic function to a random
##  number that has *already* been simulated
##
## For each model, up to six functions are provided; functions are provided for 
##   an "constrained" version of the model (which constrain predictions to be on the range [0,1])
##   and an "unconstrained" version of the model (which constrains predictions only to be non-negative)
##
## For each of these variants, the following functions are provided:
##   - a function to simulate from the model
##   - a function to generate (deterministic) predictions from the model
##   - a function to calculate parameter estimates based on summaries (mean and standard deviation)
##      via moment matching
##
## Type: aa = analytic formula for predictions, analytic formula for moment matching using mean & SD
##     : an = analytic formula for predictions, numeric approach to moment matching
##     : ai = analytic formula for predictions, moment matching not possible
##     : nn = numeric approach to predictions and moment matching
##     : ni = numeric approach to predictions, moment matching not possible
##
##  "sim." function needed in all cases
##  "pred." function needed for types "aa", "an" and "ai"
##  "mom." function need for type "aa" only
##
## Version 0.6, 12 November 2018: changed so that "ru" is standard normal rather than standard uniform
##
## Version 1.1, 4 January 2019: added "ddthresh" option
##                            : added density dependence options for "betagamma"
##
## 16 Aug 2022: added error message whenever zero value in denominator
## ###############################################################################

## ###############################################################################
## Utility functions

logit <- function(x){log(x/(1-x))}

ilogit <- function(x){ 
  
  x[x > 100] <- 100 # added Version 0.5
  x[x < -100] <- -100
  
  exp(x)/(1 + exp(x))
}

H <- function(x){x * (x > 0)} ## Added Version 1.1

## ###############################################################################
## Model list:

demomods <- as.list(NULL)

## ###############################################################################
## STOCHASTIC MODELS
## ###############################################################################

## ##############################
## mom. :: Change from (mean, SD) to (mean, precision)
## https://cran.r-project.org/web/packages/betareg/vignettes/betareg.pdf
## Change from (mean, precison) to standard parameterization

mom.beta <- function(smy){ if(smy[2] == 0){stop("Zero value in denominator! - please ensure that the standard deviations for the demographic parameters are greater than zero")} ; mu <- smy[1] ; prec <- (mu * (1 - mu) / (smy[2]^2)) - 1 ; c(mu, prec) }
 
mom.gamma <- function(smy){ if(smy[1] == 0){stop("Zero value in denominator! - please ensure that the standard deviations for the demographic parameters are greater than zero")} ; mu <- smy[1] ; scalepar <- smy[2]^2/smy[1] ; c(mu, scalepar) } 
  
## ##############################

revreparam.beta <- function(mu, prec){ prec * cbind(mu, 1 - mu) }

revreparam.gamma <- function(mu, scalepar){ if(scalepar == 0){stop("Zero value in denominator! - please ensure that the standard deviations for the demographic parameters are greater than zero")} ; cbind(mu / scalepar, rep(scalepar, length(mu))) } 

## ###############################################################################

demomods <- as.list(NULL)

## ###############################################################################
## DENSITY DEPENDENCE MODELS
## ###############################################################################

demomods$dd <- list(
  nodd = list(npdd = 1, fn = function(pars, popsize){ rep(pars[1], length(popsize)) }),
  dduloglin = list(npdd = 2, fn = function(pars, popsize){ pars[1] + pars[2] * log10(popsize + 1) }),
  ddulinear = list(npdd = 2, fn = function(pars, popsize){ pars[1] + pars[2] * popsize }),
  dduweibull = list(npdd = 3, fn = function(pars, popsize){ pars[1] + pars[2] * (popsize ^ pars[3]) }),
  dduthresh = list(npdd = 3, fn = function(pars, popsize){ pars[1] + pars[2] * H(popsize - pars[3]) }))

## ###############################################################################
## MODELS FOR ENVIRONMENTAL STOCHASTICITY
## ###############################################################################

demomods$es$deterministic.ddu <- list(npes = 1, 
                                 sim.con = function(ru, pars, popsize, ddfn) { ddfn(pars[-2], popsize) },
                                 sim.unc = function(ru, pars, popsize, ddfn) { ddfn(pars[-2], popsize) },
                                 pred.con = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) },
                                 pred.unc = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) },
                                 mom.con = function(smy){ smy }, mom.unc = function(smy){ smy})

demomods$es$deterministic.ddt <- list(npes = 1, 
                                 sim.con = function(ru, pars, popsize, ddfn) { ilogit(ddfn(pars[-2], popsize)) },
                                 sim.unc = function(ru, pars, popsize, ddfn) { exp(ddfn(pars[-2], popsize)) },
                                 pred.con = function(pars, popsize, ddfn){ ilogit(ddfn(pars[-2], popsize)) },
                                 pred.unc = function(pars, popsize, ddfn){ exp(ddfn(pars[-2], popsize)) })

## Version 3.3: bug fix in "sim.unc", on line "qgamma...":

demomods$es$betagamma.ddu <- list(npes = 1,
                             sim.con = function(ru, pars, popsize, ddfn) { 
                               mu <- ddfn(pars[-2], popsize)
                               newpars <- revreparam.beta(mu, pars[2])
                               ## if(any(is.na(newpars)|is.nan(newpars)|(newpars<0))){browser()}
                               qbeta(pnorm(ru), newpars[,1], newpars[,2])
                             },
                             sim.unc = function(ru, pars, popsize, ddfn){
                               mu <- ddfn(pars[-2], popsize)
                               newpars <- revreparam.gamma(mu, pars[2])
                               qgamma(pnorm(ru), shape = newpars[,1], scale = newpars[,2])},
                             pred.con = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) },
                             pred.unc = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) },
                             mom.con = mom.beta,
                             mom.unc = mom.gamma)

demomods$es$betagamma.ddt <- list(npes = 1,
                             sim.con = function(ru, pars, popsize, ddfn) { 
                               mu <- ilogit(ddfn(pars[-2], popsize))
                               newpars <-  revreparam.beta(mu, pars[2]) 
                               qbeta(pnorm(ru), newpars[,1], newpars[,2])},
                             sim.unc = function(ru, pars, popsize, ddfn){
                               mu <- exp(ddfn(pars[-2], popsize))
                               newpars <-  revreparam.gamma(mu, pars[2]) 
                               qgamma(pnorm(ru), shape = newpars[,1], scale = newpars[,2])},
                             pred.con = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) },
                             pred.unc = function(pars, popsize, ddfn){ ddfn(pars[-2], popsize) }) ## typo on lines 136 and 137, fixed v 4.19

demomods$es$logitnlogn.ddt <- list(npes = 1, 
  sim.con = function(ru, pars, popsize, ddfn) { ilogit(ddfn(pars[-2], popsize) + ru * pars[2]) },
  sim.unc = function(ru, pars, popsize, ddfn) { exp(ddfn(pars[-2], popsize) + ru * pars[2]) },
  pred.unc = function(pars, popsize, ddfn){ exp(ddfn(pars[-2], popsize) + (pars[2]^2/2)) }
)

## ###############################################################################