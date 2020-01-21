e## ###############################################################################################
## Created 12 September 2019, last modified 13 January 2020
## Automated testing of the NE PVA R package
##
## Version 4.11 - re-running with more plausible ranges for impact magnitudes, 
##  and (from Version 4.10) to incorporate code for Kate to do manual testing
## ###############################################################################################

library(popbio)

setwd("C://Users//adam//Work//Active//JNCC-PVA-testing//Version4.12//")

ff <- list.files(".", pattern="functions") ; for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

modeoptions <- read.csv("ModeOptions.csv") ## Added Version 2.1

path.out <- "Outputs//"

## ###############################################################################################
## Stage 1. Testing valid inputs
## ###############################################################################################

set.seed(67967)

out1 <- autocheck.validity.nepva(nsm = c(200,200,100,100), test.invalid = FALSE)

write.csv(out1, file=paste0(path.out, "autocheck-validinputs.csv"), quote=FALSE, row.names=FALSE)

out1 <- fixstatus.pva(out1) ## modified 13 January 2020
  
table(out1$status, out1$runtype)[,c(3,4,2,1)]

## table(out1$runtype, out1$errmess)

## out$errmess <- factor(out$errmess)

## sort(summary(droplevels(factor(out$errmess[out$runtype == "simplescenarios"]))))
## sort(summary(droplevels(out$errmess[out$runtype == "validation"])))
## sort(summary(droplevels(out$errmess[out$runtype == "sensitivity.local"])))
## sort(summary(droplevels(out$errmess[out$runtype == "sensitivity.global"])))

## tapply(out1$check.validoutput, out1$runtype, sum)
## tapply(out1$check.noerrors, out1$runtype, sum) - tapply(out1$check.validoutput, out1$runtype, sum)

## ###############################################################################################
## Stage 1a. Representative subset for Kate to evaluate manually using Shiny tool
##   -- added 13 January 2020
## ###############################################################################################

seed.subset <- 5986 ## Seed for random subsampling, NOT random generation of inputs

nman <- data.frame(simplescenarios = c(16,1,1,1,1), validation = c(1,1,1,1,1), sensitivity.local = c(0,2,1,1,1))

inputspecs.subset <- goodsubset.inputs.valid(inputspecs = out1, nman = nman, seed.subset = seed.subset)

nepva.save.and.run.valid(inputspecs = inputspecs.subset, outpath = "Outputs//mancheck-")

## ###############################################################################################
## Stage 2. Testing invalid inputs
## ###############################################################################################

errmessages <- list(
  e1 = "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Population size explosion - will lead to numerical overflow",
  e2 = "Error in inits.burned(nbyage.burned = nbyage.burned; inipop.totals = inipop.totals): Error! Zero values during burn-in...",
  e3.a = "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Invalid productivity rates simulated!",
  e3.b = "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Invalid survival probabilities simulated!",
  e4 = "Error in !model.demostoch: invalid argument type",
  e5 = "Error in demomods$es[[zi]]: attempt to select less than one element in get1index",
  e6 = "Error in inputs$demobase.prod[i; ]: subscript out of bounds",
  e7 = "Error in demobase.vals[i; nd; ] <- as.numeric(c(inputs$demobase.survadult)): number of items to replace is not a multiple of replacement length", 
  e8 = "Error in demobase.vals[i; nd; ] <- as.numeric(c(inputs$demobase.survadult[i; : number of items to replace is not a multiple of replacement length",
  e8.a = "Error in demobase.vals[i; j + 1; ] <- as.numeric(c(inputs$demobase.survadult[i; : number of items to replace is not a multiple of replacement length",
  e9 = "Error in ru.base[j; tt; ; ; drop = FALSE]: subscript out of bounds",
  e10 = "Error in nbyage[i; j; inipop.relyears[j]; ; a] <- inipop.counts[a; ; j]: NAs are not allowed in subscripted assignments",    
  e11 = "Error in eigen(mmat): infinite or missing values in 'x'",
  e12 = "Error in year.first:output.year.end: result would be too long a vector",
  e13 = "Error in if (max(frot) > 1e+08) {: missing value where TRUE/FALSE needed",
  e14 = "Error in if (tt > inipop.relyears[j]) {: missing value where TRUE/FALSE needed",
  e15 = "Error in out[1] <- mbs * fn.pred.con(pars = as.numeric(c(ests[1; ])); : replacement has length zero",  
  e16 = "Error in out[ids; idp; 1] <- prod: number of items to replace is not a multiple of replacement length", 
  e17 = "Error in ests[1; 1:2] <- fn.mom.con(c(prod.mn/mbs; prod.sd/mbs)): replacement has length zero",
  e18 = "Error in demobase.vals[i; j + 1; ] <- as.numeric(c(inputs$demobase.survadult)): number of items to replace is not a multiple of replacement length",
  e19 = "Error in ntot[; ic; ; j; drop = FALSE]: subscript out of bounds",   
  e20 = "Error in if (any(nprev < 0)) {: missing value where TRUE/FALSE needed", 
  e21 = "Error in apply(ntots; 1; getsumy): dim(X) must have a positive length",
  e22 = "Error in Ops.data.frame(standard.vals; (1 + (pcchange/100))): '*' only defined for equally-sized data frames",
  e23 = "Error in `colnames<-`(`*tmp*`; value = inputnames): attempt to set 'colnames' on an object with less than two dimensions",  
  e24 = "replacement has 11 rows; data has 2")

set.seed(46569)

out2 <- autocheck.validity.nepva(nsms = c(200,200,100,100), test.invalid = TRUE)

out2.stored <- out2

write.csv(out2, file=paste0(path.out, "autocheck-invalidinputs.csv"), quote=FALSE, row.names=FALSE)

levels(out2$errmess)[grep("replacement has 11 rows; data has 2", levels(out2$errmess))] <- "replacement has 11 rows; data has 2"

levels(out2$errmess) <- c(levels(out2$errmess), "e0")
out2$errmess[is.na(out2$errmess)] <- "e0"

for(k in 1:length(errmessages)){

  levels(out2$errmess)[levels(out2$errmess) == errmessages[k]] <- names(errmessages)[k]
}

levels(out2$errmess) <- gsub(".a", "", levels(out2$errmess))
levels(out2$errmess) <- gsub(".b", "", levels(out2$errmess))

tab <- table(out2$errmess, out2$ought.errtype)
tab <- tab[match(paste0("e",0:24), row.names(tab)),]

write.csv(tab, file=paste0(path.out, "autocheck-summary-invalidinputs.csv"), quote=FALSE, row.names=TRUE)

## table(out$ought.valid, out$check.noerrors)

## table(out$ought.valid, out$check.validoutput)

## tmp <- out[(! out$ought.valid) & out$check.noerrors,]
## summary(factor(tmp$ought.errtype))

## summary(factor(out$ought.errtype[(! out$ought.valid) & out$check.noerrors]))
## summary(factor(out$ought.errtype[(! out$ought.valid) & out$check.validoutput]))

## ###############################################################################################
## Stage 3. Check internal consistency
## Version 4.7: issues in code fixed, so should now be running correctly...
## ###############################################################################################

out3 <- autocheck.consistency.nepva(nsm = 20, sim.n = 5)

levels(out3$errmess1) <- gsub(",",";",levels(out3$errmess1))
levels(out3$errmess2) <- gsub(",",";",levels(out3$errmess2))

out3a <- autocheck.consistency.nepva(nsm = 3, sim.n = 10000, full=FALSE)

write.csv(out3, file=paste0(path.out, "autocheck-consistency.csv"), quote=FALSE, row.names=FALSE)

## sum(out$check.noerrors1)

## ###############################################################################################

range(as.numeric(as.character(out3$tmed[out3$model.envstoch])) - as.numeric(as.character(out3$tmed[! out3$model.envstoch])), na.rm=TRUE) ## NOT ZERO
range(as.numeric(as.character(out3$tmed[out3$model.dd])) - as.numeric(as.character(out3$tmed[! out3$model.dd])), na.rm=TRUE)
range(as.numeric(as.character(out3$tmed[out3$demobase.splitpops])) - as.numeric(as.character(out3$tmed[! out3$demobase.splitpops])), na.rm=TRUE)
range(as.numeric(as.character(out3$tmed[out3$demobase.splitimmat])) - as.numeric(as.character(out3$tmed[! out3$demobase.splitimmat])), na.rm=TRUE)
range(as.numeric(as.character(out3$tmed[out3$impacts.splitpops])) - as.numeric(as.character(out3$tmed[! out3$impacts.splitpops])), na.rm=TRUE) ## NOT ZERO
range(as.numeric(as.character(out3$tmed[out3$impacts.splitimmat])) - as.numeric(as.character(out3$tmed[! out3$impacts.splitimmat])), na.rm=TRUE)
range(as.numeric(as.character(out3$tmed[out3$impacts.provideses])) - as.numeric(as.character(out3$tmed[! out3$impacts.provideses])), na.rm=TRUE) ## NOT ZERO

## ###############################################################################################


## ###############################################################################################
