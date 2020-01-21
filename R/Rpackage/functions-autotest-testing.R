## ###############################################################################################
## Created 12 September 2019, last modified 29 September 2019
##
## 21 November 2019: a) including "CompareStructure", from stackoverflow.com/questions/32399843
##                   b) fixed "autocheck.consistency.nepva" to only run in situations where first set of inputs are valid
##
## [1] autocheck.validity.nepva 
##         [2] nepva.siminputs.valid
##         [2] testrun.nepva
##                  [3] runcheck.nepva
##                           [4] check.outputs 
##                                     [5] check.simplescenarios
##                                     [5] check.validation
##                                     [5] check.sensitivity.local
##                                     [5] check.sensitivity.global
## ###############################################################################################

## ##################################################################################################
## BLOCK 1. Functions to autocheck the validity or otherwise of a large batch of inputs
## Created 29 September 2019, last modified 29 September 2019
## ###############################################################################################

autocheck.validity.nepva <- function(nsms, test.invalid = FALSE){
  
  out <- NULL
  
  runtypes <- c("simplescenarios", "validation", "sensitivity.local", "sensitivity.global")
  
  nrt <- length(runtypes)
  
  for(i in 1:nrt){
    
    for(j in 1:(nsms[i])){
      
      print.noquote(paste(i,"  ",j,"   ",date()))

      ## ######################################      
      ## Generate a random seed
      seed.meta <- round(runif(1,0.5,100000+0.5))
      
      ## ######################################      
      ## Generate a set of valid inputs
      validinputs <- nepva.siminputs.valid(runtype = runtypes[i], seed.meta = seed.meta)
      
      ## ######################################      
      ## Check whether the tool runs successfully for these valid inputs
      tmp <- testrun.nepva(inputs = validinputs, runtype = runtypes[i])
      
      tmp$runtype <- runtypes[i]
      
      tmp$seed.meta <- seed.meta
      
      tmp$ought.valid <- TRUE
      
      tmp$ought.errtype <- 0
      
      if(test.invalid){
        
        ## ######################################      
        ## Perturb valid inputs in order to generate multiple set of invalid inputs
        
        invalidinputs <- nepva.siminputs.invalid(validinputs)
        
        ninv <- length(invalidinputs)
        
        ## ######################################      
        ## Check whether tool crashes, and produces appropriate error messages, for each set of 
        ##  invalid inputs
        
        for(k in 1:ninv){
          
          new <- testrun.nepva(inputs = invalidinputs[[k]], runtype = runtypes[i])
          
          new$runtype <- runtypes[i]
          
          new$seed.meta <- seed.meta
          
          new$ought.valid <- FALSE
          
          new$ought.errtype <- k
          
          tmp <- rbind(tmp, new)
        }
        
        ## ######################################      
      }
      
      tmp <- tmp[,c(4:7,1:3)]
      
      out <- rbind(out, tmp)
    }
  } 
  
  out$errmess <- factor(out$errmess) ## moved Version 4.8
  
  levels(out$errmess) <- gsub(",",";",levels(out$errmess)) ## moved Version 4.8
  
  out
}

## ###############################################################################################
## Functions to check validity of outputs from a single run of the NE PVA tool:

testrun.nepva <- function(inputs, runtype){
  
  obj <- ftry(fn = nepva.batchmode, inputs = inputs, runtype = runtype)
  
  runcheck.nepva(obj, inputs = inputs, runtype = runtype)
}

runcheck.nepva <- function(obj, inputs, runtype){
  
  tmp <- get.errmess(obj)
  
  if(is.na(tmp)){
    
    check <- check.outputs(obj, inputs = inputs, runtype = runtype)
  }
  else{
    
    check <- FALSE
  }
  
  data.frame(check.noerrors = is.na(tmp), check.validoutput = check, errmess = tmp)
}

## ###############################################################################################

check.outputs <- function(obj, inputs, runtype){
  
  if(runtype == "simplescenarios"){
    
    check <- check.simplescenarios(obj, inputs = inputs)
  }
  
  if(runtype == "validation"){
    
    check <- check.validation(obj, inputs = inputs)
  }
  
  if(runtype == "sensitivity.local"){
    
    check <- check.sensitivity.local(obj, inputs = inputs)
  }
  
  if(runtype == "sensitivity.global"){
    
    check <- check.sensitivity.global(obj, inputs = inputs)
  }
  
  check
}

## ###############################################################################################

check.simplescenarios <- function(out, inputs){
  
  ## #########################
  
  lims.popsize <- c(0, 1e+20)
  
  ## #########################
  
  if(inputs$output.raw){
    
    ys <- min(inputs$inipop.years):inputs$output.year.end
    
    check1 <- all(out$raw$years == (ys))
    
    check2 <- all(dim(out$raw$nbyage) == c(inputs$nscen + 1, inputs$npop, length(ys), inputs$sim.n, inputs$afb + 1))
    
    check3 <- (all(! is.na(out$raw$nbyage))) & (min(out$raw$nbyage, na.rm=TRUE) >= lims.popsize[1]) & (max(out$raw$nbyage, na.rm=TRUE) <= lims.popsize[2])
    
    check4 <- check.metricstab(tab = out$tab, inputs = inputs)
    
    check <- check1 & check2 & check3 & check4
  }
  else{
    
    check <- check.metricstab(tab = out, inputs = inputs)
  }
  
  ## #########################
  
  check
}

## ###############################################################################################

check.validation <- function(out, inputs){ 
  
  ny <- inputs$output.year.end - min(inputs$inipop.years) + 1
  
  check <- (ny == nrow(out)) & check.metricsvals(metricstab = out, globalsens = FALSE)
  
  check
}

## ###############################################################################################

check.sensitivity.local <- function(out, inputs){
  
  ## #########################
  
  lims.ppcc <- c(-100, 1000)
  
  ## #########################
  
  pnames <- c("demobase.prod.mean", "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean", "inipop.vals")
  
  mma <- which(colnames(out) == "parname")
  
  mmb <- match(paste("pcchange", pnames, sep="."), colnames(out))
  
  mmc <- match(pnames, colnames(out))
  
  check1 <- (nrow(out) == 1 + inputs$sens.npvlocal*10)
  
  check2 <- all(! is.na(match(pnames, levels(out$parname)[levels(out$parname) != "standard"])))
  
  check3 <- (min(out[,mmb]) >= lims.ppcc[1]) & (max(out[,mmb] <= lims.ppcc[2]))
  
  check4 <- check.sensinputs(out, mbs = inputs$mbs)
  
  check5 <- check.metricsvals(out[,-c(mma, mmb, mmc)], globalsens = FALSE)
  
  check <- check1 & check2 & check3 & check4 & check5
  
  check
}

## ###############################################################################################

check.sensitivity.global <- function(out, inputs){
  
  pnames <- c("demobase.prod.mean", "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean", "inipop.vals")
  
  check1 <- (nrow(out$tab) == inputs$sens.npvglobal)
  
  check2 <- nrow(out$decomposition) == length(inputs$sens.pcr)
  
  check3 <- check.sensinputs(out$tab, mbs = inputs$mbs)
  
  tabmet <- out$tab[,is.na(match(colnames(out$tab), pnames))]
  
  check4 <- check.metricsvals(tabmet, globalsens = TRUE)
  
  check5 <- check.globaldecomp(out$decomposition)
  
  check <- check1 & check2 & check3 & check4 & check5
  
  check
}

## ###############################################################################################

check.metricstab <- function(tab, inputs){
  
  ## ############################################
  
  ns <- (inputs$nscen + 1)
  
  na <- (inputs$afb + 1)^(inputs$output.agetype == "age.separated")
  
  ny <- (inputs$output.year.end - inputs$output.year.start + 1)
  
  ## print(colnames(tab))
  
  check <- ((ns * na * ny) == nrow(tab)) & check.metricsvals(metricstab = tab, globalsens = FALSE)
  
  check
}

## ###############################################################################################

check.metricsvals <- function(metricstab, globalsens = FALSE){ 
  
  ## ############################################
  
  lims.b <- c(0, 1e+20)
  
  lims.c <- c(-100, 1e+06)
  
  lims.e <- c(0, 100)
  
  ## ############################################
  
  qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
  
  cna <- c("Year", "Age", "Scenario", "Baseyear", "Currently.Impacted", "Impact.year") 
  
  cnb <- paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep=".")
  
  cnc <- c(paste(rep(c("pgr", "agr"), 1, each = 5),
                 rep(c("median", "mean", "sd", "cilo", "cihi"), 2), sep="."))
  
  cnd <- c(paste(rep(c("ppc", "m1", "m2"), 1, each = 5),
                 rep(c("median", "mean", "sd", "cilo", "cihi"), 3), sep="."))
  
  cne <- paste0("m", 3:6)
  
  if(globalsens){
    
    check <- length(colnames(metricstab)) == length(c(cnb,cnc))
    
    if(check){
      
      check <- all(colnames(metricstab) == c(cnb, cnc))
      
      check <- check & all(metricstab[,cnb] >= lims.b[1]) & all(metricstab[,cnb] <= lims.b[2]) 
      
      check <- check & all(min(metricstab[,cnc], na.rm=TRUE) >= lims.c[1]) & all(max(metricstab[,cnc], na.rm=TRUE) <= lims.c[2])
    }
  }
  else{
    
    active <- (! is.na(metricstab$Impact.year))
    
    cn <- c(cna, cnb, cnc, cnd, cne)
    
    vb <- metricstab[,cnb]
    
    vc1 <- metricstab[! active, c(cnc, cnd, cne)]
    
    vc2 <- metricstab[active, c(cnc, cnd, cne)]
    
    check1 <- all(colnames(metricstab) == cn)
    
    check2 <- all(is.na(vc1)) & all(! is.na(vc2)) & all(! is.na(metricstab[,cnb]))
    
    check3 <- all(metricstab[,cnb] >= 0) & all(metricstab[,cnb] <= 1e+20) 
    
    if(all(is.na(metricstab[,c(cnc, cnd)]))){
      
      check4 <- TRUE
    }
    else{
      
      check4 <- all(min(metricstab[,c(cnc, cnd)], na.rm=TRUE) >= -100) & all(max(metricstab[,c(cnc, cnd)], na.rm=TRUE) <= 1e+06)
    }
    
    if(all(is.na(metricstab[,cne]))){
      
      check5 <- TRUE
    }
    else{
      
      check5 <- all(min(metricstab[,cne] >= lims.e[1], na.rm=TRUE) & max(metricstab[,cne] <= lims.e[2], na.rm=TRUE))
    }
    
    check <- check1 & check2 & check3 & check4 & check5 
  }
  
  ## ############################################
  
  check
}

## ###############################################################################################

check.sensinputs <- function(out, mbs){
  
  pnames <- c("demobase.prod.mean", "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean", "inipop.vals")
  
  ## ############################################
  
  pvmin <- c(0, 0, -0.5, -0.5, 1)
  pvmax <- c(mbs, 1, 0.5, 0.5, 1e+06)
  
  ## ############################################
  
  tmp <- out[,pnames]
  
  check <- TRUE
  
  for(k in 1:length(pnames)){
    
    check <- check & (min(tmp[,k]) > pvmin[k]) & (max(tmp[,k]) < pvmax[k])
  }
  
  check
}

## ###############################################################################################

check.globaldecomp <- function(tmp){
  
  ## ############################################
  
  lims.gd <- c(-1e+20, 1e+20)
  
  ## ############################################
  
  pnames <- c("demobase.prod.mean", "demobase.survadult.mean", "impact.prod.mean", "impact.survadult.mean", "inipop.vals")
  
  qs <- c(1, 2.5, 5, 10, 20, 25, 33, 66, 75, 80, 90, 95, 97.5, 99)
  
  cnb <- paste("popsize", c("mean", "sd", "median", paste0("q", qs, "%")), sep=".")
  
  cnc <- c(paste(rep(c("pgr", "agr"), 1, each = 5),
                 rep(c("median", "mean", "sd", "cilo", "cihi"), 2), sep="."))
  
  cnc <- cnc[1:8] ## !!!!! FUDGE !!!!!!!!
  
  cn <- c(cnb, cnc)
  
  cn <- paste(rep(c("FOI", "TEI"), each = length(cn)), rep(cn, 2), sep=".")
  
  check <- all(dim(tmp) == c(length(pnames), length(cn)))
  
  check <- check & all((tmp >= lims.gd[1]) & (tmp <= lims.gd[2]))
  
  check
}

## ###############################################################################################
## BLOCK 2. Functions to compare outputs from multiple runs of the NE PVA tool, to
##   assess internal consistency
##
## Version 4.8: added "full" argument
## ###############################################################################################

autocheck.consistency.nepva <- function(nsm, sim.n, full=TRUE){
  
  ## NOTE: this part is only for "simplescenarios"
  ## Created 29 September 2019, last modified 29 September 2019
  
  ## 17 November 2019: added "sim.n" argument
  
  runtype <- "simplescenario"
  
  out <- NULL
  
  j <- 1
  
  while(j <= nsm){ ## Version 4.7: changed from "for" to "while"
    
    seed.meta <- round(runif(1,0.5,100000+0.5))
    
    allinputs <- nepva.siminputs.consistent(seed.meta = seed.meta, sim.n = sim.n, full=full) ## 17 November 2019: added "sim.n" argument
    ## Version 4.8 - added "full" argument
    
    inputs <- allinputs$inputlist
    
    zout <- testrun.nepva(inputs = inputs[[1]], runtype = "simplescenarios")
 
    if(zout$check.validoutput){ ## v4.7: now only run consistency check if the first (full) inputs were valid
    
      ## for(k in 1:128){ print.noquote(paste(k, testrun.nepva(inputs = inputs[[k]], runtype = "simplescenarios"),collapse="-")) }
      ## browser()
      
      ncombi <- nrow(allinputs$mstruc)
    
      new <- NULL
     
      for(k in 1:ncombi){
      
         print.noquote(paste(j,"  ",k,"  ",date()))
      
         ## Note: this is not the most efficient way to do this, 
         ##   as running of "inputs.1" is repeated multiple times...
      
         tmp <- testcomp.nepva(inputs.1 = inputs[[1]], inputs.2 = inputs[[k]], runtype = "simplescenarios")
      
         tmp$combi <- k
      
         new <- rbind(new, tmp)  
      }
    
      outij <- data.frame(runtype = rep(runtype, ncombi),
                        seed.meta = rep(seed.meta, ncombi),
                        combi = 1:ncombi)
    
      outij <- cbind(outij, allinputs$mstruc)
    
      outij <- cbind(outij, new)
    
      out <- rbind(out, outij)
      
      j <- j + 1
    }
    else{ ## v4.7: if invalid inputs, print error message
      
      print.noquote(as.character(zout$errmess))
    }
  }
  
  out
}

## ###############################################################################################
## Compare the results obtained by running the NE PVA tool twice
## Rewritten 29 September 2019 to simplify functionality
##
## Note: only designed to work with "runtype = 'simplescenarios'"
## ###############################################################################################

testcomp.nepva <- function(inputs.1, inputs.2, runtype){

  if(runtype == "simplescenarios"){
    
    inputs.1$output.raw <- TRUE
    inputs.2$output.raw <- TRUE
  }
  
  obj1 <- ftry(fn = nepva.batchmode, inputs = inputs.1, runtype = runtype)
  obj2 <- ftry(fn = nepva.batchmode, inputs = inputs.2, runtype = runtype)

  chk1 <- runcheck.nepva(obj1, inputs = inputs.1, runtype = runtype)
  chk2 <- runcheck.nepva(obj2, inputs = inputs.2, runtype = runtype)

  colnames(chk1) <- paste0(colnames(chk1), 1)
  colnames(chk2) <- paste0(colnames(chk2), 2)

  chk <- cbind(chk1, chk2)
  
  if(chk$check.validoutput1 & chk$check.validoutput2){
  
    ## Note: "CompareStructure" checks whether two objects have identical dimensions & 
    ##   structure, but does **not** check names (which we would ideally also do...)
    
    chk$samestruc <- CompareStructure(obj1, obj2)
  
    if(chk$samestruc){
    
      if(runtype == "simplescenarios"){
        
        adiff <- obj2$raw$nbyage - obj1$raw$nbyage
      
        denom <- obj1$raw$nbyage
      
        rdiff <- adiff / denom
        
        chk$tmed = median(obj2$raw$nbyage) ## added Version 4.8
        chk$tsdd = sd(obj2$raw$nybage)
        
        chk$amax = max(abs(adiff))
        chk$amed = median(abs(adiff))
        chk$rmax = max(abs(rdiff[denom > 0]))
        chk$rmed = median(abs(rdiff[denom > 0]))
      }
  
      ## chk <- cbind(chk, new)    
    }
  }
  else{
  
    chk$samestruc <- FALSE
    
    chk$tmed = NA
    chk$tsdd = NA
    
    chk$amax <- NA
    chk$amed <- NA
    chk$rmax <- NA
    chk$rmed <- NA
  }

  chk
}

## ###############################################################################################
## BLOCK 3. Utility functions
## ###############################################################################################

ftry <- function(fn,...){ try(fn(...), silent = TRUE) }

## ###############################################################################################
## A function to extract the error message that has been created by running a function using "try"
##   -- output will be missing (NA) if the function ran successfully, without producing an error message,
##   and will otherwise be a character string containing the error message

get.errmess <- function(z){
  
  if(inherits(z, "try-error")){ 
    
    out <- as.character(attr(z, "condition"))
    
    out <- gsub("\\n", "", gsub("<simple", "", out))
  }
  else{
    
    out <- NA
  }
  
  out
}

## ###############################################################################################

CompareStructure <-
  function(x, y) {
    # function to recursively compare a nested list of structure annotations
    # using pairwise comparisons
    TypeCompare <-
      function(xSTR, ySTR) {
        if (length(xSTR) == length(ySTR)) {
          all(mapply(
            xSTR,
            ySTR,
            FUN = function(xValue, yValue) {
              if (is.list(xValue) && is.list(yValue)) {
                all(TypeCompare(xValue, yValue))
              } else if (is.list(xValue) == is.list(yValue)) {
                identical(xValue, yValue)
              } else {
                FALSE
              }
            }
          ))
        } else {
          FALSE
        }
      }
    
    # if both inputs are lists
    if (is.list(x) && is.list(y)) {
      # use Rapply to recursively apply function down list
      xSTR <-
        rapply(
          x,
          f = function(values) {
            c(mode(values), length(values))
          },
          how = "list"
        )
      
      # use Rapply to recursively apply function down list
      ySTR <-
        rapply(
          y,
          f = function(values) {
            c(mode(values), length(values))
          },
          how = "list"
        )
      
      # call the compare function on both structure annotations
      return(TypeCompare(xSTR, ySTR))
      
    } else {
      # if inputs are not same class == automatic not same structure
      if (class(x) != class(y)) {
        FALSE
      } else {
        # get dimensions of the x input, if null get length
        xSTR <-
          if (is.null((dimsX <- dim(x)))) {
            length(x)
          } else {
            dimsX
          }
        
        # get dimensions of the y input, if null get length
        ySTR <-
          if (is.null((dimsY <- dim(y)))) {
            length(y)
          } else {
            dimsY
          }
        
        # call the compare function on both structure annotations
        return(TypeCompare(xSTR, ySTR))
      }
    }
  }

## ###############################################################################################
## Added 13 January 2020 - utility functions need for Kate to run manual testing of outputs against Shiny

## Generate input lists, and where possible output CSV files, associated with a set of input specification:

nepva.save.and.run.valid <- function(inputspecs, outpath){
  
  write.csv(inputspecs, file = paste0(outpath, "inputspecs.csv"), quote=FALSE, row.names=FALSE)
  
  inputslist <- as.list(NULL)
  
  for(k in 1:nrow(inputspecs)){
    
    inputs <- nepva.siminputs.valid(runtype = inputspecs$runtype[k], seed.meta = inputspecs$seed.meta[k])
    
    inputslist[[k]] <- inputs
    
    out <- ftry(fn = nepva.batchmode, inputs = inputs, runtype = inputspecs$runtype[k])
    
    if(! inherits(out, "try-error")){ 
      
      if(is.null(out$tab)){ ## Clause added Version 4.12
        
        tab <- out
      }
      else{
        
        tab <- out$tab
      }
    
      write.csv(tab, file = paste0(outpath, "outputs", k, ".csv"), quote=FALSE, row.names=FALSE)
    }
  }
  
  save(inputslist, file = paste(outpath, "inputslist.RData"))
  
  NULL
}

## Simplify error statuses, where evaluating performance:

fixstatus.pva <- function(out){
  
  e1 <- "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Population size explosion - will lead to numerical overflow"
  e2 <- "Error in inits.burned(nbyage.burned = nbyage.burned; inipop.totals = inipop.totals): Error! Zero values during burn-in..."
  e3a <- "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Invalid productivity rates simulated!"
  e3b <- "Error in leslie.update(demobase.ests = demobase.ests[j; ; ]; nbyage.prev = nbyage.prev; : Invalid survival probabilities simulated!"
  
  out$status <- factor("error.other", levels = c("run.full", "run.partial", "error.e1", "error.e2", "error.e3", "error.other"))
  
  out$status[out$check.validoutput] <- "run.full"
  out$status[out$check.noerrors & (! out$check.validoutput)] <- "run.partial"
  out$status[(! out$check.noerrors) & out$errmess == e1] <- "error.e1"
  out$status[(! out$check.noerrors) & out$errmess == e2] <- "error.e2"
  out$status[(! out$check.noerrors) & out$errmess == e3a] <- "error.e3"
  out$status[(! out$check.noerrors) & out$errmess == e3b] <- "error.e3"
  
  out
}

## Select a random subset of valid inputs (of each type), to use for manual checking:

goodsubset.inputs.valid <- function(inputspecs, nman, seed.subset){  
  
  set.seed(seed.subset)
  
  man.runtypes <- c("simplescenarios", "validation", "sensitivity.local")
  
  man.errtypes <- c("run.full", "run.partial", "error.e1", "error.e2", "error.e3")
  
  mm <- NULL
  
  for(i in 1:length(man.runtypes)){
    
    for(j in 1:length(man.errtypes)){
      
      ox <- (inputspecs$runtype == man.runtypes[i] & inputspecs$status == man.errtypes[j])
      
      if(any(ox)){ mm <- c(mm, sample(which(ox), size = nman[j,i])) }
    }
  }
  
  new <- inputspecs[mm,]
  
  row.names(new) <- 1:nrow(new)
  
  new
}

## ###############################################################################################
