## ##################################################################
## Created 16 Jan 2019, last modified 12 August 2019
## Version 1.9: various miscellaneous bug fixes, so code for all examples
##  now runs without error messages -- still needs checking for sanity
## Version 2.1 on 14 Feb 2019: various bugs fixed following feedback from Deena
## Version 2.2 on 15 Feb 2019: fixed inputs so there is compatability between
##   the structure of all high-level functions, so they all call the same 
##   internal function ("nepva.calcs"), and so all calculations are relegated
##   to that function (and child functions of it)
## ###########################################################
## Version 4.2 on 12 Aug 2019: added "nburn" to each example
## Version 4.11: added in Example 7, previously in a separate file
## ##################################################################

rm(list=objects())

setwd("C://Users//adam//Work//Active//JNCC-PVA-testing//Version4.15//")

ff <- list.files(".", pattern="functions") ; for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

modeoptions <- read.csv("ModeOptions.csv") ## Added Version 2.1

## ##################################################

library(popbio)

## ##################################################################
## Created Version 1.5 on 14 Jan 2018, last modified 16 Jan 2018
## Version 2.1: "demo.splitpops = TRUE" > 
##      "demobase.splitpops = TRUE" and "impacts.splitpops = FALSE"
## ##################################################################

example1 <- nepva.simplescenarios(model.envstoch = "betagamma",
                  model.demostoch = TRUE,
                  model.dd = "nodd", model.prodmax = TRUE,
                  mbs = 4, afb = 5, npop = 2, nscen = 3,
                  sim.n = 10, sim.seed = 43576, nburn = 3,
                  demobase.specify.as.params = FALSE,
                  demobase.splitpops = TRUE, demobase.splitimmat = FALSE, 
                  demobase.prod = data.frame(Mean = c(0.3, 0.4), SD = c(0.11, 0.13)),
                  demobase.survadult = data.frame(Mean = c(0.92, 0.88), SD = c(0.02, 0.03)),
                  inipop.years = c(2012, 2015), inipop.inputformat = "breeding.pairs", 
                  inipop.vals = c(791, 113),
                  impacts.splitimmat = FALSE,
                  impacts.provideses = FALSE,
                  impacts.year.start = 2035, impacts.year.end = 2055,
                  impacts.splitpops = FALSE,
                  impacts.scennames = c("ice", "fish", "bob"),
                  impacts.prod.mean = c(+0.03, 0, 0),
                  impacts.survadult.mean = c(+0.05, +0.12, +0.17),
                  output.agetype = "age.separated",
                  output.year.end = 2070, output.year.start = 2015,
                  output.popsize.target = 150, output.popsize.qe = 10, silent = FALSE, 
                  changetablenames = TRUE)

## ############################################

example2 <- nepva.simplescenarios(model.envstoch = "betagamma",
                   model.demostoch = TRUE,
                   model.dd = "dduloglin", model.prodmax = TRUE,
                   mbs = 4, afb = 5, npop = 2, nscen = 3,
                   sim.n = 10, sim.seed = 43576, nburn = 5,
                   demobase.specify.as.params = FALSE, 
                   demobase.splitpops = TRUE, demobase.splitimmat = FALSE,
                   demobase.prod = data.frame(Mean = c(0.4, 0.36), SD = c(0.11, 0.13), DD = c(-0.01, -0.02)),
                   demobase.survadult = data.frame(Mean = c(0.86, 0.89), SD = c(0.02, 0.03), DD = c(-0.03, -0.04)),
                   inipop.years = c(2012, 2015), inipop.vals = c(791, 113),
                   impacts.relative = TRUE, impacts.splitpops = FALSE,
                   impacts.splitimmat = FALSE,
                   impacts.provideses = TRUE,
                   impacts.year.start = 2020, impacts.year.end = 2070,
                   impacts.scennames = c("ice", "fish", "bob"),
                   impacts.prod.mean = c(-0.03, 0, 0),
                   impacts.prod.se = c(0.01, 0, 0),
                   impacts.survadult.mean = c(-0.05, -0.12, -0.17),
                   impacts.survadult.se = c(0.02, 0.02, 0.02),
                   output.agetype = "breeding.pairs",
                   output.year.end = 2070, output.year.start = 2019,
                   output.popsize.target = 150, output.popsize.qe = 10, silent = FALSE,
                   changetablenames = TRUE)

## ############################################

example3 <- nepva.validation(model.envstoch = "betagamma",
                  model.demostoch = TRUE,
                  model.dd = "nodd", model.prodmax = TRUE,
                  mbs = 4, afb = 5, nburn = 0,
                  sim.n = 10, sim.seed = 43576,
                  demobase.specify.as.params = FALSE,
                  demobase.prod = data.frame(Mean = 0.5, SD = 0.11),
                  demobase.survadult = data.frame(Mean = 0.82, SD = 0.02),
                  inipop.years = 2001, inipop.vals = 791,
                  output.agetype = "breeding.pairs",
                  output.year.end = 2018, 
                  output.popsize.target = 150, output.popsize.qe = 10,
                  output.validation.counts = c(501, 589, 612, 698),
                  output.validation.years = c(2004, 2008, 2009, 2016),
                  silent = FALSE, changetablenames = TRUE)
          
## ############################################

example4 <- nepva.sensitivity.local(model.envstoch = "betagamma",
                                 model.demostoch = TRUE,
                                 model.prodmax = TRUE,
                                 mbs = 4, afb = 5, nburn = 11,
                                 sim.n = 10, sim.seed = 43576,
                                 demobase.prod = data.frame(mean = 0.7, sd = 0.11),
                                 demobase.survadult = data.frame(mean = 0.92, sd = 0.02),
                                 inipop.years = 2012, inipop.vals = 791,
                                 impacts.year.start = 2030, impacts.year.end = 2050,
                                 impacts.relative = TRUE,
                                 impacts.prod.mean = -0.02, impacts.survadult.mean = -0.03,
                                 output.year.end = 2070,
                                 output.popsize.target = 250, output.popsize.qe = 10,
                                 sens.npvlocal = 3, sens.pcr = c(10, 5, 5, 50, 50), silent = FALSE,
                                 changetablenames = TRUE)

## ############################################

example5 <- nepva.sensitivity.global(model.envstoch = "betagamma",
                                    model.demostoch = TRUE,
                                    model.prodmax = TRUE,
                                    mbs = 4, afb = 5, nburn = 20,
                                    sim.n = 2, sim.seed = 43576,
                                    demobase.prod = data.frame(mean = 0.7, sd = 0.11),
                                    demobase.survadult = data.frame(mean = 0.92, sd = 0.02),
                                    inipop.years = 2012, inipop.vals = 791,
                                    impacts.year.start = 2070, impacts.year.end = 2070,
                                    impacts.relative = TRUE,
                                    impacts.prod.mean = -0.02, impacts.survadult.mean = -0.03,
                                    output.year.end = 2070,
                                    output.popsize.target = 250, output.popsize.qe = 10,
                                    sens.npvglobal = 2, sens.pcr = c(10, 5, 5, 50, 50), silent = TRUE,
                                    changetablenames = TRUE)

## ############################################

example6 <- nepva.simplescenarios(model.envstoch = "deterministic",
                                  model.demostoch = FALSE,
                                  model.dd = "nodd", model.prodmax = FALSE,
                                  mbs = NULL, afb = 5, npop = 1, nscen = 1,
                                  sim.n = 100, sim.seed = 43576, nburn = 6,
                                  demobase.specify.as.params = FALSE,
                                  demobase.splitpops = FALSE, demobase.splitimmat =
                                    FALSE,
                                  demobase.prod = data.frame(Mean = c(0.9), SD = c(0)),
                                  demobase.survadult = data.frame(Mean = c(0.82), SD = c(0)),
                                  inipop.years = c(2010), inipop.inputformat =
                                    "breeding.pairs",
                                  inipop.vals = c(10),
                                  impacts.splitimmat = FALSE,
                                  impacts.provideses = FALSE,
                                  impacts.year.start = 2035, impacts.year.end = 2055,
                                  impacts.splitpops = FALSE,
                                  impacts.scennames = c("bob"),
                                  impacts.prod.mean = c(0),
                                  impacts.survadult.mean = c(0),
                                  output.agetype = "breeding.adults",
                                  output.year.end = 2070, output.year.start = 2012,
                                  output.popsize.target = 150, output.popsize.qe = 10, silent = TRUE,
                                  changetablenames = TRUE)

## ############################################
## "Extra" example, previously in separate file
## "run-extra-example-for-defaults.R"

lookup.dir <- "C://Users//adam//Work//AlreadyBackedUp//CEH-NE-PVA-Tool//Current//Data//Lookup//Version3.4//Tables//"
## lookup.dir <- "C://Users//adam//Work//Active//NE-PVA//Data//Lookup//Version3.4//Tables//"

example7 <- nepva.calcdefaults(Species = "Herring Gull", 
                          poolregtype.BS = "Global", poolregion.BS = "Global", 
                          sourcepop.surv = "Skomer (1978-2010)", lookup.dir = lookup.dir)

## ############################################
## "Extra" examples, added Version 4.12

ina <- list(model.envstoch = "deterministic", model.demostoch = TRUE,
            model.dd = "nodd", model.prodmax = TRUE, mbs = 1, afb = 2,
            npop = 2, nscen = 1, sim.n = 2, sim.seed = 1, nburn = 0,
            demobase.specify.as.params = FALSE, demobase.splitpops = TRUE,
            demobase.splitimmat = TRUE, demobase.prod = array(dim=c(2,2), data = c(0.5,0.5,0,0)),
            demobase.survadult = array(dim=c(2,2), data = c(0.9,0.9,0,0)),
            demobase.survimmat = array(dim=c(2,2,2), data = c(rep(0.9,4),rep(0,4))),
            inipop.years = c(2010, 2010), inipop.inputformat = "breeding.adults",
            inipop.vals = c(50, 50), impacts.relative = TRUE, impacts.splitimmat = FALSE,
            impacts.provideses = FALSE, impacts.year.start = 2020, impacts.year.end = 2030,
            impacts.splitpops = FALSE, impacts.scennnames = "bob", impacts.prod.mean = 0.1,
            impacts.survadult.mean = 0.1, output.agetype = "breeding.pairs",
            output.year.start = 2010, output.year.end = 2040, output.popsize.target = NULL,
            output.popsize.qe = NULL, silent = TRUE, changetablenames = TRUE)

ota <- nepva.batchmode(ina, runtype = "simplescenarios") ; ota$tab[1:6,1:10]

inb <- ina ; inb$model.demostoch <- FALSE

otb <- nepva.batchmode(inb, runtype = "simplescenarios") ; otb$tab[1:6,1:10]

inc <- ina ; inc$impacts.splitimmat <- TRUE ; inc$impacts.survimmat.mean <- c(0.1, 0.1)

otc <- nepva.batchmode(inc, runtype = "simplescenarios") ; otc$tab[1:6,1:10]

## ###############################################################################################
