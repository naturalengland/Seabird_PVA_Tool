## ##################################################################
## Version 4.2, 12 August 2019 - updated to include "nburn" arguments
## ##################################################################

rm(list=objects())

setwd("C://Users//adam//Work//Active//JNCC-PVA-testing//Version4.13//")

ff <- list.files(".", pattern="functions") ; for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

modeoptions <- read.csv("ModeOptions.csv") ## Added Version 2.1

## ##################################################

library(popbio)

## ##################################################################

exampleCS1a <- nepva.simplescenarios(
     model.envstoch = "betagamma",
     model.demostoch = TRUE,
     model.dd = "nodd",
     model.prodmax = TRUE,
     mbs = 2,
     afb = 2,
     npop = 2,
     nscen = 5,
     sim.n = 10,
     nburn = 11,
     sim.seed = NULL,
     demobase.specify.as.params = FALSE,
     demobase.splitpops = TRUE,
     demobase.splitimmat = FALSE,
     demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
     demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
     inipop.years = c(2017, 2017),
     inipop.vals = c(45504, 6031),
     inipop.inputformat = "breeding.pairs",
     impacts.relative = TRUE,
     impacts.splitpops = FALSE,
     impacts.splitimmat = FALSE,
     impacts.provideses = FALSE,
     impacts.year.start = 2020,
     impacts.year.end = 2045,
     impacts.matchscens = TRUE,
     impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
     impacts.prod.mean = c(0,0,0,0,0),
     impacts.prod.se = NULL,
     impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
     impacts.survadult.se = NULL,
     impacts.survimmat.mean = NULL,
     impacts.survimmat.se = NULL,
     output.agetype = "breeding.adults",
     output.year.end = 2045,
     output.year.start = 2017,
     output.popsize.target = 150,
     output.popsize.qe = 10,
     silent = FALSE, changetablenames = TRUE)

## ##################################################################

## ##################################################################
## Dimensions are: subpopulation, age, parameter (mean/SD)
## Ages here are: 0-1,...,1-afb
## ##################################################################

demobase.survimmat <- array(dim=c(2,4,2)) 

demobase.survimmat[1,1,] <- c(0.790, 0.077)
demobase.survimmat[2,1,] <- c(0.790, 0.077)
demobase.survimmat[1,2,] <- c(0.790, 0.077)
demobase.survimmat[2,2,] <- c(0.790, 0.077)

exampleCS1b <- nepva.simplescenarios(
  model.envstoch = "betagamma",
  model.demostoch = TRUE,
  model.dd = "nodd",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  sim.n = 10,
  nburn = 15,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = TRUE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
  demobase.survimmat = demobase.survimmat,
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = TRUE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, 
  changetablenames = TRUE)

## ##################################################################

exampleCS1c <- nepva.simplescenarios(
  model.envstoch = "betagamma",
  model.demostoch = TRUE,
  model.dd = "nodd",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  sim.n = 10,
  nburn = 7,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = FALSE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = FALSE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(50, 100, 200, 300, 500), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, 
  changetablenames = TRUE)

## ##################################################################

exampleCS1d <- nepva.simplescenarios(
  model.envstoch = "betagamma",
  model.demostoch = TRUE,
  model.dd = "dduloglin",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  sim.n = 10,
  nburn = 9,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = FALSE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035), DDeffect = c(-0.003, -0.003)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077), DDeffect = c(-0.002, -0.002)),
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = TRUE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, 
  changetablenames = TRUE)

## ##################################################################

exampleCS1e <- nepva.simplescenarios(
  model.envstoch = "betagamma",
  model.demostoch = TRUE,
  model.dd = "nodd",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  sim.n = 1,
  nburn = 0,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = FALSE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = TRUE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, changetablenames = TRUE)

## ##################################################################

exampleCS1f <- nepva.simplescenarios(
  model.envstoch = "deterministic",
  model.demostoch = FALSE,
  model.dd = "nodd",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  sim.n = 1,
  nburn = 25,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = FALSE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = TRUE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = NULL,
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, changetablenames = TRUE)

## ##################################################################
## Version g: added Version 3.1

demobase.survimmat <- array(dim=c(2,4,2)) 

demobase.survimmat[1,1,] <- c(0.790, 0.077)
demobase.survimmat[2,1,] <- c(0.790, 0.077)
demobase.survimmat[1,2,] <- c(0.790, 0.077)
demobase.survimmat[2,2,] <- c(0.790, 0.077)

exampleCS1g <- nepva.simplescenarios(
  model.envstoch = "betagamma",
  model.demostoch = TRUE,
  model.dd = "nodd",
  model.prodmax = TRUE,
  mbs = 2,
  afb = 2,
  npop = 2,
  nscen = 5,
  nburn = -3,
  sim.n = 10,
  sim.seed = NULL,
  demobase.specify.as.params = FALSE,
  demobase.splitpops = TRUE,
  demobase.splitimmat = TRUE,
  demobase.prod = data.frame(Mean = c(0.58, 0.39), SD = c(0.074, 0.035)),
  demobase.survadult = data.frame(Mean = c(0.854, 0.854), SD = c(0.077, 0.077)),
  demobase.survimmat = demobase.survimmat,
  inipop.years = c(2017, 2017),
  inipop.vals = c(45504, 6031),
  inipop.inputformat = "breeding.pairs",
  impacts.relative = TRUE,
  impacts.splitpops = FALSE,
  impacts.splitimmat = FALSE,
  impacts.provideses = FALSE,
  impacts.year.start = 2020,
  impacts.year.end = 2045,
  impacts.matchscens = TRUE,
  impacts.scennames = c("Impact 50",  "Impact 100", "Impact 200", "Impact > 300", "Impact 500"),
  impacts.prod.mean = c(0,0,0,0,0),
  impacts.prod.se = NULL,
  impacts.survadult.mean = c(0.0005, 0.00097, 0.0019, 0.0029, 0.0049), 
  impacts.survadult.se = NULL,
  impacts.survimmat.mean = c(0.0003, 0.00097, 0.0019, 0.0029, 0.0049),
  impacts.survimmat.se = NULL,
  output.agetype = "breeding.adults",
  output.year.end = 2045,
  output.year.start = 2017,
  output.popsize.target = 150,
  output.popsize.qe = 10,
  silent = FALSE, changetablenames = TRUE)

## ##################################################################
