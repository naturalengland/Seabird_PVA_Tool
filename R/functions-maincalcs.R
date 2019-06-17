## ###################################################################################################################
## Version 2.2 on 15 February 2019
## 1. Bundled into a new file
## 2. "sens.npv" changed to "sens.npvlocal" or "sens.npvglobal"
## 3. "impact.prod.mean" changed to "impacts.prod.mean" for consistency with other functions
## 4. "demobase.prod.mean" changed to "demobase.prod$mean"
##
## Version 3.1 on 7 March 2019: added "output.raw"
## Version 3.2 on 8 March 2019: added "noround"
## ###################################################################################################################

## ###################################################################################################################
## >> nepva.calcs.main

nepva.calcs.main <- function(inputs, demomods){
  
  ## nepva.errorcheck(inputs)
  
  inputs.adj <- nepva.preprocess(inputs = inputs, demomods = demomods)
  
  out <- nepva.sim(ddfn = inputs.adj$ddfn, fn.sim.con = inputs.adj$fn.sim.con,
                   fn.sim.unc = inputs.adj$fn.sim.unc,
                   model.demostoch = inputs.adj$model.demostoch, 
                   model.prodmax = inputs.adj$model.prodmax, 
                   afb = inputs.adj$afb, 
                   mbs = inputs.adj$mbs, 
                   npop = inputs.adj$npop, 
                   nscen = inputs.adj$nscen, 
                   sim.n = inputs.adj$sim.n, 
                   sim.seed = inputs.adj$sim.seed, 
                   impacts.matchscens = inputs.adj$impacts.matchscens, 
                   year.first = inputs.adj$year.first, 
                   inipop.years = inputs.adj$inipop.years, 
                   impacts.year.start = inputs.adj$impacts.year.start, 
                   impacts.year.end = inputs.adj$impacts.year.end,
                   inipop.counts = inputs.adj$inipop.counts, 
                   output.year.start = inputs.adj$output.year.start, 
                   output.year.end = inputs.adj$output.year.end,
                   demobase.ests = inputs.adj$demobase.ests, 
                   demobase.cormat = inputs.adj$demobase.cormat, 
                   demobase.bskippc = inputs.adj$demobase.bskippc, 
                   impacts.scennames = inputs.adj$impacts.scennames,
                   impacts.demochange.mean = inputs.adj$impacts.demochange.mean, 
                   impacts.demochange.se = inputs.adj$impacts.demochange.se,
                   impacts.abschange.mean = inputs.adj$impacts.abschange.mean, 
                   impacts.abschange.se = inputs.adj$impacts.abschange.se,
                   impacts.infillpops = inputs.adj$impacts.infillpops, 
                   impacts.infillages = inputs.adj$impacts.infillages,
                   output.agetype = inputs.adj$output.agetype, 
                   output.popsize.qe = inputs.adj$output.popsize.qe, 
                   output.popsize.target = inputs.adj$output.popsize.target,
                   silent = inputs.adj$silent, output.raw = inputs.adj$output.raw,
                   noround = inputs.adj$noround)
  
  out
}

## ###################################################################################################################
