## ###############################################################################################
## Functions to run the NE PVA R package in an automated way, via a form of "batch mode"
## ###############################################################################################

nepva.batchmode <- function(inputs, runtype){
  
  if(runtype == "simplescenarios"){
    
    out <- nepva.simplescenarios(model.envstoch = inputs$model.envstoch, 
                                 model.demostoch = inputs$model.demostoch, 
                                 model.dd = inputs$model.dd, model.prodmax = inputs$model.prodmax,
                                 mbs = inputs$mbs, afb = inputs$afb, npop = inputs$npop,
                                 nscen = inputs$nscen,
                                 nburn = inputs$nburn,
                                 sim.n = inputs$sim.n,
                                 sim.seed = inputs$sim.seed,
                                 demobase.specify.as.params = inputs$demobase.specify.as.params,
                                 demobase.splitpops = inputs$demobase.splitpops,
                                 demobase.splitimmat = inputs$demobase.splitimmat,
                                 demobase.prod = inputs$demobase.prod,
                                 demobase.survadult = inputs$demobase.survadult,
                                 demobase.survimmat = inputs$demobase.survimmat, 
                                 inipop.years = inputs$inipop.years,
                                 inipop.inputformat = inputs$inipop.inputformat, 
                                 inipop.vals= inputs$inipop.vals,
                                 impacts.relative = inputs$impacts.relative,
                                 impacts.splitpops= inputs$impacts.splitpops,
                                 impacts.splitimmat= inputs$impacts.splitimmat,
                                 impacts.provideses= inputs$impacts.provideses,
                                 impacts.year.start= inputs$impacts.year.start,
                                 impacts.year.end= inputs$impacts.year.end,
                                 impacts.scennames= inputs$impacts.scennames,
                                 impacts.prod.mean= inputs$impacts.prod.mean,
                                 impacts.prod.se= inputs$impacts.prod.se,
                                 impacts.survadult.mean= inputs$impacts.survadult.mean,
                                 impacts.survadult.se= inputs$impacts.survadult.se,
                                 impacts.survimmat.mean= inputs$impacts.survimmat.mean,
                                 impacts.survimmat.se= inputs$impacts.survimmat.se,
                                 output.agetype= inputs$output.agetype,
                                 output.year.end= inputs$output.year.end,
                                 output.year.start= inputs$output.year.start,
                                 output.popsize.target= inputs$output.popsize.target,
                                 output.popsize.qe= inputs$output.popsize.qe,
                                 changetablenames = inputs$changetablenames,
                                 silent = TRUE, output.raw = TRUE)
  }
  
  if(runtype == "validation"){
    
    out <- nepva.validation(model.envstoch = inputs$model.envstoch,
                            model.demostoch = inputs$model.demostoch,
                            model.dd = inputs$model.dd, model.prodmax = inputs$model.prodmax,
                            mbs = inputs$mbs, afb = inputs$afb, nburn = inputs$nburn,
                            sim.n = inputs$sim.n, sim.seed = inputs$sim.seed,
                            demobase.specify.as.params = inputs$demobase.specify.as.params,
                            demobase.splitimmat = inputs$demobase.splitimmat,
                            demobase.prod = inputs$demobase.prod,
                            demobase.survadult = inputs$demobase.survadult,
                            demobase.survimmat = inputs$demobase.survimmat,
                            inipop.years = inputs$inipop.years, 
                            inipop.vals = inputs$inipop.vals,
                            inipop.inputformat = inputs$inipop.inputformat,
                            output.agetype = inputs$output.agetype,
                            output.year.end = inputs$output.year.end, 
                            output.popsize.target = inputs$output.popsize.target, 
                            output.popsize.qe = inputs$output.popsize.qe,
                            output.validation.counts = inputs$output.validation.counts,
                            output.validation.years = inputs$output.validation.counts,
                            changetablenames = inputs$changetablenames,
                            silent = TRUE)
  }
  
  if(runtype == "sensitivity.local"){
    
    out <- nepva.sensitivity.local(model.envstoch = inputs$model.envstoch,
                                   model.demostoch = inputs$model.demostoch,
                                   model.prodmax = inputs$model.prodmax,
                                   mbs = inputs$mbs, afb = inputs$afb, nburn = inputs$nburn,
                                   sim.n = inputs$sim.n, sim.seed = inputs$sim.seed,
                                   demobase.prod = inputs$demobase.prod,
                                   demobase.survadult = inputs$demobase.survadult,
                                   inipop.years = inputs$inipop.years, inipop.vals = inputs$inipop.vals,
                                   impacts.year.start = inputs$impacts.year.start, 
                                   impacts.year.end = inputs$impacts.year.end,
                                   impacts.relative = inputs$impacts.relative,
                                   impacts.prod.mean = inputs$impacts.prod.mean, 
                                   impacts.survadult.mean = inputs$impacts.survadult.mean,
                                   output.year.end = inputs$output.year.end,
                                   output.popsize.target = inputs$output.popsize.target, 
                                   output.popsize.qe = inputs$output.popsize.qe,
                                   sens.npvlocal = inputs$sens.npvlocal, 
                                   changetablenames = inputs$changetablenames,
                                   sens.pcr = inputs$sens.pcr, silent = TRUE)
    
  }
  
  if(runtype == "sensitivity.global"){
    
    out <- nepva.sensitivity.global(model.envstoch = inputs$model.envstoch,
                                    model.demostoch = inputs$model.demostoch,
                                    model.prodmax = inputs$model.prodmax,
                                    mbs = inputs$mbs, afb = inputs$afb, nburn = inputs$nburn,
                                    sim.n = inputs$sim.n, sim.seed = inputs$sim.seed,
                                    demobase.prod = inputs$demobase.prod,
                                    demobase.survadult = inputs$demobase.survadult,
                                    inipop.years = inputs$inipop.years, inipop.vals = inputs$inipop.vals,
                                    impacts.year.start = inputs$impacts.year.start, 
                                    impacts.year.end = inputs$impacts.year.end,
                                    impacts.relative = inputs$impacts.relative,
                                    impacts.prod.mean = inputs$impacts.prod.mean, 
                                    impacts.survadult.mean = inputs$impacts.survadult.mean,
                                    output.year.end = inputs$output.year.end,
                                    output.popsize.target = inputs$output.popsize.target, 
                                    output.popsize.qe = inputs$output.popsize.qe,
                                    sens.npvglobal = inputs$sens.npvglobal, 
                                    changetablenames = inputs$changetablenames,
                                    sens.pcr = inputs$sens.pcr, silent = TRUE)
  }
  
  out
}

## ###############################################################################################
