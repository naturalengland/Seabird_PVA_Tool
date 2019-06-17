## ########################################################################################
## Project: NEC06986
## Script purpose: NE PVA Tool interface help text
## Author: DCM, Centre for Ecology & Hydrology
## Date v0.9: 23rd February 2019
## Date v1.1:  6th March 2019
## ########################################################################################

helptext <- list()

# Species selection
helptext$species <- p("Optional: Select a species to pre-populate selected parameters for a PVA using previously
reported national or colony specific demographic rates. These values are for guidance only and may subsequently
be edited.")

# Case study selection
helptext$case_studies <- p("(help text to be added here...)")

# Summary of parameters and RUN box
helptext$summary <- p("The progress bar indicates how many mandatory numeric parameters have been completed (i.e. are not blank) for this run
based on the run type, number of scenarios and number of subpopulations specified. Optional parameters are not included in the total.
   Yes/No parameters and sliders are not included as they always have a value. A document listing all parameters is generated
         with each run.")

# Basic information about the form of PVA
helptext$basic <- p("Select options to determine if the PVA model is to include environmental stochasticity
   (and the specified form), demographic stochasticity, and density dependence in demographic rates.")

# Simulation
helptext$sim <- p("Select the number of matched paired simulations to run, and set a random seed.
   By using the same random seed and identical model specification, simulations may be repeated exactly.")

# Age at first breeding
helptext$afb <- p("Set age at first breeding to determine the structure of the Leslie Matrix underpinning
   the model.")

# Productivity
helptext$productivity <- p("If required, set an upper constraint on the productivity rate in the model. Productivity will be
   constrained to be no greater than this value.")

# Output: years
helptext$outputyears <- p("Specify the first and final years to be included in model outputs.")

# Output: target population size
helptext$targetpop <- p("Target population size to use in calculating impact metrics: If required, set a target
population size for assessing effects of impacts. The tool will output the percentage of simulations in which
the impacted final population size exceeds the user-specified target population size. Note that this applies
to the summed population size over all subpopulations. Quasi-extinction threshold to use in calculating impact
metrics: If required, set the quasi-extinction risk threshold for assessing effects of impacts. The tool will
output the percentage of simulations in which the impacted final population size is lower than the threshold for
quasi-extinction.")

# Output: ages
helptext$outputages <- p("Select the unit to be used for the outputs")

# Sensitivity
helptext$sensitivity <- p("This function can only be run for a single population, a single impact scenario,
   and with initial population size specified as number of breeding adults. This function runs the PVA
   for multiple combinations of five key input parameters - initial population size, baseline productivity,
   baseline adult survival, impact of scenario upon productivity rate, and impact of scenario upon adult
   survival rate. The tool will output how changes in the values of each of these input parameters affects
   the value of the PVA outputs (impact metrics).")

# Subpopulation options
helptext$subpop <- p("A subpopulation is a breeding colony; this enables users to generate a PVA for an SPA or region
that consists of multiple breeding colonies. Set the number of subpopulations to be included in the PVA (note subpopulations
are modelled separately but outputs are summed over all subpopulations). If demographic rates are to supplied separately
(i.e., differ) for each subpopulation, set the switch to 'Yes'")

# Format for initial population size:
helptext$inputformat <- p("Set the units for the initial population size, to be entered below")

# Immatures options
helptext$immat <- p("Specify if survival rates are to be entered separately for immatures")

# Impacts: Options 1
helptext$imp1 <- p("Specify if impacts are applied separately to each subpopulation (e.g., impacts are different
   and known for each individual subpopulation). Specify if impacts are to be specified separately
   for immatures (e.g., immatures are known to have a different level of impact than adults)
   Select if standard errors are available for impact rates, and enter below.",br(),"Select if random seeds should be
   matched for different impact scenarios. This question is only relevant if standard errors are available for
   the impacts. If standard errors are available for impacts, specify here if random seeds should be matched
   across the alternative impact scenarios. Selecting yes means you are making the assumption that the uncertainties
   for each impact scenario are in some way related to one another.")

# Impacts: Options 2
helptext$imp2 <- p("Select if impacts are to be entered in the form of a relative harvest (a percentage point change
in demographic rates for survival and/or productivity), or as an absolute harvest (the number of
mortalities arising from the impact)")

# Years in which impacts are assumed to begin and end.
helptext$impactyears <- p("Use the slider to set the start and end year of the period of impact. The first valid year for impacts
   depends on the years given for the initial population size (given on the baseline tab); it must be after the latest population
   initial year. A final year to include in the output is set on the 'run' tab; please check that the impact years and output
   year are set appropriately.")


