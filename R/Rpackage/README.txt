========================================================
NE PVA tool, R package -- Version 4.15, 19 February 2020

- Changes to "functions-runpva.R" only; these are:

    a) Bug fix resulting from error on line 360: "max(inipop.relyears)" changed to "max(inipop.years)",
         which meant "Base_Year" was previously set incorrectly in baseline-only runs
        
    b) Changes within "make.impactmetrics.table" to make sure metrics and "YRS_From_First_Imp" 
         are reported only after the start of impacts (i.e. not in baseline-only runs)

- Also moved all testing files into a "Testing" directory, which replaces "Outputs"

========================================================
NE PVA tool, R package -- Version 4.14, 5 February 2020

- Major bug fix, resulting from a typo in "create.impactmat" within "functions-preprocessing.R":
   on line 458 "(! impacts.relative)" changed to "(impacts.relative)"
 
========================================================
NE PVA tool, R package -- Version 4.13, 20 January 2020

- Fixed all code to work correctly when "changeoutputnames = TRUE"

========================================================
NE PVA tool, R package -- Version 4.12, 17 January 2020

- Bug fixes to "nepva.batchmode"

========================================================
NE PVA tool, R package -- Version 4.11, 13 January 2020

- Random simulation of impacts within automated testing using more plausible (much narrower)
    ranges

- Manual testing code now integrated into main testing code

========================================================
NE PVA tool, R package -- Version 4.10, 13 January 2020

- Includes additional code to generate inputs that can be used for manual testing

========================================================
NE PVA tool, R package -- Version 4.8, 3 December 2019

- Inclusion of code for automated testing

- Fixing some minor bugs that arose from the enhancements
    made in Version 4.7

========================================================
NE PVA tool, R package -- Version 4.7, 22 November 2019

Improvements/bug fixes following feedback from PSG:

a) fixed code for calculating metrics, so that the "baseline reference year" for calculating AGR/PGR 
     is specified explicitly --
     the issue previously was that this used "impact.year.start - 1", which is meaningless for baseline-only runs
     
     -- now fixed to be equal to "(impact.year-start - 1)" when any non-baseline scenarios are run, and 
        to "max(inipop.relyears)" when only the baseline scenario is run

b) fixed code for calculating outputs, so that:

      - there is now a 4th option ("whole.population")
      - "ages separated" also includes output for whole population
      - M5-M7 are not reported for individual age classes

c) set outputs to be NA when absolute harvest fails, rather than just crashing

========================================================
NE PVA tool, R package -- Version 4.6, 9 October 2019

- Improved error messages to make them more useful, following automated testing results for v4.5

- Included line to prevent abundance becoming negative - truncates it at zero

========================================================
NE PVA tool, R package -- Version 4.5, 8 October 2019

- Converted various "browser" statements into "stop" statements, to enable proper error testing

- In "functions-sensitivity.R" improved robustness of code by linking to column of "demobase." rather than using column names - enables inputs to be specified as matrices rather than data frames

========================================================
NE PVA tool, R package -- Version 4.4, 29 September 2019

- Added new file "functions-batchrun.R" contain a function to easily run the main versions of the R package in batch mode

========================================================
NE PVA tool, R package -- Version 4.3, 12 September 2019

1) fixes a bug in the line "simbsk <- ret2minus1(sim.bskippc, m = tt)" within "functions-runpva.R"

2) adds "output.raw" as an option for "nepva.simplescenarios" (necessary for testing)

========================================================
NE PVA tool, R package -- Version 4.2, 12 August 2019

1) This update adds the option to run a "burn-in" period at the start of the PVA

This involved the following changes:

a. Within "functions-runpva" the function "nepva.sim" has been substantially rewritten, to include functionality to run the burn-in, and (as a prerequesite to this) separation out of part of the calculation into a new function "leslie.calcs", and an additional argument ("nburn")

b. A function "plot.burnconv" has been created within "functions-runpva" to plot conergence of the age distribution

c. Within "functions-highlevel.R" the functions now have "nburn" as an additional argument, a row called "nburn" has been added to "ModeOptions.xls" and "ModeOptions.csv", and an "nburn" argument has been added when running "nepva.calcs" within "functions-maincalcs.R"

2) The update also involved initializing "nbyage" with zero rather than NA - i.e. in the line

    nbyage <- array(dim=c(nscen, npop, nyears, sim.n, na), data = NA)
    
changing NA to 0. This avoids potential issues in processing NAs in outputs, BUT it does mean that results for years in which some colonies have not been initialized should be treated with extreme caution.    

As a result of these changes the Shiny interface will need to be retested, and modified to include an additional input (for "nburn")

========================================================
NE PVA tool, R package -- Version 4.1, 6 August 2019

This update fixes two minor bugs in the R package:

1. in "functions-preprocessing.R": in "pred.numeric" an extra "..." argument has been added to "pred.numeric", and the functions defined to call it, so that "ddfn" can be passed through to "simfn"; without this fix, the tool may crash with some models (e.g. when "logitnlogn" is used for "model.envstoch"). It is unlikely that this related to any options contained in the Shiny tool, so no knock-on impact on that is expected.

2. in "functions-runpva.R": the line defining "ntotal.actualbreedpairs" has now been modified to include a "floor(.)" call, to ensure that the resulting number is an integer - if this fix is not applied, the code will sometimes lead subsequent population sizes to be missing (NA), because these values are used in rbinom(.) and calling rbinom with a non-integer sample size leads to an NA. The Shiny tool may sometimes have produced missing output as a result of this issue, but no impact on the Shiny tool is expected as a result of the fix.

========================================================