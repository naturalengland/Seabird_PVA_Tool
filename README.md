# Natural England and JNCC Seabird PVA Tool

## Project description

A suite of R functions which operate as an R package (nepva R package) designed to undertake population viability analysis (PVA) for seabird species. This R package underpins the functionality of the user-friendly web-based Shiny tool which implements a subset of the functionality available within the R package. The Shiny tool provides an interactive web-based mechanism for users to provide inputs to the R package, and to display and save the results obtained by running the R package (without the need for users to install or use R).

The PVA Tool is motivated by the use of PVAs to assess the impacts of proposed offshore renewable energy developments (ORDs) upon seabird populations, but the functionality within the package is sufficiently general that the package could also be used for assessing the population-level consequences of other anthropogenic activities that impact on population demographic rates, and for assessing impacts upon non-seabird species.

The tool allows the assessment of any type of impact in terms of a change to demographic rates, or as a cull or harvest of a fixed size per year. Impacts may also be positive, meaning that mitigation or conservation measures aimed at increasing demographic rates may also be modelled. The tool allows users to conduct PVAs at a range of scales (breeding colony to SPA or region). 
The tool can also be used to produce “historical” PVAs, for validation purposes, where outputs can be assessed against validation counts, or to undertake sensitivity analyses.

The online version of the PVA Shiny tool can be accessed via this link (the tool works best in a Chrome browser):
http://ec2-34-243-66-127.eu-west-1.compute.amazonaws.com/shiny/seabirds/PVATool_Nov2022/R/

**Please note that currently whenever the Shiny tool is updated this URL changes and therefore, we advise users to always access the tool from this repository rather than relying on bookmarks.** 

## Processing scripts

The nepva package was developed using Version 3.5.1 of R, and utilises two add-on packages: **mvtnorm** (it was developed using Version 1.0-8) and **popbio** (it was developed using Version 2.4.4).

This is **tool v 2.0 (nepva R package: v 4.18 Interface: v 1.7)**. This is the most recent version of the tool and is available on a use-at-your-own-risk basis. Please refer to the Rpackage guidance documentation for a detailed description of the methods employed and associated functions.

## Default parameters and outputs

Whilst default model parameters are provided for specific bird species within the repository, these can be replaced by user defined values within the R package or Shiny tool and are not required to run the tool.

The sources and methods used to derive the default parameters, provided in look up tables and used within the R package and Shiny tool are detailed within the relevant documentation and metadata provided within the repository. These suggested default parameters may be subject to periodical updates as new empirical evidence becomes available. Any updates will be subject to agreement with Natural England and will be recorded as an update to the repository. 

Suggested default parameters were estimated as follows. A single age at first breeding and maximum brood size for each species was derived from Horswill & Robinson (2015).  Default survival rates (mean and standard deviation) are recalculated from the source data used in Horswill & Robinson (2015), and are taken to be the estimates for the ‘National’ survival rate or, where available, colony-level estimates are provided. All breeding success (productivity rates per pair) data, across eight regional classifications, were produced using a 2018 snapshot of the JNCC’s Seabird Monitoring Programme (SMP) dataset and following the methods of Horswill & Robinson (2015). Please refer to the PVA Tool Rpackage Guidance documentation for more detail.

The tool produces a variety of outputs including the mean, median and standard deviation of projected population sizes under the input scenarios, counterfactuals of population growth rate and population size (based on impacted and unimpacted scenarios), annualised population growth rate since the “baseline reference year”, overall population growth since the “baseline reference year” and percentage population change since the “baseline reference year”. The units for output (e.g. breeding adults, breeding pairs or whole population) can also be specified by the user.

The Shiny tool provides tabular and graphical representations of the results of the model runs and also produces a log file that records the parameters and options selected by the user to derive the results.  

## Copyright and Licence

The PVA Tool is published by Natural England under the MIT Licence. Please refer to the LICENCE file supplied within the repository for full copyright and licence details.

## Ethics statement

Natural England have completed and submitted the UK Statistics Authority Ethics Self-Assessment Tool for this project. No major ethical considerations were identified. A copy of the completed Self-Assessment form is provided within the Documentation. 

## Acknowledgements

This code and Shiny tool were developed by Biomathematics and Statistics Scotland (BioSS) and UK Centre for Ecology and Hydrology (UKCEH) under contracts for Natural England and JNCC. 

We are also grateful to all those that have identified issues with the tool that have led to relevant fixes or improvements to the code.

## References

Horswill, C. & Robinson R. A. 2015. Review of seabird demographic rates and density dependence. JNCC Report No. 552. Joint Nature Conservation Committee, Peterborough. Available [here](https://data.jncc.gov.uk/data/897c2037-56d0-42c8-b828-02c0c9c12d13/JNCC-Report-552-REVISED-WEB.pdf). 

## Contact information

Please raise a new issue if you have any concerns with the code supplied within this repository or how it has been implemented. For further information, or if you have any queries, please contact the repository manager Andrew Harwood at Natural England (email: Andrew.Harwood@naturalengland.org.uk). 

## Documentation

The current guidance documents for the online version of the PVA Shiny tool, as well as the nepva R package, can be found in the Documentation folder in this GitHub repository.

The Natural England Commissioned Report for the original (Version 1) development of the PVA Tool can be accessed [here](http://publications.naturalengland.org.uk/publication/4926995073073152).

The JNCC report for the project which undertook testing of the tool, review of the R package code, along with some tool updates (Version 2) can be accessed [here](https://hub.jncc.gov.uk/assets/302a7a51-fe29-4633-95d1-b3ef458cb79a).
