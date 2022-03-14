# Seabird_PVA_Tool
A Shiny app for carrying out population viability analysis (PVA) for seabird species under different impact scenarios.

This is tool v 2.0 (nepva R package: v 4.18 Interface: v 1.7). This is the most recent version of the tool and is available on a use-at-your-own-risk basis. See Documentation folder for user manuals.

The PVA Tool is published by Natural England under the Open Government Licence.

The PVA Tool is built using Shiny as a user-friendly interface for functions within an R package (nepva). The online version of the Shiny PVA Tool allows users to access the tool via a user-friendly Graphical User Interface (GUI) using a standard web browser without having to install or use R. The underlying nepva R package is designed to undertake population viability analysis (PVA) for seabird species and has additional functionality that is not available in the Shiny app. 

The PVA Tool is motivated by the use of PVAs to assess the impacts of proposed offshore renewable energy developments (ORDs) upon seabird populations, but the functionality within the package is sufficiently general that the package could also be used for assessing the population-level consequences of other anthropogenic activities that impact on population demographic rates, and for assessing impacts upon non-seabird species.

The tool is capable of assessing any type of impact in terms of a change to demographic rates, or as a cull or harvest of a fixed size per year. Impacts may also be positive, meaning that mitigation or conservation measures aimed at increasing demographic rates may also be modelled. The tool allows users to conduct PVAs at a range of scales (breeding colony to SPA or region).

# Online Version
The online version of the PVA Tool can be accessed via this link (works best in Chrome): 

http://ec2-54-229-75-12.eu-west-1.compute.amazonaws.com/shiny/seabirds/PVATool_mar2022/R/

# Documentation
The current guidance documents for the online version of the PVA Tool as well as the nepva R package can be found in the Documentation folder in this GitHub repository.

The Natural England Commissioned Report for the original (Version 1) development of the PVA Tool can be accessed via this link:

http://publications.naturalengland.org.uk/publication/4926995073073152    

**Warning (March 2022):** This document is out of date and will be replaced soon.  

The JNCC report for the project which undertook testing of the tool, review of the R package code, along with some tool updates (Version 2) can be accessed via this link:

https://hub.jncc.gov.uk/assets/302a7a51-fe29-4633-95d1-b3ef458cb79a
