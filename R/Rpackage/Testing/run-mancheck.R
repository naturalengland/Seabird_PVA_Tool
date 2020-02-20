setwd("C://Users//adam//Work//Active//JNCC-PVA-testing//Version4.12//")

ff <- list.files(".", pattern="functions") ; for(k in 1:length(ff)){ source(ff[k])} ## Automated Version 4.8

modeoptions <- read.csv("ModeOptions.csv") ## Added Version 2.1

inputsspec <- read.csv("..//Version4.12//Outputs//mancheck-inputspecs.csv")

nepva.save.and.run.valid(inputspecs = inputsspec, outpath = "Outputs//mancheck-")

