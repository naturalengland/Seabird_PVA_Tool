## ########################################################################################
## Project: NEC06986
## Script purpose: NE PVA Tool interface functions
## Author: DCM, Centre for Ecology & Hydrology
## Date v1.1:  6th March 2019
## ########################################################################################


## Function to calculate M7 metric - added Version 3.1
#
calc.m7 <- function(out){

   ## Function to calculate M7 metric - added Version 3.1

   id <- paste(out$Age, out$Scenario, sep="-")

   bob <- out$Year

   bob[(out$m6 < 50) | is.na(out$m6)] <- NA

   vals <- tapply(bob, id, min, na.rm=TRUE)

   new <- data.frame(Age = unlist(lapply(strsplit(names(vals), "-"), function(x){x[1]})),
      Scenario = unlist(lapply(strsplit(names(vals), "-"), function(x){x[2]})),
      m7 = as.numeric(vals))

   new
}

## Reformat the inipop fields for the R function
#
inipop_vec <- function(inm,strv) {
   out <- numeric()
   for(x in 1:inm$npop) {
      out <- c(out, inm[[paste0(strv,x)]])
   }
   out
}

## error checks
#
errfn5s <- function(x,n){ ! (is.numeric(x) & is.vector(x) & (length(x) == n) & (all(!is.na(x))) & (all(x > 0))) }
errfn3s0 <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & (x > 0) & !is.na(x) & !is.null(x)) }
errfn3s <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & !is.na(x) & !is.null(x)) }

# Progress counter
prog_sum <- function(inm){

   prog <- 0

   # The single mandatory variables (apply to all run types)
   prog <- prog + ifelse(errfn3s0(inm$sim_n), 0, 1)
   prog <- prog + ifelse(errfn3s0(inm$output_year_end), 0, 1)

   # SWITCH: demobase_splitpops
   NPb <- ifelse (inm$demobase_splitpops == TRUE, inm$npop, 1)

   # These apply to all run types
   if (!errfn3s0(inm$npop)){
      for (i in 1:NPb) {
         prog <- prog + ifelse(errfn3s(inm[[paste0("pr_mn_",i)]]), 0, 1)
         prog <- prog + ifelse(errfn3s(inm[[paste0("pr_sd_",i)]]), 0, 1)
         prog <- prog + ifelse(errfn3s(inm[[paste0("ad_mn_",i)]]), 0, 1)
         prog <- prog + ifelse(errfn3s(inm[[paste0("ad_sd_",i)]]), 0, 1)
         prog <- prog + ifelse(errfn3s0(inm[[paste0("inipopyrs_",i)]]), 0, 1)
         prog <- prog + ifelse(errfn3s(inm[[paste0("inipopval_",i)]]), 0, 1)
      }
      if (inm$model_dd != "nodd") {
         for (i in 1:NPb) {
            prog <- prog + ifelse(errfn3s(inm[[paste0("pr_idd_",i)]]), 0, 1)
            prog <- prog + ifelse(errfn3s(inm[[paste0("ad_idd_",i)]]), 0, 1)
         }
      }
   }

   # Only count these if not a validation run
   if (inm$run_type != "senstivity.local"){
      if (!errfn3s0(inm$npop)){
         if (inm$demobase_splitimmat == TRUE) {
            for (i in 1:NPb) {
               for (j in 1:inm$afb) {
                  prog <- prog + ifelse(errfn3s(inm[[paste0("im_mn_",i,"_",j)]]), 0, 1)
                  prog <- prog + ifelse(errfn3s(inm[[paste0("im_sd_",i,"_",j)]]), 0, 1)
               }
            }
         }
         if (inm$demobase_splitimmat == TRUE & inm$model_dd != "nodd") {
            for (i in 1:NPb) {
               for (j in 1:inm$afb) {
                  prog <- prog + ifelse(errfn3s(inm[[paste0("im_idd_",i,"_",j)]]), 0, 1)
               }
            }
         }
      }
   }

   # SWITCH: impacts_splitpops
   NPi <- ifelse (inm$impacts_splitpops == TRUE, inm$npop, 1)

   if (!errfn3s0(inm$npop) & !errfn3s0(inm$nscen)) {
      for (i in 1:inm$nscen) {
         for (j in 1:NPi) {
            prog <- prog + ifelse(errfn3s(inm[[paste0("imp_pr_mn_", i,"_", j)]]), 0, 1)
            prog <- prog + ifelse(errfn3s(inm[[paste0("imp_ad_mn_", i,"_", j)]]), 0, 1)
         }
      }
   }

   if (inm$run_type != "sensitivity.local"){
      if (all(!errfn3s0(inm$npop), !errfn3s0(inm$nscen), inm$impacts_provideses==TRUE)) {
         for (i in 1:inm$nscen) {
            for (j in 1:NPi) {
               prog <- prog + ifelse(errfn3s(inm[[paste0("imp_pr_se_", i,"_", j)]]), 0, 1)
               prog <- prog + ifelse(errfn3s(inm[[paste0("imp_ad_se_", i,"_", j)]]), 0, 1)
            }
         }
      }

      if (all(!errfn3s0(inm$npop), !errfn3s0(inm$nscen), inm$impacts_splitimmat==TRUE)) {
         for (i in 1:inm$nscen) {
            for (j in 1:NPi) {
               prog <- prog + ifelse(errfn3s(inm[[paste0("imp_im_mn_", i,"_", j)]]), 0, 1)
            }
         }
      }

      if (all(!errfn3s0(inm$npop), !errfn3s0(inm$nscen), inm$impacts_splitimmat==TRUE, inm$impacts_provideses==TRUE)) {
         for (i in 1:inm$nscen) {
            for (j in 1:NPi) {
               prog <- prog + ifelse(errfn3s(inm[[paste0("imp_im_se_", i,"_", j)]]), 0, 1)
            }
         }
      }
   }

   if (inm$run_type == "simplescenarios") {
      prog <- prog + ifelse(errfn3s0(inm$output_year_start), 0, 1)
      prog <- prog + ifelse(errfn3s0(inm$npop), 0, 1)
      prog <- prog + ifelse(errfn3s(inm$nscen), 0, 1)

   } else if (inm$run_type == "validation") {

   } else if (inm$run_type == "sensitivity.local") {
      prog <- prog + ifelse(errfn3s0(inm$sens_npvlocal), 0, 1)
      for (i in 1:5) {
         prog <- prog + ifelse(errfn3s(inm[[paste0("sens_pcr_", i)]]), 0, 1)
      }

   }
   out <- prog
}

# How many parameters should there be?
prog_total <- function(inm){

   complete <- 0

   NS <-  ifelse (is.na(inm$nscen), 0, inm$nscen)
   NP <-  ifelse (is.na(inm$npop), 0, inm$npop)
   NPb <- ifelse (inm$demobase_splitpops == TRUE, NP, 1)
   NPi <- ifelse (inm$impacts_splitpops == TRUE, NP, 1)

   if (inm$run_type == "simplescenarios") {

      complete <- complete + 5 + (6 * NPb)
      if (inm$model_dd != 'nodd') {complete <-  complete + (2*NPb)}
      if (inm$demobase_splitimmat == TRUE) {complete <-  complete + (2*NPb*inm$afb)}
      if (inm$demobase_splitimmat == TRUE & inm$model_dd != 'nodd') {complete <-  complete + (NPb*inm$afb)}

      complete <- complete + (2*NPi*NS)
      if (inm$impacts_provideses == TRUE) {complete <-  complete + (2*NPi*NS)}
      if (inm$impacts_splitimmat == TRUE) {complete <-  complete + (NPi*NS)}
      if (inm$impacts_splitimmat == TRUE & inm$impacts_provideses == TRUE) {complete <-  complete + (NPi*NS)}

   } else if (inm$run_type == "validation") {

      complete <- 2 + (6 * NPb)
      if (inm$model_dd != 'nodd') {complete <-  complete + (2*NPb)}
      if (inm$demobase_splitimmat == TRUE) {complete <-  complete + (2*NPb*inm$afb)}
      if (inm$demobase_splitimmat == TRUE & inm$model_dd != 'nodd') {complete <-  complete + (NPb*inm$afb)}

   } else if (inm$run_type == "sensitivity.local") {

      complete <- 8 + (6 * NPb)
      if (inm$model_dd != 'nodd') {complete <-  complete + (2*NPb)}
      complete <- complete + (2*NPi*NS)

   }
   out <- complete
}


## ERROR CHECKS form the R package - could be used to check fields? ----------------------------
errfn1 <- function(x){! (is.logical(x) & is.vector(x) & length(x == 1)) }
errfn2 <- function(x){! (is.character(x) & is.vector(x) & length(x == 1)) }
errfn3a <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & (x > 0)) }
errfn3b <- function(x){! (is.numeric(x) & is.vector(x) & length(x == 1) & (x >= 0)) }
errfn4 <- function(x){  if(is.null(x)){ out <- FALSE  }
   else{ out <- ! ((is.numeric(x) & is.vector(x) & length(x == 1) & (x > 0)) | is.null(x)) }
   out
}





