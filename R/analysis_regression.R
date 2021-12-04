#' Run Cox regression
#' @description Run Cox regression, tidy the output and add names of model, outcome and analysis.
#' @param cohort_steroids A list containing a dataset for a specified outcome and analysis.
#' @param model The model formula.
#' @return A dataframe containing results.

analysis_regression <- function(cohort_post_exclusion, exposure, outcome, model, exclusion) {
    library(survival)
    library(broom)
    library(tidyverse)
    
    #Make survival object
    outcome_surv <- Surv(time = as.numeric(cohort_post_exclusion$data$tstart), 
                         time2 = as.numeric(cohort_post_exclusion$data$tstop),
                         origin = as.numeric(cohort_post_exclusion$data$indexdate),
                         event = cohort_post_exclusion$data[[outcome]])
    
    coxph(formula(paste("outcome_surv ~", exposure, model)), 
          data=cohort_post_exclusion$data) %>% 
        tidy(exponentiate=TRUE, conf.int=TRUE) %>%
        mutate(exposure=exposure,
               outcome=outcome,
               model=names(model),
               exclusion=cohort_post_exclusion$label)
    
}