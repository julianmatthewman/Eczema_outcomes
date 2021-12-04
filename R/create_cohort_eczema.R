#' Create Eczema Cohort.
#' @description Make a start-stop dataset from the cohort and the various files of events. See: Therneau T, Crowson C, Atkinson E. Using Time Dependent Covariates and Time Dependent Coefficients in the Cox Model.

#' @param path_main_cohort Path to file.
#' @param path_prescriptions Path to file.
#' @param path_cprdfract Path to file.
#' @param path_hesfract Path to file.
#' @param path_alc Path to file.
#' @param path_asthma Path to file.
#' @param path_ethn Path to file.
#' @param path_severity Path to file.
#'
#' @return A dataframe containing start-stop observations for all people with eczema.

create_cohort_eczema <- function(main_cohort, combined_eventdata) {

library(arrow)
library(rio)
library(haven)
library(survival)
library(lubridate)
library(tidyverse)
	

foo <- c("blu", "bla")    
    
# Create a start-stop dataset using the tmerge function ------------------------

tmerged <- tmerge(main_cohort, main_cohort, id=patid, tstart = indexdate-(100*365), tstop = enddate) #Therneau: "The first call sets the time range for each subject to be from 0 (default) to last follow-up. If a later call tried to add an event outside that range, at time = -2 say, that addition would be ignored." In our case we want to capture everything before the end of the study, since we will set our follow up window later, so here we set tstart to 100 years before the indexdate.
for (i in seq_along(combined_eventdata)) {
    tmerged <- tmerge(tmerged, combined_eventdata[[i]], id=patid, time_dep_cov=tdc(start))
    tmerged[str_sub(names(combined_eventdata)[i],start = str_length("eventdata_x"))] <- tmerged$time_dep_cov
}

tmerged %>%
    mutate(sex=factor(sex),
           bmi_cat=factor(bmi_cat)) %>% 
    select(-time_dep_cov)

}
