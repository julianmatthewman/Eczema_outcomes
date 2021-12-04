#' Test for Interaction
#'
#' @param dataset A lists of lists containing datasets for different outcomes.
#'
#' @return A dataframe containing results from interaction analysis.
#' @export
#'
#' @examples
analysis_interaction <- function(dataset) {

# Interaction -----------------------------------------------------

fit_age <- coxph(outcome_surv ~ pattern + agegroup, 
									data = dataset$data, timefix=FALSE) 

fit_age_int <- coxph(outcome_surv ~ pattern * agegroup, 
											data = dataset$data, timefix=FALSE) 

# Likelihood ratio test
epiDisplay::lrtest(fit_age, fit_age_int)


# Calculate linear combination (skip if analysis==sens_aged_66_plus)
if (dataset$analysis!="sens_aged_66_plus") {
results_interaction <- biostat3::lincom(fit_age_int, c("patterncontinuous",
																											 paste0("patterncontinuous+patterncontinuous:agegroup", levels(dataset$data$agegroup)[-1])),
																				eform=TRUE, level=0.95, singular.ok=TRUE) %>% 
	as.data.frame() %>%
	map_df(as.numeric) %>%
	cbind(tibble(group="interaction for age",
							 level=levels(dataset$data$agegroup),
							 outcome=dataset$outcome, 
							 analysis=dataset$analysis))

} else results_interaction <- NA

results_interaction

}
