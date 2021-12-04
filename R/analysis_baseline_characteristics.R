#' Get baseline characteristics.
#' @description Calculate descriptive statistics at baseline for different cohorts.
#' @param cohort_eczema A dataframe containing start-stop observations for all people with eczema. 
#' @param cohort_steroids A lists of lists containing datasets for different outcomes.
#'
#' @return A list of dataframes with baseline characteristics for different cohorts.
#' @export
#'
#' @examples
analysis_baseline_characteristics <- function(cohort_eczema, cohort_steroids) {
	library(lubridate)
	library(tidyverse)

# Get baseline characteristics in eczema cohort -----------------------------------------------------------

	#Make variable if participant ever experiences outcome (in entire follow up)
	flat <- cohort_eczema %>% 
		group_by(patid) %>% 
		mutate(fract_any_ever=ifelse(any(fract_any==1), 1, 0),
					 fract_composite_ever=ifelse(any(fract_composite==1), 1, 0),
					 fract_hip_ever=ifelse(any(fract_hip==1), 1, 0),
					 bisphosphonate_ever=ifelse(any(bisphosphonate==1), 1, 0)) %>% 
		ungroup()
	
#Make dataset including only the row at indexdate
flat <- flat %>% filter(tstart == indexdate)


#Numeric variables
numvars <- flat %>%
	summarise(n=n(),
						male=sum(as.numeric(sex[sex==1])),
						age_median=median(age),
						age_IQR_lower=quantile(age, 0.25, na.rm = TRUE),
						age_IQR_upper=quantile(age, 0.75, na.rm = TRUE)) %>%
	t()


#Categorical variables
catvars <- map_dfc(c("agegroup", "sex",  "asthma", "fract_any_ever", "fract_hip_ever", "fract_composite_ever", "bisphosphonate_ever", "smokstatus", "bmi_cat", "cumdose"),
									 ~flat %>%
									 	group_by(!!sym(.x)) %>%
									 	summarise(n=n()) %>%
									 	pivot_wider(names_from = !!sym(.x),
									 							values_from = n,
									 							names_prefix = .x)) %>%
	t()


#Join numvars and catvars
baseline_characteristics_eczema <- cbind(rbind(numvars, catvars)) %>%
	as.data.frame() %>%
	rename(all_eczema=V1) %>%
	rownames_to_column() %>%
	mutate(all_eczema_percent=all_eczema/all_eczema[rowname=="n"])






# Get baseline characteristics in high dose steroids cohort ---------------

#Make variable if participant ever experiences outcome (in entire follow up)
flat_steroids <- cohort_steroids$data %>% 
	group_by(patid) %>% 
	mutate(fract_any_ever=ifelse(any(fract_any==1), 1, 0),
				 fract_composite_ever=ifelse(any(fract_composite==1), 1, 0),
				 fract_hip_ever=ifelse(any(fract_hip==1), 1, 0),
				 bisphosphonate_ever=ifelse(any(outcome_surv[,"status"]==1), 1, 0))

#Make dataset with only first row per participant
flat_steroids <- flat_steroids %>% filter(!duplicated(patid))


#Numeric variables
numvars_steroids <- flat_steroids %>%
	group_by(pattern) %>% 
	summarise(n=n(),
						male=sum(as.numeric(sex[sex==1])),
						age_median=median(age),
						age_IQR_lower=quantile(age, 0.25, na.rm = TRUE),
						age_IQR_upper=quantile(age, 0.75, na.rm = TRUE)) %>%
	select(-pattern) %>% 
	t()

catvars_steroids <- map_dfc(c("agegroup", "sex",  "asthma", "smokstatus", "bmi_cat", "cumdose", "fract_any_ever", "fract_composite_ever", "fract_hip_ever", "bisphosphonate_ever"),
									 ~flat_steroids %>%
									 	group_by(pattern, !!sym(.x)) %>%
									 	summarise(n=n(), .groups = "drop") %>%
									 	pivot_wider(names_from = !!sym(.x),
									 							values_from = n,
									 							names_prefix = .x) %>% 
									 	column_to_rownames("pattern")) %>% 
	select(!starts_with("pattern")) %>% 
	t()

#Join numvars and catvars
baseline_characteristics_steroids <- rbind(numvars_steroids, catvars_steroids) %>%
	as.data.frame() %>%
	rownames_to_column() %>%
	mutate(all=intermittent+continuous,
				 intermittent_percent=intermittent/intermittent[rowname=="n"],
				 continuous_percent=continuous/continuous[rowname=="n"],
				 all_percent=all/all[rowname=="n"]) %>% 
	select(rowname, intermittent, intermittent_percent, continuous, continuous_percent, all, all_percent)
	



# Output as joint list ----------------------------------------------------
set_names(list(baseline_characteristics_eczema, baseline_characteristics_steroids),
					c("eczema", "steroids"))



}

