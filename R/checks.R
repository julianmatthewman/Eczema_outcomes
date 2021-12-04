# 
# # Checks ------------------------------------------------------------------
# 
# #Check for discrepancies between active and currdose
# main_split %>% select(-main_surv) %>% filter(active=="active" & currdose_cont==0)
# main_split %>% select(-main_surv) %>% filter(active=="not active" & currdose_cont>0)
# 
# #There are some prescriptions that are missing a dose, but only 26 observations in total!
# presc_exp %>% filter(patid==9184) %>% mutate(start=as_date(start), end=as_date(end)) %>% select(-eventdate)
# prescriptions %>% filter(patid==9184)
# 
# main_split %>% select(patid, rollgap, active, pattern_rolling, pattern_period, tstart, tstop)
# 
# 
# #Check main_split
# main_split$agegroup %>% table()
# map_dfc(c("agegroup", "sex", "bmi_cat", "smok", "imd_composite", "asthma", "fract", "gc", "high"),
# 				~main_split %>%
# 					group_by(!!sym(.x)) %>%
# 					summarise(n=n()) %>%
# 					pivot_wider(names_from = !!sym(.x),
# 											values_from = n,
# 											names_prefix = .x)) %>%
# 	t()
# 
# 
# # Follow up time ----------------------------------------------------------
# # Purpose: calculate follow up in eczema cohort starting from first oral corticostoier prescription
# 
# #main_facet_split %>% select(patid, enddate, tstart, tstop, assessment_period, recency)
# 
# #How many participants are there (there should be the same number during and after the assesment period)
# n_distinct(main_split$patid)
# n_distinct(main_split$patid[main_split$assessment_period==1])
# n_distinct(main_split$patid[main_split$assessment_period==2])
# 
# 
# 
# #How much follow up time
# pyears(main_surv ~ assessment_period, data = main_split, scale = 1)
# #How much follow up time in the assessment period (should be the same as the number of participants)
# pyears(main_surv ~ assessment_period, data = main_split[main_split$assessment_period==1,], scale = 1) %>% tidy()
# #How much follow up time after the assessment period
# pyears(main_surv ~ assessment_period, data = main_split[main_split$assessment_period==2,], scale = 1) %>% tidy()
# 
# 
# 
# # Define continuous vs intermittent use -----------------------------------
# 
# #Calculate number of participants classified as continuous vs intermittent
# map(c("0.5"=0.5, "0.4"=0.4, "0.3"=0.3, "0.2"=0.2, "0.1"=0.1), #Run the following defining continuous use as more than 50%, 40%, 30%, etc.
# 		~main_split %>% 
# 			filter(assessment_period==1) %>% #Use only the one year assesment period
# 			mutate(time=tstop-tstart) %>% #Calculate time for each observation
# 			group_by(patid) %>% #Group by patient ID
# 			summarise(active=sum(time[active=="active"])) %>% #Calculate the active time as the sum of events where receny is current, i.e. where there is an active prescription
# 			mutate(pattern=ifelse(active>=.x, "continuous", "intermittent")) %>% #Set pattern to "continuous" if active time is more than the defined threshold
# 			select(pattern) %>%
# 			table()
# )
# 
# #For the above calculated definitions, calculate how many participants have the same status during and after the assessment year (match is TRUE)
# map(c("0.5"=0.5, "0.4"=0.4, "0.3"=0.3, "0.2"=0.2, "0.1"=0.1),
# 		~main_split %>% 
# 			mutate(time=tstop-tstart) %>% #Calculate time for each observation
# 			group_by(patid, assessment_period) %>% #Group by patient ID and assessment period, to calculate time during and after assessment
# 			summarise(active=sum(time[active=="active"])) %>%
# 			mutate(pattern=ifelse(active>=.x, "continuous", "intermittent")) %>%
# 			group_by(patid) %>%
# 			summarise(match=all(pattern=="intermittent") | all(pattern=="continuous"), #Could also use ifelse(n_distinct(pattern)==1, TRUE, FALSE) for this
# 								initial=first(pattern)) %>%
# 			select(match, initial) %>%
# 			table()
# )
# 
# 
# 
# 
# 
# 
# 
# # Explore data ------------------------------------------------------------
# 
# #Using the summarytools package
# library(summarytools)
# view(dfSummary(main_cohort), file = "summary_main_cohort.html")
# view(dfSummary(prescriptions), file = "summary_prescriptions.html")
# view(dfSummary(cprdfract), file = "summary_cprdfract.html")
# view(dfSummary(hesfract), file = "summary_hesfract.html")
# view(dfSummary(allfract), file = "summary_allfract.html")
# view(dfSummary(alc), file = "summary_alc.html")
# view(dfSummary(asthma), file = "summary_asthma.html")
# view(dfSummary(ethn), file = "summary_ethn.html")
# view(dfSummary(severity), file = "summary_severity.html")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Manually
# summary(main_cohort)
# sapply(main_cohort, mean, na.rm=TRUE)
# sapply(main_cohort, sd, na.rm=TRUE)
# colMeans(is.na(main_cohort))
