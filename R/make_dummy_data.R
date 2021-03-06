#' Make dummy data
#' @description Load dummy datasets and path names, and writes them to files

make_dummy_data <- function() {
	library(tidyverse)
    library(lubridate)

# Specifications ----------------------------------------------------------

n <- 10000 #Set number of required participants
prop_presc <- 0.5 #Set number of participants with at least one prescription
prop_clin <- 0.5 #Set number of participants with at least one clinical code
prop_ther <- 0.5 #Set number of participants with at least one therapy code




# dummy main_cohort -------------------------------------------------------
main_cohort <- data.frame(patid=seq(n))
main_cohort$sex <- rbinom(n, 1, 0.5)
main_cohort$dob <- sample(seq(as.Date('1930/01/01'), as.Date('1990/01/01'), by="day"), n, replace = TRUE)
main_cohort$realyob <- year(main_cohort$dob)
main_cohort$indexdate <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n, replace = TRUE)
main_cohort$enddate <- main_cohort$indexdate + abs(rnorm(n, mean=365*6.5, sd=365*2))
main_cohort$bmi <- rnorm(n, mean = 27, sd = 6)
main_cohort$bmi_cat <- rbinom(n, 3, 0.3)
main_cohort$smokstatus <- rbinom(n, 1, 0.5)
main_cohort$carstairs <- sample(seq(5), n, replace = TRUE)
main_cohort$eth5 <- rbinom(n, 4, 0.1)
main_cohort$eth16 <- rbinom(n, 4, 0.1)
main_cohort$exposed <- 1
main_cohort$pracid <- sample(seq(500), n, replace = TRUE)

main_cohort <<- main_cohort
path_main_cohort <<- "dummy_data/dummy_main_cohort.csv"
write.csv(main_cohort, path_main_cohort, row.names = FALSE)



# dummy clinical extract files-----------------------------------------------------

clinical_codes <- map_df(dir("codelists/ICD-10", pattern = "codelist", full.names = TRUE, recursive = TRUE),
                         ~read_csv(.x, col_select = "V1", n_max = 100)
                         )

#Specify a proportion of the original patids
dummy_extract_clinical <- data.frame(patid=sample(sample(main_cohort$patid, n*prop_clin), (n*prop_clin)*10, replace = TRUE))
dummy_extract_clinical$V1 <- sample(clinical_codes$V1, n, replace = TRUE)
dummy_extract_clinical$start <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n, replace = TRUE)
dummy_extract_clinical <- arrange(dummy_extract_clinical, patid, start) %>% left_join(main_cohort[c("patid", "indexdate")], by="patid")

dummy_extract_clinical <<- dummy_extract_clinical
path_dummy_extract_clinical <<- "dummy_data/dummy_extract_clinical.csv"
write.csv(dummy_extract_clinical, path_dummy_extract_clinical, row.names = FALSE)

# dummy therapy extract files-----------------------------------------------------

therapy_codes <- map_df(dir("codelists/SNOMED", pattern = "codelist", full.names = TRUE, recursive = TRUE),
                         ~read_csv(.x, col_select = "SNOMED Code", n_max = 100)
                         )

#Specify a proportion of the original patids
dummy_extract_therapy <- data.frame(patid=sample(sample(main_cohort$patid, n*prop_ther), (n*prop_ther)*20, replace = TRUE))
dummy_extract_therapy$`SNOMED Code` <- sample(therapy_codes$`SNOMED Code`, n, replace = TRUE)
dummy_extract_therapy$start <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n, replace = TRUE)
dummy_extract_therapy <- arrange(dummy_extract_therapy, patid, start) %>% left_join(main_cohort[c("patid", "indexdate")], by="patid")

dummy_extract_therapy <<- dummy_extract_therapy
path_dummy_extract_therapy <<- "dummy_data/dummy_extract_therapy.csv"
write.csv(dummy_extract_therapy, path_dummy_extract_therapy, row.names = FALSE)

# dummy prescriptions -----------------------------------------------------
#Specify a proportion of the original patids
prescriptions_ped <- data.frame(patid=sample(sample(main_cohort$patid, n*prop_presc), (n*prop_presc)*30, replace = TRUE))
#hist(as.integer(table(prescriptions_ped$patid))) #How many people have one prescription, two prescriptions, three prescriptions, etc...
prescriptions_ped$ped <- rgamma(10000, shape = 13)
prescriptions_ped$high <- ifelse(prescriptions_ped$ped>=20, 1, 0)
prescriptions_ped$duration <- ceiling(abs(rnorm(10000, mean=22, sd=30)))+1
prescriptions_ped$start <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n, replace = TRUE)
prescriptions_ped$end <- prescriptions_ped$start + prescriptions_ped$duration

prescriptions_ped <- arrange(prescriptions_ped, patid, start) %>% left_join(main_cohort[c("patid", "indexdate")], by="patid")

prescriptions_ped <<- prescriptions_ped
path_prescriptions_ped <<- "dummy_data/dummy_prescriptions_ped.csv"
write.csv(prescriptions_ped, path_prescriptions_ped, row.names = FALSE)



# dummy severity ---------------------------------------------------------------
severity <- data.frame(patid=sample(main_cohort$patid, n*0.16))
severity$date <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n*0.16, replace = TRUE)
severity$modsevere <- rbinom(n*0.16, 2, 0.23)+1

severity <- arrange(severity, date)

severity <<- severity
path_severity <<- "dummy_data/dummy_severity.csv"
write.csv(severity, path_severity, row.names = FALSE)



# Dummy time updated variables -------------------------------------------------

# Specify name and prevalence
vars <- c(
	"fract_all" = 0.01,
	"fract_hip" = 0.008,
	"fract_spine" = 0.006,
	"fract_wrist" = 0.004,
	"fract_pelvis" = 0.002,
	"bisphosphonate" = 0.2,
	"calcium_and_vit_d" = 0.03,
	"alc" = 0.001,
	"antabuse" = 0.001,
	"asthma" = 0.01,
	"copd" = 0.001,
	"rheumatoid_arthritis" = 0.005
)

for (i in seq_along(vars)) {
	# Sample from main cohort with given prevalence
	data <- data.frame(patid=sample(main_cohort$patid, n*vars[[i]]))
	
	# Assign random date to events
	data$eventdate <- sample(seq(as.Date('1998/01/01'), as.Date('2020/01/01'), by="day"), n*vars[[i]], replace = TRUE)
	data <- arrange(data, eventdate)
	
	# Assign object to global enivronment
	assign(names(vars[i]), data, envir = .GlobalEnv)
	
	# Make path, assign to global environment and write to file
	path <- paste0("dummy_data/", names(vars[i]), ".csv")
	assign(paste0("path_", names(vars[i])), path, envir = .GlobalEnv)
	write.csv(data, path, row.names = FALSE)
}



}

# # Distribution Histograms -----------------------------------------------------------
# 
# #Normal
# hist(rnorm(n = 10000, mean = 10, sd = 5))
# #Uniform
# hist(runif(n = 10000, min = 1, max = 10))
# hist(sample(seq(5), 10000, replace = TRUE))
# #Binomial
# hist(rbinom(n = 10000, size= 1, prob = 0.3))
# hist(rbinom(n = 10000, size= 4, prob = 0.3))
# hist(rbinom(n = 10000, size= 4, prob = 0.1))
# #Gamma
# hist(rgamma(n = 10000, shape = 1)) #The shape is the mean of the distribution
# hist(rgamma(n = 10000, shape = 2))
# hist(rgamma(n = 10000, shape = 3))
# #Beta
# hist(rbeta(n = 10000, shape1 = 2, shape2 = 5))
# hist(rbeta(n = 10000, shape1 = 2, shape2 = 5)*5)
# #Sampling without replacement
# hist(sample(seq(1:100)))
# #Sampling with replacement
# hist(sample(seq(1:100), replace = TRUE))
