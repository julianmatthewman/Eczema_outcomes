library(targets)
library(tarchetypes)
library(here)
# This _targets.R file defines the {targets} pipeline.
# Run tar_make() to run the pipeline, tar_make(target) to run up to a defined target, and tar_read(target) to view the results.

# #Future locally
#library(future.callr)
#plan(callr)

# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE), source, .GlobalEnv)

# Set target-specific options such as packages.
tar_option_set(
	packages = c(
	    "here",
		"rio", #For reading various filetypes
		"arrow", #For reading parquet files
		"cli", #For producing command line interface elements such as loading bars
		"zoo", # For rolling windows
		"survival", # For survival analysis including Cox regression
		"magrittr", # To improve readability with pipes
		"haven", # To import Stata .dta
		"broom", # To clean regression output
		"epiDisplay", # For data exploration
		"biostat3", # For data exploration
		"lubridate", # To manage dates
		"summarytools", # For data exploration
		"gt", # To create publication-ready tables
		"ggplot2", # To make plots
		"forestplot", # To make forest plots
		"DiagrammeR", # To make flow diagrams
		"DiagrammeRsvg", # To export flow diagrams
		"rsvg", # To export flow diagrams
		"dtplyr", #data.table backend for dplyr
		"qs", #To store targets in qs format for quicker reading and writing
		"tidyverse" # For data management
	)
) 

#Set paths
source_path <- "dummy_data"






list(
    
    # Eventdata ---------------------------------------------------------
    # The following targets declare steps necessary to extract eventdata using source data and codelists
    
    tar_target( # Define search patterns for source files
        extract_patterns,
        c("extract_clinical","extract_therapy")
    ),
    tar_files( #Make target tracking all the source files
        extract,
        list.files("dummy_data", pattern = paste(extract_patterns, collapse = "|"), full.names = TRUE, recursive = TRUE)
    ),   
    tar_files( #Make target tracking all the codelist files
        codelists,
        list.files("codelists", pattern = "codelist.csv", full.names = TRUE, recursive = TRUE)
    ),        
    tar_target( # Creates metadata for codelists such as which is the code variable
        metadata,
        tibble(
            name=strsplit(codelists[[1]], "/")[[1]][[3]], #name is in third place of folder hierarchy i.e. codelists/coding-system/name/codelist.csv
            codevar=ifelse(str_detect(codelists, "ICD-10"), "V1", "SNOMED Code"),
            pattern=ifelse(str_detect(codelists, "ICD-10"), "extract_clinical", "extract_therapy")),
        pattern = map(codelists)
    ),
    tar_target( # Loads the codes from the codelist files
        codes, 
        sort(import(codelists)[[metadata$codevar]]),
        pattern = map(codelists, metadata),
        iteration = "list"
    ), 
    tar_target( # Iterates over all the file paths provided
        eventdata, 
        import(extract[extract_patterns==metadata$pattern]) %>% filter(eval(sym(metadata$codevar)) %in% codes), 
        pattern = map(codes, metadata),
        iteration = "list"
    ),
	
    
    
	# Specifications -------------------------------------------------------------
    # The following targets declare any specifications that can be set by the user
	
	# Specify outcomes
	tar_target(
	    outcome, 
	    c("asthma",
	      "fractures",
	      "prednisolone",
	      "lymphoma")
	),
	
	# Specify exposures
	tar_target(
	    exposure,
	    c("eczema")
	),
	
	# Specify exclusions
	tar_target(
	    exclusion,
	    c(NA, "lymphoma", "asthma")
	),
	
	# Specify models for regression
	tar_target(
	    model, 
	    c("crude" = "",
	      "adjusted_sex" = "+ sex",
	      "adjusted_sex_bmi" = "+ sex + bmi_cat"
	    )
	),
	
    
	
	# Data management ------------------------------------------------------------
    # The following targets declare any data management steps
	tar_target(
		main_cohort, 
		read_csv("dummy_data/dummy_main_cohort.csv")
		),
	tar_target(
		cohort_eczema, 
		create_cohort_eczema(main_cohort, eventdata, metadata),
	),
	tar_target(
	    cohort_post_exclusion,
	    create_cohort_post_exclusions(cohort_eczema, exclusion),
	    pattern = map(exclusion)
	),
	
	
	
	# Analysis -------------------------------------------------------------------
    # The following targets declare analysis steps
	tar_target(
		results_regression, 
		analysis_regression(cohort_post_exclusion, exposure, outcome, model, exclusion),
		pattern = cross(cohort_post_exclusion, exposure, outcome, model)
		)
)

