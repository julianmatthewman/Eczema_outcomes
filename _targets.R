library(targets)
library(tarchetypes)
library(here)
# This _targets.R file defines the {targets} pipeline.
# Run tar_make() to run the pipeline, tar_make(target) to run up to a defined target, and tar_read(target) to view the results.

# #Future locally
#library(future.callr)
#plan(callr)

# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE) ,source, .GlobalEnv)

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





# Eventdata ---------------------------------------------------------
source_path <- "dummy_data"

eventdata <- tar_map(
        unlist = FALSE,
        values = dplyr::tribble( # Specify the name of the variable, path to the codelist, search pattern and column to search the code in
            ~name, ~codelist_path, ~source_pattern, ~codevar,
            "eczema", "codelists/ICD-10/eczema/codelist.csv", "extract_clinical", "V1",
            "asthma", "codelists/ICD-10/asthma/codelist.csv", "extract_clinical", "V1",
            "fractures", "codelists/ICD-10/fractures/codelist.csv", "extract_clinical", "V1",
            "lymphoma", "codelists/ICD-10/lymphoma/codelist.csv", "extract_clinical", "V1",
            "prednisolone", "codelists/SNOMED/prednisolone/codelist.csv", "extract_therapy", "SNOMED Code",
            # To add a new entry, specify the name the variable should have, the path to the codelist, the file to be searched in (as a search pattern) and the column to be searched in
        ),
        names = name,
        tar_target(source_files, dir(source_path, pattern = source_pattern, full.names = TRUE)), # Provides file paths to search in
        tar_file(path_codelist, codelist_path), # Specifies and tracks the codelist file
        tar_target(codelist, sort(import(path_codelist)[[codevar]])), # Loads the codes from the codelist file
        tar_target(eventdata, import(source_files) %>% filter(eval(sym(codevar)) %in% codelist), # Makes the eventdata
                   pattern = map(source_files)), # Iterates over all the file paths provided
        tar_parquet(parquet, write_parquet(eventdata, paste0("eventdata/", paste0("eventdata_", name, ".parquet")))) # Writes the file in parquet format (tar_parquet is shorthand for tar_target(format="parquet"))
    )

combined <- tar_combine(
    combined_eventdata,
    eventdata[[4]],
    command = list(!!!.x))


# List of target objects.
study <- list(
	
	# Specifications -------------------------------------------------------------
	
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
	
	
	#Specify analyses
	tar_target(
		analysis,
		head(n=99, #TEMPORARILY TURN OFF SENSITIVITY ANALYSES TO SPEED UP RUNTIME
		c("main", #Main analysis
			"sens_all_follow_up", #Sensitivity analysis: Follow up not limited to 1 year
			"sens_shorter_windows", #Sensitivity analysis: Use shorter windows to define exposure
			"sens_all_ped_imputed", #Sensitivity analysis: Impute all values for PED
			"sens_no_asthma", #Sensitivity analysis: Exclude people who ever have a record for asthma
			"sens_all_ages") #Sensitivity analysis: don't restrict to only those aged 66+ when they cross the risk threshold for the first time
		)
	),
	
	# Specify models for regression
	tar_target(
	    model, 
	    c("crude" = "",
	      "adjusted_sex" = "+ sex",
	      "adjusted_sex_bmi" = "+ sex + bmi_cat"
	    )
	),
	

    # File paths --------------------------------------------------------------

	tar_file(path_main_cohort, "dummy_data/dummy_main_cohort.csv"),
	
	# Data management ------------------------------------------------------------
	tar_target(
		main_cohort, 
		read_csv(path_main_cohort)
		),
	tar_target(
		cohort_eczema, 
		create_cohort_eczema(main_cohort, combined_eventdata),
	),
	tar_target(
	    cohort_post_exclusion,
	    create_cohort_post_exclusions(cohort_eczema, exclusion),
	    pattern = map(exclusion)
	),
	
	
	
	# Analysis -------------------------------------------------------------------
	tar_target(
		results_regression, 
		analysis_regression(cohort_post_exclusion, exposure, outcome, model, exclusion),
		pattern = cross(cohort_post_exclusion, exposure, outcome, model)
		),
	

    # Report ------------------------------------------------------------------

	# Report investigating how to read the raw entity data
	tar_target(
	    results_fractures,
	    command = {
	        workflowr::wflow_build(here("analysis", "results_fractures.Rmd"))
	        fs::path_rel(here("analysis", "results_fractures.Rmd"))
	    },
	    format = "file"
	),
	tar_target(
	    results_lymphoma,
	    command = {
	        # Scan for targets of tar_read() and tar_load()
	        !!tar_knitr_deps_expr(here("analysis", "results_lymphoma.Rmd"))
	        # Explicitly mention any functions used from R/functions.R
	        # list(
	        #     raw_entity_data_read
	        # )
	        
	        # Build the report
	        workflowr::wflow_build(
	            here("analysis", "results_lymphoma.Rmd")
	        )
	        
	        # Track the input Rmd file (and not the rendered HTML file).
	        # Make the path relative to keep the project portable.
	        fs::path_rel(here("analysis", "results_lymphoma.Rmd"))
	    },
	    # Track the files returned by the command
	    format = "file"
	)
	
	
	
	
	
	
)

list(eventdata, combined, study)
