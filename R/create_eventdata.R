#' Create eventdata
#' @description Uses a codelist to extract events from a source file
#' @param codelist Path to a tabular data file, containing a column/variable defined in codevar
#' @param codevar A string specifying a variable that must be present in both the codelist and the source file
#' @param source_pattern A string specifying a search pattern
#' @param source_path Path to the folder containing the source files
#' @return A dataframe containing eventdates for clinical codes defined in codelist
#' @example create_eventdata(codelist="codelists/asthma.csv", codevar="medcode", source_pattern="Clinical_extract_ecz_")
create_eventdata <- function(codelist,
											codevar,
											source_pattern,
											source_path = "Z:/GPRD_GOLD/Ali/2021_skinepiextract/parquet/in") {
	
	#Get codes from the codelist file
	codes <- sort(import(codelist)[[codevar]])
	
	#Get all raw file paths
	file_paths <- dir(source_path, pattern = source_pattern, full.names = TRUE)
	
	#Print which files are being read
	cli_alert_info(paste("Reading:\n", paste(dir(source_path, pattern = source_pattern, full.names = FALSE), collapse = "\n")))
	
	#Read and filter each file and return a single dataframe
	purrr::map_dfr(cli_progress_along(file_paths),
								 function(i) {
								 	import(file_paths[i]) %>%
								 		filter(eval(sym(codevar)) %in% codes) %>%
								 		mutate(across(everything(), as.character))
								 })
}
