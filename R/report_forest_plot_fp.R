#' Make Forest plot (with forest plot package)
#'
#' @param results_regression A dataframe of results from Cox regression.
#' @param results_rates A dataframe containing rates, person-years and number of fractures.
#'
#' @return A forest plot
#' @export
#'
#' @examples
report_forest_plot_fp <- function(results_regression, results_rates) {

# With the forestplot package ---------------------------------------------

# Create dataframe for each model -----------------------------------------

fpdata <- map2(c("patterncontinuous", "patterncontinuous", "patterncontinuous"),
		 c("crude", "adjusted_age_sex_carstairs", "adjusted_comorb"),
	~left_join(results_regression, results_rates, by = c("term", "outcome", "analysis")) %>%
	filter(term==.x & model==.y & analysis=="main" & outcome %in% c("bisphosphonate", "fract_composite", "fract_any")) %>% 
	select(outcome, estimate, conf.low, conf.high, event, pyears, pyears_unexposed) %>% 
	mutate(txthr=paste0(sprintf("%.2f", estimate), " (", sprintf("%.2f", conf.low), " - ", sprintf("%.2f", conf.high), ")"),
				 txtevents=paste(format(round(event), big.mark=","), sep=""),
				 txtpyears=paste(format(round(pyears), big.mark = ","), sep=""),
				 txtpyears_unexposed=paste(format(round(pyears_unexposed), big.mark = ","), sep=""),
				 txtratio=paste(format(round(pyears_unexposed), big.mark = ","), sep="")))


# Create tabletext --------------------------------------------------------

rownames <- c("Bisphosphonate",
							"Major \nosteoporotic \nfracture",
							"Any fracture")


txthr_all <- paste(ifelse(!is.na(fpdata[[1]]$estimate), fpdata[[1]]$txthr, ""),  
                      ifelse(!is.na(fpdata[[2]]$estimate), fpdata[[2]]$txthr, ""),
                      ifelse(!is.na(fpdata[[3]]$estimate), fpdata[[3]]$txthr, ""),
                      #ifelse(!is.na(fpdata[[4]]$estimate), fpdata[[4]]$txthr, ""),
                      sep="\n")


txtevents_all <- paste(fpdata[[1]]$txtevents,
													 fpdata[[2]]$txtevents,
													 fpdata[[3]]$txtevents,
													 #fpdata[[4]]$txtevents,
                        sep="\n")


txtpyears_all <- paste(fpdata[[1]]$txtpyears,
													 fpdata[[2]]$txtpyears,
													 fpdata[[3]]$txtpyears,
													 #fpdata[[4]]$txtpyears,
                       sep="\n")


txtpyears_unexposed_all <- paste(fpdata[[1]]$txtpyears_unexposed,
																		 fpdata[[2]]$txtpyears_unexposed,
																		 fpdata[[3]]$txtpyears_unexposed,
																		 #fpdata[[4]]$txtpyears_unexposed,
                       sep="\n")


tabletext <- cbind(c("Outcome", rownames), 
                   c("HR (95% CI)", txthr_all),
                   c("n outcomes \n in those with \n continous use", txtevents_all),
                   c("person years \n in those with \n continous use", txtpyears_all),
                   c("person years \n in those with \n intermittent use", txtpyears_unexposed_all))




# Plot --------------------------------------------------------------------

#png("output/forestplot.png", width=11, height=6, units="in", res=400)




forestplot(tabletext, 
           mean = rbind(NA, cbind(fpdata[[1]][, "estimate"], fpdata[[2]][, "estimate"], fpdata[[3]][, "estimate"])), # include point estimates as the coeffs for all models
           lower = rbind(NA, cbind(fpdata[[1]][, "conf.low"], fpdata[[2]][, "conf.low"], fpdata[[3]][, "conf.low"])), # lower CI
           upper = rbind(NA, cbind(fpdata[[1]][, "conf.high"], fpdata[[2]][, "conf.high"], fpdata[[3]][, "conf.high"])), # upper CI
           col=fpColors(zero="#707070", box=c("#00AEC7", "#FFB81C", "#FE5000")), # makes colours for models different shades light grey to black
           fn.ci_norm=c("fpDrawCircleCI",  "fpDrawDiamondCI","fpDrawCircleCI"),
           #zero=1, # set the zero line at 1
           boxsize = .08,
           #title = "HRs (99% CIs) for the risk of fracture\n comparing those with continuous use\n to those with intermittent use",
           #grid = TRUE,
           #line.margin=.1,
           graphwidth = unit(2, "inches"), 
           #colgap = unit(1, "mm"),
           legend = c("crude",
                      "adjusted for age, sex and deprivation",
                      "adjusted for age, sex, deprivation and comorbidities"),
           legend_args = fpLegend(pos=list(x=.5, y=0.9), # specify position of legend
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")), # specify colour of outline and background
           # txt_gp = fpTxtGp(legend=gpar(cex=.7),
           #                  ticks=gpar(cex=.6),
           #                  xlab=gpar(cex = .9),
           #                  summary=list(gpar(cex=1),
           #                               gpar(cex=0.9), #HR (99% CI)
           #                               gpar(cex=.6),
           #                               gpar(cex=.6)),
           #                  label=list(gpar(cex=.85), #Model names
           #                             gpar(cex=.7),
           #                             gpar(cex=.7),
           #                             gpar(cex=.7))),
           hrzl_lines = list("2" = gpar(lwd=1, col="#e8e8e8", lty=1), 
                             "3" = gpar(lwd=1, col="#e8e8e8", lty=2),
                             "4" = gpar(lwd=1, col="#e8e8e8", lty=2)),
           xlog=TRUE,
           xlab="HR (99% CI) for outcome comparing those with continuous use to those with intermittent use",
           #clip =c(0.9, 2.0), # clip axis
           #xticks = c(0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0), # specified positions for xticks
           #align = c("r", "c", "c"), # Use "l", "c", or "r" for left, center, or right aligned
           #is.summary=c(TRUE,rep(FALSE,13)), # vector with logical values representing if value is a summary val (will have diff font style)
           graph.pos=2)

#Save as a recallable object (forestplot plots can't be recalled like ggplots, see: https://stackoverflow.com/questions/65364172/how-to-store-forest-plot-in-an-object-to-be-recalled-in-r)
#forest_plot_fp <- grid::grid.grab()

# save plot
#dev.off()

#forest_plot_fp
}


