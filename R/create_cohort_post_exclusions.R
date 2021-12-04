create_cohort_post_exclusions <- function(cohort_eczema, exclusion) {
    
    if (is.na(exclusion)) {
        
        data <- cohort_eczema
        label <- "none"
        
    } else {
        
    data <- cohort_eczema %>% 
        group_by(patid) %>% 
        filter(!any(eval(exclusion) == 1)) %>% 
        ungroup()
    label <- exclusion 
    
    }
    
    set_names(
        list(data, label),
        c("data", "label")
    )
}