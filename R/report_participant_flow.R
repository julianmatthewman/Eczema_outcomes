#' Make a participant flow diagram.
#'
#' @param cohort_steroids A lists of lists containing participant counts for different outcomes.
#'
#' @return A participant flow diagram.
#' @export
#'
#' @examples
report_participant_flow <- function(cohort_steroids) {
	library(DiagrammeR)
	library(DiagrammeRsvg)
	library(rsvg)
	library(magrittr)
	

a <<- cohort_steroids[[1]] %$% paste(counts$step[1], "\nn =", format(counts$n[1], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[1]), big.mark = ","))
b <<- cohort_steroids[[1]] %$% paste(counts$step[2], "\nn =", format(counts$n[2], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[2]), big.mark = ","))
c1 <<- cohort_steroids[[1]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[3], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[3]), big.mark = "," ))
d1 <<- cohort_steroids[[1]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[4], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[4]), big.mark = "," ))
e1 <<- cohort_steroids[[1]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[5], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[5]), big.mark = "," ))
c2 <<- cohort_steroids[[1+(length(cohort_steroids)/4)]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[3], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[3]), big.mark = "," ))
d2 <<- cohort_steroids[[1+(length(cohort_steroids)/4)]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[4], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[4]), big.mark = "," ))
e2 <<- cohort_steroids[[1+(length(cohort_steroids)/4)]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[5], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[5]), big.mark = "," ))
c3 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*2]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[3], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[3]), big.mark = "," ))
d3 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*2]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[4], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[4]), big.mark = "," ))
e3 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*2]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[5], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[5]), big.mark = "," ))
c4 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*3]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[3], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[3]), big.mark = "," ))
d4 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*3]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[4], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[4]), big.mark = "," ))
e4 <<- cohort_steroids[[1+(length(cohort_steroids)/4)*3]] %$% paste("Outcome: ", outcome, "\nn =", format(counts$n[5], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[5]), big.mark = "," ))
stepc <<- cohort_steroids[[1]] %$% counts$step[3] %>% str_wrap(60)
stepd <<- cohort_steroids[[1]] %$% counts$step[4]
stepe <<- cohort_steroids[[1]] %$% counts$step[5]
excl1 <<- cohort_steroids[[1]] %$% paste("Never cross risk threshold", "\nn =", format(counts$n[1]-counts$n[2], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[1]-counts$pyrs[2]), big.mark = "," ))
excl2 <<- cohort_steroids[[1]] %$% paste("Exclude people with previous major osteoporotic fracture or bisphosphonate prescription", "\nn =", format(counts$n[2]-counts$n[3], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[2]-counts$pyrs[3]), big.mark = ","))
excl3 <<- cohort_steroids[[1]] %$% paste("Restrict follow up from crossing the risk threshold to a maximum of one year thereafter or the first occurence of the outcome", "\nn =", format(counts$n[3]-counts$n[5], big.mark = ","), "\np-yrs =", format(round(counts$pyrs[3]-counts$pyrs[5]), big.mark = ","))




#The diagram uses the graphViz language
full <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      a [label = '@@1']
      b [label = '@@2']
      
      c1 [label = '@@3']
      d1 [label = '@@4']
      e1 [label = '@@5']
      
      c2 [label = '@@6']
      d2 [label = '@@7']
      e2 [label = '@@8']
      
      c3 [label = '@@9']
      d3 [label = '@@10']
      e3 [label = '@@11']

      c4 [label = '@@12']
      d4 [label = '@@13']
      e4 [label = '@@14']
      
      invis[ shape = point, width = 0 ]
      excl1 [label = '@@18']
      


        subgraph cluster_1 {
        node [style=filled];
        c1  c2  c3  c4;
        label = '@@15';
        color=black}
        
        subgraph cluster_2 {
        node [style=filled];
        d1  d2  d3  d4;
        label = '@@16';
        color=black}
        
        subgraph cluster_3 {
        node [style=filled];
        e1  e2  e3  e4;
        label = '@@17';
        color=black}
        
				
        
				# edge definitions with the node IDs
				a -> invis[ arrowhead = none ]; 
				invis -> b;
				invis -> excl1; { rank = same; invis; excl1 } 
				b -> {c1, c2, c3, c4}
				c1 -> d1 -> e1;
				c2 -> d2 -> e2;
				c3 -> d3 -> e3;
				c4 -> d4 -> e4;
}
      }

      [1]: a
      [2]: b
      [3]: c1
      [4]: d1
      [5]: e1
      [6]: c2
      [7]: d2
      [8]: e2
      [9]: c3
      [10]: d3
      [11]: e3
      [12]: c4
      [13]: d4
      [14]: e4
      [15]: stepc
      [16]: stepd
      [17]: stepe
      [18]: excl1




      
      ")


#The diagram uses the graphViz language
simple <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      a [label = '@@1']
      b [label = '@@2']
      
      c1 [label = '@@3']
      e1 [label = '@@4']
      
      
      invis1[ shape = point, width = 0 ]
      excl1 [label = '@@5']
      
			invis2[ shape = point, width = 0 ]
      excl2 [label = '@@6']
      
			invis3[ shape = point, width = 0 ]
      excl3 [label = '@@7']

        
        
				# edge definitions with the node IDs
				a -> invis1[ arrowhead = none ];
				invis1 -> excl1; { rank = same; invis1; excl1 } 
				invis1 -> b;
				
				b -> invis2[ arrowhead = none ];
				invis2 -> excl2; { rank = same; invis2; excl2 } 
				invis2 -> c1;
				
				c1 -> invis3[ arrowhead = none ];
				invis3 -> excl3; { rank = same; invis3; excl3 } 
				invis3 -> e1;
				

}
      }

      [1]: a
      [2]: b
      [3]: c1
      [4]: e1
      [5]: excl1
      [6]: excl2
      [7]: excl3




      
      ")


# participant_flow %>%
# 	export_svg %>%
# 	charToRaw %>%
# 	rsvg_svg("output/participant_flow.svg")



# This will be saved to the list: -----------------------------------------
set_names(list(full, simple),
					c("full", "simple"))

}
