## ----include=FALSE------------------------------------------------------------
library(targets)
library(tidyverse)
library(dagitty)
library(ggdag)


## ----Define all DAGs, include=FALSE-------------------------------------------
DAGs <- list(
        confounder_adjusted_simple = dagitty('dag {
bb="0,0,1,1"
Confounder [pos="0.500,0.200"]
Eczema [exposure,pos="0.100,0.500"]
Outcome [outcome,pos="0.900,0.500"]


Confounder -> Eczema
Confounder -> Outcome
Eczema -> Outcome
}'),
        
        
        confounder_adjusted_additional = dagitty('dag {
bb="0,0,1,1"
Confounder1 [pos="0.400,0.200"]
Confounder2 [pos="0.600,0.200"]
Eczema [exposure,pos="0.100,0.500"]
Outcome [outcome,pos="0.900,0.500"]


Confounder1 -> Eczema
Confounder1 -> Outcome
Confounder2 -> Eczema
Confounder2 -> Outcome
Eczema -> Outcome
}'),
        
        
        mediator_adjusted = dagitty('dag {
bb="0,0,1,1"
Confounder [pos="0.500,0.200"]
Mediator [pos="0.500,0.500"]
Eczema [exposure,pos="0.100,0.500"]
Outcome [outcome,pos="0.900,0.500"]


Confounder -> Eczema
Confounder -> Outcome
Eczema -> Mediator -> Outcome
}')
    )   


## -----------------------------------------------------------------------------
map(DAGs, plot)

