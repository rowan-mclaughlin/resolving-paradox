# resolving-paradox

R code for the paper Marine exploitation and the arrival of farming: resolving the paradox of the Mesolithic-Neolithic transition in Denmark

by T. Rowan McLaughlin, Harry K. Robson, Rikke Maring, Adam Boethius, Eric Guiry, Daniel Groß, Satu Koivisto, Bente Philippsen, Nicky Milner, Geoff Bailey and Oliver E. Craig.


The R code contained in this repository enables the replicaiton of the analysis and modelling described in this paper. 


In addition to the R code here, the DEMRE database will need to be downloaded into the working directory -- see doi.org/10.5281/zenodo.14019080. 


This repository contains the following documents:


SI_Markdown.Rmd -- R code (Markdown format) for replicating the analysis, and supplementary information

SI_Markdown.pdf -- As above, compiled in to a PDF document

ABM.R -- Agent-based model simulation described in section 4.3

Figs.R -- Code to replicate the figures in the paper once the code in SI_Markdown.Rmd has seen executed


The following support files are also needed for some of the code to run correctly:

Denmark_UTM_buffered_5km.* -- Shapefile of the Danish coastline buffered by 5km

Robson_et_al_OxCal.rData -- Posterior probability distributions of cereamic artefacts (from Robson et al 2021 the https://doi.org/10.1016/j.jasrep.2021.102829)

dk_fish.csv -- .csv version of fish zooarchaeological finds in Denmark, derived from the data available in the DEMRE database

dk_marine_mam.csv -- .csv version of 

dk_smr.csv -- Details of extant shell midden sites in Denmark, taken from the Fund og Fortidsminder (https://www.kulturarv.dk/fundogfortidsminder/) 
