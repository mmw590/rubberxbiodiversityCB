# rubberxbiodiversityCB
This repository contains the R code used to perform the analysis in the following paper (in submission):  

Wang M. M. H., L. R. Carrasco, D. P. Edwards. 2020. Reconciling rubber expansion with biodiversity conservation. *In submission.*


## About

A static version of the code and input data can be accessed at [this link](https://doi.org/10.5281/zenodo.3886851). The development version of this repository is available at [github.com/mmw590/rubberxbiodiversityCB](https://github.com/mmw590/rubberxbiodiversityCB).  

I wrote the scripts to be run in the specified order, but feel free to use what you need. I have tried to make the filenames informative of their content.  

The scenario simulations (Scripts `12.0-country_scenarios_fxn.R` through `13.9-scenarios_hpc_vulnA_scen3c.R`) were run on the HPC at the University of Sneffied (ShARC). I have included an example of the accompanying bash script `hpc_bash_example_1000reps.sh` that was used to submit the R scripts as jobs to the HPC. Please modify as required for your systems. It is possible to run the code on your local computer given sufficient RAM, but an alternative would be to run a single rep of the simulations, which will enable you to get output which should be close enough to the output averaged over 1000 reps. I have included examples of how to do this in the script `11-scenarios_prep_country.R`  


## Note
The analysis largely followed the methodology of a previously published study by Strona et al. (2018), who provided their data and scripts at [https://doi.org/10.5281/zenodo.1200339](https://doi.org/10.5281/zenodo.1200339). A development version of their respository is also available at [github.com/giovannistrona/oil-palm-primates](https://github.com/giovannistrona/oil-palm-primates). (Primary language: Python)  

Citation:
**Strona G., S. D. Stringer, G. Vieilledent, Z. Szantoi, J. Garcia-Ulloa, S. Wich.** 2018. Small room for compromise between oil palm cultivation and primate conservation in Africa. _Proceedings of the National Academy of Sciences PNAS_. **115**(35): 8811-8816. 
[DOI:10.1073/pnas.1804775115](https://doi.org/10.1073/pnas.1804775115).  
  



## Figure
A high-resolution version of Figure 2: 
![](output/results/fig2_highres.png)

**Figure 2.** Spatial overlay of rubber bioclimatic suitability with extinction vulnerability. Map of rubber bioclimatic suitability and extinction vulnerability along two colour scales, divided into four classes for rubber suitability and five classes for extinction vulnerability for plotting. The main plots show extinction vulnerability scores aggregated for threatened amphibians, birds, mammals and reptiles in Africa (A) and Asia and New Guinea (B). The subplots show extinction vulnerability scores for each taxonomic class separately. Protected areas are shaded in dark grey in the main plots.  
  



## Key Resources Table (where to download data)
[Key Resources Table (in pdf)](output/KEY-RESOURCES-TABLE.pdf)  
Some datasets are not available for redistribution due to licensing restrictions, but can be downloaded from the original sources as listed in the Key Resources Table.  


  




### Animal icons
I made the animal icons used in Figure 2, and they are freely available for download/use/share etc. No need to credit me (the reason I made them was to avoid licensing issues), but please don't claim that you made them, thank you.  
![](images/170px_frog_icon.png)
![](images/170px_bird_icon.jpg)
![](images/170px_elephant_icon.png)
![](images/170px_snake_icon.png)  
(They're not that great zoomed in, I'll admit :P )
