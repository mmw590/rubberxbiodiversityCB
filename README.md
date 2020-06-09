# rubberxbiodiversityCB
This repository contains the R code used to perform the analysis in the following paper (in submission):  

Wang M. M. H., L. R. Carrasco, D. P. Edwards. 2020. Reconciling rubber expansion with biodiversity conservation. *In submission.*


## About

A static version of the code and input data can be accessed at [link](https://doi.org/10.5281/zenodo.1200255). The development version of this repository is available at [github.com/mmw590/rubberxbiodiversityCB](https://github.com/mmw590/rubberxbiodiversityCB).  

Please refer to [Key Resources Table][tbl] (also in Methods of the article) for where to download non-redistributable data.  

I wrote the scripts to be run in the specified order, but feel free to use what you need. I have tried to make the filenames informative of their content.  

The scenario simulations (Scripts `12.0-country_scenarios_fxn.R` through `13.9-scenarios_hpc_vulnA_scen3c.R`) were run on the HPC at the University of Sneffied (ShARC). I have also included an example of the accompanying bash script that was used to submit the R scripts as jobs to the HPC. Please modify as required for your systems. It is possible to run the code on your local computer given sufficient RAM, but an alternative would be to run a single rep of the simulations, which will enable you to get output which should be close enough to the output averaged over 1000 reps. I have included examples of these in scripts `10-scenarios_prep.R` and `11-scenarios_prep_country.R`  


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
Apologies that I didn't have time to figure out redistribution rights so I am not currently able to provide all the input data I used directly...but here's where you can get the data...  
  



## sessionInfo()

'''
> sessionInfo()
R version 4.0.0 (2020-04-24)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17134)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] classInt_0.4-3    magick_2.3        scales_1.1.1      data.table_1.12.8 lwgeom_0.2-4     
 [6] renv_0.10.0       cowplot_1.0.0     ggplot2_3.3.1     fasterize_1.0.2   sf_0.9-3         
[11] raster_3.1-5      sp_1.4-2          tidyr_1.1.0       dplyr_1.0.0      

loaded via a namespace (and not attached):
 [1] xfun_0.14          tidyselect_1.1.0   purrr_0.3.4        lattice_0.20-41    colorspace_1.4-1  
 [6] vctrs_0.3.0        generics_0.0.2     testthat_2.3.2     htmltools_0.4.0    utf8_1.1.4        
[11] rlang_0.4.6        e1071_1.7-3        pillar_1.4.4       glue_1.4.1         withr_2.2.0       
[16] DBI_1.1.0          lifecycle_0.2.0    munsell_0.5.0      gtable_0.3.0       codetools_0.2-16  
[21] evaluate_0.14      knitr_1.28         labeling_0.3       parallel_4.0.0     class_7.3-17      
[26] fansi_0.4.1        Rcpp_1.0.4.6       KernSmooth_2.23-17 backports_1.1.7    desc_1.2.0        
[31] pkgload_1.1.0      farver_2.0.3       digest_0.6.25      grid_4.0.0         rprojroot_1.3-2   
[36] rgdal_1.5-8        cli_2.0.2          tools_4.0.0        magrittr_1.5       tibble_3.0.1      
[41] crayon_1.3.4       pkgconfig_2.0.3    ellipsis_0.3.1     assertthat_0.2.1   rmarkdown_2.2     
[46] rstudioapi_0.11    R6_2.4.1           units_0.6-6        compiler_4.0.0  '''


### Animal icons
I made the animal icons used in Figure 2, and they are freely available for download/use/share etc. No need to credit me (the reason I made them was to avoid licensing issues), but please don't claim that you made them, thank you.  
![](images/170px_frog_icon.png)
![](images/170px_bird_icon.jpg)
![](images/170px_elephant_icon.png)
![](images/170px_snake_icon.png)  
(They're not that great zoomed in, I'll admit :P )
