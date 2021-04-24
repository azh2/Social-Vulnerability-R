# SocialVulnR
SOCIAL VULNERABILITY MAPPING IMPLEMENTED IN TIDYCENSUS FOR R

---
Title: Building an Adaptive Capacity Index Using TidyCensus in R<br>
Author: Anthony Holmes - Prepared on Behalf of The Nature Conservancy - Oregon<br>
Copyright: 2020 - The Nature Conservancy<br>
Date: March, 2020<br><br>
---

<b>Introduction</b>

<p>According to the CDC, Social vulnerability refers to the "resilience of communities when confronted by external stresses on human health, stresses such as natural or human-caused disasters, or disease outbreaks." These methods implement an index similar to the one maintained by the CDC which are adapted from methods published by Flanagan et al. (2011) in "A Social Vulnerability Index for Disaster Management," which describes 13 metrics for assesing the 'social vulnerability' or 'adaptive capacity' (Davies et al.2018) of a census tract. This study attempts to replicate these metrics as closely as possible at a block group scale using 5 year ACS data in order to quantify the capacity of a block-group to respond to a given disturbance, minimizing risks to health, safety, property and other essential services. This index could be used to identify areas and communities at an increased risk and target appropriate response, recovery and mitigation efforts.

The SVI/ACI is <b>just one</b> component of a vulnerability assesment which would  include assesing the impact of the hazard itself (disease, fire, flood hurricane, etc.), the vulnerability of the physical infrastructure, and community assets/resources that could diminsish the impact of a given hazard and/or aid in recovery efforts in a given community (Flanagan et al. 2011).

The index comprises 4 domains including:

> <b>Socioeconomic Status</b>

    Percent of Persons Below Poverty Level
    Percent of Persons (age 16+) Unemployed
    Per Capita Income

> <b>Language & Education</b>

    Percent of Persons With No 12th Grade Education
    Percent of Persons Do Not Who Speak English

> <b>Demographics</b>

    Percent of Persons 65 Years of Age or Older
    Percent of Persons 17 Years of Age or Younger
    Percent of Persons 5 Years of Age or Older With a Disability
    Percent of Single Parent Households

> <b>Housing & Transportation</b>

    Percent of Persons Living in Multi-Unit Structure
    Percent of Persons Living in a Mobile Home
    Percent of Persons Living in ‘Crowded’ Conditions - more than one person per room
    Percent of Household With No Vehicle Available
    Percent of Persons Residing in Group Quarters

The index is constructed according to Flanagan et al. (2011) by assigning a percentile rank to each of the above named variables, ranked from highest to lowest (excluding per capita income which is ranked from lowest to highest). To calculate the final SVI, the sum for each of the previously calculated percentile ranks is taken for a given blockgroup and the percentile rank of these sums across a given geographic area results in the final SVI score, accordingly, the geographic area and enumeration units used in this equation will effect the final SVI derived for a given area so it is advised that the user consider these factors carefully.

Additional variables were brought in based on a review of the existing literature, however, these were not included in the index in this script since we were attempting to implement the existing literature of Flanagan et al. as closely as possible, however, the literature cited here supports their application in future iterations of the SVI index and their contribution to social vulnerability is worthy of further review.

<b>Works Cited</b>

>Davies, Ian P., Haugo, Ryan D., Robertson, James C., & Levin, Phillip S. (2018). The unequal vulnerability of communities of color to wildfire. PLoS ONE, 13(11), E0205825.

>Flanagan BE, Gregory EW, Hallisey EJ, Heitgerd JL, Lewis B. A Social Vulnerability Index for Disaster Management. J Homel Secur Emerg Manag. 2011;8. https://doi.org/10.2202/1547-7355.1792

<b>Tidy Census</b>

This script implements the R Package TidyCensus to bring in the necessary variables for this index, this allows the index to be easily reproduced for a vairety of geographies and time periods, many of the variables were found at the block group level and some could not be derived at this scale, there may be alternative variables or indicators available, however, this project only attempts to reproduce the index as published at the finest geographic scale available.

Kyle Walker (2020). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames. R package version 0.9.6. https://CRAN.R-project.org/package=tidycensus

<b>Dependencies</b>
>R-3.6.0 - https://cran.r-project.org/<br>
>R Packages (CRAN): 'knitr','sp','sf','spdep','tidycensus','dplyr','tidyr','RColorBrewer','leaflet','ggplot2'<br>
>Census API Key: Get a Free Census API Key Here - https://api.census.gov/data/key_signup.html<br>

<b>Licensing/Disclaimer:</b>
THIS SOFTWARE HAS NOT BEEN PEER-REVIEWED AND IS SUBJECT TO REVISION. THE AUTHOR NOR THE NATURE CONSERVANCY MAKE ANY WARRANTY AS TO THE CURRENCY, COMPLETENESS, ACCURACY OR UTILITY OF THIS SOFTWARE. THIS SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IT IS STRONGLY RECOMMENDED THAT CAREFUL ATTENTION BE PAID TO THE DOCUMENTATION ASSOCIATED WITH THIS SOFTWARE. THE AUTHOR NOR THE NATURE CONSERVANCY SHALL BE HELD LIABLE FOR IMPROPER OR INCORRECT USE OF THIS SOFTWARE OR ANY DATA PRODUCED HEREIN. ALL PARTIES UTILIZING THIS SOFTWARE MUST BE INFORMED OF THESE RESTRICTIONS. THE NATURE CONSERVANCY AND AUTHOR SHALL BE ACKNOWLEDGED IN ANY REPORTS OR OTHER PRODUCTS DERIVED FROM THIS SOFTWARE. 
