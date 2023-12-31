# Using Wind Direction Interpolation Methods to Improve Pesticide Exposure Estimates
## 1. Introduction
<p align="justify">
Pesticide exposure poses a notable public health concern with potential negative effects ranging from acute toxicity to neurodegenerative diseases. Therefore, assessment of pesticide exposures are critical to identify areas of high risk. Among the various factors that influence estimates of exposure, meteorological conditions are considered to play a significant role. Here, the common approach to account for these elements is to incorporate information from the nearest weather station. However, this approach may inaccurately represent the meteorological conditions at the pesticide application area. As a result, exposure estimates might be under or overestimated. Therefore, this research aimed to evaluate and compare spatial interpolation methods that may improve pesticide exposure estimates using wind direction records from the Netherlands in 2017. 
</p>

## 2. Methods
### 2.1 Data Extraction
<p align="justify">
In this study, a total of 403,392 meteorological observations were obtained from the Royal Netherlands Meteorological Institute (KNMI). In this context, data included hourly wind speeds (in m/s) and directions (in degrees), as measured by 47 climatic stations spread across the Netherlands in 2017. Moreover, information about fields used for agricultural purposes were retrieved from the national geo-register. These parcels served as the primary source of pesticide spray drift. Finally, administrative boundaries of the country were obtained from the Central Bureau of Statistics. This facilitated the creation of regular grids (1x1km) covering the area of interest, leading to a surface suitable for interpolating wind fields. 
</p>

### 2.2 Data Preparation and Enrichment
<p align="justify">
To supplement and ensure consistency of the extracted information, data preparation and enrichment procedures were performed, as illustrated in Figure 1. Both stations and climatic records were combined into a single dataset to ease further analysis. Inspection of this data revelaed some missing values and outliers. To address and resolve these types of records, exclusion criteria were being in receipt. First, climatic stations with partial or complete absence of wind field observations were excluded from the analysis, ensuring consistent data coverage across the study area. Moreover, sensor failures were the primary source of absence, indicating that records were missing completely at random. As a result, inferences obtained from the remaining data were considered still valid. 
</p>

<p align="justify">
Second, exclusion criteria were being in place for measures representing calm or highly changeable wind directions. The main rationale behind this exclusion was that these conditions were not considered significant or accurate contributes to simulate pesticide drift given the setup of the gaussian plume models, i.e. exposures are estimated based on the assumption that sources of drift are upwind, making it challenging to simulate dispersion in stagnant or unstable wind conditions. Furthermore, centroids for each agricultural field were enumerated to reduce computational complexity while still capitalising on the implicit spatial distribution of parcels in the Netherlands. Finally, administrative boundaries were used to obtain regular grids covering the area of interest at a resolution of 1 km, providing a balance between capturing relative fine-grain variations in wind fields while maintaining computational efficiency. 
</p>

![Methodological_Procedure](/Thesis/Figures/Methodological_Procedures.png)
Figure 1. Schematic overview of methodological procedures conducted. 

### 2.3 Spatial Interpolation Methods
<p align="justify">
Four spatial interpolation methods were adopted to estimate wind fields at unknown locations, namely a naïve interpolator, nearest neighbours, inverse distance weighting, universal kriging and random forests. Here, the primary objective was to obtain models capable to accurately predict wind directions at unobserved sites based on available samples. In order to compare and evaluate methods suitable to provide reliable interpolations of wind fields, a measure of performance was adopted that considers the directional nature of the data, the circular root-mean-squared error (CRMSE). 
</p>

<p align="justify">
Moreover, spatial k-fold cross validation was used to examine and appraise the performance and suitability of the proposed interpolation models. Here, observations were partitioned into k disjoint folds by performing K-means clustering on the spatial coordinates, acknowledging the spatial dependencies present in the data. Finally, a sensitivity analysis was conducted that encompassed wind direction records from a collection of randomly selected hours across 2017. This facilitated a more robust comparison and evaluation of the four spatial interpolation models employed. 
</p>

### 3. Results and Discussion
<p align="justify">
The results showed that the employed spatial interpolation methods applied to wind direction records produced distinct visual patterns. In this context, a visual comparison of interpolations on a randomly selected hour revealed that the inverse distance weighting and universal kriging approaches resulted in a more continuous field while the nearest neighbour method provided abrupt transitions across space. Interpolations obtained from the random forest model exhibited an intermediate behaviour. Conversely, the naïve approach provided a singular estimate across the study area, disregarding the spatial variation of the wind direction records. Overall, these findings align with findings from previous studies. 
</p>

<p align="justify">
Furthermore, the sensitivity analysis revealed that the proposed models performed relatively similar, as presented in Figure 2. This might be explained by the limited variability observed in hourly wind field measures. In this context, all models outperformed the baseline wind field interpolations of the naïve approach. Notably, inverse distance weighting and universal kriging exhibited superior performance compared to the nearest neighbour approach that is commonly used in pesticide dispersion simulations. In contrast, the random forest algorithm was found to perform slightly worse compared to the other interpolators, only outperforming the naïve method. 
</p>

![Model_Comparison](/Thesis/Figures/Sensitivity_Analysis.png)
Figure 1. Mean out-of-sample circular root-mean-squared error of each spatial interpolator across 382 randomly selected hours. 

### 4. Conclusion
<p align="justify">
The results revealed that the adopted spatial interpolation approaches applied to wind direction records produced distinct visual patterns. Nonetheless, the performance among the four models was relatively similar. The main theoretical explanation for this finding was the limited variety observed in hourly wind field measures. Moreover, inverse distance weighting demonstrated the lowest median out-of-sample error for interpolating wind fields among the five models, suggesting that the adoption of this method in pesticide drift simulations might provide a more valid representation of wind directions at the application areas compared to the nearest neighbour approach that is often employed. In doing so, this might improve the accuracy of pesticide exposure estiamtes obtained from these simulations. 
</p>

### Appendix I - Project Organisation

```.
├── README.md          
│
├── Data                        
│   ├── 1. External                             <- Data from third party sources (e.g. KNMI station coordinates).
│   ├── 2. Pre-Processed                        <- Intermediate data that has been transformed.
│   ├── 3. Final                                <- The final, canonical data sets used for interpolation.
│   └── 3. Output                               <- Sensitivity analysis results. 
│   
├── Scripts                                     <- R-Code Scripts. 
│   ├── 1. Data Extraction                      <- Script to download data. 
│   ├── 2. Data Preparation and Enrichment      <- Script to turn raw data into features for modeling.
│   ├── 3. Spatial Interpolation                <- Script to interpolate wind directions. 
│   └── 4. Visualisations                       <- Script to generate graphs and figures used in reporting.
│
├── Poster
│   ├── Poster                                  <- Generated poster as PDF (Available on request).
│   └── References                              <- References related to the poster. 
│
└── Thesis
    ├── Thesis                                  <- Generated report as PDF (Available on request).
    └── Figures                                 <- Generated graphs and figures used in reporting.

