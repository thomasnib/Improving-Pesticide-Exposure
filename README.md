# Using Wind Direction Interpolation Methods to Improve Pesticide Exposure Estimates
## 1. Introduction
<p align="justify">
Pesticide exposure poses a notable public health concern with potential negative effects ranging from acute toxicity to neurodegenerative diseases. Therefore, assessment of pesticide exposures are critical to identify areas of high risk. Among the various factors that influence estimates of exposure, meteorological conditions are considered to play a significant role. Here, the common approach to account for these elements is to incorporate information from the nearest weather station. However, this approach may inaccurately represent the meteorological conditions at the pesticide application area. As a result, exposure estimates might be under or overestimated. Therefore, this research aimed to examine spatial interpolation methods that may improve pesticide exposure estimates using wind direction records from the Netherlands in 2017. 
</p>

## 2. Methods
### 2.1 Data Extraction
<p align="justify">
In this study, a total of 403,392 meteorological observations were obtained from the Royal Netherlands Meteorological Institute (KNMI). In this context, data included hourly wind speeds (in m/s) and directions (in degrees), as measured by 47 climatic stations spread across the Netherlands in 2017. Moreover, information about fields used for agricultural purposes were retrieved from the national geo-register. These parcels served as the primary source of pesticide spray drift. Finally, administrative boundaries of the country were obtained from the Central Bureau of Statistics. This facilitated the creation of regular grids (1x1km) covering the area of interest, leading to a surface suitable for interpolating wind fields. 
</p>

### 2.2 Data Preparation and Enrichment
<p align="justify">
To supplement and ensure consistency of the extracted information, data preparation and enrichment procedures were performed, as illustrated in Figure 1. Both stations and climatic records were combined into a single dataset to ease further analysis. Inspection of this data revelaed some missing values and outliers. To address and resolve these types of records, exclusion criteria were being in receipt. First, climatic stations with partial or complete absence of wind field observations were excluded from the analysis, ensuring consistent data coverage across the study area. Moreover, sensor failures were the primary source of absence, indicating that records were missing completely at random. As a result, inferences obtained from the remaining data were considered still valid. Second, exclusion criteria were being in place for measures representing calm or highly changeable wind directions. The main rationale behind this exclusion was that these conditions were not considered significant or accurate contributes to simulate pesticide drift given the setup of the gaussian plume models, i.e. exposures are estimated based on the assumption that sources of drift are upwind, making it challenging to simulate dispersion in stagnant or unstable wind conditions. 
</p>

<p align="justify">
Furthermore, centroids for each agricultural field were enumerated to reduce computational complexity while still capitalising on the implicit spatial distribution of parcels in the Netherlands. Finally, administrative boundaries were used to obtain regular grids covering the area of interest at a resolution of 1 km, providing a balance between capturing relative fine-grain variations in wind fields while maintaining computational efficiency. 
</p>

![Methodological_Procedure](../Methodological_Procedure.png)

### 2.3 Spatial Interpolation Methods
<p align="justify">
Four spatial interpolation methods were adopted to estimate wind fields at unknown locations, namely nearest neighbours, inverse distance weighting, universal kriging and random forests. Here, the primary objective was to obtain models capable to accurately predict wind directions at unobserved sites based on available samples. In order to compare and evaluate methods suitable to provide reliable interpolations of wind fields, measures of performance were adopted. Among the various alternatives available, the root-mean-squared error (RMSE) and mean absolute error (MAE) were considered due to their prevalent usage across topical literature and relative ease of interpretation. Moreover, leave-one-out cross validation was used to examine and appraise the performance and suitability of the proposed interpolation methods. 
</p>
