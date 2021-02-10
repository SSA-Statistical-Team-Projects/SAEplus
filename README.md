# SAEplus
This package uses the existing packages in `R` for small area estimation (SAE) such as `EMDI` to create a system of tools that will also carry out 
spatial analysis including injesting available remote sensing data and creating other functional for reporting results. 

## Process Overview
### 1. Construct tile shapefile  
A. Take boundaries of admin-1 shapefile, construct tile grid within boundary, 2.4 km by 2.4 km tiles  
B. Use only populated grids (according to facebook population estimation) 

### 2. Obtain geospatial indicators for each tile 
Building footprint stats from Worldpop 
GEE suite of indicators 
Open streetmap 
Population estimates from Facebook 

### 3. Create sample data 
A. Start with welfare data, merge EA centroids, spatial join with tile shapefile to merge tile ID, merge geospatial indicators 

### 4. Estimate model using sample data 
A. in sample using glmnet relaxed model (lasso) and store selected variables 

### 5. Create synthetic census 
A. Generate estimated tile population 
B. Generate estimate of average household size by representative level of survey (province) 
C. Divide population by average household size to generate number of households 
D. Merge selected variables 
D. Expand the data to include estimated number of households 

### 6. Run EMDI package 
