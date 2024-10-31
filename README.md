# Analysis of Property Prices in Middlesbrough

## Author
**Glauco Rampone**  
Department of Statistics, University of Glasgow, UK  
Email: [3059030R@student.gla.ac.uk](mailto:3059030R@student.gla.ac.uk) | [Glauco.Rampone@glasgow.ac.uk](mailto:Glauco.Rampone@glasgow.ac.uk)

## Project Overview
Understanding property price dynamics in the housing market is critical for estate agency companies. This study utilizes a dataset developed by University College London (2021), which integrates transaction records from the Land Registry Price Paid Data (PPD) with energy performance and property characteristics from Domestic Energy Performance Certificates (EPCs). By examining these combined datasets, we aim to understand how specific property features influence prices among a set of 10546 houses in Middlesbrough using statistical modelling. 

## Methodology
- **Data Selection**: The dataset contains records for 10,546 properties in Middlesbrough. Thirteen covariates were identified based on relevance to price, size, location, and energy efficiency.
- **Modeling Approach**: Nine models were evaluated, with the best fit determined to be a Generalized Additive Gamma Model with a log link, yielding an Adjusted R² of 0.826.
- **Significant Variables**:
  - Total Floor Area (p < 2e-16)
  - Number of Rooms (p < 2e-16)
  - Energy Consumption (p = 1.24e-06)
  - Environmental Efficiency Rating (p = 8.244e-06)
  - Property Type (p < 2e-16)
  - Postcode Group (p < 2e-16)
  
- **Interactions**:
  - *Total Floor Area* × *Number of Rooms*
  - *Energy Consumption* × *Environmental Efficiency Rating*

## Key Findings
1. **Impact of Floor Area**: There is a positive correlation between total floor area and property prices, indicating that larger homes tend to be more expensive.
2. **Energy Efficiency**: Homes with lower energy consumption and higher environmental efficiency ratings generally command higher prices, suggesting a premium on energy-efficient properties.
3. **Geographical Trends**: Analysis by postcode shows that the Southeastern districts (Marton, Nunthorpe, Ormesby) have the highest prices, while the town center has lower average prices.
