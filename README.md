# Collecting homicide data for Colombia (2003-2023)

## Purpose of this Repository
The Colombian National Police publishes yearly a compilation of all events of homicide in the country with information about location, weapon used and gender of the victim. However, these datasets have not been consistent for the last 20 years. Some of the ways they write the municipalities change, they use different words for variables, they add or remove variables, among other differences. This repository´s main goal is to import all of this data, clean it and produce a single big dataset that contains all the homicides in the country during the last 20 years. As it can be observed, we have obtained finally a dataset with more than 300,000 observations that can be easily manipulated and merged with other datasets produced in Colombia. I hope this can be an useful source for me and for other researchers using public data on violence in Colombia.

## List of Files
- "Homicides_Colombia.Rmd": This is the main document of the repository, as it contains all the code using to import and clean the data, as well as some examples on how to use the data and merge it with public available information.
- "homicides_2003_2023.csv": This is the final dataset with one observation per homicide during the last 20 years.
- "department_year.csv": This is a derived dataset aggrupating homicides by department-year.
- "municipality_year.csv": This is a derived dataset aggrupating homicides by municipality-year.
- "raw_data": this is a folder containing all the original data used.

## Required Packages
- readxl
- stringr
- ggridges
- estimatr
- modelsummary

## Additional Information
One of the datasets used (competition) has been built by me and a co-author (Luis Fernando Trejos Rosero) for a different project.