<h1 align="center">

PYFOREST

</h1>

<h2 align="center">

Informing Forest Conservation Regulations in Paraguay

</h2>

<h2 align="center">

<img src="https://github.com/cp-PYFOREST/Land-Use-Plan-Simulation/blob/main/img/pyforest_hex_sticker.png" alt="Banner" width="200">

</h2>

# [PYFOREST-Shiny](https://reedalexandria.shinyapps.io/pyforest-dashboard/) 

<h2 align="center">

[Land-Use-Assesment](https://github.com/cp-PYFOREST/Land-Use-Assessment) | [Land-Use-Plan-Simulation](https://github.com/cp-PYFOREST/Land-Use-Plan-Simulation) | [PYFOREST-ML](https://github.com/cp-PYFOREST/PYFOREST-ML) | [PYFOREST-Shiny](https://github.com/cp-PYFOREST/PYFOREST-Shiny)

</h2>

<h2 align="center">

<img src="https://github.com/cp-PYFOREST/.github/blob/main/img/shiny-home.png" alt="RShiny Homepage">

</h2>


- To make our results accessible to policymakers and the general public, we have developed an RShiny dashboard that provides an interactive platform for exploring the data and visualizations generated from Objectives 1-3. The dashboard is designed to be user-friendly and intuitive, ensuring that stakeholders with varying technical expertise can easily understand the impacts of forest policies on the Paraguayan Chaco. 

Features
- Visualize historical deforestation rates and land use plan (LUP) compliance statistics. 
- Analyze the impact of current and alternative LUP laws on forest conservation. 
- Examine Predictions of future deforestation patterns under various scenarios. 
- Gain insights to support informed decision-making for sustainable land use and forest conservation policies.

## Table of Contents
- [Getting Started](#getting-started)
- [Usage](#usage)
- [Contributing](#contributing)
- [Authors](#authors)
- [License](#license)

### Getting Started
To run the app locally, follow these steps:

1. Clone the repository: git clone https://github.com/yourusername/PYFOREST-Shiny.git
2. Navigate to the project folder: cd PYFOREST-Shiny 
3. Open the R project file in RStudio: PYFOREST-Shiny.Rproj 
4. Install the required R packages by running the following in the R console: install.packages(c("shiny", "leaflet", "ggplot2", "dplyr", "tidyr", "readr", "sf", "randomForest")) 
5. Run the app in RStudio: shiny::runApp()

### Usage

The **Home** page of the RShiny dashboard is designed to provide the user with all the necessary information about the dashboard's purpose, functionality, and features. The home page also contains sections dedicated to general and tab information. These sections cover essential aspects such as the data source, project information, and further assistance or guidance to help navigate and make the most out of the various tabs within the dashboard.

# Land Use Plan Assessment
</h2>

<h2 align="center">

<img src="https://github.com/cp-PYFOREST/.github/blob/main/img/shiny-assesment.png" alt="Land Use Assessment">

</h2>

The **Land Use Plan Assessment** page includes two subsections. The first sub-section enables users to access information based on political boundaries, while the second sub-section allows information retrieval by property ID. Within the political boundary section, users can explore maps, plots, and data specific to each department and district. They have the option to choose between unauthorized or authorized deforestation tabs and select different years to analyze land use patterns over time. In the property ID section, users have access to a map displaying all properties included in the assessment. Users can easily identify the compliance status of each property and view the amount of unauthorized deforestation associated with them. Additionally, a data table is available for searching and exploring this information.

# Deforestation and Forest Cover Statistics

</h2>

<h2 align="center">

<img src="https://github.com/cp-PYFOREST/.github/blob/main/img/shiny-deforestation.png" alt="Deforestation Tab">

</h2>


The **Deforestation and Forest Cover Statistics** page has two sub-sections: one for deforestation statistics and the other for forest cover statistics. These sections offer visual aids like maps and plots to understand the spatial distribution of deforestation and forest cover in the study area. Users can also visualize the data based on political boundaries and explore different years for temporal analysis.

# Simulations and Prediction Comparisons

</h2>

<h2 align="center">

<img src="https://github.com/cp-PYFOREST/.github/blob/main/img/shiny-ml.png" alt="Simulations and Predictions Tab">

</h2>

The **Simulations and Prediction Comparisons** page offers users the ability to compare results between Objectives 2 and 3. The user can compare statistics of total forest conserved per land use type from the LUP simulations and deforestation predictions. The user can also explore a map of pixel-wise probability of deforestation based on LUPs generated in the land use simulations.

### Contributing
If you would like to contribute to the project, please follow these steps:

Fork the repository.
Create a new branch for your feature or bugfix.
Commit your changes to the new branch.
Submit a pull request to merge your changes into the main branch.

### Authors
[Atahualpa Ayala](Atahualpa-Ayala),  [Dalila Lara](https://github.com/dalilalara),  [Alexandria Reed](https://github.com/reedalexandria),  [Guillermo Romero](https://github.com/romero61)
Any advise for common problems or issues.

### License
This project is licensed under the Apache-2.0 License - see the LICENSE.md file for detail
