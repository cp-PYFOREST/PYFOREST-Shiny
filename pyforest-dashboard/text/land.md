---
editor_options: 
  markdown: 
    wrap: sentence
---

Land use plan assessment:

-   For each year of the analysis, it is necessary to determine which private property is considered active from the Permitted land use dataset.
    The active_inactive.qmd file performs the preprocessing required to determine the unique identifier of the active properties for each year between 2011 to 2020.
    Each subset of yearly properties provides the unique identifiers required to subset the Land use dataset, as these are the approved LUPs.

-   Each row of an LUP subset is a vector polygon of the approved land use type.
    The analysis done in lup\_{year}-compliance.qmd uses the yearly subsets of LUPs and overlays them with the corresponding 'Forest Loss' dataset to determine the cell count per land use type.
    Each cell of the 'Forest Loss' dataset is a deforested area.

-   Yearly subsets of the Land use dataset contain a categorical column of 'GRUPOS,' identifying the approved land use type.
    The analysis done in each lup\_{year}-compliance.qmd uses the 'GRUPOS' column to filter by the land use types of 'authorized area' and 'forest reserve' ('AREA_AUTORIZADA,' 'BOSQUES').

-   Pixel counts were converted to an area for each property and land use type.
    Pixel counts greater than zero in the area designated as a forest reserve is considered illegal deforestation, placing the property out of compliance with its approved land use plan.
