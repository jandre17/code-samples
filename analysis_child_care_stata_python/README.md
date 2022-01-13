# Washington, DC Child Care Analysis

This code sample is derived from a project I completed for a Geographic Information Systems (GIS) course. Geospatial analyses are not included in this sample.

Tools: 
* Stata (processing, analysis, and visualization)
* Python (data retrieval)

## Background

The United States has an inadequate supply of licensed child care capacity, and available care options are often unaffordable. 
The lack of supply and high cost of child care leave parents with difficult decisions regarding finances, child care arrangements, and work hours 
Further, there are disparities in access to affordable and reliable child care by race and socioeconomic status, with low-income families and families 
of color less likely to be able to access and afford licensed child care. 
This crisis has been exacerbated by the COVID-19 pandemic, with many child care centers unable to financially withstand ongoing closures.

Sources: 
[Center for American Progress](https://www.americanprogress.org/article/costly-unavailable-america-lacks-sufficient-child-care-supply-infants-toddlers/), 
[NPR WAMU 88.5](https://www.npr.org/local/305/2020/01/09/794851835/for-some-d-c-parents-it-s-too-expensive-to-work), 
[Economic Policy Institute](https://www.epi.org/child-care-costs-in-the-united-states/#/DC), 
[Under 3 DC](https://under3dc.org/wp-content/uploads/2020/05/DC-Child-Care-Investments_May2020.pdf)

This code sample explores licensed child care supply in Washington, DC, looking primarily at differences in capacity and center availability by quality tier and Ward.

## Data
- Child Development Centers (Source: [Open Data DC](https://opendata.dc.gov/datasets/DCGIS::child-development-centers/about))

Includes location, quality, and capacity information for all licensed child care centers in Washington, DC (as of June 2019). 
The CSV used for this analysis was downloaded in April 2021. The source data has since been updated. The quality tiers included in this dataset 
represent the previous "Going for the Gold" system utilized by the 
[Office of the State Superintendent of Education in Washington, DC](https://osse.dc.gov/page/capital-quality-qris) to assign quality ratings. 
This system has since been replaced with the new "Capital Quality" system.

- Child Population by Ward (Source: [US Census Bureau American Community Survey 2019 5-Year Data](https://www.census.gov/data/developers/data-sets/acs-5year.html))

Population under age 5 by Ward was collected via the Census ACS API tool. 
See [get_acs_api_data.py](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/get_acs_api_data.py).

## Analysis & Primary Findings

See Stata .do file [dc_child_care_analysis.do](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/dc_child_care_analysis.do) 
for all data exploration, processing, and analysis. This page includes only a brief summary of primary findings and takeaways.

In these analyses, capacity includes licensed capacity for children under age 5 only. Centers serving only school-age children are excluded.

### Analysis 1: Capacity by Tier

Licensed child care capacity tends to vary widely across centers. The distributions of capacity by quality tier (see below box plots) show that gold centers 
tend to have higher licensed capacity than silver and bronze centers. The distribution of capacity for private centers (which do not provide subsidized care and are not assigned 
a quality tier rating) is about as wide as the distribution for gold centers, but with a lower median (similar to the median capacity for silver centers). 
Chart labels include center counts for each quality tier, showing a small number of silver centers.

![image](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/charts/box_cap_tier.jpg?raw=true)

The statistical significance of average capacity differences across tiers (and relative to the overall total) can be tested using a simple regression. The below bar chart plots 
the average licensed capacity for each tier, including error bars indicating the 95% confidence interval. The wide error bar for the silver tier reflects the small sample size. 
When error bars do not overlap, we can conclude that there is a statistically significant difference in average capacity. For example, gold centers, on average, have 
significantly higher capacity for children under age 5, and bronze centers have significantly lower capacity.

![image](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/charts/avg_cap_by_tier.jpg?raw=true)

### Analysis 2: Quality by Ward

There are differences in quality of available care by Ward. The first chart below shows the proportion of total capacity in each Ward (and for DC overall) 
that is rated in each quality tier. The second chart below shows the proportion of total centers in each Ward (and for DC overall) that are rated in each quality tier. 

The general takeaways from each chart are similar. Wards 2 and 3, which contain many of the highest-income areas in Washington, DC, have high levels of private child care 
capacity and centers. Wards 7 and 8, which cover many lower-income areas, have far fewer private child care centers, but do have proportionally high gold capacity.

![image](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/charts/prop_tier_ward_capacity.jpg?raw=true)

![image](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/charts/prop_tier_ward_count.jpg?raw=true)

### Analysis 3: Ratio of Care Capacity to Child Population

In this analysis, the "care capacity ratio" is the ratio of licensed child care capacity for children under age 5 to the population of children under age 5. The below chart 
plots this ratio by Ward (and for DC overall). 

It is immediately apparent that the ratio for Ward 2 is exceptionally high. This likely reflects the fact that Ward 2 contains 
the central business area of Washington, DC. It is likely that there is a disproportionate amount of care capacity in this area, relative to the population of children, 
because parents may choose to bring their child to a care center that is close to where they work, rather than where they live.

The care capacity ratio for DC overall is 0.429. This means that there is enough licensed capacity for about 43% of children under age 5 who live in Washington, DC. 
Notably, this ratio is lower (about 29-32%) for those lower-income Wards 7 and 8. This analysis does not capture the availability or utilization of informal child care 
arrangements (e.g., with family or community members).

![image](https://github.com/jandre17/code-samples/blob/main/analysis_child_care_stata_python/charts/ccratio_by_ward.jpg?raw=true)

