# Background Point Generator

MoveApps

Github repository: https://github.com/nilanjanchatterjee/Background_point_generator

## Description

This app allows users to generate background points that can the be passed to MoveApps' resource selection function (RSF) App. For RSFs, *observed* points are the locations where the animal(s) were detected and *background* points are randomly generated locations located within a given extent.


## Documentation
  
This app generated the background points based on input from the user. Namely, whether to generate the points per individual or population, the extent of the area in which the background points should be generated, and how many background points to generate per observed location.

### Input data

move2 in Movebank format

### Output data

move2 in Movebank format

### Artefacts

A plot of the background and oberserved locations. If the background points are generated at the individual scale, then the plot shows one pane per individual.

### Settings
**Scale (`scale`):** Specify how you want to perform the resource selection function. *Population* will generate the background point for the whole area used by the individuals and *Individual* will generate background points based on the range of individuals. Default value is *Population*.

**Ratio of used versus background points (`points_ratio`):** The ratio of observed versus background points that you want to use for the resource selection function. Default value is *10*.

**Extent Type (`extent_type`):** Specify where you want to generate background points for the resource selction function. *Bounding Box* will generate the background points for the whole bounding box rectangle area used by the individuals/population and *Minimum Convex Polygon* (MCP) will generate the background points in the MCP used by the individuals/population. Default value is *Bounding Box*.