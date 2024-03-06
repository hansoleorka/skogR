skogR
=====

Norske skogfunksjoner (Norwegian Forestry Functions)

# Install
```{r install}
library(devtools)
install_github("hansoleorka/skogR")
```

# List of parameters used in package 

#### Tree level parameters
|Parameter     | Description                           | Unit  |
|------------- |-------------                          |-----|
|d             |Diameter at breast height              |cm|
|h             |Tree height of a tree                  |meter    |
|sp            |Species code                           |1=spruce, 2= pine, 3= brich  |


#### Stand level parameters

|Parameter     | Description                           | Unit  |
|------------- |-------------                          |-----|
|HL            |Lorey's Mean heigh                            |   meter |
|Ho            |Topheight                             |   meter |
|G            |Basal area                             |   m2/ha |
|SP            |Species code                          |1=spruce, 2= pine, 3= brich  |
|N2            |Number of stems after thining         |    n |
|T13           |Age                                   | years |
|Dmb           |Diameter below bark                   ||
