# gridr

You can install the package directly from GitHub with `devtools`. If you don't have it, you can install it with:

`install.packages("devtools")`

Then you can install the packege with: 

`devtools::install_github("anton-etype/gridr")`

## Usage
```R
library(gridr)
library(sf)

#### INPUT VARIABLES ####
# Here you set the settings for the program

# File path to the file geodatabase, containing stands:
gdbPath <- gridr_example("best.gpkg") #This is an example dataset

# Name of the layer containing the stands:
lyrName <- "best"

# East/west distance (m) between grid points:
xdistGrid <- 20

# North/south distance (m) between grid points:
ydistGrid <- 20

# Displacemant, in relation to the stands minimum bounding box, for the grid points. c(x, y):
displacementXY <- c(-5,-10)

# File path to the geopackage file where the results will be saved:
outPath <- "testdata/results.gpkg"

# Name of the result layer in the geopackage:
outLyrName <- "grid20"

#### END OF INPUT VARIABLES ####

#### THE PROGRAM ####
# Here you (hopefully) don't have to change anything

# Read the GIS-functions from the functions.R-file
source("functions.R")

# Running the createGrid function from the functions.R-file
grid <- createGrid(gdbPath, lyrName, xdistGrid, ydistGrid, displacementXY)

# Selecting one gridpoint at random for each stand and ranking the the closest points to it.
subsample <- getSubsample(grid)

# Saving to geopackage. 
st_write(subsample, outPath, outLyrName, delete_layer = TRUE)

```
