# stardogR

This package contains a number of functions that call the Stardog HTTP API. [Stardog](https://www.stardog.com/) is an enterprise level knowledge graph database system. `stardogR` does not implement the full API, only the functionality most likely to be of interest to a data analyst or data scientist. Advanced administrative functions are not included in this version.

`stardogR` includes `starmap`, a function that creates a basic mapping file (SMS2 format) and simple ontology from an R data.frame. Relationships between the variables in the data.frame are expressed using syntax based on the R formula language. Together with function `add_dataframe`, the user can map data from an R data.frame and push it to Stardog within the same R work session.

This package includes two vignettes:

1. *Data mapping with Starmap*, a tutorial on the use of Starmap
2. *Using StardogR*, a tutorial on the use of `stardogR`. 


