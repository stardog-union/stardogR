# stardogR

This package contains a number of functions that call the Stardog HTTP API. It does not implement the full API, only the functionality most likely to be of interest to a data analyst or data scientist. Advanced administrative functions are not included in this version.

`stardogR` also includes `starmap` and `meta`. 

`starmap` is a function that creates a basic mapping file (SMS format) and simple ontology from a data.frame. Relationships between the variables in the data.frame are expressed using syntax based on the R formula language. Together with function `add_dataframe`, the user can map data from an R data.frame and push it to Stardog within the same R work session.

`meta` allows to the user to readily create, organize, edit and view named graphs for the purpose of managing and retaining a data science workflow. 

This package includes three vignettes:

1. *Using Starmap*, a tutorial on the use of Starmap
2. *Data science with Stardog and R*, a tutorial on the use of `stardogR`. 
3. *Ontologies and data science*, an introduction to semantic graph reasoning in data science.

