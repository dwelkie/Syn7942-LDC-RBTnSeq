# Syn7942-LDC-RBTnSeq

[![Shiny](https://github.com/dwelkie/Syn7942-LDC-RBTnSeq/blob/master/R_Shiny_app-logo.png)](https://dwelkie.shinyapps.io/S7942_LDC_RbTnSeq_Data_App/)



Shiny App for Synechococcus 7942 LDC RB-TnSeq Data


This is my quick Shiny web application in R to explore the data in a interactive way. 
The code is stored on a Git repository.

Ways to download and run it are below:

```R
library(shiny)

# Easiest way is to use runGitHub,
#Using R, load Shiny using library(shiny) and then run the line below:
runGitHub("Syn7942-LDC-RBTnSeq", "dwelkie")

Or you can clone the git repository, then use `runApp()`:

​```R
# First clone the repository with git. If you have cloned it into
# ~/shiny_example, first go to that directory, then use runApp().
setwd("~/Syn7942-DC-RBTnSeq")
runApp()
```
