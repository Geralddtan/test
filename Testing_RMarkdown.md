R Markdown
----------

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    library(readxl)
    library(psych)
    library(car)

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:psych':
    ## 
    ##     logit

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:car':
    ## 
    ##     recode

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

    library(forecast)

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

    library(tseries)
    library(bucky) #install.packages('bucky')
    library(miceadds) #install.packages('miceadds')

    ## Loading required package: mice

    ## Loading required package: lattice

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

    ## 
    ## Attaching package: 'mice'

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

    ## * miceadds 3.7-6 (2019-12-15 13:38:43)

    library(multiwayvcov) #install.packages('multiwayvcov')
    library(stargazer) #install.packages('stargazer')

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

    #library(corrr)
    #library(tidyverse)

    file = read.csv('energydata_complete.csv')
    # 26 IVs here at first.
    colname <- c("Date","Appliances", "Lights", "Temp_Kitchen", "Humd_Kitchen", "Temp_Living_Room", 
                 "Humd_Living_Room", "Temp_Laundry", "Humd_Laundry", "Temp_Office", "Humd_Office", 
                 "Temp_Bath", "Humd_Bath", "Temp_Outside_North", "Humd_Outside_North", "Temp_Ironing", 
                 "Humd_Ironing", "Temp_Teen_2", "Humd_Teen_2", "Temp_Parents", "Humd_Parents", 
                 "Temp_Outside_Chievres", "Pressure_chievres", "Humd_Outside_Chievres", "Wind_Speed", 
                 "Visibility", "Tdewpoint", "rv1","rv2")

    colnames(file) <- colname
    file$Lights = NULL
    file$rv1 = NULL
    file$rv2 = NULL
    file$Temp_Outside_North = NULL #we only consider Temp_Outside_Chievres to measures temp outside
    file$Humd_Outside_North = NULL #we only consider Humd_Outside_Chievres to measure humd outside
    file$Temp_Bath = NULL #temperature of bathroom does not make conceptual sense to be used to analyse power usage of appliances
    file$Humd_Bath = NULL #humidity of bathroom does not make conceptual sense to be used to analyse power usage of appliances
    # 20 IVs here.

    head(file)

    ##                  Date Appliances Temp_Kitchen Humd_Kitchen
    ## 1 2016-01-11 17:00:00         60        19.89     47.59667
    ## 2 2016-01-11 17:10:00         60        19.89     46.69333
    ## 3 2016-01-11 17:20:00         50        19.89     46.30000
    ## 4 2016-01-11 17:30:00         50        19.89     46.06667
    ## 5 2016-01-11 17:40:00         60        19.89     46.33333
    ## 6 2016-01-11 17:50:00         50        19.89     46.02667
    ##   Temp_Living_Room Humd_Living_Room Temp_Laundry Humd_Laundry Temp_Office
    ## 1             19.2         44.79000        19.79     44.73000    19.00000
    ## 2             19.2         44.72250        19.79     44.79000    19.00000
    ## 3             19.2         44.62667        19.79     44.93333    18.92667
    ## 4             19.2         44.59000        19.79     45.00000    18.89000
    ## 5             19.2         44.53000        19.79     45.00000    18.89000
    ## 6             19.2         44.50000        19.79     44.93333    18.89000
    ##   Humd_Office Temp_Ironing Humd_Ironing Temp_Teen_2 Humd_Teen_2
    ## 1    45.56667     17.20000     41.62667        18.2    48.90000
    ## 2    45.99250     17.20000     41.56000        18.2    48.86333
    ## 3    45.89000     17.20000     41.43333        18.2    48.73000
    ## 4    45.72333     17.13333     41.29000        18.1    48.59000
    ## 5    45.53000     17.20000     41.23000        18.1    48.59000
    ## 6    45.73000     17.13333     41.26000        18.1    48.59000
    ##   Temp_Parents Humd_Parents Temp_Outside_Chievres Pressure_chievres
    ## 1     17.03333        45.53              6.600000             733.5
    ## 2     17.06667        45.56              6.483333             733.6
    ## 3     17.00000        45.50              6.366667             733.7
    ## 4     17.00000        45.40              6.250000             733.8
    ## 5     17.00000        45.40              6.133333             733.9
    ## 6     17.00000        45.29              6.016667             734.0
    ##   Humd_Outside_Chievres Wind_Speed Visibility Tdewpoint
    ## 1                    92   7.000000   63.00000       5.3
    ## 2                    92   6.666667   59.16667       5.2
    ## 3                    92   6.333333   55.33333       5.1
    ## 4                    92   6.000000   51.50000       5.0
    ## 5                    92   5.666667   47.66667       4.9
    ## 6                    92   5.333333   43.83333       4.8

Including Plots
---------------

You can also embed plots, for example:

![](Testing_RMarkdown_files/figure-markdown_strict/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
