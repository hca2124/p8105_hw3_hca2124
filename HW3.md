p8105_hw3_hca2124
================
Helene Apollon
2026-10-18

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
data("ny_noaa")
```

The data contains daily weather records from New York weather stations.
It has 2595176 rows and 7 columns. Important variables include
precipitation (tents of mm), snowfall (mm), snow depth (mm), minimum and
maximum temperature (tents of degrees C), date of observation, and
weather station ID. The data shows a specific station at a date in time
from 1981-01-01 to 2010-12-31 across a number of 747 stations.
