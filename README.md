
# raquarius

<!-- badges: start -->
<!-- badges: end -->

The raquarius package provides access to the [Aquatic Informatics](https://aquaticinformatics.com/) [Aquarius](https://aquaticinformatics.com/products/aquarius-environmental-water-data-management/)
APIs. Currently, the Publish, Provision, and Web Portal endpoints are implemented.
Not all routes are currently implemented.

## Installation

You can install the development version of raquarius from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jasonelaw/raquarius")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(raquarius)

Sys.setenv("AQUARIUS_URL" = "URL")
Sys.setenv("AQUARIUS_USER" = "USERNAME")
Sys.setenv("AQUARIUS_PW" = "PASSWORD")

locs <- GetLocationDescriptionList()
ids  <- GetTimeSeriesUniqueIdList(LocationIdentifier = locs$Identifier[1])
ts   <- GetTimeSeriesData(TimeSeriesUniqueIds = ids$UniqueId[1:10])

Sys.setenv("AQUARIUS_WEBPORTAL_URL"  = "URL")
Sys.setenv("AQUARIUS_WEBPORTAL_USER" = "USERNAME")
Sys.setenv("AQUARIUS_WEBPORTAL_PW"   = "PASSWORD")

locs  <- GetMapDataAllLocations()
dsets <- GetMapDataDatasetsByParameter("Temperature")
ts    <- GetExportDataSet(DataSet = dsets$dataSetIdentifier[1])

(DS <- Datasets(dsets$dataSetIdentifier[1:5], Calculation = "Maximum"))

resp <- GetExportBulk(Datasets = DS, Interval = "Monthly", DateRange = "Years1")

```

