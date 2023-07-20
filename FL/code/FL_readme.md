# garrett_elections

Local judicial and criminal justice elections project.

## Florida Election

-   This is the data extraction and munging of Florida Election Data.

-   The election data was collected from this ["link"](https://results.elections.myflorida.com/)

-   Election data is collected for available years 1978 - 2022.

## Notes

-   Cleaned election, office and result files are found in FL/data/clean arranged by date and election type
-   Merged files for all election data is found in FL/data/merged
-   Data Schema changes: special and runoff columns are moved from elections to offices
-   Code workflow
    1.  Automated download of all election data from the link using Rselenium browser (use_browser.R)

    2.  Wrangle data (wrangle_data.R)

        2.1 Wrangle non-retention elections (non_retention.R)

        2.2 Wrangle retention elections (retention.R)

    3.  Merge all cleaned data (merge_cleaned.R)

    4.  run_all.R runs all the above steps
-   Data Storage
    1.  raw data - data/raw
    2.  clean data - data/clean
    3.  merged clean data - data/merged
