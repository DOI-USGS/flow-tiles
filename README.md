## Recreating the streamflow tile map

This repository uses the [`targets` package](https://docs.ropensci.org/targets/) to build the chart. Run `tar_make()` to execute `_targets.R` .

Install all packages needed to run:

```         
install.packages(c('targets', 'tidyverse', 'lubridate', 'geofacet', 'cowplot', 'ggfx', 'showtext', 'magick', 'maps', 'svglite', 'xml2', 'rsvg', 'ragg', 'arrow', 'dataRetrieval'))
```

Running `tar_make()` will produce:

![flow_cartogram](out/flow_cartogram.png) 

![ig_1_title](out/ig_1_title.png) 
![ig_2_national](out/ig_2_national.png) 
![ig_3_cartogram](out/ig_3_cartogram.png) 

To disable `showtext` font styling after running pipeline, run:

```         
showtext_auto(enable = FALSE)
```

## Data

Streamflow data come from the [current conditions pipeline](https://code.usgs.gov/water/computational-tools/current-conditions-pipeline), which writes a daily parquet file of streamflow percentile categories at USGS streamgages to S3. This pipeline downloads one file per day of the focal month from:

```
https://dfi09q69oy2jm.cloudfront.net/visualizations/current_conditions/streamflow/data/sf_categorizations_{YYYY-MM-DD}.parquet
```

To build a new month, set `focal_month` in `_targets.R` to the first day of that month (e.g. `as.Date("2026-09-01")`) and run `tar_make()`. Any day without a file is skipped with a warning.
