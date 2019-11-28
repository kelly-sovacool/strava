# Strava

Having fun plotting my activity data from [Strava](http://bit.ly/strava-kelly)!

## Dependencies

Most dependencies are listed in the [conda](https://docs.conda.io/projects/conda/en/latest/index.html) environment file: [`config/env.yml`](config/env.yml).

Additional dependencies not available in conda:

- [rStrava](https://github.com/fawda123/rStrava)
- [googleway](https://cran.r-project.org/package=googleway)
- [googlePolylines](https://cran.r-project.org/package=googlePolylines)
- dev version of [lubridate](https://github.com/tidyverse/lubridate)

## Workflow

### Run everything
```
snakemake
```
See the [snakemake documentation](https://snakemake.readthedocs.io/en/stable/) for more on how to use snakemake.

### View the workflow DAG
```
snakemake -n --forceall --dag | dot -Tsvg > figures/dag.svg
```

![](figures/dag.svg)


Activity Report
================
Kelly Sovacool
2019-11-27

## Plot duration by week

![](/Users/kelly/projects/strava/figures/bar_all.png)<!-- -->

## Split by year

![](/Users/kelly/projects/strava/figures/bar_2017.png)<!-- -->![](/Users/kelly/projects/strava/figures/bar_2018.png)<!-- -->![](/Users/kelly/projects/strava/figures/bar_2019.png)<!-- -->

## Plot cumulative duration

![](/Users/kelly/projects/strava/figures/line_all.png)<!-- -->
