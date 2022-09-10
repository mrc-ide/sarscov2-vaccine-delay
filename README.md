# sarscov2-vaccine-delay

This is an [orderly](https://www.vaccineimpact.org/orderly/) repository which contains the analysis to our preprint

> Quantifying the impact of delaying the second COVID-19 vaccine dose in England: a mathematical modelling study

## Running

A sequence of tasks needs to be run with a set of parameters to generate the final results.  This is sketched out in the [`Master_vaccine_delay.R`](run.R) script, though this is provided only as a form of documentation. In practice these were run over several days on a HPC.

* regions: `c("north_west", "north_east_and_yorkshire", "midlands", "east_of_england", "london", "south_west", "south_east")`

1. Run the `vaccine_delay_fits_data` task to prepare the raw data for fitting.
2. Run the `vaccine_delay_parameters_fits` task.
3. Run the `vaccine_delay_fits` task for each of the seven regions. 
4. Run the `vaccine_delay_fits_combined` task.
5. Run the `vaccine_delay_simulation_parameters` task.
6. Run the `vaccine_delay_simulation` task with parameters defining the particular sensitivity considered.
7. Run the `vaccine_delay_simulation_plots` task to output all paper figures.



## Requirements

The core requirement is our [sircovid](https://mrc-ide.github.io/sircovid/) package and its dependencies. Because that package is in constant development you will probably want to pin your versions of the software to the versions we used for preparation:

```r
remotes::install_github(c(
  "mrc-ide/dust@v0.11.26",
  "mrc-ide/mcstate@v0.9.1",
  "mrc-ide/sircovid@0.13.14",
  "mrc-ide/spimalot@v0.7.11"))
```

However, you can always install the versions that we are using with

```r
drat:::add("ncov-ic")
install.packages(c("sircovid", "spimalot"))
```

You will also need a recent [orderly](https://www.vaccineimpact.org/orderly/) which can be installed with

```r
drat:::add("vimc")
install.packages("orderly")
```

## License

MIT © Imperial College of Science, Technology and Medicine
