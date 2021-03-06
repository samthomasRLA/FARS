<!-- README.md is generated from README.Rmd. Please edit that file -->
FARS
====

The goal of FARS is to provide a set of tools to allow users to more easily work with data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS). This package contains functions to facilitate reading FARS data into R, summarizing data by year, and mapping incidents by year and US state.

Example: summarizing by month and year
--------------------------------------

For example, you can summarize the data for all events by month and year:

``` summary


## use fars_summarize_years and supply the desired year range

  summary_output <- fars_read_years(2013:2015)
  head(summary_output)
```

This package has been built and checked on Travis [![Build Status](https://travis-ci.org/samthomasRLA/FARS.svg?branch=master)](https://travis-ci.org/samthomasRLA/FARS)
