
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **nfl4th** <img src="man/figures/logo.png" align="right" width="25%" min-width="120px"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/guga31bb/nfl4th/workflows/R-CMD-check/badge.svg)](https://github.com/guga31bb/nfl4th/actions)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version-last-release/nfl4th)](https://CRAN.R-project.org/package=nfl4th) -->
[![Twitter
Follow](https://img.shields.io/twitter/follow/ben_bot_baldwin.svg?style=social)](https://twitter.com/ben_bot_baldwin)
<!-- badges: end -->

This is the package that powers the [fourth down
calculator](https://rbsdm.com/stats/fourth_calculator/) introduced in
[this piece on The
Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/).

The code that powers the Twitter fourth down bot [is in this folder
here](https://github.com/guga31bb/fourth_calculator/tree/main/bot) and
the [code that runs the Shiny app is
here](https://github.com/guga31bb/fourth_calculator/blob/main/app.R).

## Installation

You can install the released version of nfl4th from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nfl4th")
```

You can install nfl4th from [GitHub](https://github.com/) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("guga31bb/nfl4th")
```

## Features

-   The **go for it** model gives probabilities for possibilities of
    yards gained and includes the possibility of earning a first down
    via defensive penalty
-   The **punt** model includes the possibility for getting blocked,
    returned for a touchdown, or fumbled on the return
-   The **field goal** model is a simple model of field goal % by
    distance and roof type

## Current limitations

There are some edge cases that are not accounted for. These should only
make a marginal difference to the recommendations as they are largely
edge cases (e.g. the possibility for a field goal to be blocked and
returned).

-   The **go for it** model does not allow for the possibility of a
    turnover return. However, long returns are extremely rare: For
    example, in 2018 and 2019 there were only four defensive touchdowns
    on plays where teams went for fourth downs out of 1,236 plays, and
    all of these happened when the game was well in hand for the other
    team.
-   The **punt** model doesn’t account for the punter or returner,
    ignores penalties on returns and ignores the potential for blocked
    punts to be returned for touchdowns
-   The **field goal** model doesn’t account for who the kicker is, what
    the weather is (only relevant for outdoor games), or the possibility
    of a kick being blocked and returned for a touchdown

## Get Started

To get started with nfl4th please see [this
article](https://www.nfl4th.com/articles/articles/nfl4th.html). Some
examples of 4th down research using the package [can be found
here](https://www.nfl4th.com/articles/articles/4th-down-research.html).
