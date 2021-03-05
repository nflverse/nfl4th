
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **nfl4th** <img src="man/figures/logo.png" align="right" width="25%" min-width="120px"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/guga31bb/nfl4th/workflows/R-CMD-check/badge.svg)](https://github.com/guga31bb/nfl4th/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/nfl4th)](https://CRAN.R-project.org/package=nfl4th)
<!-- badges: end -->

This is the package that powers the [fourth down
calculator](https://rbsdm.com/stats/fourth_calculator) introduced in
[this piece on The
Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/).

The code that powers the Twitter fourth down bot [is in this folder
here](https://github.com/guga31bb/fourth_calculator/tree/main/bot).

## Installation

You can install the released version of nfl4th from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nfl4th")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("guga31bb/nfl4th")
```

## Example 1: from nflfastR

Let’s start by loading 2020 play-by-play data from `nflfastR`.

``` r
library(nfl4th)
library(tidyverse)

data <- nflfastR::load_pbp(2020)
```

Here’s how to calculate all probabilities from one game:

``` r
tictoc::tic("One game")
data %>%
  dplyr::filter(week == 20, home_team == "GB") %>%
  nfl4th::add_4th_probs() %>%
  dplyr::filter(down == 4) %>%
  dplyr::select(
    posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
#> home_opening_kickoff not found. Assuming an nflfastR df and doing necessary cleaning . . .
#> Performing final preparation . . .
#> Computing probabilities for  9 plays. . .
```

| posteam | ydstogo | yardline\_100 | go\_boost | first\_down\_prob | wp\_fail | wp\_succeed | go\_wp | fg\_make\_prob | miss\_fg\_wp | make\_fg\_wp | fg\_wp | punt\_wp |
| :------ | ------: | ------------: | --------: | ----------------: | -------: | ----------: | -----: | -------------: | -----------: | -----------: | -----: | -------: |
| GB      |      17 |            65 |    \-3.74 |              0.15 |     0.33 |        0.47 |   0.35 |           0.00 |         0.31 |         0.47 |   0.31 |     0.38 |
| TB      |      15 |            56 |    \-2.96 |              0.17 |     0.53 |        0.71 |   0.56 |           0.00 |         0.52 |         0.66 |   0.52 |     0.59 |
| GB      |       6 |             6 |    \-2.16 |              0.31 |     0.36 |        0.57 |   0.43 |           0.98 |         0.33 |         0.45 |   0.45 |       NA |
| TB      |       9 |            47 |    \-2.73 |              0.33 |     0.55 |        0.67 |   0.59 |           0.00 |         0.52 |         0.68 |   0.52 |     0.62 |
| TB      |       4 |            45 |    \-0.12 |              0.51 |     0.55 |        0.67 |   0.62 |           0.00 |         0.54 |         0.69 |   0.54 |     0.62 |
| GB      |      15 |            86 |    \-2.52 |              0.19 |     0.16 |        0.38 |   0.20 |           0.00 |         0.14 |         0.34 |   0.14 |     0.22 |
| GB      |      10 |            76 |    \-0.37 |              0.32 |     0.15 |        0.37 |   0.22 |           0.00 |         0.14 |         0.32 |   0.14 |     0.23 |
| TB      |       8 |            28 |    \-3.45 |              0.37 |     0.72 |        0.92 |   0.80 |           0.75 |         0.69 |         0.88 |   0.83 |       NA |
| GB      |       8 |             8 |      3.77 |              0.33 |     0.04 |        0.31 |   0.13 |           0.98 |         0.03 |         0.09 |   0.09 |       NA |

``` r
tictoc::toc()
#> One game: 1.38 sec elapsed
```

We see the infamous field goal at the bottom.

## Example 2: from user input

Let’s input the play ourselves to verify that we get the same thing. The
below shows the bare minimum amount of information that has to be fed to
`nfl4th` in order to compute 4th down decision recommendations.

``` r
one_play <- tibble::tibble(
  
  # things to help find the right game (use "reg" or "post")
  home_team = "GB",
  away_team = "TB",
  posteam = "GB",
  type = "post",
  season = 2020,
  
  # information about the situation
  qtr = 4,
  quarter_seconds_remaining = 129,
  ydstogo = 8,
  yardline_100 = 8,
  score_differential = -8,

  home_opening_kickoff = 0,
  posteam_timeouts_remaining = 3,
  defteam_timeouts_remaining = 3
)

one_play %>%
  nfl4th::add_4th_probs() %>%
  dplyr::select(
    posteam, ydstogo, yardline_100, posteam, go_boost, first_down_prob, 
    wp_fail, wp_succeed, go_wp, fg_make_prob, miss_fg_wp, make_fg_wp, 
    fg_wp, punt_wp
  ) %>%
  knitr::kable(digits = 2)
#> Performing final preparation . . .
#> Computing probabilities for  1 plays. . .
```

| posteam | ydstogo | yardline\_100 | go\_boost | first\_down\_prob | wp\_fail | wp\_succeed | go\_wp | fg\_make\_prob | miss\_fg\_wp | make\_fg\_wp | fg\_wp | punt\_wp |
| :------ | ------: | ------------: | --------: | ----------------: | -------: | ----------: | -----: | -------------: | -----------: | -----------: | -----: | -------: |
| GB      |       8 |             8 |      3.77 |              0.33 |     0.04 |        0.31 |   0.13 |           0.98 |         0.03 |         0.09 |   0.09 |       NA |

As expected, the output is the same.

## Check coaches’ alignment with the model

Here’s how to create one of the tables shown in [the piece on The
Athletic](https://theathletic.com/2144214/2020/10/28/nfl-fourth-down-decisions-the-math-behind-the-leagues-new-aggressiveness/):

``` r
library(gt)

calculated <- data %>%
  nfl4th:::add_4th_probs() %>%
  dplyr::filter(down == 4, !is.na(go_boost)) 
#> home_opening_kickoff not found. Assuming an nflfastR df and doing necessary cleaning . . .
#> Performing final preparation . . .
#> Computing probabilities for  3811 plays. . .
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ncjrajxkar .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: black;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: white;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ncjrajxkar .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ncjrajxkar .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ncjrajxkar .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ncjrajxkar .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ncjrajxkar .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ncjrajxkar .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ncjrajxkar .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ncjrajxkar .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ncjrajxkar .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ncjrajxkar .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ncjrajxkar .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: black;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ncjrajxkar .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: black;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: black;
  vertical-align: middle;
}

#ncjrajxkar .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ncjrajxkar .gt_from_md > :first-child {
  margin-top: 0;
}

#ncjrajxkar .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ncjrajxkar .gt_row {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: white;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ncjrajxkar .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ncjrajxkar .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ncjrajxkar .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ncjrajxkar .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ncjrajxkar .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ncjrajxkar .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ncjrajxkar .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ncjrajxkar .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ncjrajxkar .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ncjrajxkar .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ncjrajxkar .gt_left {
  text-align: left;
}

#ncjrajxkar .gt_center {
  text-align: center;
}

#ncjrajxkar .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ncjrajxkar .gt_font_normal {
  font-weight: normal;
}

#ncjrajxkar .gt_font_bold {
  font-weight: bold;
}

#ncjrajxkar .gt_font_italic {
  font-style: italic;
}

#ncjrajxkar .gt_super {
  font-size: 65%;
}

#ncjrajxkar .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ncjrajxkar" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="3" class="gt_heading gt_title gt_font_normal" style>

NFL team decision-making by go recommendation, 2020

</th>

</tr>

<tr>

<th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="color: black; font-weight: bold;">

Recommendation

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: black; font-weight: bold;">

Went for it %

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" style="color: black; font-weight: bold;">

Plays

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Definitely go for it

</td>

<td class="gt_row gt_center">

72

</td>

<td class="gt_row gt_center">

232

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Probably go for it

</td>

<td class="gt_row gt_center">

35

</td>

<td class="gt_row gt_center">

659

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Toss-up

</td>

<td class="gt_row gt_center">

18

</td>

<td class="gt_row gt_center">

1557

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Probably kick

</td>

<td class="gt_row gt_center">

2

</td>

<td class="gt_row gt_center">

882

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Definitely kick

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

306

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="3">

<strong>Notes</strong>: “Definitely” recommendations are greater than 4
percentage point advantage,<br> “probably” 1-4 percentage points

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

Thus, we can see that the model is strongly aligned to what coaches do.
