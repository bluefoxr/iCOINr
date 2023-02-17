
# iCOINr

<!-- badges: start -->
<!-- badges: end -->

iCOINr is an extension package to [COINr](https://bluefoxr.github.io/COINr/) which is focused on interactive plots. This package is a work in progress and I will add to it occasionally.

## About

When COINr underwent a major overhaul in 2022 I deliberately left out many of the interactive plots which were available in older versions. This was to streamline and focus the package, to reduce dependencies and to focus efforts on the core business of building and analysing composite indicators. Instead, I replicated many of the left-out plots using ggplot2. Therefore COINr now only includes static plots.

Static plots are great (in fact better) for static documents (pdf and docx, etc) because you have more control over how they render. However, interactive plots are better if the aim is to make apps, host content on websites and so on.

Although it is still possible to use the older interactive COINr plots via the [COINr6](https://github.com/bluefoxr/COINr6), this requires a conversion from the new "coin" class to the older "COIN" class. And COINr6 must be installed which is cluttered with unnecessary dependencies and some older code I'm not too proud of. In short, using COINr6 for interactive plotting is a hassle and is slower than it needs to be.

Enter iCOINr. The idea is to replicate and add to the library of interactive plots that are available in COINr6, but with direct compatibility with the latest COINr package. This means that interactive plotting is as simple as loading iCOINr and calling the relevant function.

## Installation

You can install the development version of iCOINr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bluefoxr/iCOINr")
```

## Latest

I will add to this package bit by bit over time, as I generate the code for other projects. Hopefully it should eventually include statistical plots, results visualisation and mapping.

