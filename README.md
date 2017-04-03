# shinyMlr: Integration of the mlr package into shiny

[![Build Status](https://travis-ci.org/mlr-org/shinyMlr.svg?branch=master)](https://travis-ci.org/mlr-org/shinyMlr) 

With help of this package **mlr** can be accessed via a shiny interface. 

This project has started last year and contains now **mlr**'s major functionalities:

- Data import
- Data exploration and preprocessing
- Creating regression or classification tasks
- Making use of any **mlr** learner
- Tuning of learner hyper parameters
- Training and predicting a model
- Benchmark experiments with different learners and measures
- Many visualisations

## Installation

```r
devtools::install_github("mlr-org/shinyMlr/package")
```