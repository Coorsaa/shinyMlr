#### help texts ####

### Import ###

output$import.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("In this panel you can import your data. You can choose between predefined mlr tasks, CSV and ARFF files and the broad variety of OpenML datasets.")
  )
})

################################################################################

### Data summary ###

output$summary.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("In this panel you can get an overview over your dataset. In the table below you can see different location parameters for numeric variables and the number of levels for factor variables. Moreover, if any of the data's variables contains missing values, you can see the no. of NAs. Clicking on a numeric variable leads to a histogram shown below, clicking on a factor variable shows a bar plot.")
  )
})

################################################################################

### Preprocessing ###

output$preproc.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("You can choose here between several data preprocessing methods. The first ones in group 'On Task' only require a dataset while the ones of group 'On Task' are wrappers which need you to create a task first. You can do this in the next tab 'Task'.")
  )
})

## Impute ##

output$impute.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Impute' allows imputation of missing feature values through various techniques. The imputation techniques can be specified for certain feature classes.")
  )
})

## Create dummy Features ##

output$createdummy.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Create dummy features' replaces all factor features with their dummy variables. Non factor features will be left untouched and passed to the result.")
  )
})

## Drop Features ##

output$dropfeature.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Drop variable(s)' allows you to remove one or more features of your dataset.")
  )
})

## Remove constant features ##

output$remconst.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Constant features can lead to errors in some models and obviously provide no information in the training set that can be learned from. With setting a percentage, there is a possibility to also remove features for which less than this amount of percent of the observations differ from the mode value.")
  )
})

## Normalize features ##


output$normfeat.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Normalize features by different methods. only numeric features are considered. For constant features most methods fail, special behaviour for this case is implemented.")
  )
})

## Cap large values ##

output$caplarge.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Cap large values' converts numeric entries which have large/infinite (absolute) values in your dataset. Only numeric/integer variables are considered.")
  )
})

## Convert variables ##

output$convar.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Covert variable(s)' allows you to convert the selected variables into numeric, integer or factor variables.")
  )
})

## Subset ##

output$subset.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Subset' allows you to take a smaller subset of your data. This can be a random sample without replacement or a predefined fixed range of your dataset.")
  )
})

## Feature selection ##

output$feature.sel.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Feature selection' allows you to investigate the variable importance of your task's features by several filter methods. Then you have the option to keep either an absolute or relative amount of the most important features depending on the preliminary chosen filter method.", br(),
      br(),
      "Note: An overview of all supported filter method can be found in the ", a(href="https://mlr-org.github.io/mlr-tutorial/devel/html/filter_methods/index.html", target="_blank", "mlr tutorial"), ".")
  )
})

## Merge small factor levels ##

output$merge.factors.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("'Merge small factor levels' merges factor levels that occur only infrequently into combined levels with a higher frequency. The smallest levels of a factor are merged until their combined proportion w.r.t. the length of the factor exceeds the selected percentage.")
  )
})

################################################################################

### Task ###

output$task.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Here you can create your task. If your target variable is numeric, you will get a regression task while you will get a classification task for factor variables.")
  )
})

################################################################################

### Learnes ###

output$learners.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("After creating your task, you can choose your preferred learners here. Afterwards you have the option to change the learner parameters below. For a more detailed description of each learner and links to the depending R packages where you can find more information for each learner parameter, please check out the mlr ", a(href="https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/index.html", target="_blank", "learner section"), ".")
  )
})

################################################################################

### Tuning ###

## General tuning ##

output$tuning.explanation.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Many machine learning algorithms have hyperparameters that need to be set. If you want to select them by yourself, you can simply da that in the previous tab 'Learners'. Often suitable parameter values are not obvious and it is preferable to tune the hyperparameters, that is automatically identify values that lead to the best performance. ", br(),
      "In order to tune a machine learning algorithm, you have to specify:", br(),
      "- the search space", br(),
      "- the optimization algorithm (aka tuning method)", br(),
      "- an evaluation method, i.e., a resampling strategy and a performance measure. Here, we use use k-fold cross-validation to assess the quality of a specific parameter setting.", br(),
      "We offer you three different methods for hyperparameter tuning.", br(),
      br(),
      "1. Grid Search:", br(),
      "A grid search is one of the standard - albeit slow - ways to choose an appropriate set of parameters from a given range of values. The specified values have to be vectors of feasible settings and the complete grid simply is their cross-product.", br(),
      br(),
      "2. Random Search:", br(),
      "For this method, the search space is created by random samples of the feasible parameter set.", br(),
      br(),
      "3. irace:", br(),
      "The last method is the so called iterated F-racing from the irace package (technical description ",
      a(href="http://iridia.ulb.ac.be/IridiaTrSeries/link/IridiaTr2011-004.pdf", target="_blank", "here"),". ",
      "This not only works for arbitrary parameter types (numeric, integer, discrete, logical), but also for so-called dependent / hierarchical parameters.", br(),
      br(),
      br(),
      "Note that parallel tuning is supported. Simply activate it and enter the number threats your CPU is capable of handling.")
  )
})

## Tuning settings ##

output$tuning.settings.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Here you can specify your search space. Afterwards you can start the hyperparameter tuning. When you're happy with the results, transfer them to your learner by clicking on 'Transfer opt. hyperpars to learner'.")
  )
})

################################################################################

### Train and Predict ###

## Train ##

output$train.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Choose one of your previously selected learners and train a model for your task.")
  )
})

## Prediction ##

output$prediction.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Here you can use your model to predict on new data or the data included by your task.")
  )
})

## Evaluation ##

output$performance.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Evaluate your model performance by choosing a measure of your choice.")
  )
})

## Visualisation ##

output$visualisation.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("After you trained your model and predicted it on new data, you can visualise your results here. You can choose between prediction plots, residual plots and partial dependency plots for all task types. Moreover, for classification task you can choose the confusion matrix to evaluate prediction errors. Additionally the ROC curve can be viewed for binary classification tasks.")
  )
})

# Prediction Plot #

output$prediction.plot.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Prediction plot trains the model for 1 or 2 selected features, then displays it via ggplot. Good for teaching or exploring models.", br(),
      "For classification, only 2D plots are supported. The data points, the classification and potentially through color alpha blending the posterior probabilities are shown.", br(),
      "For regression, 1D and 2D plots are supported. 1D shows the data, the estimated mean and potentially the estimated standard error. 2D does not show estimated standard error, but only the estimated mean via background color."
    )
  )
})

# Residual Plot #

output$residual.plot.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Residual plots are used for model diagnostics. They provide scatterplots of true vs. predicted values and histograms of the model's residuals.")
  )
})

# Partial Dependency Plot #

output$partial.dep.plot.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Learners use features to learn a prediction function and make predictions, but the effect of those features is often not apparent. However, we can estimate the partial dependence of a learned function on a subset of the feature space. Partial dependence plots reduce the potentially high dimensional function estimated by the learner, and display a marginalized version of this function in a lower dimensional space.", br(),
      "With one feature and a regression task the output is a line plot, with a point for each point in the corresponding feature's grid. For regression tasks, if the learner has prediction type set to 'se', the std. error bounds will automatically be displayed using a gray ribbon.", br(),
      "With a classification task, a line is drawn for each class, which gives the estimated partial probability of that class for a particular point in the feature grid.", br(),
      br(),
      "Note: When you set individual to 'Yes' each individual conditional expectation curve is plotted. For classification the individual curves are centered by subtracting the individual conditional expectations estimated at the minimum of the feature. This results in a fixed intercept which aids in visualizing variation in predictions."
    )
  )
})

# ROC Plot #

output$roc.plot.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("For binary scoring classifiers a threshold (or cutoff) value controls how predicted posterior probabilities are converted into class labels. ROC curves serve to visualize and analyse the relationship between one or two performance measures and the threshold.", br(),
      "Receiver operating characteristic (ROC) curves plot the true positive rate (sensitivity) on the vertical axis against the false positive rate (1 - specificity, fall-out) on the horizontal axis for all possible threshold values. In addition to performance visualization ROC curves are helpful in", br(),
      br(),
      "- determining an optimal decision threshold for given class prior probabilities and misclassification costs (for alternatives see also the pages about cost-sensitive classification and imbalanced classification problems in this tutorial),", br(),
      "- identifying regions where one classifier outperforms another and building suitable multi-classifier systems,", br(),
      "- obtaining calibrated estimates of the posterior probabilities."
    )
  )
})

# Confusion Matrix #

output$confusion.matrix.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("The confusion matrix shows the relative frequency of correctly and incorrectly classified observations.")
  )
})

################################################################################

### Benchmark ###

output$benchmark.explanatory.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("In a benchmark experiment different learning methods are applied to your dataset with the aim to compare and rank the algorithms with respect to one or more performance measures. You can choose between different resample strategies which are used for evaluating the benchmark results.")
  )
})

## Benchmark plots ##

output$benchmark.plots.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Here you can see box or violin plots which show the distribution of performance values across resampling iterations for each performance measure and for all learners.")
  )
})

################################################################################

### Report ###

output$report.text = renderUI({
  box(width = 12, status = "primary", collapsible = TRUE,
    helpText("Finally you can draw a report of your analysis here. More infos to come!")
  )
})



################################################################################
################################################################################

#### Show/Hide Help ####

observeEvent(input$show.help, {
  help = input$show.help
  texts = list(
    "import.text",
    "summary.text",
    "preproc.text",
    "impute.text",
    "createdummy.text",
    "dropfeature.text",
    "remconst.text",
    "normfeat.text",
    "caplarge.text",
    "convar.text",
    "subset.text",
    "feature.sel.text",
    "merge.factors.text",
    "task.text",
    "learners.text",
    "tuning.explanation.text",
    "tuning.settings.text",
    "train.text",
    "prediction.text",
    "performance.text",
    "visualisation.text",
    "prediction.plot.text",
    "residual.plot.text",
    "partial.dep.plot.text",
    "roc.plot.text",
    "confusion.matrix.text",
    "benchmark.explanatory.text",
    "benchmark.plots.text",
    "report.text"
  )
  if (help == "Yes") {
    lapply(texts, shinyjs::show)
  } else {
    lapply(texts, shinyjs::hide)
  }
})
