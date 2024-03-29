% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_model.R
\name{fx_model}
\alias{fx_model}
\title{Train/Test Model}
\usage{
fx_model(
  df.set,
  covar = NULL,
  voi = NULL,
  outcome = NULL,
  model.type = "logistic",
  z.pred = F
)
}
\arguments{
\item{df.set}{list containing train and test data frames}

\item{outcome}{df0 column name for outcome measure to be predicted (string)}

\item{model.type}{machine learning model ('rf', 'logistic', 'regression', 'rf.regression', 'svm') (string)}

\item{z.pred}{standardize predictive features (boolean)}
}
\value{
A list of length three, containing the following elements:
\itemize{
\item "pred.covar":   data frame of predicted values from covariate model
\item "pred.full":    data frame of predicted values from full model
\item "parameters":   model parameters
}
}
\description{
Train/test model on sub data frames
}
