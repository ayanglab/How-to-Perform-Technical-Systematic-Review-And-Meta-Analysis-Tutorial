% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_stat.R
\name{compare_multi_features}
\alias{compare_multi_features}
\title{p value for whether one feature affects performance.}
\usage{
compare_multi_features(rdata, one_metric, selected_features, save_dir)
}
\arguments{
\item{rdata}{the input dataframe}

\item{one_metric}{metric for performance, must be a continuous variable}

\item{selected_features}{a vector of features to compare. If a feature is
categorical, Wilcoxon rank sum test will be carried out. If it is
continuous, the R2 correlation will be computed}

\item{save_dir}{which directory to save the results as a text file}
}
\description{
p value for whether one feature affects performance.
}
