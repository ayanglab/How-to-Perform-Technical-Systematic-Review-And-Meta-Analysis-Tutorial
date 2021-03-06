% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{revdat}
\alias{revdat}
\title{Design traits of different studies}
\format{
An object of class \code{data.frame} with 92 rows and 69 columns.
}
\usage{
revdat
}
\description{
a dataframe with the following columns:
\describe{
\item{GAN.based}{logical vector, whether the study uses GAN}
\item{PI}{logical vector, whether the study uses parallel imaging}
\item{acceleration}{string, acceleration ratios across all
experimental conditions}
\item{architecture}{string, deep neural network architecture}
\item{augmentation}{string, data augmentation technique}
\item{better}{logical, whether the performance exceeds all the
comparison methods listed in the study}
\item{category}{string (factor), whether the study is 'unrolled
optimisation' or 'end-to-end'}
\item{channel.merging}{string, how channel/coil merging is
performed if the study uses parallel imaging}
\item{channel.number}{string, number of channels in the input
image}
\item{comparison}{string, comparison methods}
\item{computation.time}{string, GPU computation time per
reconstructed image in seconds}
\item{country}{string, the country of the institute where the
first author of the study is affliated to}
\item{data.consistency}{logical, whether data consistency layer as
in DC-CNN is used}
\item{dataset}{string, public dataset in the study}
\item{dimension}{numeric, the input dimension of the image, can be
2, 3 or 4}
\item{ground.truth}{string, how the ground truth is obtained}
\item{input.domain}{string, the input domain of the image}
\item{input.size}{string, the size of the input}
\item{limitation}{string, limitation of the study reported by the
authors}
\item{loss}{string, loss function}
\item{mask}{string, undersampling mask}
\item{metric}{string, metric used to assess reconstruction performance}
\item{modality}{string, the imaging sequence, e.g. T1, T2 weighted}
\item{name}{string, the name of the model proposed}
\item{novelty}{string, novelty mentioned by the authors}
\item{open.source}{string, the url to the source code}
\item{optimiser}{string, the type of optimiser used}
\item{package}{string, deep learning framework, e.g. tensorflow,
pytorch}
\item{parameter.number}{string, the number of parameters}
\item{region}{string, anatomical region in the image dataset,
e.g. brain, knee}
\item{residual}{logical, whether residual learning is applied}
\item{result}{string, descriptions of reconstruction results}
\item{result.table}{string, whether result table is provided for
quantitative analysis}
\item{spatial.domain}{string, whether spatial domain was used}
\item{strength}{string, the strength of the model}
\item{testing.data.pathology}{logical, whether the testing dataset
contains pathological features}
\item{testing.data.size}{string, sample size of the testing
dataset}
\item{testing.mode}{string, mode of testing, retrospective or
prospective}
\item{testing.number}{numeric, number of scans used in testing}
\item{training.data.pathology}{logical, whether the training dataset
contains pathological features}
\item{training.data.size}{string, sample size of the training
dataset}
\item{training.number}{numeric, number of scans used in training}
\item{unet.like}{logical, whether unet-like architecture is used}
\item{unrolled}{logical, whether unrolled optimisation is used}
\item{validation.data.size}{string, sample size of the validation
dataset}
\item{zero.filled}{logical, whether zero-filling is used as a
comparison method}
\item{others}{string, other information}
\item{ID}{string, ID for each study used in this review paper}
\item{year}{string, publication year according to the 'publish or
perish' website}
\item{journal}{string, in which journal the paper is found}
\item{supervised}{logical, whether supervised learning is used}
\item{institute}{logical, the institute which first author of the
study is affliated to}
\item{full_GT}{logical, whether fully sampled ground truth is
available}
\item{adam}{logical, whether adam optimiser is used}
\item{central_mask}{logical, whether the mask samples the center
of the image fully}
\item{max_acc}{numeric, maximal acceleration ratio}
\item{MSE_loss}{logical, whether mean squared error loss is used}
\item{L1_loss}{logical, whether L1 loss is used}
\item{L2_loss}{logical, whether L2 loss is used}
\item{complex_inp}{logical, whether the input is complex signal}
\item{public_code}{logical, whether the code is published}
\item{public_data}{logical, whether the data is published}
\item{reproducibility}{string, reproducibility of the study}
\item{autoencoder}{logical, whether autoencoder is used}
\item{MLP}{logical, whether multi-layer perceptron is used}
\item{stack_conv}{logical, whetther stacked convolution is used}
\item{model_structure}{string, architecture of the model}
\item{cluster}{string, raw cluster output from \code{mclust::Mclust}}
\item{new_cluster}{string, annotation of the raw cluster}
}
\strong{NB}: If a particular study uses multiple methods for a particular trait,
each method is separated by a comma. A bracket may follow each method
describing the dataset that the method is applied in the study.
}
\author{
Yutong Chen
}
\keyword{datasets}
