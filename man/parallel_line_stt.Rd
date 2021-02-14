\name{parallel_line_stt}
\alias{parallel_line_stt}
\title{parallel-line method for Bioassay data analysis in K.K.K design}
\usage{
parallel_line_stt(AT,AU,sdose,tdose,udose,ns,nt,nu,ys,yt,yu,ratio,method=c("random","block"),select=c("2.2.2","3.3.3"))
}
\description{
A convenient way to analyse biaassay data using parallel-line method in K.K.K design.
}
\arguments{
  \item{AT}{Estimated potency of trial 1.}
  \item{AU}{Estimated potency of trial 2.}
  \item{sdose}{Dose of standard group in the experiment.}
  \item{tdose}{Dose of trial 1 group in the experiment.}
  \item{udose}{Dose of trial 2 group in the experiment.}
  \item{ns}{Sample number of each standard group in the experiment.}
  \item{nt}{Sample number of each trial 1 group in the experiment.}
  \item{nu}{Sample number of each trial 2 group in the experiment.}
  \item{ys}{Response values of standard group in the experiment.}
  \item{yt}{Response values of trial 1 group in the experiment.}
  \item{yu}{Response values of trial 2 group in the experiment.}
  \item{ratio}{Dose dilution ratio.}
  \item{method}{Design of experiment, random or random block.}
  \item{select}{Design of experiment, K.K.K.}
}
