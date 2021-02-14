\name{parallel_line_st}
\alias{parallel_line_st}
\title{parallel-line method for Bioassay data analysis in K.K design}
\usage{
parallel_line_st(AT,sdose,tdose,ns,nt,ys,yt,ratio,method=c("random","block"),select=c("2.2","3.3","4.4","3.2","4.3"))
}
\description{
A convenient way to analyse biaassay data using parallel-line method in K.K design.
}
\arguments{
  \item{AT}{Estimated potency.}
  \item{sdose}{Dose of standard group in the experiment.}
  \item{tdose}{Dose of trial group in the experiment.}
  \item{ns}{Sample number of each standard group in the experiment.}
  \item{nt}{Sample number of each trial group in the experiment.}
  \item{ys}{Response values of standard group in the experiment.}
  \item{yt}{Response values of trial group in the experiment.}
  \item{ratio}{Dose dilution ratio.}
  \item{method}{Design of experiment, random or random block.}
  \item{select}{Design of experiment, K.K.}
}

