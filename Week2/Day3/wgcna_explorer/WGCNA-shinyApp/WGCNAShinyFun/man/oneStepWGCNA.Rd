% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneStepWGCNA.R
\name{oneStepWGCNA}
\alias{oneStepWGCNA}
\title{oneStepWGCNA}
\usage{
oneStepWGCNA(
  exp.mat,
  lazy_modle = TRUE,
  save_fig = T,
  F.path = "./",
  iterative = T,
  grey_cut = 10,
  datatype = "peak area",
  RcCutoff = 1,
  samplePerc = 0.3,
  method = "raw",
  GeneNumCut = 0,
  cutmethod = "MAD",
  rscut = 0.8,
  minModuleSize = 30,
  mergeCutHeight = 0.25,
  maxBlocksize = 999999,
  r_cutoff = 0.8,
  weight_cutoff = 0.02,
  pheofile
)
}
\arguments{
\item{exp.mat}{'character' or 'data.frame'; expression matix.}

\item{lazy_modle}{"logical'; run oneStepWGCNA in lazy_model or not. Default is FALSE, Enabling lazy mode will result in skipping interactive steps, primarily the removal of outliers and the selection of the optimal power value. This can lead to significant differences in the final results. However, other parameters can be modified by changing the default values. If not set, the process will run according to the default parameters.}

\item{save_fig}{'logical'; how many cores you want to use. Default is TRUE}

\item{F.path}{'character'; output file path.}

\item{datatype}{'character'; Data type of your input rawdata. "count", "expected count". "normalized count", "peak area" or "protein abundance"}

\item{RcCutoff}{'numeric'; Cutoff of rawdata, if readcount or FPKM less than this "RcCutoff" in x samples, it should be treat as the noise.}

\item{samplePerc}{'numeric'; If readcount or FPKM less than RcCutoff "insamplePerc"  samples, it should be treat as the noise.}

\item{method}{'character'; Conversion method of input raw data. "vst" for count and expected count, "raw" and "logarithm" for other datatype.}

\item{GeneNumCut}{'numeric'; How many transcripts you want to retain for WGCNA.}

\item{cutmethod}{'character'; The datExpr will ranked by MAD or Var. which method you want.}

\item{rscut}{'numeric'; cutoff of R2.}

\item{minModuleSize}{'numeric'; minimal size of module.}

\item{mergeCutHeight}{'numeric'; Dynamic cutting height, clusters less than this value will be merged}

\item{maxBlocksize}{'numeric'; number of select genes, default is 9999999 means all input genes were select for next steps.}

\item{r_cutoff}{'numeric'; cutoff of module-trait correlation value r, default is 0.8}

\item{weight_cutoff}{'numeric'; cutoff of cytoscape weight value. default is 0.02}

\item{pheofile}{'character'; phenotyp file.}
}
\value{
All result of WGCNA
}
\description{
one step WGCNA pipeline
}
\author{
Shawn Wang <http://www.shawnlearnbioinfo.top>.
\email{shawnwang2016@126.com}
}
