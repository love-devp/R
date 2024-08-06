\name{gse}
\alias{gse}
\docType{data}
\title{Airway smooth muscle cells - Salmon quantification}
\description{Estimated counts, abundance and effective length per gene for airway smooth muscle cell lines RNA-Seq experiment}
\details{
  The dataset contains four cell lines in two conditions: control and
  treatment with dexamethasone.

  In addition, in version 1.6, the package was updated to include two
  samples, SRR1039508 and SRR1039509, quantified using Salmon, in order
  to demonstrate the tximport/tximeta Bioconductor packages. For details
  on the quantification steps for these files, consult the airway2
  package: \url{https://github.com/mikelove/airway2}.

  This data object provides a SummarizedExperiment \code{gse} in which
  the Salmon quantification data for 8 samples have been loaded into
  R/Bioconductor using the tximeta package.
}
\usage{
data("gse")
}
\format{RangedSummarizedExperiment}
\source{FASTQ files from SRA, phenotypic data from GEO}
\references{Himes BE, Jiang X, Wagner P, Hu R, Wang Q, Klanderman B,
  Whitaker RM, Duan Q, Lasky-Su J, Nikolos C, Jester W, Johnson M,
  Panettieri R Jr, Tantisira KG, Weiss ST, Lu Q. 'RNA-Seq Transcriptome
  Profiling Identifies CRISPLD2 as a Glucocorticoid Responsive Gene that
  Modulates Cytokine Function in Airway Smooth Muscle Cells.' PLoS
  One. 2014 Jun 13;9(6):e99625. PMID: 24926665. GEO: GSE52778.
}
\keyword{datasets}
