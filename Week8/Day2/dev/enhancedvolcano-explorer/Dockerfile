FROM ubuntu:22.04
LABEL author="zach@viascientific.com" description="Dockerfile for Via Scientific PCA Explorer app"

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update --fix-missing && apt-get install -y software-properties-common gnupg make wget libcurl4-openssl-dev gcc unzip curl zlib1g-dev libglpk-dev libgmp3-dev libxml2-dev pandoc libicu-dev libssl-dev

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

RUN apt update && apt install -y r-base

RUN R -e 'install.packages(c("BiocManager", "shiny", "shinyjs", "shinycssloaders", "stringr", "dplyr", "DT", "ggplot2", "rhandsontable", "htmlwidgets", "plotly", "bslib"), \
    repos="https://packagemanager.posit.co/cran/__linux__/focal/2024-07-01")'

# Install Bioconductor Packages if necessary
RUN R -e "BiocManager::install(version = '3.19', ask=FALSE)"
RUN R -e "BiocManager::install(c('DESeq2', 'sva'))"

RUN apt-get install -y gdebi-core && \
    wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.21.1012-amd64.deb && \
    gdebi shiny-server-1.5.21.1012-amd64.deb

# Copy app directory onto image
ADD pca-explorer /pca-explorer/

# Run app on container start 
CMD ["R", "-e", "shiny::runApp('/pca-explorer')"]
