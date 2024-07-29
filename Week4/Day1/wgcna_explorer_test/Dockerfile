FROM ubuntu:22.04 (jammy)
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for the WGCNA explorer App"

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
ENV PATH /opt/conda/bin:$PATH

RUN apt-get update --fix-missing && \
    apt-get install -y vim wget bzip2 unzip ca-certificates git libtbb-dev gcc g++ cmake
    
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
    echo "conda activate base" >> ~/.bashrc

COPY environment.yml /
RUN . /opt/conda/etc/profile.d/conda.sh && \ 
    conda activate base && \
    conda update conda && \
    conda env create -n via -f environment.yml && \
    conda install -n via pandas && \
    conda clean -a

ENV PATH /opt/conda/envs/via/bin:$PATH

# Install required R packages
RUN R -e 'install.packages(c("shiny", "BiocManager", "dplyr", "DT", "ggplot2", "igraph", "shinyjs", "dashboardthemes", "shinydashboard", "DT", "ggpmisc", "dplyr", "stringr", "ape", "tidyr","reshape2","devtools", "shinythemes", "ggplotify", "ggprism", "ggpubr", "patchwork", "tidyverse", "shinyjqui", "colourpicker", "conflicted"), \
    repos="https://packagemanager.posit.co/cran/__linux__/focal/2024-05-01")'
RUN R -e "BiocManager::install(version = '3.18', ask=FALSE)"
RUN R -e "BiocManager::install(c('GO.db', 'WGCNA', 'ComplexHeatmap','edgeR', 'DESeq2'))"

# Copy app directory onto image
ADD WGCNA-shinyApp /WGCNA-shinyApp/
RUN R -e 'devtools::install("/WGCNA-shinyApp/WGCNAShinyFun")'

CMD ["R", "-e", "library(shiny); runApp('/WGCNA-shinyApp/WGCNAbyClick.v1.R', host='0.0.0.0', port=3838, launch.browser=FALSE)"]