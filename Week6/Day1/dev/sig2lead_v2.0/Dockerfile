FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for sig2lead"

# Use the official R base image
FROM rocker/shiny:4.0.3

# Install system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    git \
    libudunits2-dev \
    libopenbabel-dev \
    libeigen3-dev \
    librsvg2-bin \
    libglpk-dev

# Remove default Shiny server directory and create necessary directories
RUN rm -r /srv/shiny-server && \
    mkdir -p /var/lib/shiny-server/bookmarks/shiny && \
    mkdir -p /srv/shiny-server/files

# Update Shiny server configuration
RUN sed -i 's/8789/8789 0.0.0.0/' /etc/shiny-server/shiny-server.conf

# Install specific versions of necessary R packages
RUN R -e "install.packages('htmltools', version = '0.5.7', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages(c('tidyverse', 'shinydashboard', 'lubridate', 'magrittr', 'glue', 'shiny', 'shinyjs', 'ggplot2', 'plotly', 'httr', 'jsonlite', 'DT', 'visNetwork', 'plyr', 'dendextend', 'colorspace', 'ggforce', 'rlist', 'scatterpie', 'ggrepel', 'bazar', 'XML', 'RCurl', 'bitops', 'scrapeR', 'igraph', 'circlize', 'enrichR', 'readr', 'dplyr', 'gplots', 'doBy'), repos = 'https://packagemanager.rstudio.com/cran/__linux__/focal/2021-02-17')"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); BiocManager::install(version = '3.12'); BiocManager::install(c('ChemmineOB', 'ChemmineR'))"

# Copy application files to the image
COPY ./app/Sig2Lead_Backend.Rproj /srv/shiny-server/
COPY ./app/ui.R /srv/shiny-server/
COPY ./app/server.R /srv/shiny-server/
COPY ./app/lib /srv/shiny-server/lib
COPY ./app/similarity_search /srv/shiny-server/similarity_search
COPY ./app/www /srv/shiny-server/www
COPY ./app/files /srv/shiny-server/files
COPY ./app/files/minSim_apfp_RObjects.Rdata /srv/shiny-server/files/
COPY ./app/files/lincs_fps.Rdata /srv/shiny-server/files/
COPY ./app/files/LINCSCompounds.csv /srv/shiny-server/files/

# Set permissions
RUN chmod -R +rx /srv/shiny-server

# Expose port and run Shiny server
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 8789)"]
