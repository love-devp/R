FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for sig2lead"

# Base image from Rocker Project
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

# Remove default Shiny server directory
RUN rm -r /srv/shiny-server

# Create necessary directories
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN mkdir -p /srv/shiny-server/files

# Update Shiny server configuration
RUN sed -i 's/8789/8789 0.0.0.0/' /etc/shiny-server/shiny-server.conf

# Install R packages
RUN R -e "install.packages(c('tidyverse', 'shinydashboard', 'lubridate', 'magrittr', 'glue', 'shiny', 'shinyjs', 'ggplot2', 'plotly', 'httr', 'jsonlite', 'DT', 'visNetwork', 'plyr', 'dendextend', 'colorspace', 'ggforce', 'rlist', 'scatterpie', 'ggrepel', 'bazar', 'XML', 'RCurl', 'bitops', 'scrapeR', 'igraph', 'circlize', 'enrichR', 'readr', 'dplyr', 'gplots', 'doBy', 'shinyvalidate', 'mixOmics'))"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); BiocManager::install(version = '3.12'); BiocManager::install(c('ChemmineOB', 'ChemmineR'))"

# Copy application files to the image
COPY ./app/Sig2Lead_Backend.Rproj /srv/shiny-server/
COPY ./app/ui.R /srv/shiny-server/
COPY ./app/server.R /srv/shiny-server/
COPY ./app/lib /srv/shiny-server/lib
COPY ./app/similarity_search /srv/shiny-server/similarity_search
COPY ./app/files /srv/shiny-server/files

# Set permissions
RUN chmod -R +rx /srv/shiny-server

# Expose port and run Shiny server
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 8789)"]

