FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for the Holomics"

# Use the rocker/shiny base image
FROM rocker/shiny:4.4.1

# Install system dependencies and update libstdc++6
RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository -y ppa:ubuntu-toolchain-r/test \
    && apt-get update \
    && apt-get install -y g++-11 \
    && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libstdc++6 \
    && apt-get upgrade -y libstdc++6

# Install R packages
RUN R -e "install.packages(c('bs4Dash', 'config', 'dplyr', 'DT', 'ggplot2', 'golem', 'openxlsx', 'readxl', 'shiny', 'shinyalert', 'shinybusy', 'shinyjs', 'shinyvalidate', 'shinyWidgets', 'stringr', 'tippy', 'visNetwork', 'BiocManager', 'ggrepel', 'matrixStats', 'RSpectra', 'reshape2', 'tidyr', 'uuid', 'Rcpp', 'pkgload', 'markdown'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/2024-06-11')"
RUN R -e "install.packages(c('igraph'), repos='https://cloud.r-project.org')"
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')"
RUN R -e "BiocManager::install('BiocParallel')"
RUN R -e "BiocManager::install('mixOmics')"


# Set the working directory
WORKDIR /srv/shiny-server

# Copy the app code into the container
COPY . /srv/shiny-server/

# Ensure the app has executable permissions
RUN chmod +x /srv/shiny-server/app.R

# Expose the Shiny app port
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=8789)"]
