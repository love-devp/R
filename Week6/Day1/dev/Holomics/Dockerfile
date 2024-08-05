# Use the official R base image with Ubuntu base
FROM rocker/shiny:4.2.0
	
RUN apt-get update && apt-get install -y software-properties-common && \
    add-apt-repository ppa:ubuntu-toolchain-r/test && \
    apt-get update && \apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdebi-core \
	gcc-11 \
	g++-11 \
	libstdc++6 \
    wget

# Install R packages
RUN R -e "install.packages(c('bs4Dash', 'config', 'dplyr', 'DT', 'ggplot2', 'golem', 'igraph', 'openxlsx', 'readxl', 'shiny', 'shinyalert', 'shinybusy', 'shinyjs', 'shinyvalidate', 'shinyWidgets', 'stringr', 'tippy', 'visNetwork', 'BiocManager', 'ggrepel', 'matrixStats', 'RSpectra', 'reshape2', 'tidyr', 'uuid', 'fastmap', 'pkgload'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/2024-06-11')"
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')"
RUN R -e "BiocManager::install(c('BiocParallel', 'mixOmics'))"

# Copy the app code into the container
COPY . /srv/shiny-server/

# Ensure the app has executable permissions
RUN chmod +x /srv/shiny-server/app.R

# Expose the Shiny app port
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=8789, host='0.0.0.0')"]