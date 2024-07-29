FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for ROGUE"

# Use the official R image with Shiny server
FROM rocker/shiny:latest

# Install system dependencies for additional R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libhdf5-dev \
    libcairo2-dev \
    libxt-dev

# Copy the app files to the Docker image
COPY . /srv/shiny-server/

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'shinyjs', 'markdown', 'MASS', 'pheatmap', 'plotly', 'RColorBrewer', 'ggplot2', 'dplyr', 'heatmaply', 'gridExtra', 'gtools', 'colourpicker', 'Rtsne', 'scatterplot3d', 'grid', 'cowplot', 'reshape2', 'rintrojs', 'uwot', 'shinyalert', 'shinycssloaders'), repos='https://cloud.r-project.org/')"
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')"
RUN R -e "BiocManager::install(c('edgeR', 'fgsea', 'DESeq2'))"

# Ensure the shiny user has permission to run the app
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the port where the app will be served
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=3838, host='0.0.0.0')"]
