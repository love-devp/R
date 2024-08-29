# Use the official R base image with Shiny Server
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN packages required by the app
RUN R -e "install.packages(c('shiny', 'shinyjs', 'dplyr', 'ggplot2', 'DT', 'shinycssloaders', 'shinydashboard', 'shinydashboardPlus', 'scales', 'stringr', 'shinythemes', 'bslib', 'ggrepel', 'htmlwidgets', 'igraph', 'colourpicker'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/2024-05-01')"

# Install BiocManager and use it to install Bioconductor packages
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install(c("fgsea", "graph", "sonlite"))'

# Copy the Shiny app code from the repository to the Docker container
COPY . /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server/

# Allow Shiny to be run as an app
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]
