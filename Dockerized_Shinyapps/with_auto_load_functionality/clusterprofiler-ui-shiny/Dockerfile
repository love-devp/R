FROM ubuntu:22.04
LABEL author="woonghee.lee001@umb.edu" description="Docker image containing all requirements for the Cluster Profiler"

# Use the rocker/shiny base image
FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
	libglpk40 \
	&& rm -rf /var/lib/apt/lists/*

# Install R packages required by the app
RUN R -e "install.packages(c('ggplot2', 'DT', 'waiter', 'BiocManager'), repos='https://cloud.r-project.org/')"
RUN R -e "BiocManager::install(c('AnnotationHub', 'clusterProfiler'))"

# Copy the app to the image
COPY clusterprofiler.ui.app.R /srv/shiny-server/

# Make the Shiny app available at port 8789
EXPOSE 8789

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/clusterprofiler.ui.app.R', host='0.0.0.0', port=8789)"]
