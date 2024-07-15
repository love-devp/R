# Use an appropriate base image
FROM continuumio/miniconda3

# Update conda and install dependencies
RUN conda update -n base -c defaults conda \
    && conda install -c conda-forge -c bioconda bioconductor-debrowser

# Expose the necessary port (if applicable, change the port as needed)
EXPOSE 8000

# Define the entrypoint
CMD ["debrowser"]
