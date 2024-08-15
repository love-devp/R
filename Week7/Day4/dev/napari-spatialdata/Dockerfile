# Use a Miniconda image to have access to conda
FROM continuumio/miniconda3

# Set the working directory in the container
WORKDIR /app

# Install system dependencies (including xcb and related libraries, and Xvfb for headless environments)
RUN apt-get update && apt-get install -y \
    build-essential \
    libglfw3-dev \
    libgles2-mesa-dev \
    gcc \
    git \
    libx11-6 \
    libx11-xcb1 \
    libxcb1 \
    libxext6 \
    libxrender1 \
    libxi6 \
    libgl1-mesa-glx \
    libglib2.0-0 \
    libdbus-1-3 \
    libfontconfig1 \
    libsm6 \
    xvfb \
    && rm -rf /var/lib/apt/lists/*

# Create a new conda environment with Python 3.10
RUN conda create -n napari_env python=3.10 -y

# Set SHELL to use bash so conda commands can run correctly
SHELL ["/bin/bash", "-c"]

# Initialize Conda for all bash sessions
RUN echo "source /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc

# Clone the repository to ensure git metadata is available
RUN git clone https://github.com/scverse/napari-spatialdata.git /app

# Activate conda environment and install project dependencies
RUN /bin/bash -c "source /opt/conda/etc/profile.d/conda.sh && conda activate napari_env && \
    pip install --upgrade pip && \
    pip install anndata click cycler dask>=2024.4.1 geopandas loguru matplotlib napari[all] napari-matplotlib numba numpy packaging pandas pillow qtpy scanpy scipy shapely scikit-learn spatialdata>=0.2.1 superqt typing_extensions>=4.8.0 vispy xarray xarray-datatree && \
    conda install -c conda-forge pyqt -y"

# Install the napari-spatialdata package from the source code
RUN /bin/bash -c "source /opt/conda/etc/profile.d/conda.sh && conda activate napari_env && pip install ."

# Set the environment variable for Qt platform
ENV QT_QPA_PLATFORM=xcb

# Use Xvfb and explicitly call napari using its full path
CMD ["bash", "-c", "source /opt/conda/etc/profile.d/conda.sh && conda activate napari_env && Xvfb :99 -screen 0 1024x768x16 & export DISPLAY=:99 && /opt/conda/envs/napari_env/bin/napari"]
