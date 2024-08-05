FROM rocker/shiny:4.2.3

# Install dependencies
RUN apt-get update && \
    apt-get install vim libjpeg9 libtiff5-dev cmake libmagick++-dev dos2unix -y --no-install-recommends && \
    apt-get autoremove -yqq --purge && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set timezone
ENV TZ=Asia/Dubai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

USER shiny
WORKDIR /home/shiny

# Install Miniconda
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
    /bin/bash ~/miniconda.sh -b -p /home/shiny/miniconda3 && \
    rm ~/miniconda.sh && \
    echo ". /home/shiny/miniconda3/etc/profile.d/conda.sh" >> ~/.bashrc

# Switch to root to create symlink
USER root
RUN ln -s /home/shiny/miniconda3/etc/profile.d/conda.sh /etc/profile.d/conda.sh

# Switch back to shiny user
USER shiny

# Set PATH
ENV PATH="/home/shiny/miniconda3/bin:$PATH"

# Install mamba
RUN . /home/shiny/miniconda3/etc/profile.d/conda.sh && \
    conda install -c bioconda -c conda-forge mamba -y

# Create conda environment
COPY environment.yaml environment.yaml
RUN . /home/shiny/miniconda3/etc/profile.d/conda.sh && \
    conda init bash && \
    mamba env create -f environment.yaml && \
    echo "conda activate nasqar_env" >> ~/.bashrc && \
    conda clean --all -y

# Install R packages
RUN . /home/shiny/miniconda3/etc/profile.d/conda.sh && conda activate nasqar_env && \
    R -e "install.packages(c('stringi', 'GOplot', 'wordcloud2'), repos='http://cran.rstudio.com/')" && \
    R -e "devtools::install_github('YuLab-SMU/clusterProfiler')" && \
    R -e "BiocManager::install(c('org.Hs.eg.db','org.Mm.eg.db','org.Rn.eg.db','org.Sc.sgd.db','org.Dm.eg.db','org.At.tair.db','org.Dr.eg.db','org.Bt.eg.db','org.Ce.eg.db','org.Gg.eg.db','org.Cf.eg.db','org.Ss.eg.db','org.Mmu.eg.db','org.EcK12.eg.db','org.Xl.eg.db','org.Pt.eg.db','org.Ag.eg.db','org.Pf.plasmo.db','org.EcSakai.eg.db'))" && \
    R -e "devtools::install_github('smin95/smplot2')" && \
    R -e "devtools::install_github('nyuad-corebio/seuratv3wizard')" && \
    R -e "BiocManager::install(c('ATACseqQC'))" && \
    R -e "BiocManager::install(c('ChIPpeakAnno', 'MotifDb', 'GenomicAlignments'))" && \
    R -e "BiocManager::install(c('BSgenome.Hsapiens.UCSC.hg19'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('TxDb.Hsapiens.UCSC.hg19.knownGene'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('BSgenome.Celegans.UCSC.ce11'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('TxDb.Celegans.UCSC.ce11.refGene'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('BSgenome.Drerio.UCSC.danRer11'), ask = FALSE)" && \
    R -e "BiocManager::install(c('TxDb.Drerio.UCSC.danRer11.refGene'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('BSgenome.Mmusculus.UCSC.mm10'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install(c('TxDb.Mmusculus.UCSC.mm10.knownGene'), ask = FALSE, update = FALSE)" && \
    R -e "BiocManager::install('compbiomed/animalcules')" && \
    R -e "BiocManager::install(c('BiocGenerics', 'DelayedArray', 'DelayedMatrixStats', 'limma', 'lme4', 'S4Vectors', 'SingleCellExperiment', 'SummarizedExperiment', 'batchelor', 'HDF5Array', 'terra', 'ggrastr'))" && \
    R -e "devtools::install_github('cole-trapnell-lab/monocle3')" && \
    R -e "remotes::install_github('satijalab/seurat', 'seurat5', quiet = TRUE)"

USER root

RUN mkdir /opt/nasqar_build
# ADD NASQAR2/nasqar.tar /opt/nasqar_build/

COPY ClusterProfShinyGSEA /opt/nasqar_build/ClusterProfShinyGSEA
COPY ClusterProfShinyORA /opt/nasqar_build/ClusterProfShinyORA
COPY DEApp /opt/nasqar_build/DEApp
COPY deseq2shiny /opt/nasqar_build/deseq2shiny
COPY GeneCountMerger /opt/nasqar_build/GeneCountMerger
COPY NASQAR /opt/nasqar_build/NASQAR
COPY ATACseqQCShniy /opt/nasqar_build/ATACseqQCShniy
COPY dada2Shiny /opt/nasqar_build/dada2Shiny

USER shiny
WORKDIR /home/shiny
RUN mkdir ~/.ssh

USER root

RUN apt-get update && \
    apt-get install sshfs -y --no-install-recommends && \
    apt-get autoremove -yqq --purge && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /opt/nasqar_build
RUN wget https://github.com/UMMS-Biocore/debrowser/archive/refs/heads/master.zip && \
    unzip master.zip

RUN wget https://github.com/elliefewings/monocle3_shiny/archive/refs/heads/main.zip && \
    unzip main.zip

RUN wget https://github.com/compbiomed/animalcules/archive/a24aced16297b12b92b63520b0046ddbae288322.zip && \
    unzip a24aced16297b12b92b63520b0046ddbae288322.zip && \
    mv animalcules-a24aced16297b12b92b63520b0046ddbae288322/inst/shiny animalcules && \
    sed -i '1 i\library(animalcules)' animalcules/app.R

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /opt/nasqar_build/shiny-server.sh
RUN dos2unix /opt/nasqar_build/shiny-server.sh && \
    chmod a+x /opt/nasqar_build/shiny-server.sh && \
    mkdir /opt/nasqar_build/logs && \
    chown -R shiny:shiny /opt/nasqar_build/

USER shiny
RUN chmod a+x /opt/nasqar_build/shiny-server.sh

EXPOSE 3232

CMD ["/bin/bash", "/opt/nasqar_build/shiny-server.sh"]
