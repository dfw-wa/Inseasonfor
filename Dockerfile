FROM rocker/r-ver:4.5.1

# Install system dependencies needed by the daily update workflow
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    rsync \
    zstd \
    cmake \
    libx11-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    make \
    pandoc \
    libfreetype6-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libicu-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libxml2-dev \
    libcairo2-dev \
    zlib1g-dev \
    libnode-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv and pre-restore all R packages into the image's system library
COPY renv.lock /renv.lock
RUN Rscript -e "\
    install.packages('renv', repos = 'https://cloud.r-project.org'); \
    renv::restore(lockfile = '/renv.lock', library = .libPaths()[1], prompt = FALSE)"
