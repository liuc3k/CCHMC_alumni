FROM rocker/shiny-verse:3.6.1

COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), prompt='R > ', download.file.method = 'libcurl')" > /.Rprofile

RUN apt-get update \
  && apt-get install -yqq --no-install-recommends \
  # libcurl4-openssl-dev \
  # libssl-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  liblwgeom-dev \
  libudunits2-dev \
  # libcairo2-dev \
  # protobuf-compiler \
  # libprotobuf-dev \
  # libjq-dev \
  # libv8-3.14-dev \
  && apt-get clean

RUN R -e "devtools::install_version(package = 'shiny', version = '1.3.2', upgrade = FALSE, quiet = TRUE)"
RUN R -e "devtools::install_version(package = 'flexdashboard', version = '0.5.1.1', upgrade = FALSE, quiet = TRUE)"
RUN R -e "devtools::install_version(package = 'tidyverse', version = '1.2.1', upgrade = FALSE, quiet = TRUE)"
RUN R -e "devtools::install_version(package = 'leaflet', version = '2.0.2', upgrade = FALSE, quiet = TRUE)"
RUN R -e "devtools::install_version(package = 'DT', version = '0.8', upgrade = FALSE, quiet = TRUE)"
RUN R -e "devtools::install_version(package = 'sf', version = '0.7-7', upgrade = FALSE, quiet = TRUE)"

RUN mkdir /srv/shiny-server/app

COPY . /srv/shiny-server/app

EXPOSE 3838

CMD exec shiny-server >> /var/log/shiny-server.log 2>&1
