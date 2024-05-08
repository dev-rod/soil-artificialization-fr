# git clone https://gitlab.com/rod74/soilArtificialization.git .
# todo improve, options to exclude generate tar.gz ? really necessary ?, only one shot to get deploy.renv.lock.prod
# golem::add_dockerfile_with_renv(output_dir = "deploy") & rm soilArtificialization_x.x.x.x.tar.gz
#
# docker build -t soilartificialization .
# DEV : docker compose -f docker-compose.dev.yml up
# PROD : docker compose -f docker-compose.prod.yml up
#
#### (below not recommended because not use renv cache :
#### (docker run --rm -p 8080:8081 soilartificialization)
#
# Ex. https://soil-artificialization.shuffledata.net

FROM rocker/verse:4.3.1

WORKDIR /srv/shiny-server/

ENV RENV_PATHS_CACHE /srv/shiny-server/.cache/R/renv

RUN apt-get update -y && apt-get install -y make zlib1g-dev pandoc libpq-dev libxml2-dev libicu-dev libpng-dev libssl-dev libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev libfontconfig1-dev libfreetype6-dev git && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

#COPY deploy/.Renviron .Renviron
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
#RUN R -e 'remotes::install_version("renv", version = "1.0.3")'
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
COPY deploy/renv.lock.prod renv.lock
#COPY deploy/renv.lock.test renv.lock
#COPY deploy/renv.lock.minimal renv.lock

# copy the renv auto-loader tools into the container so that
# a project-local library can be automatically provisioned and used when R is launched
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# restore packages as defined in the lockfile
## reported on run action to speed gitlabci image generation
#RUN R -e 'renv::restore()'

# delete default example application
RUN sudo rm -rf /srv/shiny-server/sample-apps

# copy the app to the image
COPY app.R app.R
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE
ADD R R
ADD inst inst
ADD data data

#RUN sed -i "s|3838|8080|" /etc/shiny-server/shiny-server.conf

EXPOSE 8081

# run app
CMD ["shiny-server"]

# run app on container start
CMD ["R", "-e", "install.packages(\"pkgload\");renv::restore();shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 8080)"]
