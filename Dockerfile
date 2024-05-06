# git clone https://gitlab.com/rod74/soilArtificialization.git .
# todo improve, options to exclude generate tar.gz ? really necessary ?, only one shot to get deploy.renv.lock.prod
# golem::add_dockerfile_with_renv(output_dir = "deploy") & rm soilArtificialization_x.x.x.x.tar.gz
#
# docker build -t soilartificialization .
# docker run --rm -p 8080:8081 soilartificialization
#
# Ex. https://soil-artificialization.shuffledata.net

FROM rocker/verse:4.3.1

RUN apt-get update -y && apt-get install -y make pandoc zlib1g-dev libxml2-dev \
libicu-dev libpng-dev libudunits2-dev libssl-dev libgdal-dev gdal-bin libgeos-dev \
libproj-dev libsqlite3-dev libfontconfig1-dev libfreetype6-dev git && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 8)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.3")'
COPY deploy/renv.lock.prod renv.lock
RUN R -e 'renv::restore()'

# delete default example application
RUN sudo rm -rf /srv/shiny-server/sample-apps

# copy the app to the image
#COPY project.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY DESCRIPTION /srv/shiny-server/
COPY NAMESPACE /srv/shiny-server/
ADD R /srv/shiny-server/R
ADD inst /srv/shiny-server/inst
ADD data /srv/shiny-server/data

#RUN sed -i "s|3838|8080|" /etc/shiny-server/shiny-server.conf

EXPOSE 8081

# run app
CMD ["shiny-server"]

# run app on container start
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 8080)"]
#CMD R -e "options('shiny.port'=8081,shiny.host='0.0.0.0');library(soilArtificialization);soilArtificialization::run_app()"
