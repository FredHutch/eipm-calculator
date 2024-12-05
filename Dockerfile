FROM fredhutch/r-shiny-server-base:4.4.1

RUN apt-get update -y && apt-get install -y git curl libfontconfig1-dev  libharfbuzz-dev libfribidi-dev libfreetype-dev libtiff5-dev libsodium-dev

RUN R -q -e 'install.packages(c( "shiny", "shinydashboard", "shinyvalidate", "tidyverse", "tidymodels", "glmnet"))'


RUN rm -rf /srv/shiny-server/
COPY app/ /srv/shiny-server/

EXPOSE 3838

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1

CMD /usr/bin/shiny-server
