# LymphMutSig Shiny app - Docker image for SciLifeLab Serve
# https://serve.scilifelab.se/docs/application-hosting/shiny/

# Base image (R 4.4.x; match the R version you use locally)
FROM rocker/shiny:4.4.1

# System dependencies
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libmagick++-dev libssl-dev libharfbuzz-dev libfribidi-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install renv
RUN Rscript -e 'install.packages("renv")'

# Copy app (includes app.R, data, www, scripts, renv.lock) then restore packages
COPY app/ /srv/shiny-server/
RUN Rscript -e 'setwd("/srv/shiny-server"); renv::restore();'

# Ensure shiny user exists with expected UID
RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny; \
        id -u 999 &>/dev/null && userdel -r "$(id -un 999)"; \
    fi; \
    useradd -u 999 -m -s /bin/bash shiny 2>/dev/null || true; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

USER shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
