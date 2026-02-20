# Base image (linux/amd64 for Apple Silicon and for SciLifeLab Serve)
FROM --platform=linux/amd64 rocker/shiny:4.4.1

# General updates
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libmagick++-dev libssl-dev libharfbuzz-dev libfribidi-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install R packages used by app (library + requireNamespace + helpers like tibble/scales)
RUN Rscript -e 'install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readr", "markdown", "paletteer", "scales", "tibble"), dependencies = TRUE)'

# Copy the app files (scripts, data, etc.)
RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/



# Ensure shiny user exists with UID 999 (only create if missing; base image may already have it)
RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny 2>/dev/null || true; \
        id -u 999 &>/dev/null && userdel -r "$(id -un 999)" 2>/dev/null || true; \
    fi; \
    (id -u shiny &>/dev/null && [ "$(id -u shiny)" -eq 999 ]) || useradd -u 999 -m -s /bin/bash shiny; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

# Other settings
USER shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]