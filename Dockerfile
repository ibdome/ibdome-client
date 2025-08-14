FROM rocker/shiny:4.5.1

RUN install2.r --error remotes ggplot2 ggpubr tidyr dplyr plotly RSQLite dbplyr shiny.semantic R.devices markdown png readr shiny.router forcats box zip waiter

# For whatever reason, ggbeeswarm is not available on the rstudio binary mirror.
RUN install2.r --error --repos https://cloud.r-project.org/ ggbeeswarm

ADD shiny /srv/shiny-server/ibdome
ADD CHANGELOG.md /srv/shiny-server/ibdome/

USER shiny
