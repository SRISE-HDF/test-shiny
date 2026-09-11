FROM rocker/shiny:4.4
COPY app.R /srv/shiny-server/app/app.R
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port = 3838)"]