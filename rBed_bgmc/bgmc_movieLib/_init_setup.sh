#!/bin/sh
sudo apt-get install r-base-core

sudo su - -c "R -e \"install.packages('hash', repos = 'http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('data.table', repos = 'http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('stringr', repos = 'http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('pryr', repos = 'http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('xfun', repos = 'http://cran.rstudio.com/')\""