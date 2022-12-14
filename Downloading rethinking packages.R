### In order to work with the rethinking package we must first download "stan" for r. 
### Then we can download the rethinking package. Both are done via github devtools.

Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
devtools::install_github("stan-dev/cmdstanr")

example(stan_model, package = "rstan", run.dontrun = TRUE)

library("rstan")

install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')

devtools::install_github("rmcelreath/rethinking")

library(rethinking)

