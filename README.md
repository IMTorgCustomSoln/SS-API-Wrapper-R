# SS-API-Wrapper-R
R version of the ScrumSaga API Wrapper

## Install
```R
library(devtools)
install_github('IMTorgCustomSoln/SS-API-Wrapper-R')

...
* installing *source* package ‘ScrumSagaR’ ...
** R
** preparing package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded
* DONE (ScrumSagaR)

library(ScrumSagaR)
ls('package:ScrumSagaR')
```

## Update
_make changes to .R/'*.R' file_
```R
library(devtools)
library(roxygen2)
document()
```
_push to repo_
