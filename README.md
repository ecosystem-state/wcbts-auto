# wcbts-auto

This repository uses Github Actions to automate the querying and analysis of west coast groundfishes, using sdmTMB. 

[![R-pull-data](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-pull-data.yml/badge.svg)](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-pull-data.yml)

The jobs for fitting models to the WCBTS data need to be split into thirds, because of time limitations with GH Actions,

[![R-run-models1](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-a.yml/badge.svg)](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-a.yml)

[![R-run-models2](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-b.yml/badge.svg)](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-b.yml)

[![R-run-models3](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-c.yml/badge.svg)](https://github.com/ecosystem-state/wcbts-auto/actions/workflows/R-run-models-c.yml)
