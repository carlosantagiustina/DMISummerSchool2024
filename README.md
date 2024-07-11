# DMISummerSchool2024
Repository with code for NLP preprocessing

##  Overview
This NLP pipeline is designed to process and analyze textual data, focusing on tasks such as language detection, text cleaning, Named Entity Recognition (NER), and dependency relation extraction. The pipeline is built using R and various R packages.

##  Initial Creation Date
03/11/2021
## Current Version
1.54
## Current Version Date
11/07/2024
## Author
Carlo Santagiustina
## Contact
carlo.santagiustina@sciencespo.fr

## Confidentiality
See licence.

## Requirements
### R Libraries
The following R libraries are required to run this pipeline:

- devtools
- tidyverse
- quanteda
-  ggplot2
- stm
- spacyr
- visNetwork
To install the necessary packages, run:

```
install.packages(c("devtools","tidyverse","quanteda","ggplot2","stm","spacyr","visNetwork"))
devtools::install_github("hadley/emo")
```
## System Dependencies: Python and Spacy 

The spacyr package requires a Python virtual environment with the spacy library and specific language models installed. The pipeline uses Miniconda to manage the Python environment.
```
reticulate::install_miniconda(path = "~/Documents/Miniconda", update = TRUE, force = FALSE)
Sys.setenv(RETICULATE_MINICONDA_PATH="~/Documents/Miniconda")
reticulate::condaenv_exists()
```
Create a Conda environment for spacy:
```
reticulate::conda_create(envname = "spacyr", packages = "spacy", forge = TRUE, channel = "conda-forge")
```

Download the necessary language model (e.g., French):
```
spacyr::spacy_download_langmodel(envname = "spacyr", model = "fr_core_news_md")
```

Initialize spacyr with the language model:
```
spacy_initialize('fr_core_news_md', condaenv = "~/Documents/Miniconda/envs/spacyr/")
```
