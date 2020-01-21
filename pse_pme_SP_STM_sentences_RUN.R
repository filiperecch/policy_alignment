##############################
####
####
####
####  Structural Topic Modeling - PME SP
####            ANALYSIS
####
####
###############################

library(conflicted)
library(tidyverse)
library(fs)
library(stm)
library(SnowballC)
library(tidytext)
library(readtext)
library(quanteda)
library(magrittr)
library(lsa)


### path setup

### SP ONLy!

uf <- "SP"

#### setup - organizing paths

proj_path <- here::here()
pse_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pse")

pme_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pme")

txt_data_SP <- fs::path(proj_path,
                     "pme_pse_doc_analysis_cloud_dir/data/txt_by_state",
                     uf)

img_pme_SP <- fs::path(proj_path,
                    "pme_pse_doc_analysis_cloud_dir/analysis/figures",
                    uf)

sp_dfm_sentences <- read_rds(path = paste(proj_path, "R", "sp_dfm_sentences", sep = "/"))
sp_clean_stm <- read_rds(path = paste(proj_path, "R", "sp_clean_stm", sep = "/"))
dropped_docs <- read_rds(path = paste(proj_path, "R", "dropped_docs", sep = "/"))

################## STM preparation and analysis

topics = 22

## running vanilla LDA

ptm <- proc.time()

stm_proc_sp <- stm(sp_clean_stm$documents, 
                   sp_clean_stm$vocab,
                   init.type = "Spectral",
                   topics)

proc.time() - ptm

topicslabel <- labelTopics(stm_proc_sp)

write_rds(stm_proc_sp, path = paste(proj_path, "/R/", "stm_proc_sp_", topics, sep = ""))

