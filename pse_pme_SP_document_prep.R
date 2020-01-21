##############################
####
####
####
####  Structural Topic Modeling - PME SP
####           DOC PREPARATION
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


### path setup

### SP ONLy!

uf <- "SP"

#### setup - organizing paths

proj_path <- here::here()
pse_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pse",
                 sep = "/")

pme_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pme")

txt_data_SP <- fs::path(proj_path,
                     "pme_pse_doc_analysis_cloud_dir/data/txt_by_state",
                     uf)

img_pme_SP <- fs::path(proj_path,
                    "pme_pse_doc_analysis_cloud_dir/analysis/figures",
                    uf)

add_stopwords_ptb <- read_delim(
  paste(proj_path,
        "/support_files/stopwords-pt.txt",
        sep = ""),
  delim = ",", 
  col_names = F) # custom stopwords

munic_stopwords <- read_delim(
  paste(proj_path,
        "/support_files/sp_municipios.txt",
        sep = ""),
  delim = ",", 
  col_names = F) # custom stopwords

pse_pme_sp <- dir_ls(txt_data_SP) 

sp_pse <- pse_pme_sp[1] # state level long term plan

sp_munic_pme <- pse_pme_sp[2:length(pse_pme_sp)] # all municipalities in SP


#### Creating corpus

sp_corpus <- readtext(pse_pme_sp,
                      encoding = "UTF-8", docvarsfrom = "filename") 

##### doc processing and preparation

sp_dfm_sentences <- corpus(sp_corpus) %>%
  corpus_reshape(to = "sentences") 

sp_dfm <- sp_dfm_sentences %>% 
  tokens(remove_numbers = TRUE,
         remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_separators = TRUE,
         remove_hyphens = TRUE,
         remove_url = TRUE,
         verbose = TRUE) %>% 
  dfm(verbose = T)

sp_dfm_clean <- dfm_select(sp_dfm, 
                           pattern = c(add_stopwords_ptb$X1,
                                       munic_stopwords$X1,
                                       "plano", "municipal", "educação", 
                                       "secretaria", "município",
                                       "fonefax", "fone",                                               
                                       "cep", "prefeitura", "telefax", "telefone",
                                       "página", "gráfico", "tabela", "cnpj", "cpf",
                                       "mail", "email", "pabx", "fax", "gov", "fonesfax",
                                       "cnpjcpf", "tel", "pme", "são paulo", "cnpj"),
                           selection = "remove", 
                           verbose = T) %>% 
  dfm_wordstem(language = "pt") %>% 
  dfm_select(pattern = ".*[\\-\\.\\;\\:\\,\\<\\>\\!\\?]+.*", 
             valuetype = "regex", 
             selection = "remove",
             verbose = T) %>% 
  dfm_select(pattern = ".*[0-9].*", 
             valuetype = "regex", 
             selection = "remove",
             verbose = T) %>% 
  dfm_trim(min_docfreq = 4, 
           docfreq_type = "count",
           verbose = T) %>% 
  dfm_select(pattern = ".{3,100}", 
             valuetype = "regex", 
             selection = "keep",
             verbose = T)

sp_clean_stm <- sp_dfm_clean %>% 
    convert("stm")

dropped_docs <- docnames(sp_dfm_clean)[which(quanteda::rowSums(sp_dfm_clean) == 0)]

write_rds(sp_dfm_sentences, path = paste(proj_path, "R", "sp_dfm_sentences", sep = "/"))
write_rds(sp_clean_stm, path = paste(proj_path, "R", "sp_clean_stm", sep = "/"))
write_rds(dropped_docs, path = paste(proj_path, "R", "dropped_docs", sep = "/"))




































