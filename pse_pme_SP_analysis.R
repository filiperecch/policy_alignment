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
library(cluster)
library(factoextra)
library(NbClust)
library(stargazer)
library(RColorBrewer)

### path setup

uf <- "SP" # choose state from list BR_STATES = AC, AL, AM, AP, BA, CE, DF, ES, GO, MA, MG, MS, MT, PA, PB, PE, PI, PR,	RJ, RN, RO, RR, RS, SC, SE, SP, TO


# Setup and paths ---------------------------------------------------------

proj_path <- here::here()
pse_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pse")

pme_raw <- fs::path(proj_path,
                 "pme_pse_doc_analysis_cloud_dir/data/raw/pme")

txt_data_SP <- fs::path(proj_path,
                     "pme_pse_doc_analysis_cloud_dir/data/txt_by_state",
                     uf)

img_pme_SP <- fs::path(proj_path,
                    "analysis/figures",
                    uf)

tabs_pme_SP <- fs::path(proj_path,
                    "analysis/tables",
                    uf)

sp_dfm_sentences <- read_rds(path = paste(proj_path, "R", "sp_dfm_sentences", sep = "/"))

sp_clean_stm <- read_rds(path = paste(proj_path, "R", "sp_clean_stm", sep = "/"))
dropped_docs <- read_rds(path = paste(proj_path, "R", "dropped_docs", sep = "/"))

saresp_diffs <- read_rds(path = paste(proj_path, "R", "saresp_diffs", sep = "/"))

# STM ANALYSIS ------------------------------------------------------------

topics <- 22

stm_proc_sp <- read_rds(path = paste(proj_path, "/R/", "stm_proc_sp_", topics, sep = ""))

stm_proc_sp_topics <- labelTopics(stm_proc_sp)

mean_top_prev <- stm_proc_sp$theta %>% 
  set_colnames(paste0(rep("topic_", 
                         topics), 
                     seq(1,topics))) %>% 
  as_tibble() %>% 
  rename("topic_01" = "topic_1",
         "topic_02" = "topic_2",
         "topic_03" = "topic_3",
         "topic_04" = "topic_4",
         "topic_05" = "topic_5",
         "topic_06" = "topic_6",
         "topic_07" = "topic_7",
         "topic_08" = "topic_8",
         "topic_09" = "topic_9") %>% 
  bind_cols(sp_clean_stm$meta) %>% 
  group_by(docvar1, docvar2) %>% 
  summarise_all(mean) 

## Checking examples by topic

examples_senteces <- function(corpus, stm_corpus, n_examples, drop_docs){
  corpus_tibble <- corpus$documents %>% 
    mutate(doc_ref = paste(corpus$documents$`_document`,
                           corpus$documents$`_segid`, 
                           sep = ".")) %>%
    dplyr::filter(!(doc_ref %in% dropped_docs))
  texts <- corpus_tibble$texts
  docs <- corpus_tibble$doc_ref
  stm_corpus_labels <- labelTopics(stm_corpus)
  for(z in 1:stm_corpus$settings$dim$K){
    out <- order(stm_corpus$theta[,z], decreasing=T)[1:n_examples]
    base::print(paste("Topic:", z,
                      paste(stm_corpus_labels$frex[z,],
                            collapse = ".")))
    for(m in 1:length(out)){
      base::print(as.character(docs[out[m]]))
      base::print(as.character(texts[out[m]]))
      readline('next - press enter')
    }
  }
}

examples_senteces(sp_dfm_sentences, stm_proc_sp, n_examples = 5, dropped_docs)


# Cluster Analysis --------------------------------------------------------

sug_cluster <- vector(length = 30)

for (i in 1:30) {
  
  gap_stat <- clusGap(x=mean_top_prev[, -c(1,2)], FUN=kmeans,iter.max =20, K.max=25)
  
  sug_cluster[i] <- with(gap_stat,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
  
}

n_clusters <- sug_cluster %>%
  tibble::enframe(name = NULL) %>% 
  count(value) %>% 
  top_n(1) %>%
  first()

sug_cluster %>%
  tibble::enframe(name = NULL) %>%
  ggplot() +
  geom_bar(aes(value))

set.seed(94305)
kmeans_optimal <- kmeans(mean_top_prev[, -c(1,2)], centers = 8, nstart = 25, iter.max = 20)

# Cosine similarity --------------------------------------------------------------

write_rds(pse_vec, fs::path(pse_raw, "pse_vec"))

doc_vecs <- as.matrix(mean_top_prev[,-c(1,2)]) 
pse_vec <- doc_vecs[1, ]
cos_sim_pse <- cosine(pse_vec, base::t(doc_vecs)) %>%
  enframe(name = NULL, value = "cos_sim")

cosine_df <- mean_top_prev %>% 
  bind_cols(cos_sim_pse,
            "cluster" = kmeans_optimal$cluster) %>% 
  mutate(co_municipio = str_replace(docvar1, "SP", "")) %>% 
  type_convert()

cosine_df %>% 
  ungroup() %>% 
  select(co_municipio, cos_sim, contains("topic_")) %>% 
  saveRDS(file = paste(proj_path, "R", "pme_pse_cosine_sim", sep = "/"))

cosine_df %>% 
  select(docvar1, cluster)

n_cluster_table <- cosine_df %>% 
  group_by(cluster) %>% 
  summarise("N. documents" = n()) %>% 
  mutate("rnames" = paste("Cluster", seq(1,8))) %>% 
  column_to_rownames("rnames") %>% 
  select(`N. documents`)

stargazer(n_cluster_table,
          header = F,
          summary = F,
          label = "tab:cluster", 
          title = "Number of plans by cluster",
          font.size = "footnotesize",
          float = F,
          out = fs::path(tabs_pme_SP,
                         "cluster_table.tex"))



# ANALYSIS: Policy similarity vs. achievment difference -------------------

## 2014 only

saresp_diffs_2014 <- saresp_diffs %>% 
  select(co_municipio,
         matches("diff.*2014")) %>% 
  as_tibble()

saresp_cosin_2014 <- saresp_diffs_2014 %>% 
  inner_join(cosine_df, by = c("co_municipio"))

m_2014_7_mat <- lm(abs(diff_mat_7_2014) ~ cos_sim, data = saresp_cosin_2014)
m_2014_7_lp <- lm(abs(diff_lp_7_2014) ~ cos_sim, data = saresp_cosin_2014)

m_2014_9_mat <- lm(abs(diff_mat_9_2014) ~ cos_sim, data = saresp_cosin_2014)
m_2014_9_lp <- lm(abs(diff_lp_9_2014) ~ cos_sim, data = saresp_cosin_2014)

model_to_file <- capture.output(
  stargazer(
    m_2014_7_mat,
    m_2014_7_lp,
    m_2014_9_mat,
    m_2014_9_lp,
    header = F,
    no.space = T,
    dep.var.labels = c("Math 7th", "Langue 7th",
                       "Math 9th", "Langue 9th"),
    covariate.labels = c("Similarity"),
    digits = 3,
    keep.stat = c("n", "rsq"),
    font.size = "footnotesize", 
    title = "Doc. similarity and test score difference", 
    label = "tab:model"))

writeLines(model_to_file,
    paste(tabs_pme_SP,
          "diff_regressions.tex",
          sep = "/"))

# Graphs ------------------------------------------------------------------

#### STM

## graph word prevalance by topic

stm_tidy_sp <- tidy(stm_proc_sp, matrix = "beta")

stm_top_sp <- 
  stm_tidy_sp %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

labels_topics <- c("1" = "Topic 01: Special Ed.",
                   "2" = "Topic 02: Municipal info.",
                   "3" = "Topic 03: Preschool",
                   "4" = "Topic 04: Teacher career",
                   "5" = "Topic 05: Adult Ed.",
                   "6" = "Topic 06: Social protection",
                   "7" = "Topic 07: Goals and targets",
                   "8" = "Topic 08: Responsability",
                   "9" = "Topic 09: Schooling data",
                   "10" = "Topic 10: Local description",
                   "11" = "Topic 11: PME monitoring",
                   "12" = "Topic 12: Pedagogy",
                   "13" = "Topic 13: Previous results",
                   "14" = "Topic 14: Challanges",
                   "15" = "Topic 15: Header",
                   "16" = "Topic 16: Footer",
                   "17" = "Topic 17: Infrastructure",
                   "18" = "Topic 18: Community envol.",
                   "19" = "Topic 19: Vocational Ed.",
                   "20" = "Topic 20: Legal aspects",
                   "21" = "Topic 21: Local economy",
                   "22" = "Topic 22: Bureacracy")

topics_stm_top_sp <- stm_top_sp %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, 
             scales = "free", 
             labeller = labeller(topic = labels_topics)) +
  theme_light() +
  theme(text = element_text(size = 32),
        strip.text.x = element_text(size = 22, 
                                    face = "bold")) +
  ylab("Word probability by topic") +
  xlab("Term") +
  scale_x_reordered() +
  coord_flip()

ggsave(paste(img_pme_SP, "/",
             "n_topics_",
             topics,
             ".png",
             sep = ""),
       topics_stm_top_sp,
       height = 23, width = 40)

# STM topic proportions  

(topics_all <- stm_proc_sp$theta %>%
    data.frame()  %>%
    rename("Topic 01: Special Ed." = "X1",
           "Topic 02: Municipal info." = "X2",
           "Topic 03: Preschool" = "X3",
           "Topic 04: Teacher career" = "X4",
           "Topic 05: Adult Ed." = "X5",
           "Topic 06: Social protection" = "X6",
           "Topic 07: Goals and targets" = "X7",
           "Topic 08: Responsibility" = "X8",
           "Topic 09: Schooling data" = "X9",
           "Topic 10: Local description" = "X10",
           "Topic 11: PME monitoring" = "X11",
           "Topic 12: Pedagogy" = "X12",
           "Topic 13: Previous results" = "X13",
           "Topic 14: Challenges" = "X14",
           "Topic 15: Header" = "X15",
           "Topic 16: Footer" = "X16",
           "Topic 17: Infrastructure" = "X17",
           "Topic 18: Community envol." = "X18",
           "Topic 19: Vocational Ed." = "X19",
           "Topic 20: Legal aspects" = "X20",
           "Topic 21: Local economy" = "X21",
           "Topic 22: Bureaucracy" = "X22") %>%
    summarise_all(mean) %>%
    gather(topic, prop) %>% 
    ggplot(aes(x = reorder(topic, + prop), 
               y = prop)) + 
    geom_col() + 
    geom_text(aes(label= format(round(prop, 3), 2)), 
              color="white", 
              size=20, 
              hjust = 1.5) + 
    theme_light() +
    ylab('Expected topic proportions') + 
    xlab(NULL) +
    theme(axis.title.x = element_text(size = 80,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 60, 
                                      margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text.y = element_text(size = 70)) + 
    coord_flip())

ggsave(paste(img_pme_SP, "/",
             "topic_prop_t22",
             ".png",
             sep = ""),
       topics_all,
       height = 23, width = 40)


#### topic prop by cluster

cluster_avg <- saresp_cosin_2014 %>% 
  select(cluster, starts_with("topic_")) %>% 
  group_by(cluster) %>% 
  summarise_all(mean, na.rm=T)

(topic_prev_cluster <- saresp_cosin_2014 %>% 
  select(cluster, starts_with("topic_")) %>% 
  group_by(cluster) %>% 
  summarise_all(mean, na.rm=T) %>% 
  gather("topic", "value", -cluster) %>% 
  ggplot() +
  geom_col(aes(-cluster, -value, fill = topic),
           col = "white")  +
  theme_light() +
  xlab("Cluster") +
  ylab("Mean topic prevalence") +
  scale_x_discrete(limits = seq(-1, -8),
                   label = seq(1,8)) +
  scale_y_continuous(labels = seq(0,1,0.25)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(22),
                      labels = c("01: Special Ed.",
                                 "02: Municipal info.",
                                 "03: Preschool",
                                 "04: Teacher career",
                                 "05: Adult Ed.",
                                 "06: Social protection",
                                 "07: Goals and targets",
                                 "08: Responsibility",
                                 "09: Schooling data",
                                 "10: Local description",
                                 "11: PME monitoring",
                                 "12: Pedagogy",
                                 "13: Previous results",
                                 "14: Challenges",
                                 "15: Header",
                                 "16: Footer",
                                 "17: Infrastructure",
                                 "18: Community envol.",
                                 "19: Vocational Ed.",
                                 "20: Legal aspects",
                                 "21: Local economy",
                                 "22: Bureaucracy"),
                      name = "Topic") +
  theme(axis.text = element_text(size=60),
        axis.title = element_text(size=60),
        legend.title = element_text(size=45, 
                                    angle = 90, 
                                    hjust = .5),
        legend.text = element_text(size=45),
        legend.position = "bottom") +
  coord_flip())

ggsave(paste(img_pme_SP, "/",
             "topic_prev_cluster",
             ".png",
             sep = ""),
       topic_prev_cluster,
       height = 23, width = 40)

#### Cluster 

(cluster_optimal <- fviz_cluster(kmeans_optimal, 
                              data = mean_top_prev[, -c(1,2)], 
                              axes = c(1,2), 
                              geom = "point", 
                              palette = "Set2",
                              pointsize = 6,
                              shape = 19,
                              ggtheme = theme_light(), main = NULL) +
    theme(text = element_text(size = 60),
          legend.position = "none"))

ggsave(paste(img_pme_SP, "/",
             "cluster_optimal",
             ".png",
             sep = ""),
       cluster_optimal,
       height = 23, width = 40)


#### Cosine similarity

(cos_distrib <- cos_sim_pse %>% 
  ggplot() +
  geom_density(aes(cos_sim), 
               size = 2,
               fill = "grey",
               col = "darkgrey",
               alpha = .5) +
  xlab("Cosine similarity") +
  ylab("Density") +
  theme_light() +
  theme(text = element_text(size = 60))) ## similarity distribution 

ggsave(paste(img_pme_SP, "/",
             "doc_cosine_sim",
             ".png",
             sep = ""),
       cos_distrib,
       height = 23, width = 40)

#### Difference distribution

for (y in c(seq(2008, 2014, 2))) {
  for (g in c(5, 7, 9)) {
    for (s in c("lp", "mat")) {
      diff_graph <- saresp_diffs %>%
        ggplot() +
        geom_density(aes(get(paste("diff", s, g, y, sep = "_"))), 
                     size = 2,
                     fill = "grey",
                     col = "darkgrey",
                     alpha = .5) +
        xlab("Test score difference") +
        ylab("Density") +
        theme_light() +
        theme(text = element_text(size = 60))
      
      ggsave(paste(img_pme_SP, "/",
                   paste("diff", s, g, y, sep = "_"),
                   ".png",
                   sep = ""),
             diff_graph,
             height = 23, width = 40)
    }
  }
}


















