

# taking top most expressed genes (testing) ##########################################

# read in

mine <- read.csv('deseq2_results.csv')

#ps <- read.csv('DESeq2 PS Results lfc shrinkage applied.csv')

#ps_no_shrinkage_applied <- read.csv('PS - Galaxy123-[DESeq2 DEG Plasmidsaurus data].csv')

# look to check that there are statistically relevant genes

mine %>% # yes
  filter(is.na(p.adj) == FALSE) %>%
  mutate(abs_log2fc = abs(log2fc)) %>%
  arrange(desc(abs_log2fc)) %>%
  slice(1:10)

ps %>% # no
  filter(is.na(p.adj) == FALSE) %>%
  mutate(abs_log2fc = abs(log2fc)) %>%
  arrange(p.adj) %>%
  select(c('gene.id', 'base.mean', 'log2fc', 'lfcSE', 'p.value', 'p.adj')) %>%
  slice(1:5)

ps_no_shrinkage_applied %>% # no
  filter(is.na(P.adj) == FALSE) %>%
  mutate(abs_log2fc = abs(log2FC)) %>%
  arrange(P.adj) %>%
  select(c('gene.id', 'base.mean', 'log2FC', 'StdErr', 'P.value', 'P.adj')) %>%
  slice(1:10)

# very different results - there are no genes that are statistically 
# different accordinf to p.adj in ps....

# ===================================================================================

# compare base means again after lfc shrinkage (shouldnt change)

mine %>%
  arrange(desc(base.mean)) %>%
  slice(1:10)

ps %>%
  arrange(desc(base.mean)) %>%
  slice(1:10)

# yes these are the exact same... good as it means that the two pipelines
# agree on the level of expression across different genes

# ====================================================================================

# get list of most DEGs

mine %>%
  filter(is.na(p.adj) == FALSE) %>%
  mutate(abs_log2fc = abs(log2fc)) %>%
  arrange(desc(abs_log2fc)) %>%
  slice(1:5)

ps %>%
  filter(is.na(p.adj) == FALSE) %>%
  mutate(abs_log2fc = abs(log2fc)) %>%
  arrange(desc(abs_log2fc)) %>%
  slice(1:5)


# ====================================================================================


# functional enrichment analysis

library(tidyverse)
library(RColorBrewer) # colour plot
library(pheatmap) # heat map
library(clusterProfiler) # for enrichment
library(enrichplot) # for visualisations
library(ggupset) # for visualisations
library(msigdbr) # for easily downloading gene sets/pathways
library(ggrepel) # annotating volcano plot


# using my data at first

# reformat data frame of DEGs for GO -----------------------------------------------

# select cols
data <- mine %>%
  select(gene_id, p.value, p.adj, log2fc)

# remove decimals from emsembl ids - remove?
data$gene_id <- sub("\\..*$", "", data$gene_id)

# add another column to determine up or down regulation
data <- data %>% mutate(diffexpressed = case_when(
  log2fc > 0 & p.adj < 0.05 ~ 'UP',
  log2fc < 0 & p.adj < 0.05 ~ 'DOWN',
  p.adj > 0.05 ~ 'NO'
))

# remove non significant genes - from 86,411 to 72,625
data_sig <- data[data$diffexpressed != 'NO', ] 

# remove genes that were not run by deseq2 - down to 381
data_sig <- data_sig %>%
  filter(is.na(p.adj) == FALSE)

# get external list of ids from ensembl id
id_list <- read.csv('geneids_genesymbols.csv')

# match gene symbols to gene ids
data_sig <- left_join(data_sig, id_list, by = c('gene_id' = 'gene_id'))

# check how many are being missed - only 4 out of 381 genes
# these are genes that are poorly characterised or understood, and will
# be excluded from analysis
data_sig %>%
  filter(is.na(p.value) == FALSE) %>%
  filter(is.na(gene_symbol) == TRUE)

# filter out where gene symbols were not able to be mapped with ensembl ids
data_sig <- data_sig %>%
  filter(is.na(gene_symbol) == FALSE)

# check original data too - 14167 reads were given a p-value or run statistically... this is a good thing?
mine %>%
  filter(is.na(p.adj) == FALSE)

# now reorganise cols
data_sig <- data_sig %>%
  select(gene_symbol, p.value, p.adj, log2fc, diffexpressed)

# split data into up and downregulated data frames as lists
deg_results_list <- split(data_sig, data_sig$diffexpressed)

# reading in and formatting pathway data ------------------------------------------

# grab the hallmark gene sets/pathways from msigdb
gene_sets_df <- msigdbr(species = 'Homo sapiens', category = 'H')

# select out columns for GO
gene_sets <- gene_sets_df %>%
  select(gs_name, gene_symbol)

# drop 'hallmark' from the start of every gene category (redundant and 
# clutters graph)
gene_sets$gs_name <- gsub('HALLMARK_', '', gene_sets$gs_name)

# selecting cell line specific genes ----------------------------------------------

# not doing for now but might come back to later

# run clusterProfiler ---------------------------------------------------------------

res <- lapply(names(deg_results_list),
              function(x) enricher(gene = deg_results_list[[x]]$gene_symbol,
                                   TERM2GENE = gene_sets,
                                   #universe = background_genes,
                                   # Let's use the cut-offs later to filter ORA results
                                   #pvalueCutoff = 0.05,
                                   #minGSSize = 5,
                                   qvalueCutoff = 0.2))

# rename results
names(res) <- names(deg_results_list)


# convert results to readable dataframe
res_df <- lapply(names(res), function(x) rbind(res[[x]]@result))
names(res_df) <- names(res)
res_df <- do.call(rbind, res_df)
head(res_df)

# save
write.csv(res_df, 'FEA_results.csv')

# Visualise Results ================================================================

# uses results direct from the enricher function

# LOOKING AT DIFFERENCIALLY UPREGULATED

results <- res$UP

# bar plot - include
p1 <- barplot(results, showCategory = 10, 
              title = paste("MSigDB Functional Enrichment of Upregulated genes"))
p1 

# now lets look at down regulated

results <- res$DOWN # only one - hallmark kras signalling down

p1 <- barplot(results, showCategory = 10, 
              title = paste("MSigDB Functional Enrichment of Downregulated Genes"))
p1 

# volcano plot ===================================================================

# use previous 'mine' df from before which contains non signficant DEGs
data_for_vp <- mine %>%
  select(gene_id, p.value, p.adj, log2fc)

# get external list of ids from ensembl id
id_list <- read.csv('geneids_genesymbols.csv')

# join gene symbols to df
data_for_vp <- left_join(data_for_vp, id_list, by = c('gene_id' = 'gene_id'))

# check for unassigned genes - 29514 genes were not identified
data_for_vp %>%
  filter(is.na(gene_symbol) == TRUE)

# label as un/down regulated for plot
data_for_vp <- data_for_vp %>% mutate(diffexpressed = case_when(
  log2fc > 0.6 & p.adj < 0.05 ~ 'Upregulated',
  log2fc < -0.6 & p.adj < 0.05 ~ 'Downregulated',
  .default = 'Non-Significant'
))

# add label for top 30 most differenciated genes
data_for_vp$delabel <- ifelse(data_for_vp$gene_symbol %in% head(data_for_vp[order(data_for_vp$p.adj), "gene_symbol"], 30), data_for_vp$gene_symbol, NA)


# volcano plot
ggplot(data = data_for_vp, aes(x = log2fc, y = -log10(p.adj), colour = diffexpressed, label = delabel)) +
  geom_vline(xintercept = c(-0.6, 0.6), col = "gray", linetype = 'dashed') +
  geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
  geom_point() +
  scale_colour_manual(values = c('red', 'grey', 'blue')) +
  theme_minimal() +
  geom_text_repel(max.overlaps = Inf) # most differentially expressed gene labels

# you will get two warning messages here: the first is telling you that 
# a bunch of genes have NA in p.adj values (were not abdunant enough
# to be tested by deseq2 - this is fine)

# the other is telling you that you have NA in the labels for all genes
# except the 30 plotted - again this is normal and is to ensure that
# only the 30 top most DEGs are labelled on the plot, rather than
# every gene


