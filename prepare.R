# 20260219
# Aim: to prepare data for LymphMutSig online platform

library(reticulate)
library(tidyverse)
use_python('/Users/hui.wan/miniconda/bin/python')
library(SigProfilerAssignmentR)
library(SigProfilerPlottingR)

setwd('/Users/hui.wan/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Documents/Project/Kataegis_2023/LymphMutSig/')

# 1. prepare R data
## siganture features
df_sbs_feat = read_tsv('data/MutSig.SBS.our.tsv')
df_dbs_feat = read_tsv('data/MutSig.DBS.our.tsv')
df_id_feat = read_tsv('data/MutSig.ID.our.tsv')


# 2. signature plots
plotSBS('data/MutSig.SBS.our.tsv', 'LymphMutSig/data/SignaturesPlots/', 'Sig' , '96', TRUE)
plotDBS('data/MutSig.DBS.our.tsv', 'LymphMutSig/data/SignaturesPlots/', 'Sig' , '78', TRUE)
plotID('data/MutSig.ID.our.tsv', 'LymphMutSig/data/SignaturesPlots/', 'Sig' , '83', TRUE)


# 3. exposure data
select_subtype = c('U-CLL', 'M-CLL',  'cMCL', 'nnMCL', 'BL', 'FL',  'GCB-DLBCL' , 'ABC-DLBCL' , 'MM' )

df_sum = read_tsv('../0.data/Sample_information.xls') %>% 
  mutate(DINUC = if_else(is.na(DINUC), 0, DINUC),
         INDEL = if_else(is.na(INDEL), 0, INDEL),
         Total_mutations = SBS + DINUC + INDEL,
         N_kataegis = if_else(is.na(N_kataegis), 0 , N_kataegis)) %>% 
  mutate(Subtype = factor(Subtype, levels = order_subtype)) %>% 
  arrange(match(Disease, order_disease), match(Subtype, order_subtype))

df_sum_f = df_sum %>% filter(Subtype %in% select_subtype)

# all mutation number / total mutations
df.exposure= df_sum_f %>% select(Subtype, Sample, S1:S16, D1:D4, ID1:ID3, K1, K2, Kapobec = N_mut_APOBEC) %>% 
  gather(Signature, value, -Sample, -Subtype) %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  group_by(Sample,Subtype) %>% mutate(pct = value/sum(value) *100) %>%
  ungroup()    %>% 
  # recalculate pct in each mut type
  mutate(MutType = if_else(grepl('^S', Signature), 'SBS', if_else(grepl('^D', Signature), 'DBS', if_else(grepl('^ID', Signature), 'Indel', 'Kataegis')))) %>% 
  group_by(Subtype, Sample, MutType) %>% mutate(pct_muttype = value/sum(value) *100) %>%
  ungroup() %>% 
  group_by(Subtype, Signature) %>% 
  # summarise(Total_sample = n_distinct(Sample), N_sample = sum(pct_muttype > 0, na.rm = T), exposure = median(pct_muttype[pct_muttype >0], na.rm = T)) %>%
  summarise(Total_sample = n_distinct(Sample), N_sample = sum(pct_muttype>0, na.rm=T), exposure = median(pct_muttype, na.rm = T)) %>%
  mutate(proportion = N_sample/Total_sample *100) %>% 
  select(Signature, Subtype, proportion, exposure)

write_tsv(df.exposure, 'data/exposure_summary.tsv')

            