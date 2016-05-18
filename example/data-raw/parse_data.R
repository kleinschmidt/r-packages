vot_adapt <-
  read.csv('supunsup-ALL-visworld-anonymized.csv', header=TRUE) %>%
  tbl_df %>%
  separate(blockname, c('block', 'supCond', 'bvotCond'), sep='_', convert=TRUE) %>%
  separate(listId, c('wordClass', 'respCategory', 'trialSupCond'), convert=TRUE) %>%
  mutate(trialSupCond = factor(trialSupCond)) %>%
  mutate(respCat = as.factor(substr(targetId, 1, 1))) %>%
  mutate(respP = as.numeric(respCat=='p')) %>%
  mutate(trueCat = respCategory) %>%
  mutate(bvotCond = as.factor(bvotCond)) %>%
  mutate(vot = as.numeric(str_extract(stimfn, '[-0-9]+'))) %>%
  mutate(labeled = ifelse(supCond == 'unsupervised', 'unlabeled',
                          ifelse(trialSupCond == 'unsupervised', 'unlabeled', 'labeled'))) %>%
  select(-condition, -errors)

devtools::use_data(vot_adapt, overwrite=TRUE)
