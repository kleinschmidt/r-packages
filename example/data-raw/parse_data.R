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



## detect repeat subjects
repeat_subjects <-
  data %>% 
    group_by(assignmentid,submittime,subject) %>%
    summarise() %>%
    group_by(subject) %>%
    mutate(rank=row_number(as.POSIXct(strptime(submittime, "%a %b %d %X EDT %Y")))) %>%
    arrange(subject,submittime)


## detect subjecs who don't classify well

contrasts(data$trialSupCond) <-
  matrix(c(1, -1), nrow=2,
         dimnames = list(c('sup', 'unsup'), 'sup'))

## Fit GLM to each subject/assignment
bysub_withsup_glms <- data %>%
  group_by(subject, assignmentid) %>%
  do(fit = glm(respP ~ vot * trialSupCond, data=., family='binomial'))

## Get fitted log-odds for 0ms and 70ms stimuli, find minimum correct log-odds,
## and mark people for exclusion if the minimum correct less than logit(80%)
bad_classification <-
  bysub_withsup_glms %>%
  mutate(lo0ms = coef(fit)[1], lo70ms = coef(fit)[1] + 70*coef(fit)[2]) %>%
  select(subject, assignmentid, lo0ms, lo70ms) %>%
  mutate(loMinCorrect = min(lo0ms * -1, lo70ms)) %>%
  mutate(exclude80PercentAcc = loMinCorrect < qlogis(0.8))


excludes <- full_join(filter(bad_classification, exclude80PercentAcc),
                      filter(repeat_subjects, rank>1))

devtools::use_data(excludes, overwrite=TRUE)

vot_adapt_clean <- data %>%
  anti_join(excludes, by=c('subject', 'assignmentid'))

devtools::use_data(vot_adapt_clean, overwrite=TRUE)
