####################
## Set up session ##
####################

# Load in packages and functions
source("0_global.R", local = TRUE)

# Load task data and get rt data subset
rt_dat <- read.csv("clean_dat.csv") %>% 
  dplyr::filter(rt < upper & rt > lower & acc == 1)

#######################
## Omnibus model: RT ##
#######################

# Run omnibus
omni_rt <- lmer(inv_rt ~ type_con_1:task_con + type_con_2:task_con +
                  type_con_1 + type_con_2 + task_con + trial_cent + order_con +
                  (1|item_fac) + (type_con_1:task_con + type_con_2 + task_con|par_fac),
                data = rt_dat)

#####################
## Language models ##
#####################

# Spanish task subset
span_mod <- lmer(inv_rt ~ type_con_1 + type_con_2 + trial_cent + order_con +
                    (1|item_fac) + (type_con_2|par_fac),
                  data = rt_dat %>% dplyr::filter(task_con == span_code))

# Palenquero task subset
palen_mod <- lmer(inv_rt ~ type_con_1 + type_con_2 + trial_cent + order_con +
                     (1|item_fac) + (type_con_2|par_fac),
                   data = rt_dat %>% dplyr::filter(task_con == palen_code))

#################################
## Real vs. non and distractor ##
#################################

# Interaction between contrast 1 and language
int_1 <- lmer(inv_rt ~ type_con_2:task_con +
                 type_con_1 + type_con_2 + task_con + trial_cent + order_con +
                 (1|item_fac) + (type_con_2 + task_con|par_fac),
               data = rt_dat)
aov_int_1 <- anova(omni_rt, int_1)
# aov_int_1

# Difference between real vs. non in Spanish task
span_1 <- lmer(inv_rt ~ type_con_2 + trial_cent + order_con +
                 (1|item_fac) + (type_con_2|par_fac),
               data = rt_dat %>% dplyr::filter(task_con == span_code))
aov_span_1 <- anova(span_mod, span_1)
# aov_span_1

# Difference between real vs. non in Palenquero task
palen_1 <- lmer(inv_rt ~ type_con_2 + trial_cent + order_con +
                  (1|item_fac) + (type_con_2|par_fac),
                data = rt_dat %>% dplyr::filter(task_con == palen_code))
aov_palen_1 <- anova(palen_mod, palen_1)
# aov_palen_1

#########################
## Non vs. distractor ##
#########################

# Interaction between contrast 2 and language
int_2 <- lmer(inv_rt ~ type_con_1:task_con +
                 type_con_1 + type_con_2 + task_con + trial_cent + order_con +
                 (1|item_fac) + (type_con_1:task_con + type_con_2 + task_con|par_fac),
               data = rt_dat)
aov_int_2 <- anova(omni_rt, int_2)
# aov_int_2 

# Difference between nonwords vs. distractors in Spanish task
span_2 <- lmer(inv_rt ~ type_con_1 + trial_cent + order_con +
                  (1|item_fac) + (1|par_fac),
                data = rt_dat %>% dplyr::filter(task_con == span_code))
aov_span_2 <- anova(span_mod, span_2)
# aov_span_2

# Difference between nonwords vs. distractors in Palenquero task
palen_2 <- lmer(inv_rt ~ type_con_1 + trial_cent + order_con +
                  (1|item_fac) + (1|par_fac),
                data = rt_dat %>% dplyr::filter(task_con == palen_code))
aov_palen_2 <- anova(palen_mod, palen_2)
# aov_palen_2

##################
## Main effects ##
##################

# Main effect of trial number
main_trial <- lmer(inv_rt ~ type_con_1:task_con + type_con_2:task_con +
                     type_con_1 + type_con_2 + task_con + order_con +
                     (1|item_fac) + (type_con_1:task_con + type_con_2 + task_con|par_fac),
                   data = rt_dat)
aov_main_trial <- anova(omni_rt, main_trial)
# aov_main_trial

# Main effect of task order
main_order <- lmer(inv_rt ~ type_con_1:task_con + type_con_2:task_con +
                     type_con_1 + type_con_2 + task_con + trial_cent +
                     (1|item_fac) + (type_con_1:task_con + type_con_2 + task_con|par_fac),
                   data = rt_dat)
aov_main_order <- anova(omni_rt, main_order)
# aov_main_order # not significant

