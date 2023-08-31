####################
## Set up session ##
####################

# Load in packages and functions
source("0_global.R", local = TRUE)

# Load task data
clean_dat <- read.csv("clean_dat.csv")

#############################
## Omnibus model: Accuracy ##
#############################

# Run omnibus
# Would not converge with random slopes or trial + order variables
omni_acc <- glmer(acc ~ type_con_1:task_con + type_con_2:task_con +
                    type_con_1 + type_con_2 + task_con +
                    (1|item_fac) + (1|par_fac),
                  data = clean_dat,
                  family = "binomial")

#####################
## Language models ##
#####################

# Spanish task subset
span_mod <- glmer(acc ~ type_con_1 + type_con_2 + 
                    (1|item_fac) + (1|par_fac),
                  data = clean_dat %>% dplyr::filter(task_con == span_code),
                  family = "binomial")

# Palenquero task subset
palen_mod <- glmer(acc ~ type_con_1 + type_con_2 +
                     (1|item_fac) + (1|par_fac),
                   data = clean_dat %>% dplyr::filter(task_con == palen_code),
                   family = "binomial")

#################################
## Real vs. non and distractor ##
#################################

# Interaction between contrast 1 and language
int_1 <- glmer(acc ~ type_con_2:task_con +
                 type_con_1 + type_con_2 + task_con +
                 (1|item_fac) + (1|par_fac),
               data = clean_dat,
               family = "binomial")
aov_int_1 <- anova(omni_acc, int_1)
# aov_int_1

# Difference between real vs. non in Spanish task
span_1 <- glmer(acc ~ type_con_2 + 
                  (1|item_fac) + (1|par_fac),
                data = clean_dat %>% dplyr::filter(task_con == span_code),
                family = "binomial")
aov_span_1 <- anova(span_mod, span_1)
# aov_span_1

# Difference between real vs. non in Palenquero task
palen_1 <- glmer(acc ~ type_con_2 +
                   (1|item_fac) + (1|par_fac),
                 data = clean_dat %>% dplyr::filter(task_con == palen_code),
                 family = "binomial")
aov_palen_1 <- anova(palen_mod, palen_1)
# aov_palen_1 # not significant

########################
## Non vs. distractor ##
#########################

# Interaction between contrast 2 and language
int_2 <- glmer(acc ~ type_con_1:task_con +
                 type_con_1 + type_con_2 + task_con + 
                 (1|item_fac) + (1|par_fac),
               data = clean_dat,
               family = "binomial")
aov_int_2 <- anova(omni_acc, int_2)
# aov_int_2

# Difference between nonwords vs. distractors in Spanish task
span_2 <- glmer(acc ~ type_con_1 + 
                  (1|item_fac) + (1|par_fac),
                data = clean_dat %>% dplyr::filter(task_con == span_code),
                family = "binomial")
aov_span_2 <- anova(span_mod, span_2)
# aov_span_2 # not significant

# Difference between nonwords vs. distractors in Palenquero task
palen_2 <- glmer(acc ~ type_con_1 + 
                   (1|item_fac) + (1|par_fac),
                 data = clean_dat %>% dplyr::filter(task_con == palen_code),
                 family = "binomial")
aov_palen_2 <- anova(palen_mod, palen_2)
# aov_palen_2

##################
## Main effects ##
##################

# Main effect of contrast 2
main_2 <- glmer(acc ~ type_con_1:task_con + type_con_2:task_con +
                  type_con_1 + task_con + 
                  (1|item_fac) + (1|par_fac),
                data = clean_dat,
                family = "binomial")
aov_main_2 <- anova(omni_acc, main_2)
# aov_main_2

