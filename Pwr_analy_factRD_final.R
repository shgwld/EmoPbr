##########################################################################
## author: Sven Hegewald, ETH Zurich
## contact: sven.hegewald@eup.gess.ethz.ch
## project: The emotional underpinning of pbr: Power analysis 
## date: 14-07-2021
##########################################################################

# 1. Basic setup #########################################################
rm(list=ls())

# 2. Load packages #######################################################
library(DeclareDesign)
library(DesignLibrary)
library(knitr)
library(DT)
library(tidyverse)
library(gridExtra)

# 3. 2x2 Factorial Design ################################################
# 3.1 Model ##############################################################
N <- 1000
prob_A <- 0.5
prob_B <- 0.5
weight_A <- 0
weight_B <- 0
mean_A0B0 <- 50
mean_A0B1 <- 56.25
mean_A1B0 <- 56.25
mean_A1B1 <- 68.75
sd_i <- 1
outcome_sds <- c(15, 15, 15, 15)

population <- declare_population(N, u = rnorm(N, sd = sd_i))
potential_outcomes <- declare_potential_outcomes(Y_A_0_B_0 = mean_A0B0 +
                                                   u + rnorm(N, sd = outcome_sds[1]), Y_A_0_B_1 = mean_A0B1 +
                                                   u + rnorm(N, sd = outcome_sds[2]), Y_A_1_B_0 = mean_A1B0 +
                                                   u + rnorm(N, sd = outcome_sds[3]), Y_A_1_B_1 = mean_A1B1 +
                                                   u + rnorm(N, sd = outcome_sds[4]))

# 3.2 Inquiry ############################################################
estimand_1 <- declare_inquiry(ate_A = weight_B * mean(Y_A_1_B_1 -
                                                        Y_A_0_B_1) + (1 - weight_B) * mean(Y_A_1_B_0 - Y_A_0_B_0))
estimand_2 <- declare_inquiry(ate_B = weight_A * mean(Y_A_1_B_1 -
                                                        Y_A_1_B_0) + (1 - weight_A) * mean(Y_A_0_B_1 - Y_A_0_B_0))
estimand_3 <- declare_inquiry(interaction = mean((Y_A_1_B_1 -
                                                    Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))

# 3.3 Data strategy ######################################################
assign_A <- declare_assignment(prob = prob_A, assignment_variable = A)
assign_B <- declare_assignment(prob = prob_B, assignment_variable = B,
                               blocks = A)
reveal_Y <- declare_reveal(Y_variables = Y, assignment_variables = c(A,
                                                                     B))

# 3.4 Answer strategy ####################################################
estimator_1 <- declare_estimator(Y ~ A + B, model = lm_robust,
                                 term = c("A", "B"), inquiry = c("ate_A", "ate_B"), label = "No_Interaction")
estimator_2 <- declare_estimator(Y ~ A + B + A:B, model = lm_robust,
                                 term = "A:B", inquiry = "interaction", label = "Interaction")

# 3.5 Design #############################################################
two_by_two_design <- population + potential_outcomes + estimand_1 +
  estimand_2 + estimand_3 + assign_A + assign_B + reveal_Y +
  estimator_1 + estimator_2

# 3.6 Evaluation #########################################################
change_the_parameters_t1 <- 
  two_by_two_design %>% 
  redesign(N = list(900, 1000, 1100, 1200, 1300, 1400, 1500),
           mean_A0B1 = list(59.375, 62.5, 65.625)) %>% 
  diagnose_design(sims = 1000) 

change_the_parameters_t1 %>% 
  get_diagnosands() %>% 
  kable()

change_the_parameters_t2 <- 
  two_by_two_design %>% 
  redesign(N = list(900, 1000, 1100, 1200, 1300, 1400, 1500),
           mean_A1B0 = list(59.375, 62.5, 65.625)) %>% 
  diagnose_design(sims = 1000) 

change_the_parameters_t2 %>% 
  get_diagnosands() %>% 
  kable()

change_the_parameters_t3 <- 
  two_by_two_design %>% 
  redesign(N = list(900, 1000, 1100, 1200, 1300, 1400, 1500),
           mean_A1B1 = list(68.75, 75, 81.25)) %>% 
  diagnose_design(sims = 1000) 

change_the_parameters_t3 %>% 
  get_diagnosands() %>% 
  kable()

# 3.7 Illustration #######################################################
### Main effects: mean_A0B1 (T1) 
df_T1 <- get_diagnosands(change_the_parameters_t1) %>%
  mutate(N = factor(N),
         mean_A0B1 = factor(mean_A0B1)) 
df_T1$ate <- NA
df_T1$ate[df_T1$mean_A0B1=="59.375"] <- 9.375
df_T1$ate[df_T1$mean_A0B1=="62.5"] <- 12.5
df_T1$ate[df_T1$mean_A0B1=="65.625"] <- 15.625
df_T1$ate <- as.factor(df_T1$ate)
df_T1 <- filter(df_T1, term=="B")

Fig1.1 <- ggplot(data = df_T1, aes(N, power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = power - 1.96*`se(power)`, 
                    ymax = power + 1.96*`se(power)`,
                    width = .3),
                position = position_dodge(width = .9)) +
  geom_hline(yintercept = 0.8) +
  labs(title = "T1: Power at each N and ate",
       subtitle = "95% Confidence Intervals")

### Main effects: mean_A1B0 (T2) 
df_T2 <- get_diagnosands(change_the_parameters_t2) %>%
  mutate(N = factor(N),
         mean_A1B0 = factor(mean_A1B0)) 
df_T2$ate <- NA
df_T2$ate[df_T2$mean_A1B0=="59.375"] <- 9.375
df_T2$ate[df_T2$mean_A1B0=="62.5"] <- 12.5
df_T2$ate[df_T2$mean_A1B0=="65.625"] <- 15.625
df_T2$ate <- as.factor(df_T2$ate)
df_T2 <- filter(df_T2, term=="A")

Fig1.2 <- ggplot(data = df_T2, aes(N, power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = power - 1.96*`se(power)`, 
                    ymax = power + 1.96*`se(power)`,
                    width = .3),
                position = position_dodge(width = .9)) +
  geom_hline(yintercept = 0.8) +
  labs(title = "T2: Power at each N and ate",
       subtitle = "95% Confidence Intervals")

### Interaction effect: mean_A1B1 (T1*T2) 
df_T1_T2 <- get_diagnosands(change_the_parameters_t3) %>%
  mutate(N = factor(N),
         mean_A1B1 = factor(mean_A1B1)) 
df_T1_T2$ate <- NA
df_T1_T2$ate[df_T1_T2$mean_A1B1=="68.75"] <- 18.75
df_T1_T2$ate[df_T1_T2$mean_A1B1=="75"] <- 25
df_T1_T2$ate[df_T1_T2$mean_A1B1=="81.25"] <- 31.25
df_T1_T2$ate <- as.factor(df_T1_T2$ate)
df_T1_T2 <- filter(df_T1_T2, term=="A:B")

Fig1.3 <- ggplot(data = df_T1_T2, aes(N, power, fill = ate)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = power - 1.96*`se(power)`, 
                    ymax = power + 1.96*`se(power)`,
                    width = .3),
                position = position_dodge(width = .9)) +
  geom_hline(yintercept = 0.8) +
  labs(title = "T1*T2: Power at each N and ate",
       subtitle = "95% Confidence Intervals")

png(file="\\\\gess-fs.d.ethz.ch/home$/shegewald/Desktop/Figure1.png",
    width = 13, height = 9, units = 'in', res = 500)
grid.arrange(Fig1.1, Fig1.2, Fig1.3, 
             ncol = 3, nrow = 1)
dev.off()  
