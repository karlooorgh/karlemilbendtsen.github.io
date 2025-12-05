library(tidyverse)
library(fixest)
library(panelView)
library(did)
library(bacondecomp)
library(tidyverse)
library(haven);library(modelsummary);library(performance)
library(lfe); library(broom); library(xtable);library(dplyr)


K2 = read_csv("20251014samlet.csv")


tab <- data.frame(table(K2$Sagsemne)) %>%
  arrange(desc(Freq))
latex_tab <- xtable(tab,
                    caption = "Distribution of case types",
                    label = "tab:casetype")
print(latex_tab,
      include.rownames = FALSE,
      file = "plots/casetype_table.tex")

##### STAD BETYDER FEJL!
test = K2 %>% group_by(Sagsemne, quarter_start) %>% summarize(stad= sum(stad))
sum(test$stad)
table(K2$CaseType,K2$stad)



missing_tolerance = 1000
K2 = K2 %>% group_by(Sagsemne) %>% 
  mutate(n = n_distinct(DatePeriode)) %>% 
  filter(n > 46-missing_tolerance)

# quarter_values <- K2 %>%
#   select(quarter_start) %>%
#   distinct() %>%
#   arrange(quarter_start) %>%
#   mutate(quarter_index = row_number() - 1)  # Start from 0
# 
# K2 <- K2 %>%
#   left_join(quarter_values, by = c("Sagsemne", "quarter_start")

K2 <- K2 %>% ungroup() %>% 
  mutate(quarter_index = as.numeric(as.factor(DatePeriode))) %>% 
  group_by(Sagsemne) %>%
  arrange(quarter_index) %>% 
    mutate(start_length = first(length, order_by = quarter_index),
         length_relative = length / start_length,
         new_length_change = length / lag(length),
         start_para = first(paragraf_henvisning, order_by = quarter_index) + 1,
         para_relative = (paragraf_henvisning + 1) / start_para)

summary(K2$length_relative)
test = K2 %>% filter(length_relative<0.3) # Scrapingfejl, skal rettes
K2 = K2 %>% filter(length_relative>0.3)

K2 %>% group_by(Lovgivning) %>% 
  summarise(n = n(),
            sager = n_distinct(Sagsemne),
            change = mean(length_relative))


### RÅDATA
# missing_tolerance = 0
# mean_overall = K2 %>% filter(n>46-missing_tolerance) %>% ungroup() %>% summarise(mean_overall = mean(stad)) %>% pull(1)
# mean_overall
# raw_plot = K2 %>% filter(n>=46-missing_tolerance) %>% group_by(Sagsemne) %>% 
#   summarise(mean = mean(stad),
#             mean_length = mean(length_relative)) %>% 
#   filter(mean_length<2,
#          mean_length>1) %>% 
#   ungroup()
# 
# raw_plot$mean_length
# 
# raw_plot %>% filter(mean_length!=1) %>% ggplot(aes(y = mean, x = mean_length))+
#   geom_point()+
#   geom_hline(yintercept = mean_overall)+
#   geom_smooth()
 



### PANELVIEW ----
thresh = 1.01
missing_tolerance = 23
K2nomissing = K2 %>% group_by(Sagsemne) %>% 
  mutate(n = n_distinct(DatePeriode)) %>% 
  filter(n > 46-missing_tolerance)

length(unique(K2$Sagsemne))

K_complete = K2nomissing %>% 
  group_by(Sagsemne, quarter_start) %>% 
  mutate(stad_pro = sum(stad)/n(),
         new_change = if_else(length_relative>thresh,1,0)) 


K_panel_plot = K_complete %>% 
  select(stad_pro, new_change, Sagsemne, quarter_index) %>% 
  distinct()

# Filter out duplicate observations based on Sagsemne and quarter_start
K_panel_plot <- K_panel_plot %>%
  group_by(Sagsemne, quarter_index) %>%
  filter(n() == 1) %>%
  ungroup()

panelview(stad_pro ~ new_change,
          data = K_panel_plot, index = c("Sagsemne", "quarter_index"))


#### PANEL_VIEW DISCRETION ----
thresh = 1.01
c("long_subject_nchar", "num_adjectives", "num_conditionals","entitlement","mandatory_delegation","permissive_delegation",
  "other","constraint","additional_mandatory_delegation")
names(K2)
K2 = K2 %>% mutate(discretion_ratio = (permissive_delegation)/(additional_mandatory_delegation+mandatory_delegation+1))

K2nomissing = K2 %>% group_by(Sagsemne) %>% 
  mutate(n = n_distinct(DatePeriode))

missing_tolerance = 23
K_complete = K2nomissing %>% 
  filter(n >=46-missing_tolerance) %>% 
  group_by(Sagsemne, quarter_start) %>% 
  mutate(stad_pro = sum(stad)/n(),
         start_ratio = discretion_ratio[quarter_start==min(quarter_start)],
         new_change = if_else(change==1,1,0))


K_panel_plot = K_complete %>% 
  select(stad_pro, new_change, Sagsemne, quarter_index) %>% 
  distinct()

# Filter out duplicate observations based on Sagsemne and quarter_start
K_panel_plot <- K_panel_plot %>%
  group_by(Sagsemne, quarter_index) %>%
  filter(n() == 1) %>%
  ungroup()

panelview(stad_pro ~ new_change,
          data = K_panel_plot, index = c("Sagsemne", "quarter_index"))
ggsave("plots/panelview.pdf")


### SIDSTE PANELVIEW ----

K2change = K2 %>% select(Sagsemne,DatePeriode,length) %>% 
  arrange(Sagsemne,DatePeriode) %>% 
  mutate(length_change = (length-lag(length))/lag(length)) %>% 
  filter(length_change!=0)
nrow(K2change)
summary(K2change$length_change)


K_panel_plot = K2 %>% select(Sagsemne,quarter_index,length,n) %>%
  distinct() %>% 
  arrange(Sagsemne,quarter_index) %>% 
  mutate(length_change = (length-lag(length))/lag(length),
         length_change = ifelse(is.na(length_change),0,length_change),
         length_change = ifelse(quarter_index==0,0,length_change),
         length_change_binary = (length_change>0.2)*1)  %>% 
  ungroup() %>% 
  group_by(Sagsemne) %>% 
  mutate(length_change_binary = ifelse(quarter_index==min(quarter_index),0,length_change_binary)) %>% 
  select(length_change, length_change_binary,Sagsemne, quarter_index,length,n) %>% 
  distinct()

# Filter out duplicate observations based on Sagsemne and quarter_start
K_panel_plot <- K_panel_plot %>%
  filter(n>45) %>% 
  group_by(Sagsemne, quarter_index) %>%
  filter(n() == 1) %>%
  ungroup()

panelview(length ~ length_change_binary,
          data = K_panel_plot, index = c("Sagsemne", "quarter_index"))

# K2 <- K2 %>%
#   group_by(Sagsemne) %>%
#   mutate(new_change = if_else(LIX_score_running>thresh,1,0),
#          min_quarter_start = min(quarter_index[new_change == 1], na.rm = TRUE),
#          min_change_start = min(quarter_index[change == 1], na.rm = TRUE)) %>%
#   ungroup()
K2 <- K2 %>%
  group_by(Sagsemne) %>%
  mutate(new_change = if_else(length_relative>thresh,1,0),
         min_quarter_start = min(quarter_index[new_change == 1], na.rm = TRUE),
         min_change_start = min(quarter_index[change == 1], na.rm = TRUE)) %>%
  ungroup()

K2 %>%
  group_by(Sagsemne) %>%
  mutate(ever_change = (mean(new_change) == 0) * 1) %>%
  group_by(ever_change, quarter_index) %>%
  summarise(
    mean = mean(stad),
    se = sd(stad) / sqrt(n()),  # Calculate standard error for CI
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean - 1.96 * se,  # Lower bound of 95% CI
    upper = mean + 1.96 * se   # Upper bound of 95% CI
  ) %>%
  ggplot(aes(x = quarter_index, y = mean, col = as.factor(ever_change), fill = as.factor(ever_change))) +
  geom_line(size = 1.2) +  # Thicker line for better visibility
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +  # Add ribbon for CI
  scale_color_manual(values = c("darkgrey", "black"), labels = c("No Change", "Change")) +
  scale_fill_manual(values = c("darkgrey", "black"), labels = c("No Change", "Change")) +
  labs(
    title = "Proportion of Administrative Mistakes Over Quarters by Change Status with 95% Confidence Intervals",
    x = "Quarter Index",
    y = "Proportion Mistakes",
    color = "Change Status",
    fill = "Change Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "bottom"  # Move legend to the bottom
  )
ggsave("plots/change_group_differences.pdf")

# OVER TID----
overtidgraf = K2 %>%
  group_by(DatePeriode) %>%
  summarise(
    mean = mean(stad),
    se = sd(stad) / sqrt(n()),  # Calculate standard error for CI
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean - 1.96 * se,  # Lower bound of 95% CI
    upper = mean + 1.96 * se   # Upper bound of 95% CI
  ) %>%
  ggplot(aes(x = DatePeriode, y = mean)) +
  geom_line(size = 1.2) +  # Thicker line for better visibility
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +  # Add ribbon for CI
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1) # from 0% to 100% in steps of 10%
  )+
  coord_cartesian(ylim = c(0.25,0.5))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # change "1 year" to "3 months" for quarterly
  labs(
    # title = "Proportion of Administrative Mistakes Over Quarters by Change Status with 95% Confidence Intervals",
    x = "",
    y = "Proportion Errors",
    color = "Change Status",
    fill = "Change Status"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "bottom"  # Move legend to the bottom
  )
overtidgraf 
ggsave("plots/overtidgraf.pdf",overtidgraf, width = 12, height = 8)


### AMOUNT ----
amount_overtid = K2 %>%
  group_by(DatePeriode) %>%
  summarise(
    sum = n()
  ) %>%
  mutate(
    se = sqrt(sum),                          # Poisson SE
    lower = sum - 1.96 * se,
    upper = sum + 1.96 * se
  ) %>%
  ggplot(aes(x = DatePeriode, y = sum)) +
  geom_line(size = 1.2) +  # Thicker line for better visibility
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +  # Add ribbon for CI
  coord_cartesian(ylim = c(0,3500))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # change "1 year" to "3 months" for quarterly
  labs(
    # title = "Proportion of Administrative Mistakes Over Quarters by Change Status with 95% Confidence Intervals",
    x = "",
    y = "Amount of errors per quarter",
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "bottom"  # Move legend to the bottom
  )
amount_overtid 
ggsave("plots/amount_overtid.pdf",amount_overtid, width = 12, height = 8)

# INDEX100 plot ----
test = K2 %>% 
  # filter(n==46) %>% 
  group_by(Sagsemne,DatePeriode) %>% select(Sagsemne,DatePeriode,length_relative) %>% 
  group_by(Sagsemne,DatePeriode) %>% 
  summarise(length_indexed = mean(length_relative))

test = K2 %>% 
  # filter(n==46) %>% 
  group_by(Sagsemne,DatePeriode) %>% select(Sagsemne,DatePeriode,length) %>% 
  group_by(Sagsemne) %>% 
  summarise(length_indexed = mean(length))

index100plot = K2 %>% filter(n==46) %>% group_by(Sagsemne,DatePeriode) %>% select(Sagsemne,DatePeriode,length_relative) %>% 
  # filter(mean(length_relative)!=1) %>% 
  group_by(DatePeriode) %>% 
  summarise(length_indexed = mean(length_relative))%>% 
  ggplot(aes(x = DatePeriode, y = length_indexed))+
  geom_line()+
  theme_minimal()
index100plot
ggsave("plots/index100plot.pdf",index100plot)

K2$length_change

K2 %>% filter(n==46) %>% group_by(Sagsemne,DatePeriode) %>% select(Sagsemne,DatePeriode,length_change) %>% 
  group_by(DatePeriode) %>% 
  summarise(length_indexed = mean(length_change))%>% 
  ggplot(aes(x = DatePeriode, y = length_indexed))+
  geom_point()+
  theme_minimal()

test =  K2 %>% filter(n==46) %>% group_by(Sagsemne,DatePeriode) %>% select(Sagsemne,DatePeriode,length_change) %>% distinct()
### 
K_complete %>% group_by(Sagsemne, quarter_index) %>% 
  summarise(mean = mean(length_relative)) %>% 
  group_by(quarter_index  ) %>% 
  summarise(mean = mean(mean)) %>% 
  ggplot(aes(x=quarter_index,y=mean))+
  geom_point()

K2 %>% group_by(quarter_index) %>% summarise(n = n()) %>% arrange(desc(n))


mean(K2$min_quarter_start %>% na.omit())
table(K2$quarter_index)
table(K2$min_quarter_start)


 ### UDEN PROCENT ----
names(K)
summary(K$DatePeriode)


# Assuming K is your data frame and 'length' is the variable of interest


plot_hist_with_normal <- function(data, var) {
  # Calculate mean, standard deviation, and bin width
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  bin_width <- 15 * IQR(data[[var]], na.rm = TRUE) / length(data[[var]])^(1/3)
  
  # Calculate quantiles
  quantiles <- quantile(data[[var]], probs = c(0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  
  # Plot
  ggplot(data, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), binwidth = bin_width, color = "black", fill = "grey") + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean_val, sd = sd_val), 
                  color = "black", size = 1) + 
    geom_vline(xintercept = quantiles, linetype = "dashed", color = "black") +
    labs(title = paste("Histogram and Normal Distribution of", var),
         x = var, 
         y = "Density") +
    theme_minimal()
}
quantile(K2$length_relative,0.9)

# Plot different variables:
plot_hist_with_normal(K2, "length_relative")

summary(K2$log_para)
K2$loglength = log(K2$length)
K2$log_para = log(K2$paragraf_henvisning)
plot_hist_with_normal(K2, "log_para")
ggsave("plots/log_para.pdf")
plot_hist_with_normal(K2, "length")
ggsave("plots/length.pdf")
plot_hist_with_normal(K2, "loglength")
ggsave("plots/loglength.pdf")
plot_hist_with_normal(K2, "LIX_score")
ggsave("plots/LIX.pdf")
plot_hist_with_normal(K2, "paragraf_henvisning")
ggsave("plots/paragraf_henvisning.pdf")
plot_hist_with_normal(K2, "log_para")
ggsave("plots/log_para.pdf")

# plot_hist_with_normal(K2, "LIX_score_running") # Der er noget helt galt her
# plot_hist_with_normal(K2, "length_relative")


# K2 = K2 %>% group_by(Kommune) %>% mutate(w = sum(Realitetsbehandledesagerialt))
library(DIDmultiplegtDYN)
K2 = K2 %>% mutate(error = stad,
                   CaseTopic = Sagsemne,
                   Municipality = Kommune,
                   Time = DatePeriode) 


library(dplyr)


desc_did <- K2 %>%
  group_by(CaseTopic) %>%
  arrange(Time, .by_group = TRUE) %>%
  summarise(
    log_change = log(length) - lag(log(length)),
    log_change_crossrefs = log(paragraf_henvisning) - lag(log(paragraf_henvisning)),
    Time = as.factor(Time))%>% distinct() %>% filter(log_change!=0,
                                                     log_change>0) %>% 
  ungroup()

summary(desc_did$log_change)
quantile(desc_did$log_change,0.999,na.rm=T)

did_test <- K2 %>%
  group_by(CaseTopic) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(
    log_change = log(length) - lag(log(length)),
    # log_change = log(paragraf_henvisning) - lag(log(paragraf_henvisning)),
    Time = as.factor(Time))%>%
  ungroup()



### ONE PERIOD----
res_norm <- did_multiplegt_dyn(
  df        = did_test,
  outcome   = "error",
  group     = "CaseTopic",
  time      = "Time",
  treatment = "log_change",
  effects   = 1,
  placebo   = 1,
  # normalized = TRUE,          # per-unit-of-treatment effects
  same_switchers = TRUE,
  cluster   = "CaseTopic",
  graph_off = F
)
summary(res_norm)
p <- res_norm$plot
p$layers[[1]]$aes_params$colour <- "grey20"  # e.g., first layer (line)
p$layers[[2]]$aes_params$colour <- "grey20"  # e.g., error bar
p$layers[[3]]$aes_params$colour <- "black"  # POINT
p$layers[[1]]$aes_params$size <- 1  # e.g., first layer (line)
p$layers[[2]]$aes_params$size <- 1  # e.g., error bar
p$layers[[3]]$aes_params$size <- 1.5 # e.g., error bar


class(res_norm$plot)   # should be "gg" "ggplot"
res_norm$plot$layers
p = p + geom_hline(yintercept = 0, alpha = 0.5)+
  scale_x_continuous(breaks = c(-1,0,1))+
  theme_minimal(base_size = 24) +
  labs(
    x = "Quarters relative to law change",
    y = "Effect on error rate",
    title = ""
  ) +
  scale_color_grey(start = 0.2, end = 0.8) +
  scale_fill_grey(start = 0.8, end = 0.95)+
  theme(legend.position = "top")
p
ggsave("plots/dynamic_did_one_period.pdf",p,width=12,height=8)


##### 2 YEARS
res_norm <- did_multiplegt_dyn(
  df        = did_test,
  outcome   = "error",
  group     = "CaseTopic",
  time      = "Time",
  treatment = "log_change",
  effects   = 6,
  placebo   = 6,
  # normalized = TRUE,          # per-unit-of-treatment effects
  same_switchers = TRUE,
  cluster   = "CaseTopic",
  graph_off = F
)
summary(res_norm)
p <- res_norm$plot
p$layers[[1]]$aes_params$colour <- "grey20"  # e.g., first layer (line)
p$layers[[2]]$aes_params$colour <- "grey20"  # e.g., error bar
p$layers[[3]]$aes_params$colour <- "black"  # POINT
p$layers[[1]]$aes_params$size <- 1  # e.g., first layer (line)
p$layers[[2]]$aes_params$size <- 1  # e.g., error bar
p$layers[[3]]$aes_params$size <- 1.5 # e.g., error bar


class(res_norm$plot)   # should be "gg" "ggplot"
res_norm$plot$layers
p = p + geom_hline(yintercept = 0, alpha = 0.5)+
  scale_x_continuous(breaks = seq(-6,6))+
  theme_minimal(base_size = 24) +
  labs(
    x = "Quarters relative to law change",
    y = "Effect on error rate",
    title = ""
  ) +
  scale_color_grey(start = 0.2, end = 0.8) +
  scale_fill_grey(start = 0.8, end = 0.95)+
  theme(legend.position = "top")
p
ggsave("plots/dynamic_did_2_years.pdf",p,width=12,height=8)

unique(test$Sagsemne)





#### HOVEDMODELLER #### ----
K2 = K2 %>% mutate(error = stad,
              CaseTopic = Sagsemne,
              Municipality = Kommune,
              Time = DatePeriode) 
K2 = K2 %>% mutate(mun_time = paste0(Time,Municipality))
K2$Lovgivning

K2 = K2 %>% group_by(CaseTopic,Time,Municipality) %>% 
  mutate(amount = n())
FE_model_length <- feols(
  error ~ log(length)| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
FE_model_length %>% summary()


FE_model_length <- feols(
  error ~ log(length)| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
FE_model_length %>% summary()


##### HURTIGE DIAGNOSTICS TING----
# Predict probabilities from your logit model
K2$predicted <- predict(FE_model_length, type = "response")

# Residualize predictor
res_x <- resid(feols(log(length) ~ 1 | CaseTopic + Municipality + Time, data = K2))

# Combine
df_plot <- data.frame(res_x = res_x, predicted = K2$predicted)

plot_hist_with_normal(df_plot,"res_x")

sumstats = K2 %>% group_by(Sagsemne) %>% summarise(mean_length = mean(length_relative),
                                                   n = n(),
                                                   error = mean(error))

df = df %>% mutate(Sagsemne = i) %>% left_join(sumstats)
# DEN HER TAGER LANG TID
# df_plot %>% ggplot(aes(x=res_x, y = predicted))+
#   geom_point(color = "grey40") +
#   geom_smooth(method = "lm", se = T, color = "black") +
#   geom_hline(aes(yintercept = mean(predicted)))+
#   # geom_smooth(se = F)+
#   # scale_x_continuous(limits = c(-1,1))+
#   # scale_x_continuous(limits = c(-0.5,0.5))+
#   # coord_cartesian(xlim = c(-0.5,0.5))+
#   theme_minimal() +
#   labs(
#     x = "Residualized log(length)",
#     y = "Predicted probability of error",
#     # title = "Partial relationship between law length and error rate",
#     # subtitle = "Controlling for case topic, municipality, and time FE"
#   )

# Bin predictor and compute mean predicted prob in each bin
library(dplyr)
df_bin <- df_plot %>%
  mutate(bin = cut(res_x, breaks = 100)) %>%
  group_by(bin) %>%
  summarise(x = mean(res_x), y = mean(predicted),
            n = n())

fit <- lm(y ~ x, data = df_bin, weights = n)

# Plot
partial_reg = ggplot(df_bin, aes(x = x, y = y)) +
  geom_point(aes(size = n), color = "grey40") +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "black") +
  theme_minimal(base_family = "sans",
                base_size = 24) +
  # coord_cartesian(xlim=c(-1,1))+
  geom_hline(aes(yintercept = mean(df_plot$predicted)),
             linetype = "dashed", color = "grey50")+
  labs(
    x = "Residualized log(length)",
    y = "Mean predicted probability of error within bin",
    # title = "Partial relationship between law length and error rate",
    # subtitle = "Controlling for case topic, municipality, and time FE"
  )
partial_reg
ggsave("plots/partial_reg_within_bins.pdf",partial_reg, width = 12, height = 8)

# choose number of bins
nbins <- 20

df_uncond <- K2 %>% ungroup() %>% 
  mutate(x = log(length)) %>%
  filter(is.finite(x), !is.na(error)) %>%
  mutate(bin = cut_number(x, nbins, labels = FALSE)) %>%
  group_by(bin) %>%
  summarise(
    x_mid      = mean(x, na.rm = TRUE),     # bin midpoint (mean of x in bin)
    mean_error = mean(error, na.rm = TRUE), # unconditional mean of outcome
    n          = dplyr::n(),                # bin size
    .groups    = "drop"
  )

uncond_means_plot = ggplot(df_uncond, aes(x = x_mid, y = mean_error)) +
  # geom_smooth()+
  geom_smooth(method = "lm",se=F,col="black")+
  geom_point(aes(size = n), color = "grey40") +
  geom_hline(yintercept = mean(K2$error, na.rm = TRUE),
             linetype = "dashed", color = "grey50") +
  # scale_size_continuous(range = c(1.5, 6), guide = guide_legend(title = "Obs. per bin")) +
  theme_minimal(base_family = "sans",
                base_size = 24) +  
  theme(legend.position = "top")+
  labs(
    x = "log(length) (binned)",
    y = "Unconditional mean error",
    # title = "Unconditional relationship between law length and error rate",
    # subtitle = paste0(nbins, " equal-frequency bins; dashed line = overall mean error")
  )
uncond_means_plot
ggsave("plots/unconditional_means.pdf",uncond_means_plot, width = 12, height = 8)



### HVIS JEG INKLUDERER ANTAL----
Kgrouped = K2 %>%
  group_by(CaseTopic, length, Time) %>%
  mutate(
    antal = n(),
    omg_procent = sum(stad) / antal
  ) %>%
  ungroup() %>%
  group_by(CaseTopic) %>%
  mutate(snit = mean(antal))

FE_model_length_mantal <- feols(
  error ~ log(length)+log(antal)| CaseTopic + Municipality + Time,  # Same fixed effects
  data = Kgrouped,
  cluster = ~CaseTopic) 
FE_model_length_mantal %>% summary()



# FE_model_length <- feols(
#   error ~ length_relative | CaseTopic + Municipality + Time,  # Same fixed effects
#   data = K2,
#   cluster = ~CaseTopic) 
# FE_model_length %>% summary()

full_model <- feols(
  error ~ log(length) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
full_model %>% summary()

names(K2)


upper_thresh = 3
upper_thresh = quantile(K2$length_relative, 0.95)
# lower_thresh = quantile(K2$relative_length, 0.05)

Krobust_highest = K2 %>% group_by(CaseTopic) %>% 
  mutate(mean_relative_length = mean(length_relative),
         min = min(length_relative),
         max = max(length_relative)) %>% 
  filter(mean_relative_length < upper_thresh) 

model_no_outliers_95 <- feols(
  error ~ log(length) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = Krobust_highest,
  cluster = ~CaseTopic
) 
model_no_outliers_95 %>% summary()

model_balanced_panel = feols(
  error ~ log(length) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2 %>% filter(n > 40),
  cluster = ~CaseTopic
) 
model_balanced_panel %>% summary()

notime_FE <- feols(
  error ~ log(length) | CaseTopic,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
notime_FE %>% summary()

# Create modelsummary table (log-odds)
models <- list(
  "Full model" = full_model,
  "95% trimmed sample" = model_no_outliers_95,
  "Balanced panel" = model_balanced_panel,
  "No time or municipal FE" = notime_FE
)

modelsummary(
  models,
  exponentiate = FALSE,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Pseudo|R2 Within",
  output = "plots/hovedmodeller.tex"
)

tidy_models <- map_dfr(
  models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Model"
)
models = ggplot(tidy_models, aes(y = estimate, x = reorder(Model,estimate))) +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey70", size=1, width=0.1) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey40", size=1, width=0.1)+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim=c(-0.15,0.05)) +  # flipped: use xlim not ylim
  labs(x = "", y = "") +
  theme_minimal(base_family = "sans",
                base_size = 24) +
  theme(panel.grid.minor = element_blank())
models
ggsave(plot = models, "plots/models.pdf", width = 12, height = 8)


# Define upper/lower threshold

# Full model
logit_full <- feglm(
  error ~ log(length) | CaseTopic + Municipality + Time,
  data = K2,
  family = binomial(link = "logit")
)
logit_full
exp(logit_full$coeftable$Estimate*0.2)


# Outlier-robust model
logit_robust <- feglm(
  error ~ log(length) | CaseTopic + Municipality + Time,
  data = Krobust_highest,
  family = binomial(link = "logit")
)
logit_robust

# Balanced panel model
logit_balanced <- feglm(
  error ~ log(length) | CaseTopic + Municipality + Time,
  data = K2 %>% filter(n > 40),
  family = binomial(link = "logit")
)
logit_balanced
test = K2 %>% filter(Sagsemne=="Arbejdsgiverrefusion - anmeldelse § 59")
# No time FE model
logit_notime <- feglm(
  error ~ log(length) | CaseTopic,
  data = K2,
  family = binomial(link = "logit")
)
logit_notime

logit_models = list(
  "Full sample (logit)" = logit_full,
  "95% trimmed sample (logit)" = logit_robust,
  "Balanced panel (logit)" = logit_balanced,
  "No time FE (logit)" = logit_notime
)
# Create modelsummary table (log-odds)
modelsummary(
  logit_models,
  exponentiate = FALSE,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Pseudo|R2 Within",
  output = "plots/logit_modeller.tex"
)


tidy_logit_models <- map_dfr(
  logit_models,
  ~ tidy(.x, conf.int = TRUE),
  .id = "Model"
)
logit_models = ggplot(tidy_logit_models, aes(y = estimate, x = reorder(Model,estimate))) +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey70", size=1, width=0.1) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey40", size=1, width=0.1)+
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim=c(-0.75,0.05)) +  # flipped: use xlim not ylim
  labs(x = "", y = "") +
  theme_minimal(base_family = "sans",
                base_size = 24) +
  theme(panel.grid.minor = element_blank())
logit_models
ggsave(plot = models, "plots/logit_models.pdf", width = 12, height = 8)

tidy_models

##### KORTERE ELLER LÆNGERE----
K2 = K2  %>% arrange(desc(Time)) %>% group_by(Sagsemne) %>% mutate(isshorter = length_relative<=1)

summary(K2$isshorter)

K2 = K2 %>% group_by(CaseTopic,Time,Municipality) %>% 
  mutate(amount = n())


FE_model_length <- feols(
  error ~ log(length)*isshorter| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
FE_model_length %>% summary()

FE_model_length <- feols(
  error ~ log(length)| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2 %>% filter(isshorter==T),
  cluster = ~CaseTopic) 
FE_model_length %>% summary()




### LIDT DESC ----
changes = K2 %>% 
  select(new_length_change, Sagsemne, DatePeriode) %>% 
  distinct() %>% 
  mutate(abs_change = abs(new_length_change-1)) %>% 
  filter(new_length_change!=1)



library(gt)

no_cases = nrow(K2)
no_casetypes = nrow(K2 %>% select(Sagsemne) %>% distinct())
no_periods = nrow(K2 %>% select(DatePeriode) %>% distinct())
no_changes = nrow(changes)
mean_no_pages = K2 %>% 
  select(Sagsemne, length, DatePeriode) %>% 
  distinct() %>% 
  group_by(Sagsemne) %>% 
  filter(DatePeriode == min(DatePeriode)) %>% 
  pull(length) %>% mean()/2400
mean_error_rate = K2 %>% pull(stad) %>% mean()

# Create a data frame
summary_df <- tibble(
  "Measure" = c(
    "Number of cases",
    "Number of case types",
    "Number of time periods",
    "Number of legal changes",
    "Average absolute change in length in %",
    "Avg. number of pages (baseline)",
    "Mean error rate"
  ),
  "Value" = c(
    round(no_cases, 1),
    round(no_casetypes, 1),
    round(no_periods, 1),
    round(no_changes, 1),
    round(mean(changes$abs_change)*100,1),
    round(mean_no_pages, 1),
    round(mean_error_rate, 1)
  )
)

library(xtable)
xtable(summary_df)




quantile(changes$abs_change)
mean(changes$abs_change)
unique(K2$length_change)

estimate = full_model$coefficients
-0.054385
estimate*quantile(changes$abs_change,0.5)/0.4*100
estimate*quantile(changes$abs_change,0.75)/0.4*100
estimate*quantile(changes$abs_change,0.9)/0.4*100
estimate*mean(changes$abs_change)*100

-6.9*quantile(changes$abs_change,0.5)/0.4*100
-6.9*quantile(changes$abs_change,0.75)/0.4*100
-6.9*quantile(changes$abs_change,0.9)/0.4*100
-6.9*mean(changes$abs_change)*100

paste("For the median change in the length of the law of",
      round(quantile(changes$abs_change,0.5)*100), 
      "percent. The estimated change in the error rate is",
      round(estimate*quantile(changes$abs_change,0.5)/0.4*100,2),
      "percent of the mean error rate."
      )
paste("For the mean change in the length of the law of",
      round(mean(changes$abs_change)*100), 
      "percent. The estimated change in the error rate is",
      round(estimate*mean(changes$abs_change)/0.4*100,2),
      "percent of the mean error rate."
)
paste("For the 75 percentile largest change in the length of the law of",
      round(quantile(changes$abs_change,0.75)*100), 
      "percent. The estimated change in the error rate is",
      round(estimate*quantile(changes$abs_change,0.75)/0.4*100,1),
      "percent of the mean error rate."
)
paste("For the 90 percentile largest change in the length of the law of",
       round(quantile(changes$abs_change,0.90)*100), 
       "percent. The estimated change in the error rate is",
       round(estimate*quantile(changes$abs_change,0.90)/0.4*100,2),
       "percent of the mean error rate."
)


#Bimodellen grouped----
names(K2)
Kgrouped = K2 %>%
  group_by(Sagsemne, length, paragraf_henvisning, DatePeriode,
           permissive_delegation,mandatory_delegation, LIX_score, 
           num_conditionals) %>%
  summarise(
    antal = n(),
    antal_fejl = sum(stad),
    omg_procent = sum(stad) / antal
  ) %>%
  ungroup() %>%
  group_by(Sagsemne) %>%
  mutate(snit = mean(antal))

sd_loglength = sd(log(K2$length))
perc_change = exp(sd_loglength)
perc_change
Kgrouped %>% ggplot(aes(x=log(length),y=omg_procent))+geom_smooth(method="lm")+geom_point()


FE_model_grouped <- feols(
  omg_procent ~ log(length)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped %>% summary()

FE_model_grouped_count <- feols(
  log(antal_fejl+1) ~ log(length)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_count %>% summary()


FE_model_grouped_over20 <- feols(
  omg_procent ~ log(length)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped %>% filter(snit>10),
  cluster = ~Sagsemne
) 
FE_model_grouped_over20 %>% summary()


groupdf = data.frame()
obsnr = 1
head(sort(unique(Kgrouped$snit)), -5)
for (i in seq(5,100,by=5)){
FE_model_grouped <- feols(
  omg_procent ~ log(length)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped %>% filter(snit>=i),
  cluster = ~Sagsemne
) 
loop = tidy(FE_model_grouped)
loop$i = i
loop$obsnr = obsnr
obsnr = obsnr+1
groupdf = rbind(groupdf,loop)
FE_model_grouped %>% summary()}


grouped = ggplot(groupdf, aes(x = i, y = estimate)) +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey70", size=1, width=0.75) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey40", size=1, width=0.75)+
  geom_point() +
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Minimum # cases per quarter per case type", y = "") +
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = scales::pretty_breaks())
grouped
ggsave(plot = grouped,"plots/grouped.pdf", width = 12, height = 8)




#ANTAL MODELLEN - skal gerne være insig---
FE_model_grouped_antal <- feols(
  log(antal) ~ log(length)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal %>% summary()

FE_model_grouped_antal_paragraf <- feols(
  log(antal) ~ log(paragraf_henvisning)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal_paragraf %>% summary()

FE_model_grouped_antal_mandatory_delegation <- feols(
  log(antal) ~ log(mandatory_delegation+1)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal_mandatory_delegation %>% summary()

FE_model_grouped_antal_permissive_delegation <- feols(
  log(antal) ~ log(permissive_delegation+1)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal_permissive_delegation %>% summary()

FE_model_grouped_antal_num_conditionals <- feols(
  log(antal) ~ log(num_conditionals+1)| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal_num_conditionals %>% summary()

FE_model_grouped_antal_LIX_score  <- feols(
  log(antal) ~ LIX_score| Sagsemne + DatePeriode,  # Same fixed effects
  data = Kgrouped,
  cluster = ~Sagsemne
) 
FE_model_grouped_antal_LIX_score %>% summary()


modelsummary(
  list(
    "Length (log)"            = FE_model_grouped_antal,
    "Paragraphs (log)"        = FE_model_grouped_antal_paragraf,
    "Mandatory delegation"    = FE_model_grouped_antal_mandatory_delegation,
    "Permissive delegation"   = FE_model_grouped_antal_permissive_delegation,
    "LIX score"               = FE_model_grouped_antal_LIX_score,
    "Conditionals (log+1)"    = FE_model_grouped_antal_num_conditionals
  ),
  exponentiate = FALSE,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Pseudo|R2 Within",
  output = "plots/antal.tex",
  notes = "Clustered SEs by Sagsemne",
  latex_options = "scale_down"
)


### HETEROMODELLEN----
FE_model_hetero <- feols(
  stad ~ log(length)*Sagsemne|Kommune + DatePeriode+Lovgivning,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
test = tidy(FE_model_hetero)
FE_model_hetero %>% summary()


K2 %>% group_by(Lovgivning) %>% 
  summarise(n = n(),
            sager = n_distinct(Sagsemne),
            change = mean(length_relative))




## EXTRA STUFF----
c("long_subject_nchar", "num_adjectives", "num_conditionals","entitlement","mandatory_delegation","permissive_delegation",
"other","constraint","additional_mandatory_delegation")
K2 = K2 %>% mutate(mandatory = additional_mandatory_delegation+mandatory_delegation)
summary(K2)
permissive_model <- feols(
  error ~ log(permissive_delegation+1) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic) 
permissive_model %>% summary()

mandatory_model <- feols(
  error ~ log(mandatory+1) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
mandatory_model %>% summary()

paragraf_model = feols(
  error ~ log(paragraf_henvisning+1) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
paragraf_model %>% summary()

LIX_model = feols(
  error ~  LIX_score| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
LIX_model %>% summary()

adjective_model = feols(
  error ~  log(num_adjectives+1)| CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
adjective_model %>% summary()

condi_model = feols(
  error ~ log(num_conditionals+1) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
condi_model %>% summary()

summary(K2$num_conditionals,na.rm=T)

other_model = feols(
  error ~ log(other+1) | CaseTopic + Municipality + Time,  # Same fixed effects
  data = K2,
  cluster = ~CaseTopic
) 
other_model %>% summary()
library(modelsummary)



modelsummary(
  list(
    "Cross-references" = paragraf_model,
    "Permissive delegation" = permissive_model,
    "Mandatory delegation" = mandatory_model,
    "Readability (LIX)" = LIX_model,
    "Syntactic complexity (Adjectives)" = adjective_model,
    "Conditional clauses" = condi_model,
    "Other provisions" = other_model
  ),
  shape = "rows",
  exponentiate = FALSE,
  stars = TRUE,
  coef_omit = "Intercept",
  gof_omit = "AIC|BIC|Log.Lik|Pseudo|R2 Within",
  output = "plots/extra_stuff.tex",
  add_rows = NULL,
  fmt = 3
)


tidy_models <- bind_rows(
  broom::tidy(paragraf_model)         |> mutate(Model = "Cross-references"),
  broom::tidy(permissive_model)       |> mutate(Model = "Permissive delegation"),
  broom::tidy(mandatory_model)        |> mutate(Model = "Mandatory delegation"),
  broom::tidy(LIX_model)              |> mutate(Model = "Readability (LIX)"),
  broom::tidy(condi_model)            |> mutate(Model = "Conditional clauses")) |> 
  filter(term != "(Intercept)")

# Plot
ggplot(tidy_models, aes(y = estimate, x = reorder(Model, estimate))) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                colour = "grey70", size = 1, width = 0.1) +
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error),
                colour = "grey40", size = 1, width = 0.1) +
  geom_point(shape = 21, fill = "grey20", colour = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-0.10, 0.05)) +
  labs(x = "", y = "") +
  theme_minimal(base_family = "sans",
                base_size = 24) +
  theme(panel.grid.minor = element_blank())
ggsave("plots/extra_stuff.pdf", width = 12, height = 8)


#### AS A LASSO REGRESSION
safe_log <- function(x) {
  if (all(is.finite(x))) {
    mn <- suppressWarnings(min(x, na.rm = TRUE))
  } else {
    mn <- NA_real_
  }
  if (is.finite(mn) && mn > 0) {
    log(x)
  } else if (is.finite(mn) && mn >= 0) {
    log1p(x)
  } else {
    asinh(x)
  }
}
# your variables to log
vars_rest <- c("paragraf_henvisning","num_adjectives","num_conditionals",
  "entitlement","permissive_delegation",
  "other","constraint","mandatory")


# add logged variables + log(length)
K2 <- K2 %>%
  mutate(
    loglength = log(length)
  ) %>%
  mutate(across(all_of(vars_rest),
                ~ safe_log(.x),
                .names = "l_{.col}"))

vars_controls <- c("l_paragraf_henvisning","l_num_conditionals",
                   "LIX_score","l_mandatory","l_permissive_delegation",
                   "")


full_model = feols(
  as.formula(paste0("error ~", paste0(vars_controls, collapse = "+"),"| CaseTopic + Municipality + Time")),
  data = K2,
  cluster = ~CaseTopic
) 
full_model
tidy(full_model)
Z_t <- sapply(Z, function(v) resid(feols(as.formula(paste0(v, " ~ 1 | CaseTopic + Municipality + Time")), data = K2)))


# Correlation matrix of Z_t
cors <- cor(Z_t, use = "pairwise.complete.obs")
round(cors, 3)

# Optional: save to CSV
# write.csv(cors, "cor_Zt.csv", row.names = TRUE)

# Optional: greyscale heatmap (requires ggplot2, reshape2)
library(ggplot2); library(reshape2)
df_cor <- reshape2::melt(cors, varnames = c("Var1","Var2"), value.name = "corr")
ggplot(df_cor, aes(Var1, Var2, fill = corr)) +
  geom_tile(color = "grey80") +
  scale_fill_gradient(limits = c(-1, 1), low = "black", high = "red") +
  labs(x = NULL, y = NULL, fill = "ρ") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())






















# Opret en liste over kombinationer af kommuner og lovgivninger, der har deltaget i frikommuneforsøg
frikommuner <- list(
  list(Kommune = c("Gladsaxe", "Gentofte", "Fredensborg", "Odsherred", "Vejle", 
                   "Vesthimmerland", "Viborg", "Fredericia", "Odense"), Lovgivning = "Sygedagpengeloven"),
  list(Kommune = c("Gladsaxe", "Gentofte", "Fredensborg", "Odsherred", "Vejle", 
                   "Vesthimmerland", "Viborg", "Fredericia", "Odense"), Lovgivning = "Serviceloven"),
  list(Kommune = c("Gladsaxe", "Gentofte", "Fredensborg", "Odsherred", "Vejle", 
                   "Vesthimmerland", "Viborg", "Fredericia", "Odense"), Lovgivning = "Aktivloven")
)

# Funktion til at kontrollere, om en række i K2 matcher en af frikommunekombinationerne
er_frikommune <- function(kommune, lovgivning) {
  for (frikommune in frikommuner) {
    if (lovgivning == frikommune$Lovgivning && kommune %in% frikommune$Kommune) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Anvend funktionen på hver række i K2 for at oprette variablen 'frikommune'
K2$frikommune <- mapply(er_frikommune, K2$Kommune, K2$Lovgivning)
table(K2$frikommune)
FE_model_length <- feols(
  stad ~ log(length)*frikommune| Sagsemne + DatePeriode+Lovgivning,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
FE_model_length %>% summary()


############################
FE_model_lix <- feols(
  stad ~ LIX_score |Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
FE_model_lix %>% summary()
##################
summary(K2$LIX_score_running)


############################
FE_model_para <- feols(
  stad ~ log(paragraf_henvisning) |Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
FE_model_para %>% summary()
##################



####
FE_model_length <- feols(
  stad ~ length | Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
FE_model_length %>% summary()


FE_model_para <- feols(
  stad ~ paragraf_henvisning | Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2 %>% filter(paragraf_henvisning!=0),
  cluster = ~Sagsemne
) 
FE_model_para %>% summary()



######
FE_model_logpara <- feols(
  stad ~ log(paragraf_henvisning) | Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 

FE_model_logpara %>% summary()


FE_model_para_rel <- feols(
  stad ~ para_relative | Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
) 
FE_model_para_rel%>% summary()

### MODELS AND PLOTS ----
FE_model_length <- feols(
  stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2,
  cluster = ~Sagsemne
)
FE_model_length %>% summary()


# Combine models into a list
models <- list("Model with LIX" = FE_model, 
               "Model with Length (Log)" = FE_model_loglength,
               "Model with cross references" = FE_model_para,
               "Model with cross references (log)" = FE_model_logpara)

modelsummary::modelsummary(models, output = "plots/number.tex", stars = TRUE)

###################


## Sanktioner ser lidt mærkelige ud, så jeg prøver lige at køre uden dem. ----
FE_model_length <- feols(
  stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = K2 %>% filter(!grepl("Sanktion",Sagsemne)),
  cluster = ~Sagsemne
)
FE_model_length %>% summary()
####################

# 1) Log length (only defined for positive values)
K2 <- K2 %>%
  mutate(
    length = ifelse(length > 0, length, NA_real_),
    loglength = log(length)
  )

# Quick check
summary(K2$loglength)

# 2) Define "isshorter" within each Sagsemne over time (ascending)
#    Important: use ascending time so lag() compares to the previous version
K2 <- K2 %>%
  arrange(Sagsemne, Time) %>%
  group_by(Sagsemne) %>%
  mutate(
    isshorter = (length - lag(length)) < 0,     # TRUE if current is shorter than the previous
    isshorter = tidyr::replace_na(isshorter, FALSE),
    isshorter = as.integer(isshorter)           # 0/1 for interaction
  ) %>%
  ungroup()

# 3) Amount per (CaseTopic, Time, Municipality)
K2 <- K2 %>%
  group_by(CaseTopic, Time, Municipality) %>%
  mutate(amount = n()) %>%
  ungroup()

# 4) FE model with interaction
FE_model_length <- feols(
  error ~ loglength * isshorter | CaseTopic + Municipality + Time,
  data = K2,
  cluster = ~ CaseTopic
)

summary(FE_model_length)



unique(K2$Sagsemne)

# 
# for (i in length(unique(K2$Sagsemne)))
# FE_model_length <- feols(
#   stad ~ length_relative| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
#   data = K2 %>% filter(),
#   cluster = ~Sagsemne
# ) 
  
#### Robustness----
means=K2 %>% group_by(Sagsemne) %>% 
  summarise(mean_length_rel = mean(length_relative),
         min(length_relative),
         max(length_relative))
upper_thresh = quantile(means$mean_length_rel,0.75)
upper_thresh = 3

lower_thresh = 0.85

Krobust_highest = K2 %>% group_by(Sagsemne) %>% 
  mutate(mean_length_rel = mean(length_relative),
         min = min(length_relative),
         max = max(length_relative)) %>% 
  filter(mean_length_rel<upper_thresh) # Fjerner dem, der vokser sig til over dobbeltstørrelse, de er også extreme

Krobust_also_lowest = K2 %>% group_by(Sagsemne) %>% 
  mutate(mean_length_rel = mean(length_relative),
         min = min(length_relative),
         max = max(length_relative)) %>% 
  filter(mean_length_rel<upper_thresh, # Fjerner dem, der vokser sig til over dobbeltstørrelse, de er også extreme
         min>lower_thresh) # Fjerner dem, der bliver mere end halveret
Krobust_lowest = K2 %>% group_by(Sagsemne) %>% 
  mutate(mean_length_rel = mean(length_relative),
         min = min(length_relative),
         max = max(length_relative)) %>% 
  filter(min>lower_thresh) # Fjerner dem, der bliver mere end halveret


# FE_model_length <- feols(
#   stad ~ length_relative| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
#   data = K2,
#   cluster = ~Sagsemne
# ) 
# FE_model_length %>% summary()

FE_model_length_robust_high <- feols(
  stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = Krobust_highest,
  cluster = ~Sagsemne
) 
FE_model_length_robust_high %>% summary()


FE_model_length_robust_also_low <- feols(
  stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = Krobust_also_lowest,
  cluster = ~Sagsemne
) 
FE_model_length_robust_also_low %>% summary()

FE_model_length_robust_low <- feols(
  stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = Krobust_lowest,
  cluster = ~Sagsemne
) 
FE_model_length_robust_low %>% summary()


##### DICHOTOMY 
df_dict = data.frame()
for (i in seq(0.5,0.9,0.005)){
print(i)
dict_thres = quantile(K2$length_relative,i)

Krobust_dict = K2 %>% mutate(complex = ifelse(length_relative>dict_thres,1,0))
# summary(Krobust_dict$complex)
FE_model_length_robust_dict<- feols(
  stad ~ complex| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
  data = Krobust_dict,
  cluster = ~Sagsemne
) 
FE_model_length_robust_dict %>% summary()
loop = tidy(FE_model_length_robust_dict)
loop$i = i
loop$thresh = dict_thres
df_dict = rbind(df_dict,loop)

}
dichotomized = df_dict %>%
  ggplot(aes(x = i, y = estimate)) +
  geom_point() +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey40", size=0.2) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey20", size=1)+
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Quantile", y = "") +
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = scales::pretty_breaks())
dichotomized
ggsave(plot = dichotomized, "plots/dichotomized.pdf", width = 12, height = 8)

#### OUTLIERS 
df_out = data.frame()
for (i in seq(1,50)){
  print(i)
  outemne = means %>% arrange(desc(mean_length_rel)) %>% 
    head(i) %>% 
    pull(Sagsemne)
  thresh = means %>% arrange(desc(mean_length_rel)) %>% 
    head(i) %>% 
    pull(mean_length_rel)
  Krobust_out = K2 %>% filter(!Sagsemne%in%outemne)
  # summary(Krobust_dict$complex)
  FE_model_length_robust_dict<- feols(
    stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
    data = Krobust_out,
    cluster = ~Sagsemne
  ) 
  FE_model_length_robust_dict %>% summary()
  loop = tidy(FE_model_length_robust_dict)
  loop$i = i
  loop$thresh = min(thresh)
  df_out = rbind(df_out,loop)
  
}
outliers = df_out %>%
  ggplot(aes(x = i, y = estimate)) +
  geom_point() +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey40", size=0.2) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey20", size=1)+
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "# Case types removed", y = "") +
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = scales::pretty_breaks())
outliers
ggsave(plot = outliers, "plots/outliers.pdf", width = 12, height = 8)


### TJEKKER LIGE EFFEKTEN AF MISSING
K2nomissing = K2 %>% group_by(Sagsemne) %>% 
  mutate(n = n_distinct(DatePeriode))


mis = data.frame()
sort(unique(K2nomissing$n))
for (i in seq(1:46)){
  print(i)
  Kloop = K2nomissing %>% filter(n >=i)
  FE_model_missing<- feols(
    stad ~ log(length)| Sagsemne + Kommune + DatePeriode,  # Same fixed effects
    data = Kloop,
    cluster = ~Sagsemne
  )
  
  FE_model_missing %>% summary()
  loop = tidy(FE_model_missing)
  loop$i = i
  loop$antalsagsemner = length(unique(Kloop$Sagsemne))
  mis = rbind(mis,loop)
}

missing = mis %>%
  ggplot(aes(x = i, y = estimate)) +
  coord_flip()+
  geom_point() +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error), colour = "grey40", size=0.2) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = estimate - 1.65 * std.error,
                    ymax = estimate + 1.65 * std.error), colour = "grey20", size=1)+
  theme_minimal(base_size = 24) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "# quarters in data", y = "") +
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = scales::pretty_breaks())
missing
ggsave(plot = missing, "plots/missing.pdf", width = 12, height = 8)



### NOGET NON-PARAMETRISK----
### JEG PRØVER IGEN NOGET DIF-in-Dif:
your_data <- K2 %>%
  mutate(unit_id = paste(Kommune, Sagsemne, sep = "_"))

your_data <- your_data %>%
  mutate(length_bin = ntile(length_relative, 4))
thresh = 2


library(dplyr)

your_data <- your_data %>%
  arrange(quarter_index) %>% 
  group_by(Sagsemne) %>%
  mutate(
    ratio = length / lag(length),
    changed = ratio > 1.1,
    # First quarter with a change (per topic)
    treat_time = if (any(changed, na.rm = TRUE)) {
      min(quarter_index[changed], na.rm = TRUE)
    } else {
      NA_real_
    },
    
    # Ever-treated indicator per topic
    D = if_else(is.na(treat_time), 0L, 1L)
  ) %>%
  ungroup() %>%
  mutate(
    event_time = quarter_index - treat_time
  ) %>% 
  filter(!is.na(event_time) & abs(event_time) < 8)



table(your_data$D)
# table(K2$length_relative>thresh)

event_plot = feols(stad ~ i(event_time, ref = -1)| Sagsemne + quarter_index, data = your_data)
event_plot
plot_data = tidy(event_plot)
plot_data





#### JEG PRØVER FECT ----
# Install the package if you haven't already
# install.packages("fect")  # available on CRAN
library(fect)

#library(fect)
df = K2 %>% group_by(Sagsemne,quarter_index) %>%
  summarise(y = sum(stad)/n()) %>% 
  ungroup() %>% 
  distinct()


K_panel_plot = K2 %>% select(Sagsemne,quarter_index,length,n) %>%
  # filter(n==46) %>% 
  distinct() %>% 
  arrange(Sagsemne,quarter_index) %>% 
  mutate(length_change = (length-lag(length))/lag(length),
         length_change = ifelse(is.na(length_change),0,length_change),
         length_change = ifelse(quarter_index==0,0,length_change),
         length_change_binary = (length_change>0.2)*1)  %>% 
  select(length_change, length_change_binary,Sagsemne, quarter_index,length,n) %>% 
  distinct() %>% 
  left_join(df)



K_panel_plot <- K2 %>%
  select(Sagsemne, quarter_index, length, n) %>%
  filter(n > 0) %>%
  distinct() %>%
  arrange(Sagsemne, quarter_index) %>%
  group_by(Sagsemne) %>%
  mutate(
    length_change = (length - lag(length)) / lag(length),
    length_change = ifelse(is.na(length_change) | quarter_index == 0, 0, length_change),
    length_change_binary = (length_change > 0.2) * 1,
    # mark current and next two quarters as 1
    length_change_binary = if_else(
      length_change_binary == 1 |
        lag(length_change_binary, 1, default = 0) == 1 |
        lag(length_change_binary, 2, default = 0) == 1|
        lag(length_change_binary, 3, default = 0) == 1|
        lag(length_change_binary, 1, default = 0) == 1|
        lag(length_change_binary, 1, default = 0) == 1,
      1, 0
    )
  ) %>%
  ungroup() %>%
  select(length_change, length_change_binary, Sagsemne, quarter_index, length, n) %>%
  distinct() %>%
  left_join(df, by = c("Sagsemne", "quarter_index"))
K_panel_plot %>% filter(length_change_binary > 0.4)
length(unique(K_panel_plot$Sagsemne))

result <- fect(
  Y = "y",  # navnet på den afhængige variabel (skal være karakterstreng)
  D = "length_change_binary",  # binary treatment
  data = K_panel_plot,  # data.frame
  index = c("Sagsemne", "quarter_index"),  # panelstruktur: unit og tid
  method = "fe",  
  force = "two-way",
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  parallel = TRUE,
  balance.period = c(-4,4)
)

# Plot dynamic treatment effects
plot(result, type = "gap",plot.ci="0.9")
plot(result, type = "gap",plot.ci="0.9")
ggsave("plots/didtry.pdf")
result$est.avg
result$est.avg.unit

#?fect


##### FECT - loopet ---
test = K_panel_plot %>% filter(length_change!=0)
summary(test$length_change)
quantile(test$length_change,0.95)

fect_df = data.frame()
for (i in seq(0.06,0.50,by=0.02)){print(i)

K_panel_plot <- K2 %>%
  select(Sagsemne, quarter_index, length, n) %>%
  filter(n > 0) %>%
  distinct() %>%
  arrange(Sagsemne, quarter_index) %>%
  group_by(Sagsemne) %>%
  mutate(
    length_change = (length - lag(length)) / lag(length),
    length_change = ifelse(is.na(length_change) | quarter_index == 0, 0, length_change),
    length_change_binary = (length_change > i) * 1,
    # mark current and next two quarters as 1
    length_change_binary = if_else(
      length_change_binary == 1 |
        # lag(length_change_binary, 1, default = 0) == 1 |
        # lag(length_change_binary, 1, default = 0) == 1|
        # lag(length_change_binary, 1, default = 0) == 1|
        # lag(length_change_binary, 1, default = 0) == 1|
        lag(length_change_binary, 1, default = 0) == 1,
      1, 0
    )
  ) %>%
  ungroup() %>%
  select(length_change, length_change_binary, Sagsemne, quarter_index, length, n) %>%
  distinct() %>%
  left_join(df, by = c("Sagsemne", "quarter_index"))

loop_test = K_panel_plot %>% group_by(Sagsemne) %>% summarise(change = sum(length_change_binary))
antal_treatede = sum((loop_test$change!=0)*1) # Antal treatede sagsemner
if (antal_treatede<15){break}

result <- fect(
  Y = "y",  # navnet på den afhængige variabel (skal være karakterstreng)
  D = "length_change_binary",  # binary treatment
  data = K_panel_plot,  # data.frame
  index = c("Sagsemne", "quarter_index"),  # panelstruktur: unit og tid
  method = "fe",  
  force = "two-way",
  CV = TRUE,
  se = TRUE,
  nboots = 200,
  parallel = TRUE,
  balance.period = c(-6,2)
)

# Plot dynamic treatment effects
plot(result, type = "gap",  start0 = TRUE)
loop = result$est.avg
result$est.avg.unit
names(loop) = c( "ATT",    "se", "CI.lower","CI.upper","p.value")
loop$i = i
fect_df = rbind(fect_df,loop)
}
fect_df %>% ggplot(aes(x=i,y=ATT))+
  geom_point() +
  # 95% CI (±1.96)
  geom_errorbar(aes(ymin = ATT - 1.96 * se,
                    ymax = ATT + 1.96 * se), colour = "grey40", size=0.2) +
  # 90% CI (±1.65)
  geom_errorbar(aes(ymin = ATT - 1.65 * se,
                    ymax = ATT + 1.65 * se), colour = "grey20", size=1)+
  theme_minimal()+
  geom_hline(yintercept = 0)
  


##### JEG PRØVER NOGET DIF-in-DIF. DEN HER GIVER FAKTISK NOGLE ESTIMATER, så er det bare lige at skrue på den ------

library(dplyr)
library(tidyr)
library(BMisc)

# Assume your original data is called `df`
thresh = 1
df = K2 %>% group_by(Sagsemne,quarter_index) %>%
  summarise(y = sum(stad)/n()) %>% 
  ungroup() %>% 
  distinct()


K_panel_plot = K2 %>% select(Sagsemne,quarter_index,length,n) %>%
  filter(n==46) %>% 
  distinct() %>% 
  arrange(Sagsemne,quarter_index) %>% 
  mutate(length_change = (length-lag(length))/lag(length),
         length_change = ifelse(is.na(length_change),0,length_change),
         length_change = ifelse(quarter_index==0,0,length_change),
         length_change_binary = (length_change>0.2)*1)  %>% 
  select(length_change, length_change_binary,Sagsemne, quarter_index,length,n) %>% 
  distinct() %>% 
  left_join(df)
table(K_panel_plot$Sagsemne)

# NYTFORSØG 16 04
K_panel_plot = K2 %>% select(Sagsemne,quarter_index,length,n) %>%
  distinct() %>% 
  arrange(Sagsemne,quarter_index) %>% 
  mutate(length_change = (length-lag(length))/lag(length),
         length_change = ifelse(is.na(length_change),0,length_change),
         length_change = ifelse(quarter_index==0,0,length_change),
         length_change_binary = (length_change>0.2)*1)  %>% 
  ungroup() %>% 
  group_by(Sagsemne) %>% 
  mutate(length_change_binary = ifelse(quarter_index==min(quarter_index),0,length_change_binary)) %>% 
  select(length_change, length_change_binary,Sagsemne, quarter_index,length,n) %>% 
  distinct()

# Filter out duplicate observations based on Sagsemne and quarter_start
K_panel_plot <- K_panel_plot %>%
  filter(n>45) %>% 
  group_by(Sagsemne, quarter_index) %>%
  filter(n() == 1) %>%
  ungroup() %>% 
  left_join(df)


df <- K_panel_plot %>%
  ungroup() %>%  # just in case
  mutate(i = as.numeric(as.factor(Sagsemne)),
         t = as.numeric(quarter_index),
         G = length_change_binary) %>%
  arrange(i, t)

test_model = feols(y~length_change_binary|i+t,df)
test_model
table(df$i)
table(df$t)


### HER KAN MAN LAVE DEN OM HVIS MAN VIL ----


# Step 2: define first treatment period per unit
df <- df %>%
  group_by(i) %>%
  mutate(first_treatment = ifelse(any(length_change_binary != 0), 
                                  min(quarter_index[length_change_binary != 0]), # Her sætter jeg threshold 
                                  0)) %>%
  ungroup() %>%
  mutate(G = ifelse(first_treatment == 0, 0, first_treatment)) # G = 0 means never treated


df

# 
# 
# # Step 3: define treatment intensity median (only among treated)
# median_d <- median(df$length_relative[df$length_relative != 0], na.rm = TRUE)
# median_d <- quantile(df$length_relative[df$length_relative != 0], 0.5)
# df <- df %>%
#   mutate(d_above_median = 1 * (length_relative > median_d & length_relative != 0),
#          d_below_median = 1 * (length_relative <= median_d & length_relative != 0))
# 
# 
# df_above <- df_balanced %>% filter(d_above_median == 1| length_relative ==0)
# df_below <- df_balanced %>% filter(d_below_median == 1 |length_relative ==0)
# nrow(df_above)+nrow(df_below)


# Step 5: create balanced panel
df_balanced <- BMisc::makeBalancedPanel(df, "i", "t")

att <- did::att_gt(yname = "y",
                   tname = "t",
                   idname = "i",
                   gname = "G",
                   data = df_balanced,
                   control_group = "notyettreated",
                   base_period = "varying",
                   est_method = "reg")

es <- did::aggte(att,
                 type = "dynamic",
                 min_e = -8,
                 max_e = 8)


print(es)
es$overall.att
es$overall.se

# # Optionally split into above/below median treatment groups
summary(df_above$G)
att_above <- did::att_gt(yname = "y",
                         tname = "t",
                         idname = "i",
                         gname = "G",
                         data = df_above,
                         control_group = "notyettreated",
                         base_period = "varying",
                         est_method = "reg")

es_above <- did::aggte(att_above,
                       type = "dynamic",
                       min_e = -4,
                       max_e = 4,
                       na.rm = TRUE)
print(es_above)


## HER ER BELOW
att_below <- did::att_gt(yname = "y",
                         tname = "t",
                         idname = "i",
                         gname = "G",
                         data = df_below,
                         control_group = "notyettreated",
                         base_period = "varying",
                         est_method = "reg")

es_below <- did::aggte(att_below,
                       type = "dynamic",
                       min_e = -4,
                       max_e = 4,
                       na.rm = TRUE)

print(es_below)



##### Jeg prøver lige noget dif-in-dif ----
df <- K2 %>%
  mutate(complex = as.integer(length_relative >= 1.25),
         quarter = (Year - 2013 + Quarter / 4) * 4) %>%
  group_by(Sagsemne) %>%
  mutate(first_complex_quarter = if (any(complex == 1)) min(quarter[complex == 1]) else NA_real_,
         event_time = quarter - first_complex_quarter) %>%
  ungroup()

library(fixest)

df_event <- df %>%
  filter(event_time >= -4, event_time <= 4) %>%  # Trim window (optional)
  mutate(event_time = ifelse(event_time < -4, -4, event_time),  # Collapse early leads
         event_time = factor(event_time))  # Turn into factor for feols

# Choose -1 as the omitted reference category
df_event$event_time <- relevel(df_event$event_time, ref = "-1")

event_model <- feols(
  stad ~ i(event_time, complex, ref = "-1") | quarter + Sagsemne,
  data = df_event
)
event_estimates <- tidy(event_model)
event_estimates
ATT <- event_estimates %>%
  filter(str_detect(term, "complex") & as.numeric(str_extract(term, "-?\\d+")) >= 1) %>%
  summarise(ATT = mean(estimate), .groups = "drop")
ATT







