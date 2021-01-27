# Health core domain ------------------------------------------------------


col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["health_dark"]],
                 sg_light = gi_col_palette[["health_light"]])

plots[["health"]] <- data[["health"]] %>% plot_domain()

plots[["health_access"]] <- data[["health_access"]] %>% plot_subdomain()

plots[["health_status"]] <- data[["health_status"]] %>% plot_subdomain()

plots[["mental_wellbeing"]] <- data[["mental_wellbeing"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = ""))) %>%
  plot_indicator()

plots[["patient_exp"]] <- data[["patient_exp"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% percent(0.1))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 1.05),
                      tickformat = "%"))

plots[["gp_access"]] <- data[["gp_access"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% percent(0.1))) %>%
  plot_indicator() %>%
  layout(xaxis = list(range = c(0, 1.05),
                      tickformat = "%"))

plots[["mental_wellbeing"]] <- data[["mental_wellbeing"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(0.1, suffix = ""))) %>%
  plot_indicator()

plots[["mental_wellbeing_Age"]] <- all_data[["mental_wellbeing_Age"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = ""))) %>%
  plot_indicator("breakdown") 

plots[["mental_wellbeing_Disability"]] <- all_data[["mental_wellbeing_Disability"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = ""))) %>%
  plot_indicator("breakdown")

plots[["mental_wellbeing_Socio_Economic_Status"]] <- all_data[["mental_wellbeing_Socio_Economic_Status"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = ""))) %>%
  plot_indicator("breakdown")

plots[["hle"]] <- data[["hle"]] %>% 
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator()%>%
  layout(xaxis = list(ticksuffix = "%"))

plots[["hleSES"]] <- all_data[["hleSES"]] %>% 
  mutate(text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(50, 85)))

plots[["health_risk"]] <- all_data[["health_risk"]] %>%
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": </b>", value %>% percent(1, suffix = "%"))) %>%
  plot_indicator_time(nudge=c(-10,10))%>%
  layout(yaxis = list(range = c(0, 1.05), tickformat = "%"),
         xaxis = list(range = ~range(year)+c(-0.15,0.15)))

plots[["health_risk_Age"]] <- all_data[["health_risk_Age"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["health_risk_Disability"]] <- all_data[["health_risk_Disability"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["health_risk_Socio_Economic_Status"]] <- all_data[["health_risk_Socio_Economic_Status"]] %>% 
  filter(ref_period == max(ref_period)) %>%
  mutate(text = paste0(description, "\n", "<b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator("breakdown")

plots[["life_sat"]] <- data[["life_sat"]] %>%
  mutate(text = paste0(description, "\n", "<b>", gender, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator()%>%
  layout(xaxis = list(range = c(0, 105),
                      ticksuffix = "%"))

plots[["life_satA"]] <- all_data[["life_satA"]] %>% 
  mutate(breakdown = breakdown %>% fct_rev(),
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(0, 105),
                      ticksuffix = "%"))

plots[["life_satD"]] <- all_data[["life_satD"]] %>% 
  mutate(breakdown = breakdown %>% fct_relevel(c("No Long-term Condition", "Non limiting Long-term Condition", "Limiting Long-term Condition")),
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(0, 105),
                      ticksuffix = "%"))

plots[["life_satI"]] <- all_data[["life_satI"]] %>% 
  mutate(breakdown = breakdown %>% fct_rev(),
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(0, 105),
                      ticksuffix = "%"))

plots[["life_satSES"]] <- all_data[["life_satSES"]] %>% 
  mutate(breakdown = breakdown %>% fct_rev(),
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", breakdown, ": </b>", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_barbell()%>%
  layout(xaxis = list(range = c(0, 105),
                      ticksuffix = "%"))

