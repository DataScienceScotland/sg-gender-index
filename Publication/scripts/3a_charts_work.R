# Work domain -------------------------------------------------------------

col_palette <- c(sg_grey = gi_col_palette[["gi_grey"]],
                 sg_dark = gi_col_palette[["work_dark"]],
                 sg_light = gi_col_palette[["work_light"]])

plots[["work"]] <- data[["work"]] %>% plot_domain()

plots[["work_participation"]] <- data[["work_participation"]] %>% plot_subdomain()

plots[["work_segregation"]] <- data[["work_segregation"]] %>% plot_subdomain()

plots[["work_quality"]] <- data[["work_quality"]] %>% plot_subdomain()

plots[["fte_emp"]] <- all_data[["fte_emp"]] %>% 
  mutate(text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% comma(1, suffix = "%"))) %>%
  plot_indicator_time() %>%
layout(yaxis = list(range = c(0,105),
                    ticksuffix = "%"))

plots[["lm_inactive2"]] <- all_data[["lm_inactive2"]] %>% 
  mutate(value = value / 100,
         text = paste0("% of adults 'economically inactive'\n due to looking after family/home\n who are ", 
                       gender, "\n", "<b>", ref_period, ": </b>", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(range = c(0,1.05),
                      tickformat = "%"))

plots[["self_employed"]] <- all_data[["self_employed"]]  %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(tickformat = "%"))

plots[["under_emp_hrs"]] <- all_data[["under_emp_hrs"]]  %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time(nudge=c(20, -10)) %>%
  layout(yaxis = list(tickformat = "%"))

plots[["flexible_work"]] <- all_data[["flexible_work"]]  %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(range = c(0,0.35), tickformat = "%"))

plots[["job_sec"]] <- all_data[["job_sec"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time(nudge=c(10, -10)) %>%
  layout(yaxis = list(range = c(0,1.05), tickformat = "%"))

plots[["seg_care"]] <- all_data[["seg_care"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(range = c(0, 0.22), tickformat = "%"))

plots[["seg_stem"]] <- all_data[["seg_stem"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(tickformat = "%"))

plots[["occ_group1"]] <- all_data[["occ_group1"]] %>% 
  mutate(value = value / 100,
         text = paste0(description, "\n", 
                       "<b>", gender, ": </b>", ref_period, ": ", value %>% percent(1))) %>%
  plot_indicator_time() %>%
  layout(yaxis = list(tickformat = "%"))

