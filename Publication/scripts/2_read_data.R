plots <- list()
data <- list()


# Chart data --------------------------------------------------------------

indicator_data <- scores_2020 %>%
  #filter(!score %in% NA ) %>%
  gather("gender", "value", women, men) %>%
  left_join(gi_meta, by = c("variable")) %>%
  group_by(variable) %>%
  mutate(ref_range = ifelse(max(ref_period) == min(ref_period), ref_period, paste(min(ref_period), "to", max(ref_period)))) %>%
  ungroup() %>%
  mutate(description = map_chr(description, ~ {.x %>% strwrap(50) %>% paste0(collapse = "\n")}),
         title_text = case_when(subdomain == "Total" ~ paste0("**",domain," Domain",ref_range,"**"),
                                indicator == "Total" ~ paste0("**",subdomain," Sub-Domain",ref_range,"**"),
                                ! score %in% NA ~ paste0("**",description,", by gender ",ref_range,"**"),
                                TRUE ~ paste0("**",description," ",ref_range,"**")),
         calculation_notes = gender_score_note(description, reversed, ref_population),
         source_text = paste0("Source: [",
                              map2_chr(str_split(source, "\n"), str_split(link, "\n"), ~ paste0(.x,"](",.y, collapse = "); [")),
                              ")")) %>%
  ungroup()

all_data <- gi_data %>%
  gather("gender", "value", women, men, women_n, men_n, overall, overall_n) %>%
  filter(!is.na(value)) %>%
  left_join(gi_meta, by = c("variable")) %>%
  left_join(scores_2020 %>% distinct(variable, indicator, reversed, score), by = c("variable")) %>%
  group_by(variable) %>%
  mutate(ref_range = ifelse(max(ref_period) == min(ref_period), ref_period, paste(min(ref_period), "to", max(ref_period))),
         description = map_chr(description, ~ {.x %>% strwrap(50) %>% paste0(collapse = "\n")}),
         title_text = case_when(! score %in% NA ~ paste0("**",description,", by gender ",ref_range,"**"),
                                TRUE ~ paste0("**",description," ",ref_range,"**")),
         source_text = paste0("Source: [",source,"](",link,")")) 

domain_data <- scores_2020 %>%
  # filter(!score %in% NA ) %>%
  mutate(text = case_when(domain == "Total" ~ paste0("<b>Scotland</b>\n",
                                                     "gender equality score: <b>", score %>% comma(1), "</b>"),
                          subdomain == "Total" ~ paste0("<b>",domain," domain</b>\n",
                                                        "gender equality score: <b>", score %>% comma(1), "</b>"),
                          indicator == "Total" ~ paste0("<b>",subdomain," sub-domain</b>\n",
                                                        "gender equality score: <b>", score %>% comma(1), "</b>"),
                          TRUE ~ paste0("<b>",indicator,"</b>\n",
                                        "gender equality score: <b>", score %>% comma(1), "</b>")),
         domain_variable = case_when(domain == "Violence Against Women" ~ "vaw",
                                     domain == "Time" ~ "time_use",
                                     TRUE ~ domain %>% tolower() %>% gsub("\\W","_",.)),
         subdomain_variable = subdomain %>% tolower %>% gsub("\\W","_",.),
         variable = case_when(subdomain == "Total" ~ domain_variable,
                              indicator == "Total" ~ paste(domain_variable, subdomain_variable, sep = "_"),
                              TRUE ~ variable))

combine_data <- bind_rows(domain_data %>% filter(subdomain == "Total") %>% mutate(key = "Scotland"), #Top level
                          domain_data %>% filter(domain != "Total", indicator == "Total") %>% mutate(key = domain_variable), #Domain level
                          domain_data %>% filter(subdomain != "Total") %>% mutate(key = paste(domain_variable, subdomain_variable, sep = "_")) #Sub-domain level
) %>%
  group_by(variable) %>%
  mutate(ref_range = ifelse(max(ref_period) == min(ref_period), ref_period, paste(min(ref_period), "to", max(ref_period))),
         chart_title = case_when(subdomain == "Total" ~ paste0("**",domain," Domain gender equality scores ",ref_range,"**"),
                                 indicator == "Total" ~ paste0("**",subdomain," Sub-Domain gender equality scores ",ref_range,"**"))
  ) %>%
  ungroup()

data <- c(combine_data %>% group_split(key),
          indicator_data %>% group_split(variable))
names(data) <- c(combine_data %>% group_keys(key) %>% pull(key),
                 indicator_data %>% group_keys(variable) %>% pull(variable))

all_data <- all_data %>% group_by(variable)
index_all_data <- all_data %>% group_keys()
all_data <- all_data %>% group_split()
names(all_data) <- index_all_data %>% pull(variable)
all_data <- all_data %>%
  map(~{
    if(all(grepl("overall", .x$gender))) {
      .x
    } else {
      .x %>% filter(!grepl("overall", gender))
    }
  })

