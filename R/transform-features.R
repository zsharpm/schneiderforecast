

holidays <- holidays_raw %>%
  group_by(date, siteid) %>%
  summarize(holiday = paste0(holiday, collapse = " and ")) %>%
  ungroup() %>%
  mutate(holiday = ifelse(is.na(holiday), FALSE, TRUE))

metadata <- metadata_raw %>%
  mutate_if(is.character, as.logical)
