covid_uk <- covid %>%
  filter(iso3 == "GBR", date >= lubridate::dmy("01-03-2020"))

covid_uk[1, 6] <- 103
covid_new <- covid_uk[1, ]
covid_new[1, 1] <- lubridate::ymd("2020-03-27")
covid_new[1, 2] <- 27
covid_new[1, 5] <- 2129
covid_new[1, 6] <- NA
covid_new[1, "date"] <- lubridate::ymd("2020-03-27")

covid_uk <- bind_rows(covid_new, covid_uk)

covid_uk %>%
  ggplot(aes(x = date, y = cases)) +
  geom_col() +
  geom_line(aes(y = deaths), colour = "red")

covid_uk %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_point() +
  geom_smooth(se = FALSE)

covid_plot <- covid %>% filter(iso3 %in% c("GBR", "ITA", "ESP", "USA")) %>%
  select(date, cases, deaths, iso3, cname) %>%
  group_by(iso3) %>%
  arrange(date) %>%
  mutate(cumulative_cases = cumsum(cases),
         cumulative_deaths = cumsum(deaths)) %>%
  filter(cumulative_deaths > 9) %>%
  mutate(days_elapsed = date - min(date),
         end_label = ifelse(date == max(date), cname, NA))

covid_plot %>%
  mutate(end_label = recode(end_label, `United States` = "USA",
                            `United Kingdom` = "UK")) %>%
  ggplot(aes(x = days_elapsed, y = cumulative_deaths, colour = iso3)) +
  geom_line() +
  geom_text_repel(aes(label = end_label),
                  nudge_x = 1.1,
                  nudge_y = 0.1,
                  segment.color = NA) +
  guides(colour = FALSE) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     breaks = 2^seq(4, 13),
                     trans = "log2") +
  labs(x = "Days Since 10th Confirmed Death",
     y = "Cumulative Number of Deaths (log2 scale)",
     title = "Cumulative Number of Reported Deaths from COVID-19, Selected Countries",
     subtitle = paste("Data as of", format(max(cov_curve$date), "%A, %B %e, %Y")),
     caption = "David Barron / Data: https://www.ecdc.europa.eu/")
