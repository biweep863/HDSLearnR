library(tidyverse)
micro_world <- read_csv("micro_world.csv")

# overall account ownership
overall_account <-
  micro_world %>%
  group_by(economy) %>%
  summarize(account = 100 * weighted.mean(account, w = wgt))

# account ownership by gender
account_gap_female <-
  micro_world %>%
  group_by(economy, female) %>%
  summarize(account = 100 * weighted.mean(account, w = wgt))
#gender gap in account ownership
account_gap_female <-
  micro_world %>%
  group_by(economy, female) %>%
  summarize(account = 100 * weighted.mean(account, w = wgt)) %>%
  pivot_wider(id_cols = economy,
              names_from = female,
              names_prefix = "female",
              values_from = account) %>%
  mutate(gap_female = female1 - female2) %>%
  select(economy, gap_female)
# income gap in account ownership
account_gap_inc <-
  micro_world %>%
  mutate(lower_inc = inc_q <= 2) %>%
  group_by(economy, lower_inc) %>%
  summarize(account = 100 * weighted.mean(account, w = wgt)) %>%
  pivot_wider(id_cols = economy,
              names_from = lower_inc,
              names_prefix = "lower_inc",
              values_from = account) %>%
  
  mutate(gap_inc = lower_incFALSE - lower_incTRUE) %>%
  select(economy, gap_inc)

#indicator table
indicator_table <- 
  full_join(overall_account,
            account_gap_female,
            by = "economy") %>%
  full_join(account_gap_inc,
            by = "economy")%>%
  mutate(account = round(account),
         gap_female = round(gap_female),
         gap_inc = round(gap_inc))

#3.1
micro_world_account <-
  micro_world %>%
  group_by (economycode) %>%
  summarize(account = 100 * weighted.mean (account, w = wgt))

wb_income <-
  read_csv("wb_income.csv")
account_by_income <-
  left_join (micro_world_account, wb_income)

## GGplot
ggplot(account_by_income,
       aes (account,
            income_category,
            color = income_category)) +
  geom_point () +
  scale_x_continuous (limits = c(0, 100),
                      breaks = seq(from = 0, to = 100, by = 20))
ggsave("account_by_income.png")


write_csv(x = indicator_table, file = "ac_by_inc.csv")
