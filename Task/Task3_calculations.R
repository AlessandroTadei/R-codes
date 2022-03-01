# ================== FINN Task 3. Infleeting ====================#

# Assumptions:
# - today is 2021-12-02
# - cars arrive at the compound by the expected arrival date (tested at line 32)
# - subscription_handover_date is only the date agreed upon in the contract, also for the "active" subscriptions
# - PDI (pre-delivery inspection) and tire change are NOT executed on the same day
# - when PDI in "NOT_TRACKED" I assume it can be either done or not done
# - when tires_current is NA I assume it can be either to be changed or already changed
# - the estimated duration of PDI and tires changes already takes in consideration weekends, holidays, sick leaves
# - Mitsubishi is not taken in consideration in this analysis because I was not provided with the average PDI and tires change time


#### 1. Import libraries ####

library(readxl)
library(dplyr)
library(lubridate)


#### 2. Import datasets ####

infleeting_raw <- read_excel("Desktop/Case/datasets.xlsx", 
                       sheet = "Infleeting", 
                       col_types = c("numeric", "text", "date", "text", "text", "date", "text", "text"))

head(infleeting_raw)


#### 3. Test assumptions ####

# Do cars reach the compound by the expected date?
test_assumption_compund_dt <- infleeting_raw %>%
  filter(is.na(car_location) & expected_compound_arrival_date < "2021-12-02")

#### 4. Clean, enrich dataset ####

infleeting_rich <- infleeting_raw %>%
                    mutate(compound = case_when((product_brand == "BMW" | product_brand == "MINI") ~ "AKB",
                                                product_brand == "Skoda" ~ "Feser",
                                                product_brand == "Jeep" ~ "Mosolf-Kippenheim",
                                                product_brand == "Fiat" ~ "Mosolf-Etzin",
                                                product_brand == "Polestar" ~ "Eder"),
                           min_pdi_days = case_when((pdi_status == "YES" | pdi_status == "NOT_TRACKED") ~ 0,
                                                    (product_brand == "BMW" | product_brand == "MINI") & pdi_status == "NO" ~ 2,
                                                    (product_brand == "Jeep" | product_brand == "Fiat") & pdi_status == "NO" ~ 1,
                                                    product_brand == "Skoda" & pdi_status == "NO" ~ 3,
                                                    product_brand == "Polestar" & pdi_status == "NO" ~ 4),
                           max_pdi_days = case_when(pdi_status == "YES" ~ 0,
                                                    (product_brand == "BMW" | product_brand == "MINI") & (pdi_status == "NO" | pdi_status == "NOT_TRACKED") ~ 5,
                                                    (product_brand == "Jeep" | product_brand == "Fiat") & (pdi_status == "NO" | pdi_status == "NOT_TRACKED") ~ 2,
                                                    product_brand == "Skoda" & (pdi_status == "NO" | pdi_status == "NOT_TRACKED") ~ 4,
                                                    product_brand == "Polestar" & (pdi_status == "NO" | pdi_status == "NOT_TRACKED") ~ 6),
                           min_tires_days = ifelse((month(subscription_handover_date) >= 10 | month(subscription_handover_date) <= 4),
                                                  case_when((product_brand == "BMW" | product_brand == "MINI") & tires_current == "summer" ~ 4,
                                                    (product_brand == "Jeep" | product_brand == "Fiat") & tires_current == "summer" ~ 2,
                                                    product_brand == "Skoda" & tires_current == "summer" ~ 3,
                                                    product_brand == "Polestar" & tires_current == "summer" ~ 6,
                                                    TRUE ~ 0),
                                                  case_when((product_brand == "BMW" | product_brand == "MINI") & tires_current == "winter" ~ 4,
                                                     (product_brand == "Jeep" | product_brand == "Fiat") & tires_current == "winter" ~ 2,
                                                     product_brand == "Skoda" & tires_current == "winter" ~ 3,
                                                     product_brand == "Polestar" & tires_current == "winter" ~ 6,
                                                     TRUE ~ 0)),
                           max_tires_days = ifelse((month(subscription_handover_date) >= 10 | month(subscription_handover_date) <= 4),
                                                    case_when((product_brand == "BMW" | product_brand == "MINI") & (tires_current == "summer" | is.na(tires_current)) ~ 6,
                                                              (product_brand == "Jeep" | product_brand == "Fiat") & (tires_current == "summer" | is.na(tires_current)) ~ 4,
                                                              product_brand == "Skoda" & (tires_current == "summer" | is.na(tires_current)) ~ 5,
                                                              product_brand == "Polestar" & (tires_current == "summer" | is.na(tires_current)) ~ 8,
                                                              TRUE ~ 0),
                                                    case_when((product_brand == "BMW" | product_brand == "MINI") & (tires_current == "winter" | is.na(tires_current)) ~ 6,
                                                              (product_brand == "Jeep" | product_brand == "Fiat") & (tires_current == "winter" | is.na(tires_current)) ~ 4,
                                                              product_brand == "Skoda" & (tires_current == "winter" | is.na(tires_current)) ~ 5,
                                                              product_brand == "Polestar" & (tires_current == "winter" | is.na(tires_current)) ~ 8,
                                                              TRUE ~ 0))
                           )

infleeting_rich2 <- infleeting_rich %>%
                      mutate(min_exp_handover = as.Date(expected_compound_arrival_date) + min_pdi_days + min_tires_days,
                             max_exp_handover = as.Date(expected_compound_arrival_date) + max_pdi_days + max_tires_days,
                             subscription_handover_date = as.Date(subscription_handover_date),
                             expected_compound_arrival_date = as.Date(expected_compound_arrival_date)) 

infleeting_fin <- infleeting_rich2 %>%
                    mutate(delay = case_when(subscription_handover_date < expected_compound_arrival_date | subscription_handover_date < min_exp_handover ~ "Late",
                                            subscription_handover_date >= min_exp_handover & subscription_handover_date <= max_exp_handover ~ "On time",
                                            subscription_status == "signed" & subscription_handover_date < "2021-12-02" ~ "Late",
                                            subscription_handover_date > max_exp_handover ~ "Early")) %>%
                    mutate(min_days_delay = case_when(delay == "Late" ~ as.numeric(min_exp_handover - subscription_handover_date)), 
                           max_days_delay = case_when(delay == "Late" ~ as.numeric(max_exp_handover - subscription_handover_date))) %>%
                    select(x, product_brand, expected_compound_arrival_date, subscription_status, subscription_handover_date, compound, min_pdi_days, max_pdi_days, min_tires_days, max_tires_days, min_exp_handover, max_exp_handover, delay, min_days_delay, max_days_delay) %>%
                    filter(!is.na(subscription_handover_date), !is.na(delay))


#### 5. Export dataset for visualization ####

infleeting_fin$x <- format(infleeting_fin$x, scientific = F)
write.csv(infleeting_fin, "Desktop/Case/Task3_Infleeting/infleeting_fin.csv")
