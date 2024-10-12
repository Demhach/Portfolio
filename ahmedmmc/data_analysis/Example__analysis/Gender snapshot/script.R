library(readxl)
library(writexl)
library(janitor)
library(dplyr)
library(tidyr)
library(janitor)
#install.packages("ggplot2")
#install.packages("kableExtra")
library(kableExtra)
library(knitr)
library(lubridate)
library(stringr)
#install.packages("quantmod")
library(quantmod)
#install.packages("outliers")
library(outliers)
library(htmltools)


#Loading the data frame
df <- read_excel("../create_sample/agg_briefing_paper.xlsx", col_names = FALSE)

#Setting up the data frame

titles <- df[1:2,]

colnames(df) <- df [2,]

df_1 <- df[-c(1,2),]

#converting all numeric date variables to dates

df_1 <- df_1 %>% 
  mutate(across(c(`_submission_time`,today, Q45_1, Q25,Q165, Q141_3, S1_3, S2_3, S3_3, S4_3), ~ as.numeric(.))) %>%
  mutate(across(c(`_submission_time`,today, Q45_1), ~ excel_numeric_to_date(.)))

#Grouping variables

df_1 %>% 
  mutate(Q133 = ifelse(grepl("^Yes",Q133), "Yes", Q133)) %>%
  tabyl(Q133, Q27) %>%
  adorn_totals(where = c("row", "col"))%>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()%>%
  
  #Nice formatting
  adorn_title(row_name = "Smuggler use", col_name = "Gender", "combined") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Grouping Variables","","",""),align = "l")


#Profiling

#Q13 - country of interview

df_1 %>% 
  tabyl(Q13) %>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Country of interview", "", ""),align = "l")


#Q14 - Place of interview

df_1 %>% 
  tabyl(Q14) %>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Place of interview", "", ""),align = "l")


#Q25 - Age - Summary

df_1 %>%
  summarize(across(Q25, list(Mean = ~mean(., na.rm = TRUE),
                             Median = ~median(., na.rm = TRUE),
                             Min = ~min(., na.rm = TRUE),
                             Max = ~max(., na.rm = TRUE))))%>%
  rename_with(~ gsub("Q25_", "", .)) %>%  # Remove the "Q25_" prefix
  kable(align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Age", "", "",""),align = "l")


#Q27 - Sex

df_1 %>% 
  tabyl(Q27) %>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Sex", "", ""),align = "l")




#Q30 - Dependent children

df_1 %>% 
  tabyl(Q30) %>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Carrying dependent children", "", ""),align = "l")


#Q31 - country of origin

df_1 %>% 
  tabyl(Q31) %>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Country of origin", "", ""),align = "l")


#Q33 - migration status

df_1 %>% 
  tabyl(Q33) %>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Migration status", "", ""),align = "l")


# i. How does the use of smuggling services influence the choice of migration routes?

#Q41 - country of departure

df_1 %>% 
  tabyl(Q41) %>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Country of departure", "", ""),align = "l")


#Q45_1 - duration of journey today 

class(df_1[["today"]])

df_1 %>%
  mutate(journey_duration = difftime(df_1[["today"]],Q45_1))%>%
  mutate(journey_class = case_when (
    journey_duration < 30 ~ "Less than a month",
    journey_duration < 120 ~ "Between 1 to 6 months",
    journey_duration < 365 ~ "Between 6 to 12 months",
    TRUE ~ "More than a year"
  ))%>%
  tabyl(journey_class)%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Duration of journey", "", ""),align = "l")


############################################################################ 
#Countries transited

df_1 %>%
  select(`_id`,Q47:Q54)%>%
  select(-matches("_1"))%>%
  select(where(~!all(is.na(.))))%>%
  pivot_longer(cols = -`_id`, values_to = "countries_transited")%>%
  distinct(`_id`, countries_transited , .keep_all = TRUE) %>%
  filter(!is.na(countries_transited), countries_transited !="None")%>%
  tabyl(countries_transited)%>%
  mutate(percent = n/1102)%>%
  arrange(desc(n))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Countries transited", "", "N = 1102"),align = "l")


#transited vs passed directly

df_1 %>% 
  mutate(transited = case_when(
    Q47 == "None" ~ "Passed directly",
    TRUE ~ "Transited at least one country"
  ))%>%
  tabyl(transited)%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Transited", "", ""),align = "l")

################################################################################# 

# Countries of stops and their top 5 locations of stops with at least N=100

countries <- df_1 %>%
  pivot_longer(cols = c(Q56,Q59,Q62,Q65,Q68), names_to = "country_stop_rank", values_to = "country_stops")%>%
  select(country_stops)


locations <- df_1 %>%
  pivot_longer(cols = c(Q57,Q60,Q63,Q66,Q69), names_to = "location_stop_rank", values_to = "location_stops")%>%
  select(location_stops)

countries_locations <- cbind(countries,locations)

countries_locations %>%
  filter(if_all(c(country_stops, location_stops), ~ !is.na(.) & . != "None" & . != "Other"))%>%
  tabyl(country_stops,location_stops)%>%
  pivot_longer(-country_stops, names_to = "location_stops", values_to = "n") %>%  # Make it long
  arrange(country_stops, desc(n))%>%
  filter( n != 0)%>%
  adorn_totals(where = "row")%>%
  group_by(country_stops) %>%
  slice_max(order_by = n, n = 5) %>%  # Get top 5 for each country
  mutate(percent = n / 5866)%>%
  adorn_pct_formatting(column = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left")%>%
  add_header_above(header = c("Country/Location stops", "", "","N = 5866"),align = "l")




###########################################################  
# Countries of stops and their top 5 locations of stops and the top reasons 

countries <- df_1 %>%
  pivot_longer(cols = c(Q56,Q59,Q62,Q65,Q68), names_to = "country_stop_rank", values_to = "country_stops")%>%
  select(country_stops)


locations <- df_1 %>%
  pivot_longer(cols = c(Q57,Q60,Q63,Q66,Q69), names_to = "location_stop_rank", values_to = "location_stops")%>%
  select(location_stops)

countries_locations <- cbind(countries,locations)

df_2 <- cbind(df_1,countries_locations)



#First stop with location and reasons

stop_matrix <- df_1 %>%
  select(Q56:Q58_1)%>%
  select(-matches("_1"), -c(Q58, `Q58/Refused`, `Q58/Other`))%>%
  pivot_longer(col = -c(Q56,Q57),  # Pivot all except `country_stops` and `location_stops`
               names_to = "reasons_stop_rank", 
               values_to = "reasons_stop")%>%
  filter(if_all(c(reasons_stop), ~ !is.na(.) & . != "None"))%>%
  select(-reasons_stop_rank)


#aggregating second stop with location and reasons

stop_matrix <- rbind (stop_matrix, df_1 %>%
  select(Q59:Q61_1)%>%
  select(-matches("_1"), -c(Q61, `Q61/Refused`, `Q61/Other`))%>%
  pivot_longer(col = -c(Q59,Q60),  # Pivot all except `country_stops` and `location_stops`
               names_to = "reasons_stop_rank", 
               values_to = "reasons_stop")%>%
  rename(
    Q56 = Q59,
    Q57 = Q60)%>%
  filter(if_all(c(reasons_stop), ~ !is.na(.) & . != "None"))%>%
  select(-reasons_stop_rank))

#aggregating third stop with location and reasons

stop_matrix <- rbind (stop_matrix, df_1 %>%
  select(Q62:Q64_1)%>%
  select(-matches("_1"), -c(Q64, `Q64/Refused`, `Q64/Other`))%>%
  pivot_longer(col = -c(Q62,Q63),  # Pivot all except `country_stops` and `location_stops`
               names_to = "reasons_stop_rank", 
               values_to = "reasons_stop")%>%
  rename(
    Q56 = Q62,
    Q57 = Q63)%>%
  filter(if_all(c(reasons_stop), ~ !is.na(.) & . != "None"))%>%
  select(-reasons_stop_rank))


#aggregating fourth stop with location and reasons

stop_matrix <- rbind (stop_matrix, df_1 %>%
  select(Q65:Q67_1)%>%
  select(-matches("_1"), -c(Q67, `Q67/Refused`, `Q67/Other`))%>%
  pivot_longer(col = -c(Q65,Q66),  # Pivot all except `country_stops` and `location_stops`
               names_to = "reasons_stop_rank", 
               values_to = "reasons_stop")%>%
  rename(
    Q56 = Q65,
    Q57 = Q66)%>%
  filter(if_all(c(reasons_stop), ~ !is.na(.) & . != "None"))%>%
  select(-reasons_stop_rank))

#aggregating fifth stop with location and reasons

stop_matrix <- rbind (stop_matrix, df_1 %>%
  select(Q68:Q70_1)%>%
  select(-matches("_1"), -c(Q70, `Q70/Refused`, `Q70/Other`))%>%
  pivot_longer(col = -c(Q68,Q69),  # Pivot all except `country_stops` and `location_stops`
               names_to = "reasons_stop_rank", 
               values_to = "reasons_stop")%>%
  rename(
    Q56 = Q68,
    Q57 = Q69)%>%
  filter(if_all(c(reasons_stop), ~ !is.na(.) & . != "None"))%>%
  select(-reasons_stop_rank))

stop_matrix <- rename(stop_matrix,
       Country = Q56,
       Location = Q57) 


#Compute frequencies for each reason for each country

stop_matrix%>% 
  filter(Location!="Other")%>%
  group_by(Country, Location, reasons_stop)%>%
  summarize(frequency = n())%>%
  arrange(desc(frequency))%>%
  group_by(Country) %>%
  slice_max(order_by = frequency, n = 5)%>%
  mutate(percent= frequency / 5866) %>%
  adorn_pct_formatting(column = "percent")%>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left")%>%
  add_header_above(header = c("Country/Location stops", "", "","","N = 5866"),align = "l")
  
##############################################################################

#Q72 - Reasons for choosing a route

df_1 %>%
    select(`Q72/Safest`:`Q72/Refused`)  %>%
    pivot_longer(cols = everything(), values_to = "Reasons_for_choosing_route")%>%
    filter(!is.na(Reasons_for_choosing_route), 
           Reasons_for_choosing_route !="None", 
           Reasons_for_choosing_route !="Other", 
           Reasons_for_choosing_route !="Refused",
           Reasons_for_choosing_route !="Don't know")%>%
    tabyl(Reasons_for_choosing_route)%>%
    mutate(percent = n/2674)%>%
    arrange(desc(n))%>%
    #adorn_totals(where = "row")%>%
    adorn_pct_formatting(columns = "percent") %>%
    knitr :: kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = TRUE, 
                  position = "left")%>%
    add_header_above(header = c("Reasons for choosing_route", "", "N = 2674"),align = "l")


##############################################################################

#Q22 - end of journey

df_1 %>% 
  tabyl(Q22, show_na = FALSE) %>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("End of journey", "Removed NAs", ""),align = "l")


##############################################################################

#Q92 - Preferred destination

df_1 %>% 
  tabyl(Q92, show_na = FALSE) %>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Preferred destination", "Removed NAs", ""),align = "l")

##############################################################################

#S7 - Reasons for using smuggler services

df_1 %>%
  select(`S7/I thought it would be cheaper`:`S7/Refused`)  %>%
  pivot_longer(cols = everything(), values_to = "Reasons_for_using_smugglers")%>%
  filter(!is.na(Reasons_for_using_smugglers) & 
           Reasons_for_using_smugglers != "None" & 
           Reasons_for_using_smugglers != "Other" & 
           Reasons_for_using_smugglers != "Refused" & 
           Reasons_for_using_smugglers != "Don't know")%>%
  tabyl(Reasons_for_using_smugglers)%>%
  mutate(percent = n/1550)%>%
  arrange(desc(n))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Reasons for using smugglers", "", "N = 1550"),align = "l")


##############################################################################

#Q133 - use of smuggler

df_1 %>% 
  tabyl(Q133, show_na = FALSE) %>%
  filter(Q133 != "No")%>%
  #mutate(percent = n/1550)%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Use of smuggler", "Removed NAs", ""),align = "l")

##############################################################################

#Q134 - Types of smuggler services

df_1 %>%
  filter(smuggler_module_open == "Yes" )%>%
  select(`Q134/Provision of documents`:`Q134/Refused`)  %>%
  pivot_longer(cols = everything(), values_to = "Types_smuggler_services")%>%
  filter(!is.na(Types_smuggler_services) & 
           Types_smuggler_services != "None" & 
           Types_smuggler_services != "Other" & 
           Types_smuggler_services != "Refused" & 
           Types_smuggler_services != "Don't know")%>%
  tabyl(Types_smuggler_services)%>%
  mutate(percent = n/1550)%>%
  arrange(desc(n))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Types of smuggler services", "", "N = 1550"),align = "l")

##############################################################################

#S10 + S10_3: Use of smuggler to cross a border

df_1 %>%
  tabyl(S10, show_na = FALSE)%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Use of smuggler to cross a border", "", ""),align = "l")


df_1 %>% 
  select(S10, `S10_3/Egypt/Libya`:`S10_3/West - Togo/Burkina Faso`)%>%
  pivot_longer(cols = `S10_3/Egypt/Libya`:`S10_3/West - Togo/Burkina Faso`, values_to = "border_crossed")%>%
  select(-name)%>%
  filter(!is.na(border_crossed))%>%
  tabyl(border_crossed,S10)%>%
  mutate(percent = Yes/1550)%>%
  arrange(desc(Yes))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Borders crossed with a smuggler", "", "N = 1550"),align = "l")

#S6: predeparture intentions for using smuggling services

df_1 %>%
  tabyl(S6, show_na = FALSE)%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("predeparture intentions for using smuggling services", "", ""),align = "l")



# ii.	How do smuggling fees affect journey costs and financing options?

#Q138: initial finance of the journey

df_1 %>% 
  select(`Q138/Borrowing`: `Q138/Refused`)%>%
  select(-`Q138/Other`)%>%
  pivot_longer(cols = everything(), values_to = "smuggling_financing")%>%
  select(-name)%>%
  filter(!is.na(smuggling_financing))%>%
  tabyl(smuggling_financing)%>%
  mutate(percent = n/2674)%>%
  arrange(desc(percent))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Borders crossed with a smuggler", "", "N = 2674"),align = "l")


#Q139: enough funding?

df_1 %>%
  tabyl(Q139, show_na = FALSE)%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Enough funding?", "Removed NAs", "", ""),align = "l")



#Q140: How have you paid for the journey since then?

df_1 %>% 
  select(`Q140/I worked`: `Q140/Refused`)%>%
  select(-`Q140/Other`)%>%
  pivot_longer(cols = everything(), values_to = "ongoing_journey_financing")%>%
  select(-name)%>%
  filter(!is.na(ongoing_journey_financing))%>%
  tabyl(ongoing_journey_financing)%>%
  mutate(percent = n/2674)%>%
  arrange(desc(percent))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(columns = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Financing means of ongoing journey", "", "N = 2674"),align = "l")


#Q141 - cost of journey - Summary


getFX("XOF/USD")
cfa_usd <- as.numeric(last(XOFUSD$XOF.USD))
getFX("EUR/USD")
eur_usd <- as.numeric(last(EURUSD$EUR.USD))

df_1 %>%
  filter(Q141 == "Know the amount") %>%
  select(Q141_1, Q141_3) %>%
  mutate(
    Q141_3 = as.numeric(Q141_3),  # Convert the column to numeric
    Q141_3 = case_when(
      Q141_1 == "CFA" ~ round(Q141_3 * cfa_usd),  # Convert CFA to USD
      Q141_1 == "EUR" ~ round(Q141_3 * eur_usd),  # Convert EUR to USD
      TRUE ~ Q141_3  # Keep the original value for other cases
    )
  ) %>%
  filter(Q141_3 != outlier(Q141_3))%>%
  summarize(across(Q141_3, list(Mean = ~paste0(round(mean(., na.rm = TRUE),2), ' $'),
                                Median = ~paste0(median(., na.rm = TRUE),' $'),
                                Min = ~paste0(min(., na.rm = TRUE),' $'),
                                Max = ~paste0(max(., na.rm = TRUE),' $'))))%>%
  rename_with(~ gsub("Q141_3_", "", .)) %>%  # Remove the "Q25_" prefix
  kable(align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Cost of journey", "", "","N = only those that know the amount"),align = "l")


#Q135: Payment modalities of smuggling services

df_1 %>%
  filter(Q133 != "No" & smuggler_module_open == "Yes")%>%
  tabyl(Q135)%>%
  #filter(!is.na(Q139))%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Payment modalities for smuggling services","", ""),align = "l")


#S1 - Total amount paid for smuggling services


getFX("XOF/USD")
cfa_usd <- as.numeric(last(XOFUSD$XOF.USD))

getFX("EUR/USD")
eur_usd <- as.numeric(last(EURUSD$EUR.USD))

getFX("GHS/USD")
ghs_usd <- as.numeric(last(GHSUSD$GHS.USD))

getFX("GNF/USD")
gnf_usd <- as.numeric(last(GNFUSD$GNF.USD))

getFX("NGN/USD")
ngn_usd <- as.numeric(last(NGNUSD$NGN.USD))

df_1 %>%
  filter(smuggler_module_open == "Yes" & Q133 != "No" & S1 =="Know the amount") %>%
  select(S1_1, S1_3) %>%
  mutate(
    S1_3 = as.numeric(S1_3),  # Convert the column to numeric
    S1_3 = case_when(
      S1_1 == "CFA" ~ round(S1_3 * cfa_usd),  # Convert CFA to USD
      S1_1 == "GHS" ~ round(S1_3 * ghs_usd),  # Convert GHS to USD
      S1_1 == "GNF" ~ round(S1_3 * gnf_usd),
      S1_1 == "NGN" ~ round(S1_3 * ngn_usd),
      TRUE ~ S1_3  # Keep the original value for other cases
    )
  ) %>%
  filter(S1_3 != 0)%>%
  summarize(across(S1_3, list(Mean = ~paste0(round(mean(., na.rm = TRUE),2), ' $'),
                              Median = ~paste0(median(., na.rm = TRUE),' $'),
                              Min = ~paste0(min(., na.rm = TRUE),' $'),
                              Max = ~paste0(max(., na.rm = TRUE),' $'))))%>%
  rename_with(~ gsub("S1_3_", "", .)) %>%  # Remove the "Q25_" prefix
  kable(align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Total amount paid so far for smugglers", "", "","N = only those that know the amount"),align = "l")




#S2/S3/S4 - Total amount paid for smuggling services in installments


getFX("XOF/USD")
cfa_usd <- as.numeric(last(XOFUSD$XOF.USD))

getFX("EUR/USD")
eur_usd <- as.numeric(last(EURUSD$EUR.USD))

getFX("GHS/USD")
ghs_usd <- as.numeric(last(GHSUSD$GHS.USD))

getFX("GNF/USD")
gnf_usd <- as.numeric(last(GNFUSD$GNF.USD))

getFX("NGN/USD")
ngn_usd <- as.numeric(last(NGNUSD$NGN.USD))

getFX("SLL/USD")
sll_usd <- as.numeric(last(SLLUSD$SLL.USD))

cbind(df_1 %>%
  filter(smuggler_module_open == "Yes" & Q133 != "No" & is.na(S1)) %>%
  select(S2_1,S3_1,S4_1) %>%
  pivot_longer(cols = everything(), values_to = "Currency"),
  df_1 %>%
  filter(smuggler_module_open == "Yes" & Q133 != "No" & is.na(S1)) %>%
  select(S2_3,S3_3,S4_3) %>%
  pivot_longer(cols = everything(), values_to = "Amount"))%>%
  select(-name)%>%
  filter(!is.na(Amount))%>%
  mutate(
    Amount = as.numeric(Amount),  # Convert the column to numeric
    Amount = case_when(
      Currency == "CFA" ~ round(Amount * cfa_usd),  # Convert CFA to USD
      Currency == "GHS" ~ round(Amount * ghs_usd),  # Convert GHS to USD
      Currency == "GNF" ~ round(Amount * gnf_usd),
      Currency == "NGN" ~ round(Amount * ngn_usd),
      Currency == "EUR" ~ round(Amount * eur_usd),
      Currency == "SLL" ~ round(Amount * sll_usd),
      TRUE ~ Amount  # Keep the original value for other cases
    )
  ) %>%
  filter(Amount != 0)%>%
  arrange(desc(Amount))%>%
  summarize(across(Amount, list(Mean = ~paste0(round(mean(., na.rm = TRUE),2), ' $'),
                              Median = ~paste0(median(., na.rm = TRUE),' $'),
                              Min = ~paste0(min(., na.rm = TRUE),' $'),
                              Max = ~paste0(max(., na.rm = TRUE),' $'))))%>%
  rename_with(~ gsub("Currency_", "", .)) %>%  
  kable(align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Total amount paid for smuggling services in installments", "", "","N =  only those that know the amount"),align = "l")


#Q163: contact with state officials

df_1 %>%
  filter(smuggler_module_open == "Yes" & Q133 != "No")%>%
  select(`Q163/Police at a border`: `Q163/None of these`)%>%
  #select(-`Q140/Other`)%>%
  pivot_longer(cols = everything(), values_to = "contact_with_officials")%>%
  select(-name)%>%
  filter(!is.na(contact_with_officials))%>%
  tabyl(contact_with_officials)%>%
  mutate(percent = n/1550)%>%
  arrange(desc(percent))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(colum = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Contact with state officials", "", "N = 1550"),align = "l")


#Q164: corruption

df_1 %>%
  filter(smuggler_module_open == "Yes" & Q133 != "No")%>%
  select(`Q164/Police at a border`: `Q164/None of these`)%>%
  #select(-`Q140/Other`)%>%
  pivot_longer(cols = everything(), values_to = "contact_with_officials")%>%
  select(-name)%>%
  filter(!is.na(contact_with_officials))%>%
  tabyl(contact_with_officials)%>%
  mutate(percent = n/1550)%>%
  arrange(desc(percent))%>%
  #adorn_totals(where = "row")%>%
  adorn_pct_formatting(colum = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Corruption with officials", "", "N = 1550"),align = "l")


#S11: Were state officials involved in or did they facilitate migrant smuggling during your journey? 

df_1 %>%
  filter(Q133 != "No" & smuggler_module_open == "Yes")%>%
  tabyl(S11)%>%
  #filter(!is.na(Q139))%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("State officials involvement in smuggling","", ""),align = "l")



# iii.	What are the links between reliance on smuggler services and protection risks?

# Q99 ; 103 ; 107 ; 111 ; 115 - Most dangerous location

df_1 %>%
  select(Q99,Q103,Q107,Q111,Q115)%>%
  pivot_longer(cols = everything(), values_to = "dangerous_locations")%>%
  filter(dangerous_locations != "Other" & !is.na(dangerous_locations))%>%
  tabyl(dangerous_locations)%>%
  arrange(desc(n))%>%
  adorn_totals()%>%
  mutate(percent = n/2674)%>%
  adorn_pct_formatting(column = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Most dangerous locations","", "N= 2674"),align = "l")




###################################################################

# Q100 ; 104 ; 108 ; 112 ; 116: perceived risks


# Main locations of danger and the top perceived risks


#First danger location and perceived risks
perceived_risks <- df_1 %>%
  select(Q98, Q99:`Q100/Refused`) %>%
  select(-matches("_1"), -c(Q100, `Q100/Refused`, `Q100/Other`, Q99_2)) %>%
  pivot_longer(cols = -c(Q98, Q99),  # Pivot all except Q98 and Q99
               values_to = "perceived_risks") %>%
  filter(if_all(c(perceived_risks, Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
  select(-name)


#aggregating second stop with location and reasons

perceived_risks <- rbind (perceived_risks, df_1 %>%
                            select(Q102:`Q104/Refused`) %>%
                            select(-matches("_1"), -c(Q104, `Q104/Refused`, `Q104/Other`, Q103_2)) %>%
                            pivot_longer(cols = -c(Q102, Q103),  # Pivot all except Q98 and Q99
                                         values_to = "perceived_risks") %>%
                            rename(
                              Q98 = Q102,
                              Q99 = Q103)%>%
                            filter(if_all(c(perceived_risks,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

#aggregating third stop with location and reasons

perceived_risks <- rbind (perceived_risks, df_1 %>%
                            select(Q106:`Q108/Refused`) %>%
                            select(-matches("_1"), -c(Q108, `Q108/Refused`, `Q108/Other`, Q107_2)) %>%
                            pivot_longer(cols = -c(Q106, Q107),  # Pivot all except Q98 and Q99
                                         values_to = "perceived_risks") %>%
                            rename(
                              Q98 = Q106,
                              Q99 = Q107)%>%
                            filter(if_all(c(perceived_risks,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))


#aggregating fourth stop with location and reasons

perceived_risks <- rbind (perceived_risks, df_1 %>%
                            select(Q110:`Q112/Refused`) %>%
                            select(-matches("_1"), -c(Q112, `Q112/Refused`, `Q112/Other`, Q111_2)) %>%
                            pivot_longer(cols = -c(Q110, Q111),  # Pivot all except Q98 and Q99
                                         values_to = "perceived_risks") %>%
                            rename(
                              Q98 = Q110,
                              Q99 = Q111)%>%
                            filter(if_all(c(perceived_risks,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

#aggregating fifth stop with location and reasons

perceived_risks <- rbind (perceived_risks, df_1 %>%
                            select(Q114:`Q116/Refused`) %>%
                            select(-matches("_1"), -c(Q116, `Q116/Refused`, `Q116/Other`, Q115_2)) %>%
                            pivot_longer(cols = -c(Q114, Q115),  # Pivot all except Q98 and Q99
                                         values_to = "perceived_risks") %>%
                            rename(
                              Q98 = Q114,
                              Q99 = Q115)%>%
                            filter(if_all(c(perceived_risks,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

perceived_risks <- rename(perceived_risks,
                      Country = Q98,
                      Location = Q99) 


#Compute frequencies for each reason for each country
perceived_risks %>%
  group_by(Country, Location, perceived_risks) %>%
  summarize(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency)) %>%
  group_by(Country) %>%
  slice_max(order_by = frequency, n = 5, with_ties = FALSE) %>%  # Ensure only the top 5 per country
  mutate(percent = frequency /1715)%>% 
  adorn_pct_formatting(column = "percent")%>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left") %>%
  add_header_above(header = c("Top 5 perceived risks per country", "", "","", "N= 1715"), align = "l")



# Q101 ; 105 ; 109 ; 113 ; 117: likely perpetrators


#First danger location and likely perpetrators
perpetrators_df <- df_1 %>%
  select(Q98, Q99,`Q101/Smugglers`:`Q101/Refused`) %>%
  select(-matches("_1"), -c(`Q101/Refused`, `Q101/Other`)) %>%
  pivot_longer(cols = -c(Q98, Q99),  # Pivot all except Q98 and Q99
               values_to = "perpetrators") %>%
  filter(if_all(c(perpetrators, Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
  select(-name)


#aggregating second stop with location and likely perpetrators

perpetrators_df <- rbind (perpetrators_df, df_1 %>%
                            select(Q102, Q103,`Q105/Smugglers`:`Q105/Refused`) %>%
                            select(-matches("_1"), -c(`Q105/Refused`, `Q105/Other`)) %>%
                            pivot_longer(cols = -c(Q102, Q103),  # Pivot all except Q98 and Q99
                                         values_to = "perpetrators") %>%
                            rename(
                              Q98 = Q102,
                              Q99 = Q103)%>%
                            filter(if_all(c(perpetrators,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

#aggregating third stop with location and likely perpetrators

perpetrators_df <- rbind (perpetrators_df, df_1 %>%
                            select(Q106, Q107,`Q109/Smugglers`:`Q109/Refused`) %>%
                            select(-matches("_1"), -c(`Q109/Refused`, `Q109/Other`)) %>%
                            pivot_longer(cols = -c(Q106, Q107),  # Pivot all except Q98 and Q99
                                         values_to = "perpetrators") %>%
                            rename(
                              Q98 = Q106,
                              Q99 = Q107)%>%
                            filter(if_all(c(perpetrators,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))


#aggregating fourth stop with location and likely perpetrators


perpetrators_df <- rbind (perpetrators_df, df_1 %>%
                            select(Q110, Q111,`Q113/Smugglers`:`Q113/Refused`) %>%
                            select(-matches("_1"), -c(`Q113/Refused`, `Q113/Other`)) %>%
                            pivot_longer(cols = -c(Q110, Q111),  # Pivot all except Q98 and Q99
                                         values_to = "perpetrators") %>%
                            rename(
                              Q98 = Q110,
                              Q99 = Q111)%>%
                            filter(if_all(c(perpetrators,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

#aggregating fifth stop with location and likely perpetrators

perpetrators_df <- rbind (perpetrators_df, df_1 %>%
                            select(Q114, Q115,`Q117/Smugglers`:`Q117/Refused`) %>%
                            select(-matches("_1"), -c(`Q117/Refused`, `Q117/Other`)) %>%
                            pivot_longer(cols = -c(Q114, Q115),  # Pivot all except Q98 and Q99
                                         values_to = "perpetrators") %>%
                            rename(
                              Q98 = Q114,
                              Q99 = Q115)%>%
                            filter(if_all(c(perpetrators,  Q99), ~ !is.na(.) & . != "None" & . != "Other")) %>%
                            select(-name))

perpetrators_df <- rename(perpetrators_df,
                          Country = Q98,
                          Location = Q99) 


#Compute frequencies for  likely perpetrators for each country
perpetrators_df %>%
  group_by(Country, Location, perpetrators) %>%
  summarize(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency)) %>%
  group_by(Country) %>%
  slice_max(order_by = frequency, n = 5, with_ties = FALSE) %>%  # Ensure only the top 5 per country
  mutate(percent = frequency /1715)%>% 
  adorn_pct_formatting(column = "percent")%>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left") %>%
  add_header_above(header = c("Top 5 likely perpetrators per country", "", "","", "N= 1715"), align = "l")



#Q118: Have you personally experienced any of these types of incidents on your journey?


df_1 %>%
  select(`Q118/Witnessed death`: `Q118/Refused`)%>%
  pivot_longer(cols = everything(), values_to = "experienced_incidents")%>%
  select(-name)%>%
  tabyl(experienced_incidents, show_na = FALSE)%>%
  arrange(desc(n))%>%
  mutate(percent = n / 1170)%>%
  adorn_pct_formatting( column = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Experienced incidents", "", "N = 1170"),align = "l")



#Q123: Did you receive assistance along the way?

df_1 %>%
  tabyl(Q123)%>%
  #filter(!is.na(Q139))%>%
  arrange(desc(n))%>%
  adorn_totals(where = "row")%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Did you receive assistance along the way?","", ""),align = "l")



#Q124: What kind of assistance did you receive?


df_1 %>%
  select(`Q124/Shelter`: `Q124/Refused`)%>%
  select(-`Q124/Other`)%>%
  pivot_longer(cols = everything(), values_to = "assistance_received")%>%
  select(-name)%>%
  tabyl(assistance_received, show_na = FALSE)%>%
  mutate(percent = n / 1387)%>%
  arrange(desc(n))%>%
  #adorn_totals()%>%
  adorn_pct_formatting(column = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Received assistance", "", "N= 1387"),align = "l")



#Q125: Who did you receive assistance from?

df_1 %>%
  select(`Q125/The government`: `Q125/Refused`)%>%
  select(-`Q125/Other`)%>%
  pivot_longer(cols = everything(), values_to = "assistance_provider")%>%
  select(-name)%>%
  tabyl(assistance_provider, show_na = FALSE)%>%
  mutate(percent = n / 1387)%>%
  arrange(desc(n))%>%
  #adorn_totals()%>%
  adorn_pct_formatting(column = "percent") %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Assitance providers", "", "N= 1387"),align = "l")


# Q126/Q127: Places where assistance was needed the most and not received

df_1 %>%
  select(Q126, Q127)%>%
  filter(Q126 != "None")%>%
  group_by(Q126,Q127)%>%
  summarize( frequency = n())%>%
  arrange(desc(frequency))%>%
  adorn_totals(where = "row")%>%
  mutate(percent = frequency / 1019)%>%
  adorn_pct_formatting(column = "percent")%>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Places where assistance \n was needed the most and not received", "", "removed NAs",""),align = "l")




#Q125: Who did you receive assistance from?

df_1 %>%
  select(`Q128/Shelter`: `Q128/Refused`)%>%
  select(-`Q128/Other`)%>%
  pivot_longer(cols = everything(), values_to = "most_needed_assistance")%>%
  select(-name)%>%
  tabyl(most_needed_assistance, show_na = FALSE)%>%
  arrange(desc(n))%>%
  adorn_totals()%>%
  adorn_pct_formatting() %>%
  knitr :: kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = TRUE, 
                position = "left")%>%
  add_header_above(header = c("Most needed assistance en route", "", ""),align = "l")




