# Create Dataset for Use in the Shiny App #
# --------------------------------------- #
rm(list = ls())

# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(countrycode)


# terrorism ---------------------------------------------------------------
# https://www.rand.org/nsrd/projects/terrorism-incidents.html
terror <- read_csv(
  here("data", "RAND_Database_of_Worldwide_Terrorism_Incidents.csv")
)
terror <- terror %>%
  mutate(year = sub(".*-", "", Date) %>%
           as.numeric())
terror$year[terror$year %in% 68:99] <- 
  terror$year[terror$year %in% 68:99] + 1900
terror$year[terror$year %in% 0:9] <- 
  terror$year[terror$year %in% 0:9] + 2000
terror_final <- terror %>%
  group_by(year, Country) %>%
  transmute(
    terrorist_attacks = n(),
    terrorist_attack_injuries = sum(Injuries),
    terrorist_attack_fatalities = sum(Fatalities)
  )
write_csv(
  terror_final,
  here("data", "terrorism.csv")
)

# nuclear weapons ---------------------------------------------------------
# https://fas.org/issues/nuclear-weapons/status-world-nuclear-forces/
nukes <- read_csv(
  here("data", "nuclear_weapons.csv")
)
nukes_long <- nukes %>% 
  .[-1, ] %>%
  pivot_longer(cols = 2:79,
               names_to = "year",
               values_to = "nuclear_weapons") %>%
  rename(
    Country = ...1
  )
write_csv(
  nukes_long,
  here("data", "nuclear_weapons_long.csv")
)

# internet access ---------------------------------------------------------
# https://data.worldbank.org/indicator/IT.NET.USER.ZS
internet_wide <- read_csv(
  here("data", "wdi_internet_wide.csv")
)
internet_wide %>%
  pivot_longer(
    cols = 2:63,
    names_to = "year",
    values_to = "internet_per_capita"
  ) -> internet_long
write_csv(
  internet_long,
  here("data", "internet_long.csv")
)

# climate change ----------------------------------------------------------
# https://github.com/owid/co2-data
climate <- read_csv(
  here("data", "owid-co2-data.csv")
)
climate <- climate %>%
  filter(year > 1967) %>%
  mutate(Country = country) %>%
  select(Country, year, co2)

# global health -----------------------------------------------------------
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN
health_wide <- read_csv(
  here("data", "wdi_life_expectancy_wide.csv")
)
health_long <- health_wide %>%
  pivot_longer(cols = -1) 
names(health_long) <- c("Country", "year", "life_expectancy")

# migration ---------------------------------------------------------------
# https://data.worldbank.org/indicator/SM.POP.TOTL
migration_wide <- read_csv(
  here("data", "wdi_migrant_stock.csv")
)
migration_long <- migration_wide %>%
  pivot_longer(cols = -1)
names(migration_long) <- c("Country", "year", "immigrant_population")

# trade -------------------------------------------------------------------
# https://data.worldbank.org/indicator/NE.IMP.GNFS.KD
imports_wide <- read_csv(
  here("data", "wdi_imports.csv")
)
imports_long <- imports_wide %>%
  pivot_longer(
    cols = -1
  )
names(imports_long) <- c("Country", "year", "imports")

# https://data.worldbank.org/indicator/NE.EXP.GNFS.KD
exports_wide <- read_csv(
  here("data", "wdi_exports.csv")
)
exports_long <- exports_wide %>%
  pivot_longer(
    cols = -1
  )
names(exports_long) <- c("Country", "year", "exports")

# money -------------------------------------------------------------------
library(pwt10)
money <- pwt10.0 %>%
  mutate(
    Country = countrycode(isocode, "iso3c", "country.name"),
    exchange_rate = xr
  ) %>%
  select(Country, year, exchange_rate) 

# development -------------------------------------------------------------
development <- pwt10.0 %>%
  mutate(
    Country = countrycode(isocode, "iso3c", "country.name"),
    development = rgdpna
  ) %>%
  select(Country, year, development)

# additional variables ----------------------------------------------------

#### Democracy
library(democracyData)
democracy <- download_polity_annual()
democracy <- democracy %>%
  select(polity_annual_country, year, polity2) %>%
  filter(year > 1960)
names(democracy) <- c("Country", "year", "democracy_score")

#### Population
pop <- pwt10.0 %>%
  mutate(
    Country = countrycode(isocode, "iso3c", "country.name"),
    population = pop
  ) %>%
  select(Country, year, population)

# combine -----------------------------------------------------------------

DATA <- list(
  terror_final,
  nukes_long,
  internet_long,
  climate,
  health_long,
  migration_long,
  imports_long,
  exports_long,
  money,
  development,
  democracy,
  pop
)
DATA <- map(DATA, 
            ~ {
              .x %>%
                mutate(year = as.numeric(year),
                       Country = 
                         countrycode(Country, "country.name", "iso3c"))
            })
save(
  DATA,
  file = here("data", "DATA.R")
)
rm(list = ls())
library(tidyverse)
library(here)
load(here::here("data", "DATA.R"))
index <- expand.grid(
  Country = lapply(DATA, function(x) sort(unique(x$Country))) %>%
    unlist() %>% unique() %>% sort(),
  year = lapply(DATA, function(x) sort(unique(x$year))) %>%
    unlist() %>% unique() %>% sort
)
mdata <- index
# data <- DATA %>%
#   reduce(full_join, by = c("Country", "year")) 
for(i in 1:length(DATA)) {
  mdata <- left_join(x = mdata,
                     y = DATA[[i]],
                     by = c("Country", "year"))
}
mdata <- mdata %>%
  mutate(Country = countrycode(Country, "iso3c", "country.name"))
mdata <- mdata %>% distinct()
mdata <- mdata %>%
  group_by(Country, year) %>%
  summarize(
    across(everything(), ~max(.x, na.rm=T))
  ) 
mdata <- mdata %>%
  ungroup %>%
  mutate(
    across(-c(1:2), ~ ifelse(is.infinite(.x), NA, .x))
  )
mdata$exchange_rate <- log(mdata$exchange_rate)
mdata$development <- mdata$development / mdata$population
mdata$trade_balance <- log(mdata$exports / mdata$imports)
write_csv(
  mdata,
  here("data", "full_data.csv")
)
