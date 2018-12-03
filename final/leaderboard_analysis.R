
# laod --------------------------------------------------------------------
library(dplyr)
library(readr)
library(xtable)

# read --------------------------------------------------------------------
#dat <- read_csv("final/daily-leaderboard-2018-11-02.csv") 
# dat <- read_csv("final/daily-leaderboard-2018-11-08.csv") 
dat <- read_csv("final/daily-leaderboard-2018-11-30.csv") 

dat_filtered <- dat %>%
  select(rank, 
         name, 
         score, 
         max_beta_to_spy_126day, 
         max_cumulative_common_returns, 
         max_leverage, 
         max_max_drawdown,
         max_net_dollar_exposure,
         max_total_returns,
         min_total_returns,
         max_turnover, 
         max_volatility_126day) 

all <- dat_filtered %>%
  summarise_all(funs(mean)) %>% 
  mutate_at(vars(-name), funs(round(., 3))) %>%
  tidyr::gather(Metric, Result)

joyce <- dat_filtered %>% 
  filter(rank == 105)  %>% 
  mutate_at(vars(-name), funs(round(., 3))) %>%
  tidyr::gather(Metric, Result) %>%
  bind_cols(all[,2])

xtable(joyce)
