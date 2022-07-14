args = commandArgs(trailingOnly=TRUE)

print(paste0("Monitoring observations for experiemtn ", args[1]))

library(tidyverse)
library(lubridate)
library(data.table)
library(mesoda)

basedir <- "/glade/scratch/pcorrales/EXP/"
exp = args[1]


if (exp %in% c("E10_long", "EG3_long")) {

## Radiances -------------------------------------------------------------------

satinfo <- fread("~/comGSIv3.7_EnKFv1.3/fix/global_satinfo.txt") %>%
  .[iuse > 0] %>%
  setnames(c("!sensor/instr/sat", "chan"), c("sensor", "channel"))

files <- Sys.glob(paste0(basedir, exp, "/ANA/*/diagfiles/asim*ensmean"))

files <- files[str_detect(files, "conv", negate = TRUE)]

diag_rad_mean <- map(files, function(f) {

  # print(basename(f))

  read_diag_rad(f, this_exp = exp) %>%
    .[, channel := fifelse(sensor == "abi_g16", channel + 6, channel )] %>%
    satinfo[., on = c("sensor", "channel")] %>%
    .[qc == 0 & iuse > 0 & errinv %between% c(0.0000316227, 1000),
      .(mean_om = mean(tbc),
        sd_om   = sd(tbc),
        count   = .N), by = .(sensor, channel, exp, date)]
}) %>% rbindlist()

diag_rad_mean %>%
  separate(sensor, into = c("sensor", "plat"), sep = "_") %>%
  setDT() %>%
  .[, .(count = sum(count)), by = .(sensor, exp, date)] %>%
  ggplot(aes(date, count)) +
  geom_line(aes(color = sensor), size = 0.1) +
  geom_point(aes(color = sensor)) +
  scale_color_viridis_d(labels = toupper) +
  scale_y_log10() +
  scale_x_datetime(date_breaks = "2 days", date_label = "%d", expand = c(0,0)) +
  labs(x = "Day in November",
       y = "# obs",
       title = "Radiances",
       subtitle = paste0("Experiment: ", exp)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(paste0("n_", exp, "_rad.png"), bg = "white")

sensors <- unique(diag_rad_mean$sensor)

for (s in sensors) {
  print(s)

  diag_rad_mean %>%
    .[sensor == s] %>%
    melt(id.vars = c("channel", "date"), measure.vars = c("mean_om", "sd_om")) %>%
    ggplot(aes(date, value)) +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_line(aes(color = variable)) +
    scale_x_datetime(date_breaks = "2 days", date_label = "%d", expand = c(0,0)) +
    facet_wrap(vars(channel)) +
    labs(x = "Day in November",
         color = NULL,
         title = paste0("Sensor: ", s),
         subtitle = paste0("Experiment: ", exp)) +
    theme_minimal()

  ggsave(paste0("omb_", exp, "_", s, ".png"), bg = "white")
}

# write_rds(diag_rad_mean, here::here("analysis/data/derived_data/diag_rad_mean_E10_long_all.rds"))

}
## Convencional  -------------------------------------------------------------------

files <- Sys.glob(paste0(basedir, exp, "/ANA/*/diagfiles/asim*ensmean"))

files <- files[str_detect(files, "conv", negate = FALSE)]

diag_conv_mean <- map(files, function(f) {

#  print(basename(f))

  read_diag_conv(f, exp = exp) %>%
    .[usage.flag > 0 & rerr >0,
      .(mean_om = mean(obs.guess),
        sd_om   = sd(obs.guess),
        count   = .N), by = .(var, type, exp, date)]
}) %>% rbindlist()


diag_conv_mean %>%
  .[, count := sum(count), by = .(var, date)] %>%
  ggplot(aes(date, count)) +
  geom_line(aes(color = var)) +
  scale_x_datetime(date_breaks = "2 days", date_label = "%d", expand = c(0,0)) +
  labs(x = "Day in November",
       y = "# obs",
       color = "variable",
       title = "Convencional obs",
       subtitle = paste0("Experiment: ", exp)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(paste0("n_", exp, "_conv.png"), bg = "white")

vars <- unique(diag_conv_mean$var)

for (v in vars) {


  diag_conv_mean %>%
    .[var == v & !(type %in% c(130, 180, 230, 280, 243))] %>%
  # .[, count := sum(count), by = .(var, date)] %>%
  melt(id.vars = c("type", "date"), measure.vars = c("mean_om", "sd_om")) %>%
  ggplot(aes(date, value)) +
    geom_hline(yintercept = 0, color = "grey80") +
  geom_line(aes(color = variable)) +
  scale_x_datetime(date_breaks = "2 days", date_label = "%d", expand = c(0,0)) +
  facet_wrap(vars(type), scales = "free_y") +
  labs(x = "Day in November",
       y = "# obs",
       color = "variable",
       title = paste0("Variable: ", v),
       subtitle = paste0("Experiment: ", exp)) +
  theme_minimal() +
  theme(legend.position = "bottom")

  ggsave(paste0("omb_", exp, "_", v, ".png"), bg = "white")
}

# write_rds(diag_conv_mean, here::here("analysis/data/derived_data/diag_conv_mean_E10_long_all.rds"))

