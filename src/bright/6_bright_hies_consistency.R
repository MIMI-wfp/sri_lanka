

hies_adm1_avg <- read.csv("data/processed/hies_adm1_avg.csv")
bright_adm1_avg <- read.csv("data/processed/bright_adm1_avg.csv")


combined_df_long <- 
  bind_rows(
    "hies" = hies_adm1_avg,
    "bright" = bright_adm1_avg,
    .id = "source"
  )%>% 
  select(-X)


combined_df_wide <- hies_adm1_avg %>%
  inner_join(bright_adm1_avg,
             by = "adm1",
             suffix = c("_hies", "_bright"))


# ---- 1. Define variable mapping ----
vars <- list(
  iron   = c("fe_inad_hies", "fe_inad_bright"),
  zinc   = c("zn_inad_hies", "zn_inad_bright"),
  vita   = c("vita_inad_hies", "vita_inad_bright"),
  folate = c("folate_inad_hies", "folate_inad_bright"),
  b12    = c("vitb12_inad_hies", "vitb12_inad_bright"),
  # add MAR only if you have mar_hies
  MAR = c("mar_hies", "mar_bright")
)

# ---- 2. Build plotting dataset ----
plot_df <- bind_rows(lapply(names(vars), function(v) {
  tibble(
    variable = v,
    x = combined_df_wide[[vars[[v]][1]]],
    y = combined_df_wide[[vars[[v]][2]]]
  )
}))

# ---- 3. Compute correlations ----
cor_df <- plot_df %>%
  group_by(variable) %>%
  summarise(cor = cor(x, y, use = "complete.obs"), .groups = "drop")

# ---- 4. Plot ----
ggplot(plot_df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  facet_wrap(~variable, scales = "free") +
  geom_text(
    data = cor_df,
    aes(x = -Inf, y = Inf,
        label = paste0("r = ", round(cor, 2))),
    hjust = -0.1, vjust = 1.2,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(
    x = "HIES",
    y = "BRIGHT"
  )



