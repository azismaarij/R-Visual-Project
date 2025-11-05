

  library(ggparliament)
  library(dplyr)
  library(ggplot2)
  library(tibble)
  library(tidyr)
  library(scales)

# -------------------------
# 1) DPR seat data (majors)
# -------------------------
dpr_hist <- tribble(
  ~year, ~party, ~seats,
  1999, "PDI-P", 153,
  1999, "Golkar", 120,
  1999, "PPP",   58,
  1999, "PKB",   51,
  1999, "PAN",   34,
  1999, "PBB",   13,
  
  2004, "Golkar",   128,
  2004, "PDI-P",    109,
  2004, "Demokrat",  55,
  2004, "PKB",       52,
  2004, "PPP",       58,
  2004, "PKS",       45,
  2004, "PAN",       53,
  2004, "PBB",       11,
  2004, "PBR",       14,
  
  2009, "Demokrat", 148,
  2009, "Golkar",   106,
  2009, "PDI-P",     94,
  2009, "PKS",       57,
  2009, "PAN",       46,
  2009, "PPP",       38,
  2009, "PKB",       28,
  2009, "Gerindra",  26,
  2009, "Hanura",    17,
  
  2014, "PDI-P",   109,
  2014, "Golkar",   91,
  2014, "Gerindra", 73,
  2014, "Demokrat", 61,
  2014, "PKB",      47,
  2014, "PAN",      49,
  2014, "PKS",      40,
  2014, "NasDem",   35,
  2014, "PPP",      39,
  2014, "Hanura",   16,
  
  2019, "PDI-P",   128,
  2019, "Golkar",   85,
  2019, "Gerindra", 78,
  2019, "PKB",      58,
  2019, "NasDem",   59,
  2019, "PKS",      50,
  2019, "Demokrat", 54,
  2019, "PAN",      44,
  2019, "PPP",      19,
  
  2024, "PDI-P",   110,
  2024, "Golkar",  102,
  2024, "Gerindra", 86,
  2024, "PKB",      68,
  2024, "NasDem",   69,
  2024, "PKS",      53,
  2024, "Demokrat", 44,
  2024, "PAN",      48
)

# Add “Others” to reach the official totals (1999=462, 2004=550)
add_others <- function(df, yr, total) {
  cur <- df %>% filter(year == yr) %>% summarise(n = sum(seats), .groups = "drop") %>% pull(n)
  if (!is.na(cur) && total > cur) bind_rows(df, tibble(year = yr, party = "Others", seats = total - cur)) else df
}
dpr_hist <- dpr_hist %>% add_others(1999, 462) %>% add_others(2004, 550)

# -------------------------
# 2) Pastel palette + coalitions
# -------------------------
party_cols <- c(
  "PDI-P"="#f6a5a3","Golkar"="#fce79f","Gerindra"="#e5b9a8","PKB"="#a8d5ba",
  "NasDem"="#a9c9e4","PKS"="#fbc7a7","Demokrat"="#a7bfe8","PAN"="#a8e0ef",
  "PPP"="#b9dfc9","PBB"="#b5c6a9","PBR"="#d7c0dc","Hanura"="#fdd7a4","Others"="#e5e5f7"
)

gov_map <- tribble(
  ~year, ~party, ~government,
  1999,"PKB",1, 1999,"PAN",1, 1999,"Golkar",1, 1999,"PPP",1,
  2004,"Demokrat",1,2004,"Golkar",1,2004,"PAN",1,2004,"PPP",1,2004,"PKS",1,2004,"PKB",1,
  2009,"Demokrat",1,2009,"Golkar",1,2009,"PAN",1,2009,"PPP",1,2009,"PKS",1,2009,"PKB",1,
  2014,"PDI-P",1,2014,"NasDem",1,2014,"PKB",1,2014,"Hanura",1,2014,"PPP",1,
  2019,"PDI-P",1,2019,"Golkar",1,2019,"PKB",1,2019,"NasDem",1,2019,"PPP",1,2019,"Gerindra",1,
  2024,"Gerindra",1,2024,"Golkar",1,2024,"PAN",1,2024,"Demokrat",1,2024,"NasDem",1
)

# Gov vs Opp percentages (from base tables)
gov_stats <- dpr_hist %>%
  left_join(gov_map, by = c("year","party")) %>%
  mutate(government = replace_na(government, 0L)) %>%
  group_by(year) %>%
  summarise(
    total_seats = sum(seats),
    gov_pct = sum(seats[government == 1]) / sum(seats),
    opp_pct = 1 - gov_pct,
    .groups = "drop"
  )

# -------------------------
# 3) Parliament layout design
# -------------------------
layout_df <- dpr_hist %>%
  group_by(year) %>%
  group_modify(~ ggparliament::parliament_data(
    election_data = .x,
    type = "semicircle",
    parl_rows = 10,
    party_seats = .x$seats
  )) %>%
  ungroup() %>%
  left_join(dpr_hist, by = c("year","party")) %>%
  left_join(gov_map,  by = c("year","party")) %>%
  mutate(government = replace_na(government, 0L))%>%
  mutate(bar_col = party_cols[as.character(party)])

layout_df

dpr_hist |> count(year, party, wt = seats)

# -------------------------
# 4) Plot
# -------------------------
ggplot(layout_df, aes(x, y)) +
  # Facet background (soft neutral); include 'year' to avoid warnings
  geom_rect(
    data = gov_stats,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, group = year),
    inherit.aes = FALSE, fill = "grey98"
  ) +
  # Parliament seats (pastel colors)
  geom_parliament_seats(
    aes(colour = party),
    size = 1.5, show.legend = TRUE
  ) +
  # Top "bar chart" filled by party colors (per your wireframe)
  # Centered pill label: Gov vs Opp %
  geom_label(
    data = gov_stats,
    aes(x = 0, y = 0.3,
        label = paste0(total_seats, " Seats (",
                       percent(gov_pct, accuracy = 1),
                       " Gov", ")")),
    inherit.aes = FALSE,
    size = 3, fontface = "bold",
    label.size = 0, fill = "white", color = "#333333"
  ) +
  facet_wrap(~ year, ncol = 3) +
  scale_colour_manual(values = party_cols, name = "Party") +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    fill   = "none",
    shape  = "none",
    linetype = "none",
    alpha  = "none",
    size   = "none"
  ) +
  coord_equal() +
  theme_void(base_size = 12) +
  labs(
    title = "Indonesia DPR Seat Composition & Coalitions (1999–2024)",
    subtitle = paste(
      " Government coalition reflects the presidential election outcome,",
      "which can shift once the new administration begins.\n",
      "PDI-P led as the largest party in 1999, and again in 2014–2024,",
      "especially during the Megawati (2001–2004) and Jokowi (2014–2024) periods.\n",
      "Golkar has remained consistently strong, even without a presidential win.",
      "Demokrat surged in 2004–2009 under SBY, \n",
      "while Gerindra gained after 2014 as Prabowo's base.",
      "PKB and PAN have maintained steady mid-tier relevance since 1999.",
      sep = " "),
    caption  = "Sources: KPU, Wikipedia"
  ) +
  theme(
    strip.text    = element_text(face = "bold", size = 10),
    plot.title    = element_text(face = "bold", size = 15, margin = margin(t = 8)),
    plot.subtitle = element_text(face = "italic", size = 12, margin = margin(t = 8)),
    plot.caption  = element_text(hjust = 0, size = 9, margin = margin(t = 8)),
    legend.position = "bottom",
    legend.key.size = unit(0.38, "cm"),
    legend.text  = element_text(size = 9),
    panel.spacing = unit(1.2, "lines")
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

# Optional: save
ggsave("/Users/azisjamil/Documents/Master/DABN 19 - Data Viz/Assignments/dpr_parliament_1999_2024_wireframe_version.png", width = 13, height = 8, dpi = 320)




years <- sort(unique(dpr_hist$year))
parties <- sort(unique(dpr_hist$party))

dpr_full <- tidyr::complete(
  dpr_hist, year = years, party = parties, fill = list(seats = 0)
) |>
  arrange(party, year)



# ---- Accumulated (running total) seats since 1999 ----
accum_df <- dpr_full |>
  group_by(party) |>
  arrange(year, .by_group = TRUE) |>
  mutate(seats_cum = cumsum(seats)) |>
  ungroup()

# (Optional) order legend by total seats across all elections
party_order <- accum_df |>
  group_by(party) |>
  summarise(total_all = max(seats_cum), .groups = "drop") |>
  arrange(desc(total_all)) |>
  pull(party)

# End points (latest year for label placement)
label_df <- accum_df |>
  group_by(party) |>
  filter(year == max(year)) |>
  ungroup()

# Sort labels by total size for nicer stacking
label_df <- label_df |>
  arrange(desc(seats_cum))

# -------------------------
# 1) Accumulated line chart
# -------------------------
# ---- Accumulated line chart with inline labels ----
ggplot(accum_df, aes(year, seats_cum, colour = party, group = party)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  # End labels on right
  ggrepel::geom_text_repel(
    data = label_df,
    aes(label = party),
    size = 3.4,
    fontface = "bold",
    hjust = 0,
    direction = "y",
    nudge_x = 0.4,
    segment.color = "grey70",
    segment.size = 0.3,
    box.padding = 0.3,
    min.segment.length = 0,
    force = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = party_cols) +
  labs(
    title = "Accumulated DPR Seats by Party (1999–2024)",
    subtitle = "PDI-P has consistently remained the leading party, maintaining the highest cumulative seat count over time, 
particularly during the Megawati (2002–2004) and Jokowi (2014–2024) eras.
Golkar continues to be one of the dominant forces in parliament, despite never fielding a successful presidential candidate.
Among the newer parties that emerged after 1999, Demokrat and Gerindra stand out for their significant gains, 
while PKB and PAN have remained relevant with stable representation across elections.
    ",,
    x = "Year", y = "Seats",
    caption = "Sources: KPU; Wikipedia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",      # hide legend
    plot.title    = element_text(face = "bold", size = 15, margin = margin(t = 8)),
    plot.subtitle = element_text(face = "italic", size = 12, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

print(p_accum)

ggsave("/Users/azisjamil/Documents/Master/DABN 19 - Data Viz/Assignments/dpr_parliament_1999_2024_cummulative.png", width = 13, height = 8, dpi = 320)