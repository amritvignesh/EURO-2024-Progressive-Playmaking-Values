library(rvest)
library(dplyr)
library(stringr)
library(fitdistrplus)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggtext)
library(ggpath)

scrape_category <- function(category) {
  page <- read_html(paste0("https://fbref.com/en/comps/676/", category, "/European-Championship-Stats"))

  comments <- page %>% 
    html_nodes(xpath = "//comment()") %>% 
    html_text()

  comm_html <- read_html(comments[str_detect(comments, paste0("div_stats_", category))]) 
  
  table <- comm_html %>% 
    html_node(paste0("#div_stats_", category)) %>%
    html_nodes("table") %>%
    html_table()
  
  urls <- comm_html %>%
    html_nodes("td[data-stat='player'] a") %>%
    html_attr("href")
  
  df <- as.data.frame(table[[1]])
  
  colnames(df) <- df[1,]
  df <- df[-1, ] 

  output <- list(df = df, urls = urls)
  return(output)
}

categories <- c("passing", "possession", "gca")

for (category in categories) {
  output <- scrape_category(category)
  assign(category, output$df)
  assign(paste0(category, "_urls"), output$urls)
}

passing <- data.frame(passing)

possession <- data.frame(possession)

passes <- passing %>%
  mutate(Cmp = as.numeric(Cmp), PrgP = as.numeric(PrgP), prog_pct = PrgP/Cmp) %>%
  dplyr::select(player = Player, team = Squad, passes = Cmp, prog_passes = PrgP, prog_pct) %>%
  filter(player != "Player") %>%
  cbind(passing_urls) %>%
  filter(passes != 0)

carries <- possession %>%
  mutate(Carries = as.numeric(Carries), PrgC = as.numeric(PrgC), prog_pct = PrgC/Carries) %>%
  dplyr::select(player = Player, team = Squad, carries = Carries, prog_carries = PrgC, prog_pct) %>%
  filter(player != "Player") %>%
  cbind(possession_urls) %>%
  filter(carries != 0)
  
passes_filt <- passes %>% filter(passes >= 50, prog_pct > 0, prog_pct < 1)

pass_dist <- fitdistrplus::fitdist(passes_filt$prog_pct, "beta")

pass_dist_alpha <- pass_dist$estimate['shape1']
pass_dist_beta <- pass_dist$estimate['shape2']

passes <- passes %>%
  mutate(adj_prog_pct = (prog_passes + pass_dist_alpha)/(passes + pass_dist_alpha + pass_dist_beta))

carries_filt <- carries %>% filter(carries >= 50, prog_pct > 0, prog_pct < 1)

carry_dist <- fitdistrplus::fitdist(carries_filt$prog_pct, "beta")

carry_dist_alpha <- carry_dist$estimate['shape1']
carry_dist_beta <- carry_dist$estimate['shape2']

carries <- carries %>%
  mutate(adj_prog_pct = (prog_carries + carry_dist_alpha)/(carries + carry_dist_alpha + carry_dist_beta))

passes$adj_prog_pct <- scale(passes$adj_prog_pct)
carries$adj_prog_pct <- scale(carries$adj_prog_pct)

passes_final <- passes %>% dplyr::select(player, team, url = passing_urls, pass_metric = adj_prog_pct) %>% arrange(-pass_metric)
carries_final <- carries %>% dplyr::select(player, team, url = possession_urls, carry_metric = adj_prog_pct) %>% arrange(-carry_metric)
passes_final$pass_metric <- scale(passes_final$pass_metric) 
carries_final$carry_metric <- scale(carries_final$carry_metric) 

passes_final$id <- str_extract(passes_final$url, "(?<=players/)\\w+")
passes_final$headshot_url <- paste0("https://fbref.com/req/202302030/images/headshots/", passes_final$id, "_2022.jpg")

carries_final$id <- str_extract(carries_final$url, "(?<=players/)\\w+")
carries_final$headshot_url <- paste0("https://fbref.com/req/202302030/images/headshots/", carries_final$id, "_2022.jpg")

flags <- read_csv("flags_iso.csv") %>% dplyr::select(team = `Alpha-2 code`, flag = URL)

passes_final <- passes_final %>%
  mutate(team = toupper(substr(team, 1, 2))) %>%
  left_join(flags, by = "team") %>%
  mutate(flag = ifelse(team == "EN", "https://cdn.britannica.com/44/344-004-494CC2E8/Flag-England.jpg", flag), pass_metric = round(pass_metric, 2)) %>%
  dplyr::select(headshot_url, player, flag, pass_metric)

carries_final <- carries_final %>%
  mutate(team = toupper(substr(team, 1, 2))) %>%
  left_join(flags, by = "team") %>%
  mutate(flag = ifelse(team == "EN", "https://cdn.britannica.com/44/344-004-494CC2E8/Flag-England.jpg", flag), carry_metric = round(carry_metric, 2)) %>%
  dplyr::select(headshot_url, player, flag, carry_metric)

t10_pass <- passes_final %>% head(10)
t10_carry <- carries_final %>% head(10)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>FBRef</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

pass_table <- t10_pass %>% gt() %>%
  gt_img_rows(columns = flag, height = 40) %>%
  gt_img_rows(columns = headshot_url, height = 40) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_url, player, flag, pass_metric)
  ) %>%
  gt_hulk_col_numeric(pass_metric) %>%
  cols_label(
    headshot_url = md(""),
    player = md("**Player**"),
    flag = md("**Team**"),
    pass_metric = md("**PPVP**")
  ) %>%
  tab_header(
    title = "EURO 2024 Progressive Playmaking Value (Passing)",
    subtitle = md("*Updated After **Two** Games For Each Team In The Group Stage | PPVP Is **Scaled***")
  ) %>% 
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(player, pass_metric)
    )
  ) %>%
  cols_width(player ~ px(175), headshot_url ~ px(75), flag ~ px(125), pass_metric ~ px(150))

carry_table <- t10_carry %>% gt() %>%
  gt_img_rows(columns = flag, height = 40) %>%
  gt_img_rows(columns = headshot_url, height = 40) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_url, player, flag, carry_metric)
  ) %>%
  gt_hulk_col_numeric(carry_metric) %>%
  cols_label(
    headshot_url = md(""),
    player = md("**Player**"),
    flag = md("**Team**"),
    carry_metric = md("**PPVC**")
  ) %>%
  tab_header(
    title = "EURO 2024 Progressive Playmaking Value (Carrying)",
    subtitle = md("*Updated After **Two** Games For Each Team In The Group Stage | PPVC Is **Scaled***")
  ) %>% 
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(player, carry_metric)
    )
  ) %>%
  cols_width(player ~ px(175), headshot_url ~ px(75), flag ~ px(125), carry_metric ~ px(150))

gt_two_column_layout(tables = list(pass_table, carry_table), "viewer")

final_stats <- inner_join(passes_final, carries_final, by = c("headshot_url", "player", "flag"))

passes_100 <- passes %>% filter(passes >= 100)
carries_100 <- carries %>% filter(carries >= 100)

final_stats <- final_stats %>% filter(player %in% passes_100$player, player %in% carries_100$player)

comp_plot <- final_stats %>%
  ggplot(aes(x = pass_metric, y = carry_metric)) +
  geom_hline(yintercept = mean(final_stats$carry_metric), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(final_stats$pass_metric), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_from_path(aes(path = headshot_url), height = 0.05) +
  labs(x = "Progressive Playmaking Value (Passing)",
       y = "Progressive Playmaking Value (Carrying)",
       title = "Comparing Progressive Playmaking Values: Passing vs Carrying",
       subtitle = "Players With 100+ Passes and Carries After 2 Games In The Group Stage",
       caption = "Data from **FBRef** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"), plot.caption = element_markdown(hjust = 0.5))

ggsave("comp_plot.png", comp_plot)