get_mahalanobis_distance <- function(df, auto_drop, re_center) {
  df$mahal <- stats::mahalanobis(df, colMeans(df), cov(df))
  df$p <- pchisq(df$mahal, df = ncol(df) - 1, lower.tail = FALSE)
  print(nrow(df %>% filter(p < .001)))
  # if (auto_drop) {
  #   df <- df %>%
  #     filter(p >=.001)
  #   if (re_center) {
  #     #df <- df %>%
  #     #  mutate(across(!mahal & !p, ~ as.numeric(base::scale(.,center = TRUE, scale = TRUE))))
  #   }
  # }
  #df <- df %>% select(!mahal & !p)
  #df <- subset(df, select = -c(mahal, p))
  return(df)
}