st_dissolve <- function(df, group_var,viv_ae) {
  group_var <- enquo(group_var)
  df |>
    group_by(!! group_var,viv_ae) |>
    summarise(STATUS="Dissolved")
}