# Orderly set-up ---------------------------------------------------------------
orderly::orderly_resource("data/")

orderly::orderly_artefact(
  description = "WMR cases, deaths and PAR",
  files = "wmr_cases_deaths.csv"
)
orderly::orderly_artefact(
  description = "WMR ITNs distributed",
  files = "wmr_itns_distributed.csv"
)
orderly::orderly_artefact(
  description = "WMR ITNs distributed interpolation plot",
  files = "wmr_itns_distributed.png"
)
orderly::orderly_artefact(
  description = "WMR IRS people protected",
  files = "wmr_irs_people_protected.csv"
)
orderly::orderly_artefact(
  description = "WMR IRS people protected interpolation plot",
  files = "wmr_irs_people_protected.png"
)
# ------------------------------------------------------------------------------

# Cases and deaths -------------------------------------------------------------
wmr_cases_deaths <- read.csv("data/wmr_annex_4h.csv") |>
  tidyr::pivot_wider(
    id_cols = c("region", "iso", "country", "year"),
    names_from = "indicator",
    values_from = "value"
  ) |>
  dplyr::rename(
    iso3c = "iso",
    wmr_par = "Population at risk",
    wmr_cases_l = "Cases Lower",
    wmr_cases = "Cases Point",
    wmr_cases_u = "Cases Upper",
    wmr_deaths_l = "Deaths Lower",
    wmr_deaths = "Deaths Point",
    wmr_deaths_u = "Deaths Upper"
  )
write.csv(wmr_cases_deaths, "wmr_cases_deaths.csv", row.names = FALSE)
# ------------------------------------------------------------------------------

# Shared intervention table (WMR annex 4g), reshaped below for IRS and ITNs
wmr_int <- read.csv("data/wmr_annex_4g.csv") |>
  dplyr::rename("iso3c" = "iso") |>
  dplyr::mutate(value = as.numeric(value))

# IRS people protected ---------------------------------------------------------
irs_people_protected_wmr <- wmr_int |>
  dplyr::filter(indicator == "Number of people protected by IRS") |>
  dplyr::select("iso3c", "year", "value")

# Prefer WMR values where present, fall back to the archival series, then carry
# the last known value forward to fill gaps
irs_people_protected <- read.csv("data/irs_people_protected_archival.csv") |>
  dplyr::full_join(irs_people_protected_wmr, by = c("iso3c", "year")) |>
  dplyr::mutate(
    irs_people_protected = ifelse(!is.na(value), value, irs_people_protected),
    irs_interpolated = irs_people_protected
  ) |>
  dplyr::select(-value) |>
  dplyr::group_by(iso3c) |>
  tidyr::complete(year) |>
  dplyr::arrange(year) |>
  tidyr::fill(irs_interpolated, .direction = "down") |>
  dplyr::ungroup() |>
  dplyr::arrange(iso3c, year)

irs_people_protected_plot <- ggplot2::ggplot(irs_people_protected) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = irs_interpolated / 1e6)) +
  ggplot2::geom_point(ggplot2::aes(x = year, y = irs_people_protected / 1e6)) +
  ggplot2::facet_wrap(~iso3c, scale = "free_y") +
  ggplot2::ylab("IRS people protected (M)") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw()

write.csv(irs_people_protected, "wmr_irs_people_protected.csv", row.names = FALSE)
ggplot2::ggsave("wmr_irs_people_protected.png", irs_people_protected_plot, height = 15, width = 20)
# ------------------------------------------------------------------------------

# ITNs distributed -------------------------------------------------------------
itns_distributed_wmr <- wmr_int |>
  dplyr::filter(indicator == "Number of ITNs distributed") |>
  dplyr::select("iso3c", "year", "value")

# Same merge-and-fill logic as IRS above, applied to ITNs distributed
itns_distributed <- read.csv("data/itns_delivered_archival.csv") |>
  dplyr::full_join(itns_distributed_wmr, by = c("iso3c", "year")) |>
  dplyr::mutate(
    itns_distributed = ifelse(!is.na(value), value, itns_distributed),
    itns_interpolated = itns_distributed
  ) |>
  dplyr::select(-value) |>
  dplyr::group_by(iso3c) |>
  tidyr::complete(year) |>
  dplyr::arrange(year) |>
  tidyr::fill(itns_interpolated, .direction = "down") |>
  dplyr::ungroup() |>
  dplyr::arrange(iso3c, year)

itns_distributed_plot <- ggplot2::ggplot(itns_distributed) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = itns_interpolated / 1e6)) +
  ggplot2::geom_point(ggplot2::aes(x = year, y = itns_distributed / 1e6)) +
  ggplot2::facet_wrap(~iso3c, scale = "free_y") +
  ggplot2::ylab("ITNs delivered (M)") +
  ggplot2::xlab("Year") +
  ggplot2::theme_bw()

write.csv(itns_distributed, "wmr_itns_distributed.csv", row.names = FALSE)
ggplot2::ggsave("wmr_itns_distributed.png", itns_distributed_plot, height = 15, width = 20)
# ------------------------------------------------------------------------------
