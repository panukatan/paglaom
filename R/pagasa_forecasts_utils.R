#
# PAGASA forecast regions
#

pagasa_forecast_regions <- list(
  Mindanao = data.frame(
    geographic_unit = c(
      "Laguindingan", "Cagayan de Oro City", "Malaybalay City",
      "Metro Davao", "Zamboanga City", "Surigao City", "Butuan City",
      "Cotabato City", "General Santos City", "Dipolog City", "Tagum City",
      "Marawi City", "Iligan City", "Pagadian City"
    )
  ),
  `National Capital Region` = data.frame(
    geographic_unit = c(
      "Metro Manila", "Tarlac", "Nueva Ecija", "Zambales", "Bataan", "Pampanga",
      "Bulacan", "Cavite", "Laguna", "Batangas", "Rizal", "Quezon"
    )
  ),
  `Northern Luzon` = data.frame(
    geographic_unit = c(
      "Ilocos Norte", "Ilocos Sur", "La Union", "Pangasinan", "Batanes", 
      "Cagayan", "Isabela", "Quirino", "Nueva Vizcaya", "Abra", "Benguet",
      "Mountain Province", "Ifugao", "Kalinga", "Apayao", "Aurora"
    )
  ),
  `Southern Luzon` = data.frame(
    geographic_unit = c(
      "Albay", "Camarines Norte", "Camarines Sur", "Catanduanes", "Sorsogon", 
      "Masbate", "Northern Samar", "Oriental Mindoro", "Marinduque", "Romblon"
    )
  ),
  Visayas = data.frame(
    geographic_unit = c(
      "Cebu", "Bohol", "Negros Occidental", "Negros Oriental", "Siquijor",
      "Leyte", "Southern Leyte", "Biliran", "Samar", "Eastern Samar", "Iloilo",
      "Guimaras", "Capiz", "Aklan", "Antique", "Palawan", "Occidental Mindoro"
    )
  )
) |>
  dplyr::bind_rows(.id = "regional_grouping")