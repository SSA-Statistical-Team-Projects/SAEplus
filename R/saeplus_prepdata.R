

# names_to_chg <- colnames(hhsurvey_dt)[grepl("GIN_", colnames(hhsurvey_dt))]
# changed_names <- c("impervious_surface", "no2_julsep18", "no2_aprjun19",
#                    "ntl", "precip_julsep18", "precip_aprjun19")
# setnames(hhsurvey_dt, names_to_chg, changed_names)

# hhsurvey_dt[ADM1_NAME == "Labe\r\n", ADM1_NAME := "Labe"]
#
# ### we also have several missing observations (lets view this!)
# tmap_mode("view") +
#   tm_basemap() +
#   tm_shape(sf::st_as_sf(hhsurvey_dt[is.na(ADM3_NAME) == TRUE,])) + tm_bubbles(col = "red", size = 0.0001)
#
# hhsurvey_dt[is.na(ADM1_NAME) == TRUE & prefecture == "CONAKRY",
#             c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
#               list("Conakry", "GIN002", "Conakry", "GIN002001", "Ratoma", "GIN00200105")]
#
# hhsurvey_dt[is.na(ADM1_NAME) == TRUE & prefecture == "MANDIANA",
#             c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
#               list("Kankan", "GIN004", "Mandiana", "GIN004004", "Koundianakoro", "GIN00400407")]

# ### convert the admin code variables to integer values (we cannot run emdi::ebp with character variable admin areas)
# hhsurvey_dt <- saeplus_dropchars(dt = hhsurvey_dt,
#                                  vars = c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE"))
# shapefile_dt <- saeplus_dropchars(dt = shapefile_dt,
#                                   vars = c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE"))
#
#
# NOTE : Make the sure the variable being dummified is a character string
