library(eurostat)
library(sf)
library(giscoR)
library(ggplot2)
library(dplyr)
library(tmap)

europeShapes <- get_eurostat_geospatial(resolution = 10,
                                        nuts_level = 0)

euCountries <- eu_countries |> 
  select(geo = code, name) |> 
  bind_rows(tibble(geo = "UK", name = "United Kingdom"))


euShapes <- europeShapes |>
  select(geo = NUTS_ID, geometry) |>
  inner_join(euCountries, by = join_by(geo)) |>
  arrange(geo) |>
  st_as_sf()

euShapes |>
  ggplot() +
  geom_sf(fill = alpha("#336B91", 0.7), color = "black") +
  tm_text("NUTS_NAME") +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()



library(rnaturalearth)
library(sf)
library(ggplot2)

world <- ne_countries(scale = 50, returnclass = 'sf')

world_centroids <- st_centroid(world)
world_centroids <- st_transform(world_centroids, crs = st_crs(3035))

# Create a data frame with the icon paths/URLs (this is just an example)
# Replace 'path/to/icon.png' with actual paths or URLs to your icons
icon_data <- data.frame(
  name = world_centroids$name,
  lon = st_coordinates(world_centroids)[, 1],
  lat = st_coordinates(world_centroids)[, 2]
)

ggplot(world) + 
  geom_sf(aes(fill = continent), color = 'black') +
  geom_sf_text(aes(label = name), size = 5) +
  geom_point(data = icon_data, aes(x = lon, y = lat), size = 3) +
  coord_sf(crs = st_crs(3035),
           xlim = c(2000000, 6200000), 
           ylim = c(1500000, 5500000)) +
  scale_fill_manual(values = c('#c4c0a0', NA, '#c9cc3d', '#fcd752', 
                               NA, NA, NA, NA), guide = 'none', 
                    na.value = 'white') +
  theme(panel.background = element_rect(fill = '#92c5f0'),
        panel.grid.major = element_line(linewidth = 0.1, 
                                        color = '#80808080'))

# showtext and ggtext interact. To get good results, set Graphics Device to AGG (in global settings)
library(dplyr)
library(ggplot2)
library(ggflags) # https://github.com/jimjam-slam/ggflags
library(showtext)
library(ggtext)
showtext_auto()
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

table <- readr::read_csv("EHDEN_Table_csv")
countryCounts <- table |>
  group_by(Country) |>
  count()

addIcons <- function(why, careLevel, ehr, claim, lab, caseReport, survey, nlp, death) {
  fun <- function(why, careLevel, ehr, claim, lab, caseReport, survey, nlp, death) {
    icons <- c()
    if (why == "Encounter") {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf0f0;</span>")
    } else if (why == "Population") {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf0ac;</span>")
    } else if (why == "Disease") {
      icons <- c(icons, "D<sub>x</sub>&nbsp;")
    } 
    if (careLevel == "Primary") {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#x31;</span>")
    } else if (careLevel == "Secondary") {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#x32;</span>")
    } else if (careLevel == "Mixed") {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#x31; &#x32;</span>")
    } 
    if (ehr == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf812;</span>")
    }
    if (claim == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf53d;</span>")
    }
    if (lab == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf0c3;</span>")
    }
    if (caseReport == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf15c;</span>")
    }
    if (survey == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf1e5;</span>")
    }
    if (nlp == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf031;</span>")
    }
    if (death == 1) {
      icons <- c(icons, "<span style='font-family:fa-solid'>&#xf54c;</span>")
    }
    return(paste(icons, collapse = "\t"))
  }
  return(mapply(fun, why, careLevel, ehr, claim, lab, caseReport, survey, nlp, death))
}

generateIcons <- function(why, careLevel, ehr, claim, lab, caseReport, survey, nlp, death) {
  fun <- function(y, why, careLevel, ehr, claim, lab, caseReport, survey, nlp, death) {
    icons <<- list()
    xDelta <- 1
    x <<- 0
    addIcon <- function(icon) {
      icons[[length(icons) + 1]] <<- tibble(
        x = x,
        y = y,
        label = icon
      )
      x <<- x + xDelta
    }
    if (why == "Encounter") {
      addIcon("<span style='font-family:fa-solid'>&#xf0f0;</span>")
    } else if (why == "Population") {
      addIcon("<span style='font-family:fa-solid'>&#xf0ac;</span>")
    } else if (why == "Disease") {
      addIcon("D<sub>x</sub>&nbsp;")
    } 
    if (careLevel == "Primary") {
      addIcon("<span style='font-family:fa-solid'>&#x31;</span>")
    } else if (careLevel == "Secondary") {
      addIcon("<span style='font-family:fa-solid'>&#x32;</span>")
    } else if (careLevel == "Mixed") {
      addIcon("<span style='font-family:fa-solid'>&#x31;</span><span style='font-family:fa-solid'>&#x32;</span>")
    } 
    if (ehr == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf812;</span>")
    }
    if (claim == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf53d;</span>")
    }
    if (lab == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf0c3;</span>")
    }
    if (caseReport == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf15c;</span>")
    }
    if (survey == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf1e5;</span>")
    }
    if (nlp == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf031;</span>")
    }
    if (death == 1) {
      addIcon("<span style='font-family:fa-solid'>&#xf54c;</span>")
    }
    return(icons)
  }
  icons <- mapply(fun, -seq_along(countryTable$Why), countryTable$Why, countryTable$`Care Level`, countryTable$EHR, countryTable$Claim, countryTable$Lab, countryTable$`Case Report Form`, countryTable$Survey, countryTable$NLP,  countryTable$`Death Certificate`)
  icons <- do.call(c, icons)
  return(bind_rows(icons))
}

countryTable <- table |>
  filter(Country == "GB") |> 
  arrange(`Data Source Acronym`) |>
  mutate(x = 0,
         y = -row_number(),
         label = trimws(`Data Source Acronym`),
         countLabel = format(`Person Count`, big.mark = ",", trim = TRUE, scientific = FALSE),
         countFraction = `Person Count`/max(table$`Person Count`)) 

icons <- generateIcons(countryTable$Why, countryTable$`Care Level`, countryTable$EHR, countryTable$Claim, countryTable$Lab, countryTable$`Case Report Form`, countryTable$Survey, countryTable$NLP,  countryTable$`Death Certificate`)

countWidth <- 0.2
ggplot(countryTable, aes(x = x, y = y)) +
  geom_flag(x = 0.001, y = 0, country = tolower(countryTable$Country[1])) +
  geom_text(x = 0.002, y = 0, label = "Great Britain", hjust = 0) +
  geom_text(aes(x = x -0.01, label = label), hjust = 1) +
  geom_rect(aes(xmin = countWidth - (countWidth*countFraction), xmax = countWidth, ymin = y - 0.4, ymax = y + 0.5), color = NA, fill = "#69AED5", alpha = 0.5) +
  geom_text(aes(x = countWidth + x, label = countLabel), hjust = 1) +
  geom_richtext(aes(x = countWidth + 0.05 + x*0.05, label = label), hjust = 0.5, fill = NA, label.colour = NA, data = icons) +
  coord_cartesian(xlim=c(-1, 1)) +
  theme_void()


countryTable <- table |>
  filter(Country == "GB") |> 
  arrange(`Data Source Acronym`) |>
  mutate(x = 0,
         y = -row_number(),
         label = trimws(`Data Source Acronym`)) |>
  mutate(icons = addIcons(Why, `Care Level`, EHR, Claim, Lab, `Case Report Form`, Survey, NLP,  `Death Certificate`))

ggplot(countryTable, aes(x = x, y = y)) +
  geom_flag(x = 0.001, y = 0, country = tolower(countryTable$Country[1])) +
  geom_text(x = 0.002, y = 0, label = "Great Britain", hjust = 0) +
  geom_text(aes(label = label), hjust = 1) +
  geom_richtext(aes(label = icons), hjust = 0, fill = NA, label.colour = NA) +
  theme_void()


legend <- tibble(
  icon = c("<span style='font-family:fa-solid'>&#xf0f0;</span>",
            "<span style='font-family:fa-solid'>&#xf0ac;</span>",
            "D<sub>x</sub>",
            "<span style='font-family:fa-solid'>&#x31;</span>",
            "<span style='font-family:fa-solid'>&#x32;</span>",
            "<span style='font-family:fa-solid'>&#xf812;</span>",
            "<span style='font-family:fa-solid'>&#xf53d;</span>",
            "<span style='font-family:fa-solid'>&#xf0c3;</span>",
            "<span style='font-family:fa-solid'>&#xf15c;</span>",
            "<span style='font-family:fa-solid'>&#xf1e5;</span>",
            "<span style='font-family:fa-solid'>&#xf031;</span>",
            "<span style='font-family:fa-solid'>&#xf54c;</span>"),
  label = c("Reason: Encounter",
            "Reason: Population",
            "Reason: Disease",
            "Care level: Primary",
            "Care level: Secondary",
            "Electronic Health Records",
            "Claims",
            "Lab",
            "Case report forms",
            "Survey",
            "NLP",
            "Death certificate")
  ) |>
  mutate(x = 0,
         y = -row_number())


ggplot(legend, aes(x = x, y = y)) +
  geom_richtext(aes(label = icon), hjust = 0.5, fill = NA, label.colour = NA) +
  geom_text(aes(x = x+0.02, label = label), hjust = 0) +
  coord_cartesian(xlim=c(0, 1)) +
  theme_void()



