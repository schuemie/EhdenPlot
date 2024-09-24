# showtext and ggtext interact. To get good results, set Graphics Device to AGG (in global settings)
library(dplyr)
library(ggplot2)
library(ggflags) # https://github.com/jimjam-slam/ggflags
library(showtext)
library(ggtext)
showtext_auto()
font_add('fa-solid', 'Font Awesome 6 Free-Solid-900.otf')

subplotFolder <- "subplots"

table <- readr::read_csv("EhdenData.csv")
table |>
  group_by(Country) |>
  count() |>
  arrange(desc(n)) |>
  print(n = 100)

abbr <- readr::read_csv("CountryAbbreviations.csv")
table <- table |> 
  inner_join(abbr, by = join_by(Country == abbr))

generateIcons <- function(countryTable) {
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
  icons <- mapply(fun, -seq_along(countryTable$Why), countryTable$Why, countryTable$`Care Level`, countryTable$EHR, countryTable$Claim, countryTable$Lab, countryTable$`Case Report Form`, countryTable$Survey, countryTable$NLP,  countryTable$`Death Certificate`, SIMPLIFY = FALSE)
  icons <- do.call(c, icons)
  return(bind_rows(icons))
}

countries <- table |>
  distinct(Country) |>
  pull()

# country = countries[1]
# countries = c("UA", "BA", "GB", "DE")
country = "FR"
for (country in countries) {
  countryTable <- table |>
    filter(Country == country) |> 
    arrange(`Data Source Acronym`) |>
    mutate(x = 0,
           y = -row_number(),
           label = trimws(`Data Source Acronym`),
           countLabel = format(`Person Count`, big.mark = ",", trim = TRUE, scientific = FALSE),
           countFraction = `Person Count`/max(`Person Count`)) 
  
  icons <- generateIcons(countryTable)
  
  countWidth <- 0.375
  plot <- ggplot(countryTable, aes(x = x, y = y)) +
    geom_flag(x = 0, y = 0, country = tolower(country), size = 1.5) +
    geom_text(x = 0.07, y = 0, label = countryTable$name[1], hjust = 0) +
    geom_text(aes(x = x -0.01, label = label), hjust = 1) +
    geom_rect(aes(xmin = countWidth - (countWidth*countFraction), xmax = countWidth, ymin = y - 0.4, ymax = y + 0.5), color = NA, fill = "#69AED5", alpha = 0.5) +
    geom_text(aes(x = countWidth + x, label = countLabel), hjust = 1) +
    geom_richtext(aes(x = countWidth + 0.1 + x*0.08, label = label), hjust = 0.5, fill = NA, label.colour = NA, data = icons) +
    coord_cartesian(xlim=c(-0.85, 1.1), ylim=c(min(countryTable$y)-0.2, 0.2)) +
    theme_void()
  plot
  ggsave(filename = file.path(subplotFolder, sprintf("Country_%s.png", tolower(countryTable$Country[1]))), 
         plot = plot, 
         width = 1.85, 
         height = 0.07 + nrow(countryTable)*0.07,
         dpi = 300)
}


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


plot <- ggplot(legend, aes(x = x, y = y)) +
  geom_richtext(aes(label = icon), hjust = 0.5, fill = NA, label.colour = NA) +
  geom_text(aes(x = x+0.1, label = label), hjust = 0) +
  coord_cartesian(xlim=c(0, 1.5)) +
  theme_void()

ggsave(filename = file.path(subplotFolder, "legend.png"), 
       plot = plot, 
       width = 0.75, 
       height = 0.7,
       dpi = 300)


