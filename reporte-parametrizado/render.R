library(tidyverse)
library(readxl)
library(janitor)
library(glue) #install.packages("glue")
library(quarto) #install.packages("quarto")

rem20 <- read_csv2("data/indicadores_rem20.csv") |> 
    janitor::clean_names()

establecimientos <- readxl::read_excel("data/Establecimientos DEIS.xlsx") |> 
    janitor::clean_names() |> 
    select(codigo_vigente, nombre_oficial)

rem20 <- rem20 |> 
  left_join(establecimientos, by = c("codigo_establecimiento" = "codigo_vigente")) |> 
  mutate(glosa_sss = case_when(
    glosa_sss == "Arica" ~ "Arica y Parinacota",
    glosa_sss == "Iquique" ~ "Tarapacá",
    glosa_sss == "Valdivia" ~ "Los Ríos",
    .default = glosa_sss
  ))


# Renderizar los reportes ----

# Solo 1 reporte
quarto_render(
  input = "reporte-parametrizado-4.qmd",
  output_file = "Magallanes.html",
  execute_params = list(glosa_sss = "Magallanes")
)

# Varios reportes con un parámetro
servicios <- rem20 |>
  distinct(glosa_sss) |>
  pull(glosa_sss) |> 
  as.character()


reportes <- 
  tibble(
    input = "reporte-parametrizado-4.qmd",
    output_file = str_glue("{servicios}.html"),
    execute_params = map(servicios, ~list(glosa_sss = .)) # Archivo de entrada de Quarto
  )


pwalk(reportes, quarto_render, .progress = TRUE)


# Renderear reportes con dos parámetros
reportes2 <- rem20 |>
  distinct(glosa_sss, area_funcional) |>
  mutate(across(everything(), as.character))


reportes_sample <- reportes2 |> head(10)

# Renderizar reportes
purrr::walk2(
  reportes_sample$glosa_sss,
  reportes_sample$area_funcional,
  ~ quarto::quarto_render(
    input = "reporte-parametrizado-5.qmd",
    execute_params = list(glosa_sss = .x, area_funcional = .y),
    output_file = glue::glue("{.x}-{.y}.html")
  ), 
  .progress = TRUE
)
