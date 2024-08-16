# Carregar pacotes necessários
library(tidyverse)
library(censobr)
library(geobr)
library(scales)

# Ler os dados do município do Rio de Janeiro
muni_rj <- geobr::read_municipality(code_muni = 'RJ', 
                                    year = 2010, 
                                    showProgress = FALSE) %>%
  filter(code_muni == "3304557")

# Ler os dados dos setores censitários do Rio de Janeiro
tracts_sf <- geobr::read_census_tract(code_tract = "RJ",
                                      simplified = FALSE,
                                      year = 2010,
                                      showProgress = FALSE) %>%
  filter(name_muni == "Rio De Janeiro")

# Plotar o mapa base dos setores censitários
ggplot() + 
  geom_sf(data = tracts_sf, fill = 'gray90', color = 'gray60') + 
  theme_void()

# Ler os dados do censo
tract_basico <- read_tracts(year = 2010, dataset = "Basico", showProgress = FALSE)
tract_income <- read_tracts(year = 2010, dataset = "DomicilioRenda", showProgress = FALSE)

# Selecionar e renomear variáveis de interesse
tract_basico <- tract_basico %>% select(code_tract, population = V002)  # V002: População do setor
tract_income <- tract_income %>% select(code_tract, total_income = V003)  # V003: Renda total do setor

# Combinar os dados de população e renda
tracts_df <- left_join(tract_basico, tract_income, by = "code_tract") %>%
  mutate(renda_pc = total_income / population)

# Combinar com os dados espaciais
rj_tracts <- left_join(tracts_sf, tracts_df, by = "code_tract")

# Plotar o mapa de renda per capita
p <- ggplot() +
  geom_sf(data = rj_tracts, aes(fill = renda_pc), color = NA) +
  geom_sf(data = muni_rj, color = 'black', size = 0.3, fill = NA) +
  labs(
    title = 'Distribuição Espacial da Renda Per Capita',
    subtitle = 'Município do Rio de Janeiro, 2010',
    caption = 'Fonte: IBGE, Censo Demográfico 2010'
  ) +
  scale_fill_viridis_c(
    name = "Renda per\ncapita (R$)",
    option = 'plasma',
    breaks = c(0, 200, 400, 600, 800, 1000, 1500, 2000, 3000, 5000, 10000),
    trans = "pseudo_log",
    na.value = "gray90",
    guide = guide_colorbar(
      barwidth = 15, barheight = 0.5,
      title.position = 'top', title.hjust = 0.5
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

p

# Salvar o gráfico como JPEG
ggsave("renda_per_capita_rio.jpg", plot = p, width = 10, height = 8, dpi = 300)

