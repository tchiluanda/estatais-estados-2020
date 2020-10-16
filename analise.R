
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(geobr)
library(extrafont)
library(gganimate)

library(colorspace)
library(RColorBrewer)
library(viridis)


# estilo dos gráficos -----------------------------------------------------


loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Lora", colour = "grey20"),
      title = element_text(size = 10, color = "dimgrey", face = "plain"), 
      plot.subtitle = element_text(color = "grey20", face = "plain", size = 10),
      axis.text = element_text(colour = "grey20", size = 8, family = "Source Sans Pro"),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(size = 0.4),
      axis.ticks.length = unit(.2, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none',
      legend.text = element_text(size = 8, family = "Source Sans Pro"),
      legend.title = element_text(size = 9, family = "Source Sans Pro")
    )
}

tema_barra <- function(){
  tema() +
    theme(
      axis.ticks.y = element_blank()
    )
}

tema_mapa <- function() {
  tema() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10),
          plot.background = element_blank(),
          panel.background = element_blank())
}

# dados iniciais ----------------------------------------------------------

tab_uf <- read_excel("./dados/dados-originais/tab_ufs.xlsx")
dados_raw <- read_excel("./dados/dados-originais/Quadro das Empresas Estatais Estaduais PAF 2020v1.xlsx", skip = 2, sheet = "Dados")

dados_selecionados_raw <- dados_raw %>%
  select(
    Estado,
    emp       = `Nome da Empresa`,
    seg       = `Ficha de Identificação da Estatal > Setor`,
    dep       = `Ficha de Identificação da Estatal > Dependência`,
    PL        = `Ficha de Informações Financeiras da Estatal > Patrimônio Líquido`,
    lucros    = `Ficha de Informações Financeiras da Estatal > Lucro / Prejuízo Líquido do Exercício`,
    gov_ca    = `Ficha de Identificação da Estatal > Governança > Conselho de Administração`,
    gov_cf    = `Ficha de Identificação da Estatal > Governança > Conselho Fiscal`,
    gov_aud   = `Ficha de Identificação da Estatal > Governança > Comitê de Auditoria`,
    maior_rem = `Ficha de Informações Financeiras da Estatal > Valor da Maior Remuneração Paga`,
    plr_rva   = `Ficha de Informações Financeiras da Estatal > Foi Distribuído o PLR ou RVA em 2019?`
    )



# limpeza -----------------------------------------------------------------

## Setor
#dput(unique(dados_selecionados_raw$seg))

limpa_setor = data.frame(
  seg = c(
    "SETOR IMOBILIÁRIO", 
    "FINANCEIRO", 
    "TRANSPORTES", 
    "DESENVOLVIMENTO", 
    "OUTRO", 
    "SERVIÇOS PÚBLICOS", 
    "DISTRIBUIÇÃO DE GÁS", 
    "SANEAMENTO", 
    NA, 
    "ABASTECIMENTO",
    "URBANIZAÇÃO", 
    "PESQUISA", 
    "GESTÃO DE ATIVOS",  
    "Financeiro", 
    "Serviços Públicos", 
    "Abastecimento", 
    "Saneamento", 
    "Informática", 
    "SAÚDE", 
    "ASSISTENCIA TÉCNICA", 
    "Outro", 
    "Desenvolvimento", 
    "INFORMÁTICA", 
    "ASSIS. TÉCNICA", 
    "COMUNICAÇÕES", 
    "ENERGIA", 
    "SEAF", 
    "INFORMATICA", 
    "GÁS NATURAL", 
    "ASSITÊNCIA TÉCNICA", 
    "Agricultura", 
    "Administração de Obras", 
    "Energia", 
    "Transporte Ferroviário", 
    "Primário", 
    "Saneamento, Serv. Água e Gás", 
    "ASSISTÊNCIA TÉCNICA", 
    "OUTROS"),
  setor = c(
    "IMOBILIÁRIO", 
    "FINANCEIRO", 
    "TRANSPORTES", 
    "DESENVOLVIMENTO", 
    "OUTRO", 
    "SERVIÇOS PÚBLICOS", 
    "DISTRIBUIÇÃO DE GÁS", 
    "SANEAMENTO", 
    "OUTRO",
    "ABASTECIMENTO",
    "URBANIZAÇÃO", 
    "PESQUISA", 
    "GESTÃO DE ATIVOS",  
    "FINANCEIRO", 
    "SERVIÇOS PÚBLICOS", 
    "ABASTECIMENTO",
    "SANEAMENTO", 
    "INFORMÁTICA", 
    "SAÚDE", 
    "ASSISTÊNCIA TÉCNICA", 
    "OUTRO",
    "DESENVOLVIMENTO", 
    "INFORMÁTICA", 
    "ASSISTÊNCIA TÉCNICA", 
    "COMUNICAÇÕES", 
    "ENERGIA", 
    "ASSISTÊNCIA TÉCNICA",
    "INFORMÁTICA", 
    "DISTRIBUIÇÃO DE GÁS",
    "ASSISTÊNCIA TÉCNICA", 
    "ABASTECIMENTO", 
    "ADMINISTRAÇÃO DE OBRAS", 
    "ENERGIA", 
    "TRANSPORTE FERROVIÁRIO", 
    "PESQUISA", 
    "SANEAMENTO", 
    "ASSISTÊNCIA TÉCNICA",
    "OUTRO")
  )

## Dependência
#dput(unique(dados_selecionados_raw$dep_raw))

# limpa_dep <- data.frame(
#   dep_raw = c(
#     "NÃO DEPENDENTE", 
#     "DEPENDENTE", 
#     "Dependente", 
#     "Não dependente", 
#     NA),
#   dep = c(
#     "Não dependente",
#     "Dependente",
#     "Dependente",
#     "Não dependente",
#     "Não informado"
#   )
# )

# valores da CMTP :/

linha_CMTP <- dados_selecionados_raw$emp == "CMTP"
dados_selecionados_raw[linha_CMTP, "PL"] <- as.character(20.2e6)
dados_selecionados_raw[linha_CMTP, "lucros"] <- as.character(236.8e3)
dados_selecionados_raw[linha_CMTP, "maior_rem"] <- as.character(9.4e3)

# junta todo mundo

dados_selecionados <- dados_selecionados_raw %>%
  left_join(limpa_setor) %>%
  #left_join(limpa_dep) %>%
  mutate(
    dep    = toupper(dep),
    dep    = ifelse(is.na(dep), "NÃO INFORMADO", dep),
    PL     = as.numeric(PL),
    lucros = as.numeric(lucros))

#verifica empresas repetidas
#rep <- dados_selecionados %>% count(emp)

# corrige na mão alguns setores

termos <- c("COMPESA", "SUAPE", "DOCAS", "PORTOS", "Portos", "CAEMA")

gera_vetor <- function(termo){
  return(str_detect(dados_selecionados$emp, termo))
}

linhas <- map(termos, gera_vetor)
names(linhas) <- termos

atribui <- function(termo, coluna, valor){
  #print(dados_selecionados[linhas[[termo]], coluna])
  # pulo do gato aqui é o <<- para fazer o assignment na variável global
  dados_selecionados[linhas[[termo]], coluna] <<- valor
  #print(dados_selecionados[linhas[[termo]], coluna])
}

atribui("COMPESA", "setor", "SANEAMENTO")
atribui("SUAPE", "setor", "PORTOS E HIDROVIAS")
atribui("DOCAS", "setor", "PORTOS E HIDROVIAS")
atribui("PORTOS", "setor", "PORTOS E HIDROVIAS")
atribui("Portos", "setor", "PORTOS E HIDROVIAS")
atribui("CAEMA", "setor", "SANEAMENTO")
atribui("COMPESA", "setor", "SANEAMENTO")

#dados_selecionados[linhas[["CAEMA"]], "setor"] <- "SANEAMENTO"
#dados_selecionados[linhas[["COMPESA"]], "setor"]



# gráficos anteriores -----------------------------------------------------



# mapa small multiples ----------------------------------------------------


dados_qde_setor_estado <- dados_selecionados %>%
  count(setor, Estado)

#mapa <- geobr::read_state()
#saveRDS(mapa, "./dados/dados-intermediarios/mapa.rds")
mapa <- readRDS("./dados/dados-intermediarios/mapa.rds")

mapa_qde <- mapa %>%
  inner_join(dados_qde_setor_estado, by = c("abbrev_state" = "Estado"))

# ggplot(mapa_qde %>% filter(setor == "SANEAMENTO")) + 
#     geom_sf(aes(fill = n > 0), color = "coral") +
#     scale_fill_manual(values = c("TRUE" = "lightcoral", "FALSE" = NA)) +
#     labs(fill = "Tem empresa de saneamento?")

setores <- data.frame(
  setor = unique(dados_selecionados$setor)
)
  
mapa_qde <- mapa %>%
  rename(Estado = "abbrev_state") %>%
  inner_join(dados_qde_setor_estado) %>%
  rename(qde = "n") %>%
  arrange(setor)

graf_mapa_comp <- ggplot(mapa_qde)+# %>% filter(seg == "OUTRO")) + 
  geom_sf(data = mapa, fill = "#EFEFEF", color = "ghostwhite") +
  geom_sf(aes(group = Estado, fill = ifelse(qde > 0, setor, NA)), color = "ghostwhite") + 
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = NA)) +
  scale_fill_viridis_d(direction = 1,
                       option = "plasma", na.value = "#EFEFEF")+#,
  #breaks = c(1e3, 100e3, 10000e3),
  #trans = "log", #para usar uma escala de log
  #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) +
  #labs(fill = "População \n(milhões)") +
  tema() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

graf_mapa_facet <- graf_mapa_comp + facet_wrap(~setor)
ggsave(plot = graf_mapa_facet, "./plots/segmentos_facet.png", width = 9, height = 8, dpi = 300)



# mapa gif ----------------------------------------------------------------

graf_mapa_labels <- ggplot(mapa_qde) +
  geom_sf(data = mapa, fill = "#EFEFEF", color = "ghostwhite") +
  geom_sf(aes(group = Estado, fill = ifelse(qde > 0, setor, NA)), color = "ghostwhite") + 
  geom_text(aes(label = "Estados com empresas do setor de ", 
                y = 9.5, x = -73.5), 
            color = "dimgrey", check_overlap = TRUE,
            family = "Lora", fontface = "plain", size = 5, 
            hjust = "left") +
  geom_text(aes(label = setor, y = 9.5, x = -50, color = setor), # no chute
            check_overlap = TRUE, family = "Lora", fontface = "bold",
            size = 5, hjust = "left") +
  scale_fill_viridis_d(direction = 1,
                       option = "plasma", na.value = "#EFEFEF") +
  scale_color_viridis_d(direction = 1,
                        option = "plasma", na.value = "#EFEFEF") +
  labs(x = NULL, y = NULL) +
  tema_mapa()

graf_mapa_gif <- graf_mapa_labels + transition_states(states = setor,
                                                      transition_length = 1,
                                                      state_length = 3) #+
# labs(title = "Estados que possuem empresas do setor de {closest_state}") +
# theme(title = element_text(size = 13, face = "plain"))

gif_animation <- animate(graf_mapa_gif, nframes = nrow(setores)*20, fps = 8, renderer = gifski_renderer())

anim_save("./plots/mapa.gif", animation = gif_animation)

  