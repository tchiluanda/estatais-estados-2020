
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(gganimate)
library(ggbeeswarm)
library(plotly)

library(colorspace)
library(RColorBrewer)
library(viridis)

library(geobr)
library(cartogram)
library(sf)


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

tab_uf <- readRDS("./dados/dados-intermediarios/estados.rds") 
#read_excel("./dados/dados-originais/tab_ufs.xlsx")
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
    plr_rva   = `Ficha de Informações Financeiras da Estatal > Foi Distribuído o PLR ou RVA em 2019?`,
    Dividendos = `Relação da Estatal com o Controlador > Dividendos Pagos ao Tesouro Estadual`,
    `Subvenção` = `Relação da Estatal com o Controlador > Subvenções Recebidas do Tesouro Estadual > 2019`,
    `Reforço de Capital` = `Relação da Estatal com o Controlador > Reforço de Capital > 2019`,
    `Resultado para o Estado Acionista` = Resultado
    )



# limpeza -----------------------------------------------------------------

## Setor
#dput(unique(dados_selecionados_raw$seg))

limpa_setor <- data.frame(
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
#dput(unique(dados_selecionados_raw$dep))

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
  left_join(tab_uf) %>%
  #left_join(limpa_dep) %>%
  mutate(
    dep    = str_to_title(dep),
    dep    = ifelse(is.na(dep), "Não Informado", dep),
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

# salva df com uf, estado, regiao
# estados <- data.frame("Estado" = unique(mapa_qde$Estado), "Nome_estado" = unique(mapa_qde$name_state))
# 
# estados <- estados %>% left_join(tab_uf %>% select(UF, REGIAO), by = c("Estado" = "UF"))
# 
# saveRDS(estados, "./dados/dados-intermediarios/estados.rds")


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
ggsave(plot = graf_mapa_facet, "./plots/segmentos_facet2.png", width = 9, height = 8, dpi = 300) # windows: acrescentar: , type = "cairo-png"



# mapa gif ----------------------------------------------------------------

graf_mapa_labels <- ggplot(mapa_qde) +
  geom_sf(data = mapa, fill = "#EFEFEF", color = "ghostwhite") +
  geom_sf(aes(group = Estado, fill = ifelse(qde > 0, setor, NA)), color = "ghostwhite") + 
  geom_text(aes(label = "Estados com empresas do setor de ", 
                y = 9.5, x = -73.5), 
            color = "dimgrey", check_overlap = TRUE,
            family = "Lora", fontface = "plain", size = 5, 
            hjust = "left") +
  geom_text(aes(label = setor, y = 9.5, x = -48, color = setor), # no chute
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

gif_animation <- animate(graf_mapa_gif, nframes = nrow(setores)*10, fps = 6, renderer = gifski_renderer())

anim_save("./plots/mapa.gif", animation = gif_animation)


# barchart - quantidades --------------------------------------------------

qde_empresas_seg <- dados_selecionados %>% 
  group_by(setor, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(setor) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>%
  filter(!is.na(setor))

vetor_cores_dep <- c("Dependente" = "#f2ac29",
                     "Não Dependente" = "#718c35",
                     "Não Informado" = "#5c4b51")

graf_qde_emp <- 
  ggplot(qde_empresas_seg, aes(x = reorder(setor, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -1.6,
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +  
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  labs(x = NULL, y = NULL, 
       title = NULL, #"Quantidade de empresas por segmento", 
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp, "./plots/qde_seg.png", h = 6, w = 5)#, type = "cairo-png")


qde_empresas_est <- dados_selecionados %>% 
  group_by(Nome_estado, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(Nome_estado) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>%
  filter(!is.na(dep))

graf_qde_emp_est <- 
  ggplot(qde_empresas_est, aes(x = reorder(Nome_estado, qde_tot), y = qde, fill = dep)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde), 
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -.6, 
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +  
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  labs(x = NULL, y = NULL, 
       title = NULL, #"Quantidade de empresas por Estado", 
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp_est, "./plots/qde_est.png", h = 6.5, w = 5)



# cartograma --------------------------------------------------------------

# https://github.com/sjewo/cartogram

# pula para ler o objeto direto
# brazilmaps não está mais no CRAN, mas pode ser baixado pelo github
# mapa_regiao <- brazilmaps::get_brmap("Region")
# saveRDS(mapa_regiao, "./dados/dados-intermediarios/mapa_regiao.rds")
# st_crs(mapa_regiao)
mapa_regiao <- readRDS("./dados/dados-intermediarios/mapa_regiao.rds")

# mapa_regiao <- geobr::read_region() %>%
#   mutate(name_region = str_replace(name_region, "Centro Oeste", "Centro-oeste"))
# por algum motivo, com esse shape ele deforma o sudeste de forma muito bizarra

mapa_regiao <- mapa_regiao %>%
  mutate(name_region = str_to_title(desc_rg),
         name_region = str_replace(name_region, "Centro-Oeste", "Centro-oeste"))

qde_regiao <- dados_selecionados %>%
  count(REGIAO)

mapa_cartograma <- mapa_regiao %>% 
  left_join(qde_regiao, by = c("name_region" = "REGIAO"))

#mp_sf <- as_Spatial(mapa_cartograma)

mp_sf <- mapa_cartograma %>%
  st_as_sf() %>%
  st_transform(crs = 29101) #5641

mapa_deform <- cartogram_cont(mp_sf, 'n', 3)

mp_def <- sf::st_as_sf(mapa_deform)

# testa gif transição entre mapas normal e deformado.

mp_def_join <- mp_def %>%
  mutate(tipo_geometria = "deformada")

mp_nor_join <- mp_sf %>%
  mutate(tipo_geometria = "normal")

mp_duplo <- rbind(mp_def_join, mp_nor_join)

mapa_duplo_gif <- ggplot(data = mp_duplo, aes(fill = Region, group = Region)) +
  geom_sf(color = NA) +
  geom_sf_label(aes(label = ifelse(tipo_geometria == "deformada", n, NA)),
                size = 6,    
                color = "grey20",
                family = "Source Sans Pro",
                fill = "ghostwhite", label.size = 0,
                label.r = unit(0.67, 'lines'),
                label.padding = unit(0.35, "lines")) +
  ease_aes("cubic-in-out") +
  scale_fill_viridis(option = "viridis", direction = -1) +
  labs(x = NULL, y = NULL) +
  tema_mapa() +
  transition_states(states = tipo_geometria,
                    transition_length = 1,
                    state_length = 1)

animate(mapa_duplo_gif, fps = 8, renderer = gifski_renderer())

anim_save("./plots/cartograma.gif", animation = last_animation())


# ROE - beeswarm ----------------------------------------------------------

summary(dados_selecionados$PL)
length(which(dados_selecionados$PL==0))
length(which(is.na(dados_selecionados$PL)))
length(which(is.na(dados_selecionados$lucros)))

## importante
qde_emp_fora_roe <- length(which(
  is.na(dados_selecionados$lucros) | 
  dados_selecionados$PL<=0 | 
  is.na(dados_selecionados$PL)))

top_setores <- dados_qde_setor_estado %>% 
  group_by(setor) %>% 
  summarise(qde = sum(n)) %>% 
  arrange(desc(qde)) %>%
  filter(qde >= 10 & setor != "OUTRO")

principais_setores <- top_setores$setor

dados_roe <- dados_selecionados %>%
  #filter(PL > 0 & dep != "Não Informado") %>%
  filter(PL > 0) %>%
  mutate(ROE = lucros / PL,
         PL_formatado = format(PL, big.mark = ".", decimal.mark = ',', scientific = FALSE)) %>%
  filter(!is.na(ROE)) %>%
  mutate(Empresa = paste0(emp, ' (', Estado, ')\n',
                          'Dependência: ', dep, '\n',
                          'Setor: ', setor, '\n',
                          'PL: R$ ', PL_formatado, '\n',
                          'Lucros / Prejuízos no ano: R$ ', 
                          format(lucros, big.mark = '.', decimal.mark = ","), '\n',
                          'ROE: ', percent(round(ROE,4))),
         cat_ROE = cut(ROE, 
                       breaks = c(-Inf, -0.5, 0, 0.5, Inf), 
                       labels = c("bem_neg", "neg", "pos", "bem_pos")),
         setores_principais = ifelse(setor %in% principais_setores, setor, "Demais"),
         sinal_ROE = ifelse(ROE>0, "Positivo", "Negativo"))

summary(dados_roe$ROE)[c("Min.", "Max.")]

seq(summary(dados_roe$ROE)[c("Min.")], 
    summary(dados_roe$ROE)[c("Max.")],
    by = 0.5)


define_breaks <- function(limits) {
  seq(round(limits[1],0), round(limits[2],0), by = 0.5)
}

cor_anotacoes <- "#3b7302"

cores_escala <- c("bem_neg" = "#912849",
                  "neg"     = "#ff7270",
                  "pos"     = "#91c1cc",
                  "bem_pos" = "#375e8b")

sumario_roe <- dados_roe %>%
  group_by(cat_ROE, dep) %>%
  summarise(qde = n()) %>%
  group_by(dep) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>%
  ungroup() %>%
  mutate(y = case_when(cat_ROE == "bem_neg" ~ -0.75,
                       cat_ROE == "neg" ~ -0.25,
                       cat_ROE == "pos" ~  0.25,
                       cat_ROE == "bem_pos" ~  0.75))

sumario_roe_sinal <- dados_roe %>%
  group_by(sinal_ROE, dep) %>%
  summarise(qde = n()) %>%
  group_by(dep) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>%
  ungroup() %>%
  mutate(y = ifelse(sinal_ROE == "Positivo", 0.5, -0.5))

# empresas fora do limte
dados_roe %>% filter(ROE > 2 | ROE < -2) %>% select(emp, Estado, dep, ROE)


roe <- ggplot(dados_roe %>% filter(PL>0), aes(y = ROE, color = cat_ROE, x = dep, 
                                              label = Empresa)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "Gainsboro") +
  geom_hline(yintercept = -0.5, linetype = "dotted", color = "Gainsboro") +
  geom_beeswarm() + #aes(size = PL), 
  scale_color_manual(values = cores_escala) +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = -0.5, ymax = 0, alpha = 0.2, fill = "antiquewhite") +
  annotate("rect", xmin = 1.5, xmax = 2.9, ymin = 0, ymax = 0.5, alpha = 0.2, fill = "antiquewhite") +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Dependente", y, NA),
                label = paste0(pct_qde, ' das \nDependentes'),
                color = cat_ROE),
            x = 0.8, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Source Sans Pro", 
            size = 3.5) +
  geom_text(data = sumario_roe, 
            aes(y = ifelse(dep == "Não Dependente", y, NA),
                label = paste0(pct_qde, ' das não\nDependentes'),
                color = cat_ROE),
            x = 2.4, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Source Sans Pro", 
            size = 3.5) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_y_continuous(labels = percent, breaks = define_breaks, limits = c(-2,2)) + #, 
  tema()

roe2 <- ggplot(dados_roe %>% filter(PL>0), aes(y = ROE, color = sinal_ROE, x = dep, 
                                               label = Empresa)) +
  geom_quasirandom()+ #beeswarm() + #aes(size = PL), 
  scale_color_manual(values = c("Negativo" = "#DC143C", 
                                "Positivo" = "#008080")) +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = -2, ymax = 0, alpha = 0.2, fill = "antiquewhite") +
  annotate("rect", xmin = 1.5, xmax = 2.7, ymin = 0, ymax = 2, alpha = 0.2, fill = "antiquewhite") +
  geom_text(data = sumario_roe_sinal, 
            aes(y = ifelse(dep == "Dependente", y, NA),
                label = paste0(pct_qde, ' das \nDependentes'),
                color = sinal_ROE),
            x = 0.8, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Source Sans Pro", 
            size = 3.5) +
  geom_text(data = sumario_roe_sinal, 
            aes(y = ifelse(dep == "Não Dependente", y, NA),
                label = paste0(pct_qde, ' das não\nDependentes'),
                color = sinal_ROE),
            x = 2.2, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Source Sans Pro", 
            size = 3.5) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_y_continuous(labels = percent, 
                     breaks = define_breaks, 
                     limits = c(-2,2)) + #, 
  tema()

ggsave(plot = roe2, "./plots/roe2.png", h = 6.5, w = 6.5)

# ROE - dotplot -----------------------------------------------------------

# 197 empresas
dados_roe_agreg <- dados_roe %>%  
  filter(dep != "Não Informado",
         abs(ROE) < 10) %>%
  group_by(setor, dep) %>%
  summarise(media_ROE = mean(ROE),
            soma_lucro = sum(lucros),
            soma_PL    = sum(PL),
            ROE_medio = sum(lucros)/sum(PL)) %>%
  ungroup() %>%
  select(setor, dep, ROE_medio) %>%
  spread(dep, ROE_medio) %>%
  mutate(maior = ifelse(Dependente > `Não Dependente`, "Dependente", "Não Dependente")) %>%
  rowwise() %>%
  mutate(maximo = max(Dependente, `Não Dependente`, na.rm = T)) %>%
  gather(Dependente, `Não Dependente`, key = dep, value = ROE_medio) %>%
  arrange(desc(maximo))

roe_dotplot <- ggplot(dados_roe_agreg, aes(y = reorder(setor, maximo), 
                                           color = dep, x = ROE_medio, group = setor)) +
  geom_path(color = "lightgrey", size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = ifelse(dep == maior | is.na(maior), 
                               percent(ROE_medio, accuracy = 1), NA), 
                color = dep), fontface = "bold", size = 3.5,
            family = "Source Sans Pro",
            nudge_x = 0.09) +
  geom_text(aes(label = ifelse(dep == maior, NA, percent(ROE_medio, accuracy = 1)), 
                color = dep),  size = 3.5,
            family = "Source Sans Pro",
            nudge_x = -0.07) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = percent) +
  scale_color_manual(values = vetor_cores_dep) +
  scale_fill_manual(values = vetor_cores_dep) +
  tema_barra()

ggsave(plot = roe_dotplot, "./plots/roe_dotplot.png", h = 6, w = 5)


# ROE - plotly ------------------------------------------------------------


roe_plotly <- plot_ly(dados_roe, 
                      y = ~lucros, 
                      x = ~PL, 
                      text = ~Empresa, 
                      color = ~dep,
                      hoverinfo = "text",
                      alpha = 0.75,
                      marker = list(size = 7)) %>% 
  add_markers(colors = vetor_cores_dep) %>%
  layout(yaxis = list(title = "Lucros / Prejuízos (R$)"),
         xaxis = list(title = "Patrimônio Líquido (R$)"),
         font = "Source Sans Pro",
         hoverlabel = list(font = "Source Sans Pro"),
         legend = list(orientation = 'h', x = 0, y = 1.3)) %>%
  config(displayModeBar = FALSE)

htmlwidgets::saveWidget(partial_bundle(roe_plotly), file = "roe.html")

# Lucro / Prejuízo --------------------------------------------------------

dados_lucro_preju <- dados_selecionados %>%
  filter(!is.na(lucros)) %>%
  mutate(
    ROE = lucros / PL,
    PL_formatado = format(PL, big.mark = ".", decimal.mark = ',', scientific = FALSE)) %>%
  mutate(Empresa = paste0(emp, ' (', Estado, ')\n',
                          'Dependência: ', dep, '\n',
                          'Setor: ', setor, '\n',
                          'PL: R$ ', PL_formatado, '\n',
                          'Lucros / Prejuízos no ano: R$ ', format(lucros, big.mark = '.', decimal.mark = ","), '\n',
                          'ROE: ', ifelse(is.na(ROE), 'Não disponível', percent(round(ROE,4)))),
         setores_principais = ifelse(seg %in% principais_setores, setor, "Demais"))

qde_NAs_lucro <- length(which(is.na(dados_selecionados$lucros) == TRUE))

length(which(dados_lucro_preju$lucros<=-50e6 | dados_lucro_preju$lucros>=50e6))
summary(dados_lucro_preju$lucros)
length(which(dados_lucro_preju$result<=0))

# # só pra ver a distribuição
# ggplot(dados_lucro_preju, aes(x = result)) +# geom_histogram(bins = 100) +
#   geom_density(fill = "lightcoral", color = NA)+
#   scale_x_continuous(limits = c(-2.5e8, 2.5e8), 
#                      breaks = seq(-2.5e8, 2.5e8, by = 0.5e8),
#                      labels = function(x){format(round(x/1e6, 1), big.mark = ".",
#                                                  decimal.mark = ',')}) + 
#   tema()

ggplot(dados_lucro_preju %>% filter(dep != "Não Informado"), aes(y = lucros, color = lucros>0, x = dep, 
                                                                 label = Empresa)) +
  geom_quasirandom() + #aes(size = PL), 
  #scale_color_manual(values = c(cores_escala[1], cores_escala[4])) +
  scale_y_continuous(limits = c(-2.5e8, 50e6),
                     labels = function(x){format(round(x/1e6, 1), big.mark = ".",
                                                 decimal.mark = ',')}) + 
  labs(#title = "Distribuição do ROE das empresas do estados", 
    x = NULL, y = NULL)+ #,
  #subtitle = "Mais de 60% das dependentes têm ROE negativo, mais de 60% das não dependentes têm ROE positivo") +
  tema()

# grafico barras

sumario_lucro <- dados_selecionados %>% 
  mutate(result_pos = ifelse(lucros >= 0, "Positivo", "Negativo")) %>%
  group_by(dep, result_pos) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(dep) %>%
  mutate(tot_por_dep = sum(qde),
         percent_dep = percent(qde / tot_por_dep)) 

sumario_lucro_total <- sumario_lucro %>%
  group_by(result_pos) %>%
  summarise(dep = "Total",
            qde = sum(qde),
            tot_por_dep = sum(qde)) %>%
  ungroup() %>%
  group_by(dep) %>%
  mutate(tot_por_dep = sum(qde),
         percent_dep = percent(qde / tot_por_dep)) %>%
  ungroup() %>%
  bind_rows(sumario_lucro)

graf_barra_lucro <- ggplot(sumario_lucro_total, aes(x = dep, y = qde, fill = result_pos)) +
  geom_col(position = "fill", width = 0.65) +
  geom_text(aes(label = paste0(qde, "\n(", percent_dep,")")), position = position_fill(vjust = 0.5),
            family = "Source Sans Pro", size = 3.2, color = "ghostwhite") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Negativo" = "#DC143C", 
                               "Positivo" = "#008080"), 
                    na.value = "darkgray") +
  labs(x = NULL, y = NULL) +
  tema_barra()

ggsave(plot = graf_barra_lucro, "./plots/bar_lucro.png", h = 6, w = 4, device = "png")

sumario_lucro_setor <- dados_selecionados %>%
  filter(!is.na(lucros)) %>%
  group_by(setor) %>%
  summarise(tot = sum(lucros)) %>%
  mutate(result_pos = ifelse(tot >= 0, "Positivo", "Negativo"))

sumario_lucro_setor %>% janitor::adorn_totals("row")

graf_barra_lucro_setor <- 
  ggplot(sumario_lucro_setor, 
         aes(y = tot, color = result_pos, fill = result_pos,
             x = reorder(setor, tot))) + 
  geom_col(width = 0.6) + 
  geom_text(aes(label = format(round(tot/1e6, 0), big.mark = ".",
                               decimal.mark = ','),
                y = ifelse(tot>= 0, tot*1.03 - 1e4, tot - 5e7),
                hjust = ifelse(tot>= 0, "left", "right")), 
            vjust = 0.5,
            family = "Source Sans Pro", size = 3.5) +
  coord_flip() +
  scale_color_manual(values = c("Negativo" = "#DC143C", 
                                "Positivo" = "#008080"), 
                     na.value = "darkgray") +
  scale_fill_manual(values = c("Negativo" = "#DC143C", 
                               "Positivo" = "#008080"), 
                    na.value = "darkgray") +
  scale_y_continuous(labels = function(x){
    paste(format(round(x/1e6, 1), big.mark = ".", decimal.mark = ','), "mi")},
    expand = expand_scale(add = c(.7e9, .7e9))) +
  labs(x = NULL, y = NULL) +
  tema_barra()

ggsave(plot = graf_barra_lucro_setor, "./plots/bar_lucro_setor.png", h = 6, w = 6, device = "png")


# mapa resultado----------------------------------------------------------------


colunas_interesse <- c("Dividendos", 
                       #"Passivo Assumido", 
                       "Subvenção", 
                       "Reforço de Capital", 
                       "Resultado para o Estado Acionista")

mapa_res <- dados_selecionados %>% 
  group_by(Estado) %>% 
  summarise_at(vars(colunas_interesse),
               .funs = ~-sum(as.numeric(.), na.rm = TRUE)) %>%
  mutate(Dividendos = -Dividendos) %>%
  gather(colunas_interesse, key = "variavel", value = "valor") %>%
  left_join(mapa, by = c("Estado" = "abbrev_state"))

mapa_res_graf <- ggplot(mapa_res %>% 
                          filter(variavel == "Resultado para o Estado Acionista"))+#, nome != "SÃO PAULO")) + 
  geom_sf(aes(fill = -valor, geometry = geom), color = NA) +
  # geom_sf_text(aes(label = ifelse(valor<=-1e5, 
  #                                 format(round(-valor/1e6,0),
  #                                                  big.mark = "."), NA)),
  #              family = "Source Sans Pro", size = 3) +
  scale_fill_gradient2(low = "#DC143C", mid = "#e2e2e2",
                       high = "#008080", midpoint = 0,
                       na.value = "ghostwhite", guide = "colourbar",
                       aesthetics = "fill",
                       labels = function(x){
                         format(round(x/1e6, 0), big.mark = ".",
                                decimal.mark = ",")}) +
  # scale_fill_continuous_diverging(
  #   palette = "Reds", rev = TRUE,
  #   na.value = "ghostwhite",
  #   labels = function(x){
  #     format(round(x/1e6, 0), big.mark = ".", decimal.mark = ",")}) +
  labs(title = NULL, fill = "R$ milhões", x = NULL, y = NULL) +
  tema_mapa() + 
  theme(legend.position = "left")

ggsave(plot = mapa_res_graf, "./plots/mapa_result.png", h = 6, w = 6.5, device = "png")

dados_selecionados %>% group_by(Estado) %>% summarise(soma = sum(`Resultado para o Estado Acionista`)) %>% arrange(desc(soma))


# Resultado -  decomposição -----------------------------------------------

# quantas empresas não informaram quaisquer dessas informações de resultado?
dados_selecionados %>% filter_at(.vars = vars(colunas_interesse[1:3]), all_vars(is.na(.))) %>% select(emp, colunas_interesse) %>%
  View()

# 71.

sumario_result <- dados_selecionados %>%
  select(dep, colunas_interesse) %>%
  group_by(dep) %>%
  summarise_all(~sum(as.numeric(.), na.rm = T)) %>%
  mutate(`Resultado para o Estado Acionista` = Dividendos - `Subvenção` - `Reforço de Capital`)
  

result_total_para_incorporar <- sumario_result %>%
  select(dep, `Resultado para o Estado Acionista`) %>%
  gather(`Resultado para o Estado Acionista`, key = componentes, value = y_end) %>%
  mutate(y_0 = 0)

result_waterfall <- sumario_result %>%
  select(-`Resultado para o Estado Acionista`) %>%
  mutate(Dividendos = -Dividendos) %>%
  filter(dep != "Não Informado") %>%
  gather(-dep, key = componentes, value = valor) %>%
  arrange(dep) %>%
  group_by(dep) %>%
  mutate(y_end = cumsum(valor),
         y_0 = lag(y_end,1)) %>%
  ungroup() %>%
  select(-valor) %>%
  # gather(y_0, yend, key = cats, value = preenchimento) %>%
  # mutate(preenchimento = -preenchimento) %>%
  mutate_at(.vars = vars(starts_with("y")), .funs = ~-.) %>%
  bind_rows(result_total_para_incorporar) %>%
  mutate(componentes = factor(componentes, levels = colunas_interesse)) %>%
  replace_na(list(y_0 = 0, y_end = 0)) %>%
  mutate(pto_medio = (y_0 + y_end)/2)


waterfall <- ggplot(result_waterfall %>% filter(dep != "Não Informado"), 
                    aes(x = componentes, xend = componentes, color = componentes)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey",
             size = 0.5) +
  geom_segment(aes(y = ifelse(componentes == "Resultado para o Estado Acionista", NA, y_0), yend = y_end), 
               arrow = arrow(angle = 90, ends = "both", length = unit(.05, "inches")),
               # arrow = arrow(length = unit(3, "points"), type = "closed"), 
               size = .5) + 
  geom_segment(aes(y = ifelse(componentes != "Resultado para o Estado Acionista", NA, y_0), yend = y_end), 
               size = 14) + 
  geom_text(aes(y = ifelse(componentes == "Dividendos", y_end + .4e9,
                           y_end - .3e9),
                label = format(round((y_end-y_0)/1e6,0), big.mark = ".",
                               decimal.mark = ",")), family = "Source Sans Pro", size = 3.5, hjust = "center", vjust = "center") +
  scale_color_manual(values = c("Dividendos" = "#008080", "Subvenção" = "#DC143C", "Reforço de Capital" = "#DC143C", "Resultado para o Estado Acionista" = "#DC143C")) +
  scale_fill_manual(values = c("Dividendos" = "#008080", "Subvenção" = "#DC143C", "Reforço de Capital" = "#DC143C", "Resultado para o Estado Acionista" = "#DC143C")) +
  scale_y_continuous(labels = function(x){format(round(x/1e6, 1), big.mark = ".", decimal.mark = ',')}) +
  scale_x_discrete(labels = c("Dividendos", "Subvenção", "Reforço de Capital" = "Reforço\nde Capital", "Resultado para o Estado Acionista"="Resultado\n para o \nEstado Acionista")) +
  labs(x = NULL, y = NULL) +
  tema() + theme(panel.background = element_rect(fill = "ghostwhite",
                                                 color = NA),
                 strip.text = element_text(family = "Source Sans Pro")) +
  facet_wrap(~dep)

ggsave(plot = waterfall, "./plots/waterfall.png", h = 6, w = 6)
