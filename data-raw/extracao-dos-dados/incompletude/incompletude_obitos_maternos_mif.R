# Função para fazer webscrapping do Tabnet
tabnet_obitos_mif_maternos <- function(anos,
                                       tipo_obito # opções: "Óbitos_mulheres_idade_fértil" ou "Óbitos_maternos"
){

  # Pacotes necessários
  library(rvest)
  library(httr)
  library(dplyr)

  # URL usada
  url_cgi <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/mat10br.def"
  html <- read_html(url_cgi, encoding = "ISO-8859-1")

  df_final <- data.frame()

  ## -- Código que usei para descobrir nomes e opções de campo
  # Função para extrair os valores de um campo <select>
  # get_options <- function(html_doc, name) {
  #   nodes <- html_doc %>% html_nodes(xpath = paste0("//select[@name='", name, "']/option"))
  #   data.frame(
  #     texto = html_text(nodes),
  #     valor = html_attr(nodes, "value"),
  #     stringsAsFactors = FALSE
  #   )
  # }
  #
  # # Extraindo as opções de Linha, Coluna e Arquivos
  # opcoes_linha <- get_options(html, "Linha")
  # opcoes_coluna <- get_options(html, "Coluna")
  # opcoes_arquivos <- get_options(html, "Arquivos")
  # opcoes_incremento <- get_options(html, "Incremento")
  #
  # # Verificando os primeiros resultados
  # print(head(opcoes_linha))
  # print(head(opcoes_arquivos))

  for(i in anos){
    print(paste0("Processando ano ", i))
    # Preenchendo os campos da página do Tabnet
    payload <- list(
      Linha = iconv("Município", to = "ISO-8859-1"),
      Coluna = iconv("Óbito_investigado", to = "ISO-8859-1"),
      Incremento = iconv(tipo_obito, to = "ISO-8859-1"),
      Arquivos = paste0("matbr", substr(i, 3, 4), ".dbf"),
      Formato = "table",
      pesqmes1 = "Digite o texto e ache fácil",
      Submet = "Mostre"
    )

    # Enviando seleção dos campos
    response <- POST(
      url = url_cgi,
      body = payload,
      encode = "form",
      add_headers(`Content-Type` = "application/x-www-form-urlencoded")
    )

    # Conteúdo HTML
    html_content <- content(response, as = "text", encoding = "ISO-8859-1")

    if (grepl("Tabela de conversao nao encontrada", html_content)) {
      stop("O servidor ainda não aceitou.")
    }

    # Usa o link que retornado no html_content
    link_csv <- html_content %>%
      read_html() %>%
      html_element(xpath = "//a[contains(@href, '.csv')]") %>%
      html_attr("href")

    # Construindo o link completo para o CSV
    # 2. Construir a URL completa
    url_completa_csv <- paste0("http://tabnet.datasus.gov.br", link_csv)

    # 1. Baixando e pulando as 3 primeiras linhas de título
    df_final_aux <- read_delim(url_completa_csv,
                               delim = ";",
                               escape_double = FALSE,
                               trim_ws = TRUE,
                               skip = 3,  # pula o título e o período para usar o verdadeiro header
                               locale = locale(encoding = "ISO-8859-1")) |>
      mutate(ano = i)

    df_final <- rbind(df_final, df_final_aux)
  }

  return(df_final)
}

## ---------- Óbitos de Mulheres em Idade Fértil -------

# Anos a serem baixados
anos <- c(2022:2024)

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_tabela_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(anos)), ano = anos)

# Baixar dados de óbitos de mulheres em idade fértil
df_obitos_mif_aux <- tabnet_obitos_mif_maternos(anos, "Óbitos_mulheres_idade_fértil") |>
  mutate(
    codmunres = substr(`Município`, 1, 6),
    Obito_MIF_investigado_com_Ficha_Sintese = case_when(
      `Óbito investigado, com ficha síntese informada` == "-" ~ 0,
      TRUE ~ `Óbito investigado, com ficha síntese informada`
    ),
    Obito_MIF_investigado_sem_Ficha_Sintese = case_when(
      `Óbito investigado, sem ficha síntese informada` == "-" ~ 0,
      TRUE ~ `Óbito investigado, sem ficha síntese informada`
    ),
    TOTAL_OBITOS_MULHER_IDADE_FERTIL = Total
  ) |>
  select(codmunres, ano, Obito_MIF_investigado_com_Ficha_Sintese,
         Obito_MIF_investigado_sem_Ficha_Sintese, TOTAL_OBITOS_MULHER_IDADE_FERTIL)

df_obitos_mif <- left_join(df_tabela_municipios, df_obitos_mif_aux)

write.csv(df_obitos_mif, "data-raw/csv/incompletude_sim_obitos_mif_2022_2024.csv")

## ---------- Óbitos Maternos -------

# Anos a serem baixados
anos <- c(2022:2024)

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_tabela_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(anos)), ano = anos)

# Baixar dados de óbitos de mulheres em idade fértil
df_obitos_maternos_aux <- tabnet_obitos_mif_maternos(anos, "Óbitos_maternos") |>
  mutate(
    codmunres = substr(`Município`, 1, 6),
    Obito_Materno_investigado_com_Ficha_Sintese = case_when(
      `Óbito investigado, com ficha síntese informada` == "-" ~ 0,
      TRUE ~ `Óbito investigado, com ficha síntese informada`
    ),
    Obito_Materno_investigado_sem_Ficha_Sintese = case_when(
      `Óbito investigado, sem ficha síntese informada` == "-" ~ 0,
      TRUE ~ `Óbito investigado, sem ficha síntese informada`
    ),
    TOTAL_OBITOS_MATERNOS = Total
  ) |>
  select(codmunres, ano, Obito_Materno_investigado_com_Ficha_Sintese,
         Obito_Materno_investigado_sem_Ficha_Sintese, TOTAL_OBITOS_MATERNOS)

df_obitos_maternos <- left_join(df_tabela_municipios, df_obitos_maternos_aux)

write.csv(df_obitos_maternos, "data-raw/csv/incompletude_sim_obitos_maternos_2022_2024.csv")
