
# SETUP
rm(list = ls())
gc()

# Opções globais úteis
options(
  timeout = max(600, getOption("timeout")),
  readr.show_col_types = FALSE,
  stringsAsFactors = FALSE
)

library(tidyverse)
library(magrittr)
library(lubridate)
library(openxlsx)
library(rvest)
library(janitor)
library(stringr)

# diretorio ANS
dir_ans <- "data-raw/morbidade/databases/04_mmg_ans"
if (!dir.exists(dir_ans)) dir.create(dir_ans, recursive = TRUE)

# ---------- ANS ----------

# download files

link_ans_hosp <- "https://dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/"

pag_ans_hosp <- read_html(link_ans_hosp)

#anos_ans <- pag_ans_hosp %>% html_element("table") %>% html_elements("a") %>% html_attr("href")
#anos_ans <- anos_ans[anos_ans %>% str_starts("20")]

# Anos para web e para arquivos locais
anos_ans_web <- c("2023/", "2024/")
anos_ans <- c("2023", "2024")

max_tentativas <- 5

download_retry <- function(url, dest, max_try = 5, sleep_s = 3) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  for (i in seq_len(max_try)) {
    ok <- tryCatch({
      download.file(
        url,
        destfile = dest,
        mode = "wb",
        method = "libcurl",
        quiet = TRUE
      )
      file.exists(dest) && file.info(dest)$size > 0
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (isTRUE(ok)) return(TRUE)

    if (file.exists(dest)) file.remove(dest)
    Sys.sleep(sleep_s)
  }

  FALSE
}

for (ano in anos_ans_web) {
  dir_ano <- file.path(dir_ans, str_remove(ano, "/$"))
  dir.create(dir_ano, recursive = TRUE, showWarnings = FALSE)

  links_ans_uf <- str_c(link_ans_hosp, ano)

  pag_ans_uf <- NULL
  for (i in seq_len(max_tentativas)) {
    pag_ans_uf <- tryCatch(read_html(links_ans_uf), error = function(e) NULL)
    if (!is.null(pag_ans_uf)) break
    Sys.sleep(2)
  }

  if (is.null(pag_ans_uf)) {
    message("Falhou ao acessar página do ano: ", ano, " (pulando).")
    next
  }

  uf_ans <- pag_ans_uf %>%
    html_element("table") %>%
    html_elements("a") %>%
    html_attr("href")

  uf_ans <- uf_ans[str_length(uf_ans) == 3]

  for (uf in uf_ans) {
    dir_uf <- file.path(dir_ans, str_remove(ano, "/$"), uf)
    dir.create(dir_uf, recursive = TRUE, showWarnings = FALSE)

    arqs_uf <- str_c(links_ans_uf, uf)

    pag_ans_arqs <- NULL
    for (i in seq_len(max_tentativas)) {
      pag_ans_arqs <- tryCatch(read_html(arqs_uf), error = function(e) NULL)
      if (!is.null(pag_ans_arqs)) break
      Sys.sleep(2)
    }

    if (is.null(pag_ans_arqs)) {
      message("Falhou ao acessar UF ", uf, " no ano ", ano, " (pulando UF).")
      next
    }

    arqs_dl <- pag_ans_arqs %>%
      html_element("table") %>%
      html_elements("a") %>%
      html_text()

    arqs_dl <- arqs_dl[str_starts(arqs_dl, str_sub(uf, 1, 2))]

    for (arq in arqs_dl) {
      url  <- str_c(arqs_uf, arq)
      dest <- file.path(dir_ans, str_remove(ano, "/$"), uf, arq)

      if (file.exists(dest) && file.info(dest)$size > 0) {
        message("Já existe, pulando: ", arq)
        next
      }

      ok <- download_retry(url, dest, max_try = max_tentativas, sleep_s = 3)

      if (ok) {
        message("Baixado: ", arq)
      } else {
        message("Falhou após ", max_tentativas, " tentativas: ", arq)
      }
    }

    Sys.sleep(1.0)
  }
}


# =========================
# unzip files
# =========================

# Recalcula uf_ans por ano e descompacta apenas .zip
for (anos in anos_ans) {
  uf_ans <- list.dirs(file.path(dir_ans, anos), full.names = FALSE, recursive = FALSE)

  for (uf in uf_ans) {
    pasta_uf <- file.path(dir_ans, anos, uf)
    arqs_dl <- list.files(pasta_uf, full.names = TRUE)
    arqs_zip <- arqs_dl[str_detect(arqs_dl, "\\.zip$")]

    for (arq_zip in arqs_zip) {
      unzip(arq_zip, exdir = pasta_uf, overwrite = FALSE)
    }
  }
}


# =========================
# ler e juntar os arquivos csv CONS por ano
# =========================

uf_ans <- list.dirs(file.path(dir_ans, anos_ans[1]), full.names = FALSE, recursive = FALSE)

ans_cons_resumo <- tibble(UF = character(), ANO = character(), TOTAL = integer())

for (anos in anos_ans) {

  cons_list <- list()
  resumo_list <- list()
  k <- 0L
  k_resumo <- 0L

  for (uf in uf_ans) {

    dir_uf <- file.path(dir_ans, anos, uf)
    arqs_dl_cons <- list.files(dir_uf, pattern = "CONS\\.csv$", full.names = TRUE)

    for (path in arqs_dl_cons) {

      df_cons_temp <- read_csv2(
        path,
        col_types = cols(.default = col_character())
      )

      k <- k + 1L
      cons_list[[k]] <- df_cons_temp

      # evita crescimento repetido com c(...)
      arq <- basename(path)
      k_resumo <- k_resumo + 1L
      resumo_list[[k_resumo]] <- tibble(
        UF = str_sub(arq, 1, 2),
        ANO = str_sub(arq, 4, 7),
        TOTAL = nrow(df_cons_temp)
      )

      rm(df_cons_temp)
    }

    message(anos, " ; ", uf)
  }

  ans_cons_tot <- bind_rows(cons_list)

  ans_cons_resumo <- bind_rows(
    ans_cons_resumo,
    bind_rows(resumo_list)
  )

  write_delim(
    ans_cons_tot,
    file.path(dir_ans, anos, paste0("TOTAL_", anos, "_CONS.csv")),
    delim = "|",
    na = "",
    quote = "none"
  )

  rm(cons_list, resumo_list, ans_cons_tot, k, k_resumo)
  gc(FALSE)
}


# =========================
# juntar todos os anos dos arquivos CONS
# =========================

df_list <- vector("list", length(anos_ans))

for (i in seq_along(anos_ans)) {
  anos <- anos_ans[i]

  df_list[[i]] <- read_delim(
    file.path(dir_ans, anos, paste0("TOTAL_", anos, "_CONS.csv")),
    delim = "|",
    quote = "",
    na = "",
    col_types = cols(.default = col_character())
  )

  message(anos)
}

ans_cons_tot <- bind_rows(df_list)
rm(df_list)
gc(FALSE)

ans_cons_tot %>%
  write_delim(file.path(dir_ans, "ans_cons_tot.csv"), delim = "|", na = "", quote = "none")


# =========================
# calcular registros por ano por uf total
# =========================

ans_cons_sumario <- ans_cons_tot %>%
  mutate(ANO = str_sub(ANO_MES_EVENTO, 1, 4)) %>%
  count(UF_PRESTADOR, ANO, name = "TOTAL")


# =========================
# selecionar apenas os registro de mulheres de 10 a 49 anos
# =========================

fx_etaria <- c("10 a 14", "15 a 19", "20 a 29", "30 a 39", "40 a 49")

ans_cons_mm <- ans_cons_tot %>%
  select(
    ID_EVENTO_ATENCAO_SAUDE, FAIXA_ETARIA, SEXO, CD_MUNICIPIO_BENEFICIARIO, UF_PRESTADOR,
    TEMPO_DE_PERMANENCIA, ANO_MES_EVENTO, CD_TIPO_INTERNACAO, CD_REGIME_INTERNACAO, CD_MOTIVO_SAIDA,
    starts_with("CID")
  ) %>%
  filter(
    SEXO == "Feminino",
    FAIXA_ETARIA %in% fx_etaria,
    CD_REGIME_INTERNACAO == "1"
  )

ans_cons_mm %>%
  write_delim(file.path(dir_ans, "ans_cons_mm.csv"), delim = "|", na = "", quote = "none")


# =========================
# calcular registros por ano por uf MM
# =========================

ans_cons_sumario %<>%
  left_join(
    ans_cons_mm %>%
      mutate(ANO = str_sub(ANO_MES_EVENTO, 1, 4)) %>%
      count(UF_PRESTADOR, ANO, name = "TOT_MM"),
    by = c("UF_PRESTADOR", "ANO")
  )

ans_cons_sumario %>%
  write.xlsx(file.path(dir_ans, "ans_cons_sumario.xlsx"), asTable = TRUE)


# =========================
# ler e juntar os arquivos csv DET por ano
# =========================

uf_ans <- list.dirs(file.path(dir_ans, anos_ans[1]), full.names = FALSE, recursive = FALSE)

ans_det_sumario <- tibble(UF = character(), ANO = character(), TOTAL = integer())

for (anos in anos_ans) {

  det_list <- list()
  resumo_det_list <- list()
  k <- 0L
  k_resumo <- 0L

  for (uf in uf_ans) {

    dir_uf <- file.path(dir_ans, anos, uf)
    arqs_dl_det <- list.files(dir_uf, pattern = "DET\\.csv$", full.names = TRUE)

    for (path in arqs_dl_det) {

      df_det_temp <- read_csv2(
        path,
        col_types = cols(.default = col_character())
      )

      k <- k + 1L
      det_list[[k]] <- df_det_temp

      # evita crescimento com c(...)
      arq <- basename(path)
      k_resumo <- k_resumo + 1L
      resumo_det_list[[k_resumo]] <- tibble(
        UF = str_sub(arq, 1, 2),
        ANO = str_sub(arq, 4, 7),
        TOTAL = nrow(df_det_temp)
      )

      rm(df_det_temp)
    }

    message(anos, " ; ", uf)
  }

  ans_det_tot <- bind_rows(det_list)

  ans_det_sumario <- bind_rows(
    ans_det_sumario,
    bind_rows(resumo_det_list)
  )

  write_delim(
    ans_det_tot,
    file.path(dir_ans, anos, paste0("TOTAL_", anos, "_DET.csv")),
    delim = "|",
    na = "",
    quote = "none"
  )

  rm(det_list, resumo_det_list, ans_det_tot, k, k_resumo)
  gc(FALSE)
}

ans_det_sumario %>%
  write.xlsx(file.path(dir_ans, "ans_det_sumario.xlsx"), asTable = TRUE)


# =========================
# calcular registros por ano por uf total
# =========================

ans_det_sumario %<>%
  group_by(UF, ANO) %>%
  summarise(TOTAL = sum(TOTAL, na.rm = TRUE), .groups = "drop")


# =========================
# selecionar registros DET q estao em CONS MM
# =========================

#  remove NAs do vetor de IDs
ids_cons <- unique(na.omit(ans_cons_mm$ID_EVENTO_ATENCAO_SAUDE))

det_list <- vector("list", length(anos_ans))

for (i in seq_along(anos_ans)) {
  anos <- anos_ans[i]

  path <- file.path(dir_ans, anos, paste0("TOTAL_", anos, "_DET.csv"))

  df_det_temp <- read_delim(
    path,
    delim = "|",
    quote = "",
    na = "",
    col_types = cols(.default = col_character())
  )

  df_det_temp <- df_det_temp %>%
    filter(ID_EVENTO_ATENCAO_SAUDE %in% ids_cons)

  det_list[[i]] <- df_det_temp

  message(anos)
  rm(df_det_temp)
  gc(FALSE)
}

ans_det_mm <- bind_rows(det_list)
rm(det_list)
gc(FALSE)

ans_det_mm %>%
  write_delim(file.path(dir_ans, "ans_det_mm.csv"), delim = "|", na = "", quote = "none")

ans_det_sumario %<>%
  left_join(
    ans_det_mm %>%
      mutate(ANO = str_sub(ANO_MES_EVENTO, 1, 4)) %>%
      count(UF_PRESTADOR, ANO, name = "TOT_MM"),
    by = c("UF" = "UF_PRESTADOR", "ANO" = "ANO")
  )

ans_det_sumario %>%
  write.xlsx(file.path(dir_ans, "ans_det_sumario.xlsx"), asTable = TRUE)



# =========================
# ---------- ANS CONS ----------
# =========================

ans_cons_mm <- read_delim(
  file.path(dir_ans, "ans_cons_mm.csv"),
  delim = "|",
  quote = "",
  na = "",
  col_types = cols(.default = "c")
)

# pré-calcula colunas CID e substitui NA uma única vez
cid_cols_cons <- names(ans_cons_mm)[str_starts(names(ans_cons_mm), "CID")]
ans_cons_mm <- ans_cons_mm %>%
  mutate(across(all_of(cid_cols_cons), ~ replace_na(.x, "")))

# CIDs de interesse
cid_z <- str_c("Z34", "Z35", "Z36", "Z37", "Z38", "Z39", sep = "|")

# CIDs P de interesse
cid_p <- c(
  "P000", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012", "P013",
  "P014", "P015", "P016", "P017", "P018", "P019", "P020", "P021", "P022", "P023", "P024", "P025", "P026", "P027",
  "P028", "P029", "P030", "P031", "P032", "P033", "P034", "P035", "P036", "P038", "P039", "P040", "P041", "P042",
  "P043", "P044", "P045", "P046", "P048", "P049", "P051", "P052", "P059", "P100", "P101", "P102", "P103", "P104",
  "P108", "P109", "P110", "P111", "P112", "P113", "P114", "P115", "P119", "P200", "P201", "P209", "P210", "P211",
  "P219", "P350", "P351", "P352", "P353", "P358", "P359", "P370", "P371", "P373", "P374", "P378", "P379", "P392",
  "P398", "P399", "P500", "P501", "P502", "P503", "P504", "P505", "P508", "P509", "P520", "P521", "P522", "P523",
  "P524", "P525", "P526", "P528", "P529", "P53", "P550", "P551", "P558", "P559", "P560", "P569", "P60", "P832",
  "P833", "P93", "P95", "P964", "P965", "P968", "P969"
)

ans_cons_mm %<>%
  mutate(
    IND_TP_INTER_3 = if_else(CD_TIPO_INTERNACAO == "3", 1L, NA_integer_),
    IND_TOT_P = if_else(if_any(all_of(cid_cols_cons), ~ str_detect(.x, "P")), 1L, NA_integer_),
    IND_TOT_CID_P = if_else(if_any(all_of(cid_cols_cons), ~ .x %in% cid_p), 1L, NA_integer_),
    IND_CRITERIO_INTER = if_else(
      IND_TP_INTER_3 == 1 & (is.na(IND_TOT_P) | IND_TOT_CID_P == 1),
      1L, NA_integer_
    )
  )

ans_cons_mm %<>%
  mutate(
    IND_TOT_INTER_O = if_else(if_any(all_of(cid_cols_cons), ~ str_starts(.x, "O")), 1L, NA_integer_),
    IND_TOT_INTER_Z = if_else(if_any(all_of(cid_cols_cons), ~ str_detect(.x, cid_z)), 1L, NA_integer_),
    IND_TOT_INTER_P = if_else(
      if_any(all_of(cid_cols_cons), ~ .x %in% cid_p) &
        (CD_TIPO_INTERNACAO != "4" | is.na(CD_TIPO_INTERNACAO)),
      1L, NA_integer_
    ),
    IND_CID = if_else(
      IND_TOT_INTER_O == 1 | IND_TOT_INTER_Z == 1 | IND_TOT_INTER_P == 1,
      1L, NA_integer_
    )
  )

ans_cons_mm %<>%
  mutate(
    IND_INTERN_OBSTETRICAS_CONS = if_else(
      IND_CRITERIO_INTER == 1 | IND_CID == 1,
      1L, NA_integer_
    )
  )

ans_cons_mm %>%
  write_delim(file.path(dir_ans, "ans_cons_mm.csv"), delim = "|", na = "", quote = "none")

# =========================
# ---------- ANS DET ----------
# =========================

ans_det_mm <- read_delim(
  file.path(dir_ans, "ans_det_mm.csv"),
  delim = "|",
  quote = "",
  na = "",
  col_types = cols(.default = "c")
)

aborto <- c("31309062", "31303013", "31309020", "40501124", "40501132", "40501221", "40503097")

trab_parto_e_parto <- c("20202016", "20202024", "31309011", "31309038", "31309097", "31309127", "31309135", "31309054", "31309208")

gestacao_ectopica <- c("31309186", "31309089")

pos_parto_pos_aborto <- c("31309100", "31309119", "31309194", "31309151", "31303323", "31303285", "31303315", "31306047")

exames_procedimentos <- c(
  "31309178", "31309046", "31309216", "31309224", "31309232", "40201015", "31309259", "40309444",
  "41401670", "40502139", "40502147", "40502155", "40901238", "40901246", "40901254", "40901262",
  "40901270", "40901289", "40901297", "40902013", "40902021", "40901505", "40901556"
)

ans_det_mm %<>%
  mutate(
    IND_ABORTO_DET = if_else(CD_PROCEDIMENTO %in% aborto, 1L, NA_integer_),
    IND_TRABALHO_PARTO_E_PARTO_DET = if_else(CD_PROCEDIMENTO %in% trab_parto_e_parto, 1L, NA_integer_),
    IND_GESTACAO_ECTOPICA_DET = if_else(CD_PROCEDIMENTO %in% gestacao_ectopica, 1L, NA_integer_),
    IND_POS_PARTO_POS_ABORTO_DET = if_else(CD_PROCEDIMENTO %in% pos_parto_pos_aborto, 1L, NA_integer_),
    IND_EXAMES_PROCEDIMENTOS_DET = if_else(CD_PROCEDIMENTO %in% exames_procedimentos, 1L, NA_integer_),
    IND_INTERN_OBSTETRICAS_DET = if_else(
      if_any(
        c(
          IND_ABORTO_DET, IND_TRABALHO_PARTO_E_PARTO_DET, IND_GESTACAO_ECTOPICA_DET,
          IND_POS_PARTO_POS_ABORTO_DET, IND_EXAMES_PROCEDIMENTOS_DET
        ),
        ~ .x == 1
      ),
      1L, NA_integer_
    )
  )

gravidez_ectopica <- c("31309186", "31309089")
insuficiencia_respiratoria <- c("40302016", "40302024")
choque <- "20204027"
transfusao_sanguinea <- c("40401014", "40401022", "40402045", "40402053", "40402061", "40402070", "40402096",
                          "40402100", "40402169", "41203208")
acesso_venoso_central <- c("30913098", "30913012", "30913144")
histerectomia <- c("31303323", "31303080", "31303102", "31303110", "31303129", "31303285", "31303200", "31303218",
                   "31303226", "31303234", "31309208")
admissao_unidade_trat_intensivo <- c("60001038", "60001031", "10104011", "10104020")
intervencao_cirurgica <- c("30101620", "30101638", "30602050", "31009018", "31009174", "31009298", "31302050", "31302068",
                           "31306039", "31309100", "31309119", "31303315")
parto_procs <- c("31309127", "31309135", "31309054", "31309208")

ans_det_mm %<>%
  mutate(
    IND_GRAVIDEZ_ECTOPICA_DET = if_else(CD_PROCEDIMENTO %in% gravidez_ectopica, 1L, NA_integer_),
    IND_INSUF_RESP_DET = if_else(CD_PROCEDIMENTO %in% insuficiencia_respiratoria, 1L, NA_integer_),
    IND_CHOQUE_DET = if_else(CD_PROCEDIMENTO == choque, 1L, NA_integer_),
    IND_TRANSFUSAO_SANGUINEA_DET = if_else(CD_PROCEDIMENTO %in% transfusao_sanguinea, 1L, NA_integer_),
    IND_ACESSO_VENOSO_CENTRAL_DET = if_else(CD_PROCEDIMENTO %in% acesso_venoso_central, 1L, NA_integer_),
    IND_HISTERECTOMIA_DET = if_else(CD_PROCEDIMENTO %in% histerectomia, 1L, NA_integer_),
    IND_ADMIS_UNIDADE_TRAT_INTENSIVO_DET = if_else(CD_PROCEDIMENTO %in% admissao_unidade_trat_intensivo, 1L, NA_integer_),
    IND_INTERVENCAO_CIRURGICA_DET = if_else(CD_PROCEDIMENTO %in% intervencao_cirurgica, 1L, NA_integer_),
    IND_PARTO_DET = if_else(CD_PROCEDIMENTO %in% parto_procs, 1L, NA_integer_)
  )

ans_det_mm %>%
  write_delim(file.path(dir_ans, "ans_det_mm.csv"), delim = "|", na = "", quote = "none")


# =========================
# agrupar base
# =========================

ans_det_mm_grp <- ans_det_mm %>%
  group_by(ID_EVENTO_ATENCAO_SAUDE) %>%
  summarise(
    IND_ABORTO_DET = if_else(any(IND_ABORTO_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_TRABALHO_PARTO_E_PARTO_DET = if_else(any(IND_TRABALHO_PARTO_E_PARTO_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_GESTACAO_ECTOPICA_DET = if_else(any(IND_GESTACAO_ECTOPICA_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_POS_PARTO_POS_ABORTO_DET = if_else(any(IND_POS_PARTO_POS_ABORTO_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_EXAMES_PROCEDIMENTOS_DET = if_else(any(IND_EXAMES_PROCEDIMENTOS_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_GRAVIDEZ_ECTOPICA_DET = if_else(any(IND_GRAVIDEZ_ECTOPICA_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_INSUF_RESP_DET = if_else(any(IND_INSUF_RESP_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_CHOQUE_DET = if_else(any(IND_CHOQUE_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_TRANSFUSAO_SANGUINEA_DET = if_else(any(IND_TRANSFUSAO_SANGUINEA_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_ACESSO_VENOSO_CENTRAL_DET = if_else(any(IND_ACESSO_VENOSO_CENTRAL_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_HISTERECTOMIA_DET = if_else(any(IND_HISTERECTOMIA_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_ADMIS_UNIDADE_TRAT_INTENSIVO_DET = if_else(any(IND_ADMIS_UNIDADE_TRAT_INTENSIVO_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_INTERVENCAO_CIRURGICA_DET = if_else(any(IND_INTERVENCAO_CIRURGICA_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    IND_PARTO_DET = if_else(any(IND_PARTO_DET == 1, na.rm = TRUE), 1L, NA_integer_),
    .groups = "drop"
  )

ans_det_mm_grp %>%
  write_delim(file.path(dir_ans, "ans_det_mm_grp.csv"), delim = "|", na = "", quote = "none")


# =========================
# juuntar com a base CONS
# =========================

ans_cons_det_mm <- ans_cons_mm %>%
  left_join(ans_det_mm_grp, by = "ID_EVENTO_ATENCAO_SAUDE")

ans_cons_det_mm %>%
  write_delim(file.path(dir_ans, "ans_cons_det_mm.csv"), delim = "|", na = "", quote = "none")



# =========================
# ---------- ANS CONS DET ----------
# =========================

ans_cons_det_mm <- read_delim(
  file.path(dir_ans, "ans_cons_det_mm.csv"),
  delim = "|",
  quote = "",
  na = "",
  col_types = cols(.default = "c")
)

ans_cons_det_mm %<>%
  mutate(
    across(starts_with("IND_"), ~ as.integer(.x)),
    IND_INTERN_OBSTETRICAS_DET = if_else(
      if_any(
        c(
          IND_ABORTO_DET, IND_TRABALHO_PARTO_E_PARTO_DET, IND_GESTACAO_ECTOPICA_DET,
          IND_POS_PARTO_POS_ABORTO_DET, IND_EXAMES_PROCEDIMENTOS_DET
        ),
        ~ .x == 1
      ),
      1L, NA_integer_
    ),
    TEMPO_DE_PERMANENCIA = as.integer(TEMPO_DE_PERMANENCIA)
  )

# pré-calcula CID também nesta base
cid_cols_obs <- names(ans_cons_det_mm)[str_starts(names(ans_cons_det_mm), "CID_")]
ans_cons_det_mm <- ans_cons_det_mm %>%
  mutate(across(all_of(cid_cols_obs), ~ replace_na(.x, "")))

desc_plac <- c("O450", "O458", "O459")
ectopica <- c("O000", "O001", "O002", "O008", "O009")
hemorragia <- c("O720", "O721", "O722")
rotura <- c("O710", "O711")
pre_eclamp <- "O141"
eclampsia <- c("O150", "O151", "O152", "O159")
hiper_grave_cid <- c("O11", "O13", "O149", "O16", "O100", "O101", "O102", "O103", "O104", "O109", "I10", "I11", "I12", "I13", "I15")
encef_hiper <- "I674"
hellp <- "O142"
endometrite <- "O85"
edema_pulm <- "J81"
insuf_resp <- c("J80", "J960", "J969", "R092", "I26", "I260", "I269", "O032", "O037", "O042", "O047", "O052",
                "O057", "O062", "O067", "O072", "O077", "O082", "O880", "O881", "O882", "O883", "O888")
convulsao <- "R568"
sepse <- c("A41", "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419", "A40", "A400", "A401", "A402", "A403", "A408", "A409",
           "A021", "A227", "A267", "A327", "A427", "B377", "O080", "O753", "R572")
choque_cid <- c("E86", "R570", "R571", "R578", "R579", "T811", "I460", "O083", "O751")
crise_tireo <- "E055"
transfusao_cid <- "Z513"
histerectomia_cid <- "O822"
intubacao <- c("O290", "O292", "O293", "O295", "O296", "O298", "O299", "O740", "O741", "O742", "O743", "O744",
               "O746", "O747", "O748", "O749", "O890", "O891", "O892", "O893", "O895", "O896", "O898", "O899")
parto_cid <- str_c("O80", "O81", "O82", sep = "|")

ans_obs <- ans_cons_det_mm %>%
  filter(IND_INTERN_OBSTETRICAS_CONS == 1 | IND_INTERN_OBSTETRICAS_DET == 1)

ans_obs %<>%
  mutate(
    IND_DESC_PLAC_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% desc_plac), 1L, NA_integer_),
    IND_ECTOPICA_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% ectopica), 1L, NA_integer_),
    IND_HEMORRAGIA_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% hemorragia), 1L, NA_integer_),
    IND_ROTURA_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% rotura), 1L, NA_integer_),
    IND_PRE_ECLAMP_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == pre_eclamp), 1L, NA_integer_),
    IND_ECLAMPSIA_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% eclampsia), 1L, NA_integer_),
    IND_HIPER_GRAVE_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% hiper_grave_cid), 1L, NA_integer_),
    IND_ENCEF_HIPER_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == encef_hiper), 1L, NA_integer_),
    IND_HELLP_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == hellp), 1L, NA_integer_),
    IND_ENDOMETRITE_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == endometrite), 1L, NA_integer_),
    IND_EDEMA_PULM_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == edema_pulm), 1L, NA_integer_),
    IND_INSUF_RESP_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% insuf_resp), 1L, NA_integer_),
    IND_CONVULSOES_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == convulsao), 1L, NA_integer_),
    IND_SEPSE_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% sepse), 1L, NA_integer_),
    IND_CHOQUE_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% choque_cid), 1L, NA_integer_),
    IND_CRISE_TIREO_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == crise_tireo), 1L, NA_integer_),
    IND_TRANSFUSAO_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == transfusao_cid), 1L, NA_integer_),
    IND_HISTERECTOMIA_CONS = if_else(if_any(all_of(cid_cols_obs), ~ .x == histerectomia_cid), 1L, NA_integer_),
    IND_INTUB = if_else(if_any(all_of(cid_cols_obs), ~ .x %in% intubacao), 1L, NA_integer_),
    IND_PARTO_CONS = if_else(if_any(all_of(cid_cols_obs), ~ str_detect(.x, parto_cid)), 1L, NA_integer_)
  )

ans_obs %<>%
  mutate(
    IND_MMG_ECTOPICA = if_else(IND_ECTOPICA_CONS == 1 | IND_GRAVIDEZ_ECTOPICA_DET == 1, 1L, NA_integer_),
    IND_MMG_INSUF_RESP = if_else(IND_INSUF_RESP_CONS == 1 | IND_INSUF_RESP_DET == 1, 1L, NA_integer_),
    IND_MMG_CHOQUE = if_else(IND_CHOQUE_CONS == 1 | IND_CHOQUE_DET == 1, 1L, NA_integer_),
    IND_MMG_HISTERECTOMIA = if_else(IND_HISTERECTOMIA_CONS == 1 | IND_HISTERECTOMIA_DET == 1, 1L, NA_integer_),
    casos_MMG_Transfusao = if_else(IND_TRANSFUSAO_CONS == 1 | IND_TRANSFUSAO_SANGUINEA_DET == 1, 1L, NA_integer_)
  )

ans_obs %<>%
  mutate(
    casos_MMG_TMP = if_else((IND_PARTO_CONS == 1 | IND_PARTO_DET == 1) & TEMPO_DE_PERMANENCIA > 7, 1L, NA_integer_),
    casos_MMG_hipertensao = if_else(
      IND_PRE_ECLAMP_CONS == 1 | IND_ECLAMPSIA_CONS == 1 | IND_HIPER_GRAVE_CONS == 1 |
        IND_ENCEF_HIPER_CONS == 1 | IND_HELLP_CONS == 1,
      1L, NA_integer_
    ),
    casos_MMG_hemorragia = if_else(
      IND_DESC_PLAC_CONS == 1 | IND_MMG_ECTOPICA == 1 | casos_MMG_Transfusao == 1 |
        IND_HEMORRAGIA_CONS == 1 | IND_ROTURA_CONS == 1,
      1L, NA_integer_
    ),
    casos_MMG_infeccoes = if_else(IND_ENDOMETRITE_CONS == 1 | IND_SEPSE_CONS == 1, 1L, NA_integer_),
    casos_MMG_cirurgia = if_else(IND_MMG_HISTERECTOMIA == 1, 1L, NA_integer_),
    casos_MMG_UTI = IND_ADMIS_UNIDADE_TRAT_INTENSIVO_DET,
    casos_MMG = if_else(
      IND_DESC_PLAC_CONS == 1 | IND_MMG_ECTOPICA == 1 | IND_HEMORRAGIA_CONS == 1 | IND_ROTURA_CONS == 1 |
        IND_PRE_ECLAMP_CONS == 1 | IND_ECLAMPSIA_CONS == 1 | IND_HIPER_GRAVE_CONS == 1 |
        IND_ENCEF_HIPER_CONS == 1 | IND_HELLP_CONS == 1 | IND_ENDOMETRITE_CONS == 1 |
        IND_EDEMA_PULM_CONS == 1 | IND_MMG_INSUF_RESP == 1 | IND_CONVULSOES_CONS == 1 | IND_SEPSE_CONS == 1 |
        IND_MMG_CHOQUE == 1 | IND_CRISE_TIREO_CONS == 1 | casos_MMG_Transfusao == 1 |
        IND_ACESSO_VENOSO_CENTRAL_DET == 1 | IND_MMG_HISTERECTOMIA == 1 | casos_MMG_UTI == 1 |
        casos_MMG_TMP == 1 | IND_INTERVENCAO_CIRURGICA_DET == 1 | IND_INTUB == 1,
      1L, NA_integer_
    )
  )

ans_obs %>%
  write_delim(file.path(dir_ans, "ans_obs.csv"), delim = "|", na = "", quote = "none")

ans_obs %<>%
  mutate(ANO = str_sub(ANO_MES_EVENTO, 1, 4))


# =========================
# dataframe com codigos do IBGE
# =========================

codigo_ibge <- tribble(
  ~estado, ~UF, ~codigo,
  "Acre", "AC", "12",
  "Alagoas", "AL", "27",
  "Amapá", "AP", "16",
  "Amazonas", "AM", "13",
  "Bahia", "BA", "29",
  "Ceará", "CE", "23",
  "Distrito Federal", "DF", "53",
  "Espírito Santo", "ES", "32",
  "Goiás", "GO", "52",
  "Maranhão", "MA", "21",
  "Mato Grosso", "MT", "51",
  "Mato Grosso do Sul", "MS", "50",
  "Minas Gerais", "MG", "31",
  "Pará", "PA", "15",
  "Paraíba", "PB", "25",
  "Paraná", "PR", "41",
  "Pernambuco", "PE", "26",
  "Piauí", "PI", "22",
  "Rio Grande do Norte", "RN", "24",
  "Rio Grande do Sul", "RS", "43",
  "Rio de Janeiro", "RJ", "33",
  "Rondônia", "RO", "11",
  "Roraima", "RR", "14",
  "Santa Catarina", "SC", "42",
  "São Paulo", "SP", "35",
  "Sergipe", "SE", "28",
  "Tocantins", "TO", "17"
)


# =========================
# calculo do total por MUNICIPIO por ANO
# =========================

ans_obs_mun <- tibble(
  MUNICIPIO_DE_RESIDENCIA = rep(unique(ans_obs$CD_MUNICIPIO_BENEFICIARIO), each = length(2023:2025)),
  ANO = rep(2023:2025, times = length(unique(ans_obs$CD_MUNICIPIO_BENEFICIARIO)))
) %>%
  mutate(ANO = as.character(ANO)) %>%
  arrange(MUNICIPIO_DE_RESIDENCIA, ANO)

ans_obs_mun %<>%
  # ALTERAÇÃO: trocado full_join por left_join para manter a grade base município x ano
  left_join(
    ans_obs %>%
      mutate(CID_IGN = if_else(if_all(all_of(cid_cols_obs), ~ .x == ""), 1L, NA_integer_)) %>%
      group_by(CD_MUNICIPIO_BENEFICIARIO, ANO) %>%
      summarise(
        TOTAL_INT_OBST = n(),
        across(starts_with("casos_"), ~ sum(.x, na.rm = TRUE)),
        TOTAL_TIPO_OBST = sum(IND_TP_INTER_3, na.rm = TRUE),
        CID_IGN = sum(CID_IGN, na.rm = TRUE),
        TOTAL_TIPO_OBST_2 = sum(IND_CRITERIO_INTER, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("MUNICIPIO_DE_RESIDENCIA" = "CD_MUNICIPIO_BENEFICIARIO", "ANO" = "ANO")
  ) %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, .x)))

codigos_municipios <- read.csv(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv"
) |>
  pull(codmunres) |>
  as.character()

df_aux_municipios <- data.frame(
  codmunres = rep(codigos_municipios, each = length(2023:2025)),
  ano = 2023:2025
)

df_mmg_ans_final <- left_join(
  df_aux_municipios,
  ans_obs_mun |>
    rename(codmunres = MUNICIPIO_DE_RESIDENCIA, ano = ANO, total_internacoes_ans = TOTAL_INT_OBST) |>
    rename_with(
      ~ sub("^casos_MMG", "casos_MMG_ans", .x),
      .cols = starts_with("casos_MMG")
    ) |>
    mutate(ano = as.numeric(ano)) |>
    clean_names()
) |>
  dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

df_mmg_ans_final %>%
  write.csv("data-raw/csv/indicadores_bloco6_morbidade_materna_ans_2023-2025.csv", row.names = FALSE)


# =========================
# limpeza final opcional
# =========================

rm(
  ans_cons_tot, ans_cons_mm, ans_det_mm, ans_det_mm_grp,
  ans_cons_det_mm, ans_obs, ans_obs_mun, df_mmg_ans_final
)
gc()
