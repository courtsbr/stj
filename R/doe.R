download_arq <- function(u, a, verbose = FALSE) {
  if (file.exists(a)) {
    if (verbose)
      cat("\narquivo ", a, " ja existe!\n")
    return(dplyr::data_frame(result = "exists"))
  }
  if (verbose)
    cat("\nbaixando ", a, "...", sep = "")
  res <- tryCatch({
    r <- suppressWarnings({
      httr::GET(u, httr::write_disk(a, overwrite = TRUE))
    })
    ct <- httr::headers(r)[["content-type"]]
    ct <- ifelse(is.null(ct), "application", ct)
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, "Timeout")) {
    if (verbose)
      cat("ERRO!\n")
    return(dplyr::data_frame(result = "timeout"))
  }
  if (httr::status_code(r) == 200 && stringr::str_detect(ct, "application")) {
    if (verbose)
      cat("OK!\n")
    return(dplyr::data_frame(result = "ok"))
  }
  if (verbose)
    cat("ERRO!\n")
  return(dplyr::data_frame(result = "nao tem dje"))
}

dje_stj <- function(dates, path = "data-raw/dje_pdf", verbose = FALSE) {
  u <- "http://dj.stj.jus.br/"
  pastas <- sprintf("%s/stj_dje_%s", path, dates)
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = "erro"), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(date_link = format(as.Date(date), "%Y%m%d"),
                  link = sprintf("%s%s.pdf", u, date_link),
                  arq = sprintf("%s/stj_dje_%s.pdf", pastas, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

pdf2text <- function(a, first_pg = NA, last_pg = NA, r = F, keep_file = F, new_file = 'repo.txt'){
  if(!file.exists(a)){return('')}
  if(file.size(a) > 5000 & stringi::stri_detect(a, fixed = '.pdf')){
    sprintf('pdftotext %s %s%s%s%s',
            a,
            ifelse(r,'-raw ',' '),
            ifelse(!is.na(first_pg),paste('-f',first_pg,''),' '),
            ifelse(!is.na(last_pg),paste('-l',last_pg,''),' '),
            new_file) %>%
      system()
    texto = readr::read_file(new_file)
    ifelse(keep_file,NA,file.remove(new_file))
  } else {
    texto = ''
  }
  return(texto)
}


parse_dje_stj <- function(arq) {
  txt <- pdf2text(arq, r = TRUE)
  re_cabecalho <- '(\npág\\. [0-9].*)|(\f|^)(Sup(.|\n)*?pub.*?[0-9]{4}\\.\n)'
  # spl <- stringr::str_split(txt, '\f')
  # stringr::str_replace_all(spl[[1]][], re_cabecalho, '')
  txt <- stringr::str_replace_all(txt, re_cabecalho, '')
  re_nproc <- '[\n\f](\\([0-9]+\\))[\n\f]+((.|\n)*?)\\(([0-9]{4}/[0-9]+-[0-9])\\)'
  processos <- stringr::str_match_all(txt, re_nproc)[[1]]
  id_processo <- tidyr::extract_numeric(processos[, 2])
  txt_processo <- processos[, 3]
  n_processo <- processos[, 5]
  j <- 1
  ok <- logical(length(id_processo))
  for (i in seq_along(id_processo)) {
    ok[i] <- (id_processo[i] == j)
    if (ok[i]) j <- j + 1
  }
  re_processo <- '([A-Za-z ]+?) +[nN]\\? ?([0-9.]+)[- /]+?([A-Z]{2})'
  dados <- txt_processo[ok] %>%
    desacentuar() %>%
    stringr::str_replace_all('\n', ' ') %>%
    stringr::str_match(re_processo) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(c('original', 'tipo', 'num', 'uf')) %>%
    dplyr::mutate(original = txt_processo[ok],
                  id = id_processo[ok],
                  n_stj = n_processo) %>%
    dplyr::tbl_df()
  dados
}
