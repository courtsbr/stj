#' Download de processos do STJ
#'
#' @export
download_stj <- function(n_processos, path) {
  a <- system.file('python/stj.py', package = 'stj')
  rPython::python.load(a)
  d <- dplyr::data_frame(n_processo = n_processos)
  d <- dplyr::group_by(d, n_processo)
  d <- dplyr::do(d, result = download_stj_um(.$n_processo, path))
  rPython::python.call('fecha')
  d <- tidyr::unnest(d, result)
  d
}

download_stj_um <- function(n_processo, path) {
  res <- rPython::python.call('pega_recurso', n_processo, path)
  res
}

#' Pega andamentos do STJ
#'
#' Pega andamentos do STJ a partir de arquivos já baixados.
#'
#' @param arqs character vector com nomes dos arquivos html
#' baixados usando \code{\link{download_stj}}
#'
#' @export
parse_stj <- function(arqs) {
  d_erro <- dplyr::data_frame('Erro no parse')
  f <- dplyr::failwith(d_erro, parse_stj_um, quiet = TRUE)
  d <- dplyr::data_frame(arq = arqs) %>%
    dplyr::mutate(n_processo = gsub('[^0-9]', '', basename(arq))) %>%
    dplyr::distinct(arq) %>%
    dplyr::group_by(arq, n_processo) %>%
    dplyr::do(f(a = .$arq)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-arq)
  d
}

parse_stj_um <- function(a) {
  h <- xml2::read_html(a, encoding = 'UTF-8')
  lab <- h %>% rvest::html_nodes('.clsFaseDataHora') %>% rvest::html_text()
  txt <- h %>% rvest::html_nodes('.classSpanFaseTexto') %>% rvest::html_text()
  link <- h %>%
    rvest::html_nodes('.classDivFaseLinha') %>%
    sapply(function(x) {
      y <- rvest::html_nodes(x, '.classSpanFaseTextoComLink')
      if(length(y) == 0) return('')
      z <- rvest::html_nodes(x, 'a') %>% rvest::html_attr('onclick')
      z <- dplyr::first(z)
      z <- ifelse(is.na(z), '', z)
      return(z)
    })
  link <- stringr::str_match(link, "[^']+'([^']+)[^']+")[, 2]
  link <- ifelse(is.na(link), '', link)
  d <- dplyr::data_frame(datetime = lab, val = txt, link = link)
  d
}

#' Pega andamentos do STJ
#'
#' Guarda em arquivos temporários, depois deleta
#'
#' @export
download_parse_stj <- function(n_processos) {
  tmpdir <- 'temporariotemporario'
  dir.create(tmpdir)
  cat('downloading...\n')
  d <- download_stj(n_processos, tmpdir)
  d_ok <- dplyr::filter(d, result == 'OK')
  if(nrow(d_ok) < 0) return(dplyr::data_frame())
  arqs <- sprintf('%s/%s.html', tmpdir, d_ok$n_processo)
  cat('parsing...\n')
  d_parse <- parse_stj(arqs)
  unlink(tmpdir, recursive = TRUE)
  d_parse
}
