#' Detalha os versiculos e capitulos de um determinado livro
#' definido pelo arquivo .usx
#'
#' @param usx objeto usx lido pelo pacote xml2
#'
#' @return uma \code{tibble}
#'
#' @export
numero_versiculos <- function(usx) {
  livro <- tibble::tibble(cap = integer(), ver = integer())
  styles <- sapply(usx$usx, function(el) attr(el, "style"))
  for(ind in 1:length(styles)) {
    style <- styles[ind]
    if(style == "c") {
      cap <- as.integer(attr(usx$usx[[ind]], "number"))
      next
    }
    if(style == "p") {
      ver <- as.integer(attr(usx$usx[[ind]]$verse, "number"))
      livro <- rbind(livro, tibble::tibble(cap = cap, ver = ver))
    }
  }
  return(livro)
}

#' Verifica se e necessario fazer atualizacoes no template para
#' adequar versoes com diferentes numeros de versiculos
#'
#' @param versao versao da traducao biblica: ACF, ARA, ARC, AS21, JFAA,
#' KJA, KJF, NAA, NBV, NTLH, NVI, NVT ou TB
#'
#' @return uma \code{tibble}
#'
#' @export
necessario_atualizar <- function(versao) {
  livros_nome <- xml2::read_xml("inst/usx/template/metadata.xml") %>%
    xml2::xml_find_all(xpath = "//publications//structure//content") %>%
    xml2::xml_attrs() %>% sapply(function(el) el[["role"]])
  livros_nome_aux <- purrr::set_names(1:length(livros_nome), livros_nome)
  stats_livros <- purrr::map_dfr(livros_nome, function(livro_nome) {
    usx_list <- xml2::read_xml(paste0('inst/usx/template/release/USX_1/',
                                      livro_nome, '.usx')) %>%
      xml2::as_list()
    dplyr::mutate(numero_versiculos(usx_list), liv = livro_nome, .before = cap)
  })
  biblia <- ler_biblia(versao)
  stats_biblia <- purrr::imap_dfr(livros_nome, function(livro_nome, ind) {
    purrr::imap_dfr(biblia[[ind]]$chapters, function(cap, ind) {
      tibble::as_tibble(expand.grid(cap = ind, ver = 1:length(cap)))
    }) %>% dplyr::mutate(liv = livro_nome, .before = cap)
  })
  stats_livros %>%
    dplyr::group_by(liv, cap) %>%
    dplyr::summarise(nver = max(ver)) %>%
    dplyr::inner_join(
      stats_biblia %>%
        dplyr::group_by(liv, cap) %>%
        dplyr::summarise(nver = max(ver)),
      suffix = c(".livros", ".biblia"),
      by = c("liv", "cap")
    ) %>% dplyr::mutate(alterar = !(nver.livros == nver.biblia)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(livn = livros_nome_aux[[liv]], .before = cap) %>%
    dplyr::filter(alterar)
}

#' Corrige um determinado capitulo de um determinado livro, atualizando
#' o numero de versiculos, retirando ou adicionando versos.
#'
#' @param livro inteiro correspondente a ordem do livro da biblia
#' @param capitulo inteiro correspondente ao capitulo da biblia
#' @param ver_atual numero de versiculos no arquivo usx, a ser substituido
#' @param ver_novo numero de versiculos para substituir no arquivo usx
#'
#' @return uma \code{lista}
#'
#' @export
corrigir_capitulo_usx <- function(livro, capitulo, ver_atual, ver_novo) {
  livros_nome <- xml2::read_xml("inst/usx/template/metadata.xml") %>%
    xml2::xml_find_all(xpath = "//publications//structure//content") %>%
    xml2::xml_attrs() %>% sapply(function(el) el[["role"]])
  biblia <- xml2::read_xml(paste0("inst/usx/template/release/USX_1/",
                                  livros_nome[livro], ".usx")) %>%
    xml2::as_list()
  styles <- sapply(biblia$usx, function(el) attr(el, "style"))
  for(ind in 1:length(styles)) {
    style <- styles[ind]
    if(style == "c") {
      cap <- as.integer(attr(biblia$usx[[ind]], "number"))
      next
    }
    if(style == "p") {
      ver <- as.integer(attr(biblia$usx[[ind]]$verse, "number"))
      if(cap == capitulo && ver == ver_atual) {
        if(ver_atual > ver_novo) {
          biblia$usx <- biblia$usx[-c((ind-(ver_atual-ver_novo)+1):ind)]
        }
        if(ver_atual < ver_novo) {
          usx_esq <- biblia$usx[1:ind]
          if(is.null(biblia$usx[(ind+1)][[1]])) usx_dir <- NULL
          else usx_dir <- biblia$usx[(ind+1):length(biblia$usx)]
          novos_inds <- c((ind+1):(ver_novo-ver_atual+ind))
          for(novo_ind in novos_inds) {
            usx_esq[novo_ind] <- usx_esq[novo_ind-1]
            attr(usx_esq[novo_ind][[1]]$verse, "number") <-
              as.character(ver_atual+(novo_ind-ind))
          }
          biblia$usx <- c(usx_esq, usx_dir)
          new_names <- stringr::str_replace_all(names(biblia$usx), "^$", "para")
          names(biblia$usx) <- new_names
        }
        return(biblia)
      }
    }
  }
}

#' Faz a correcao de uma versao da biblia, verificando e atualizando
#' todos os capitulos, adicionando ou removendo versiculos no template
#' para adequar a versao
#'
#' @param versao versao da traducao biblica: ACF, ARA, ARC, AS21, JFAA,
#' KJA, KJF, NAA, NBV, NTLH, NVI, NVT ou TB
#'
#' @export
corrigir_usx <- function(versao) {
  atualizar <- necessario_atualizar(versao)
  if(nrow(atualizar) > 0) {
    purrr::walk(1:nrow(atualizar), function(ix) {
      att <- atualizar[ix,]
      usx <- corrigir_capitulo_usx(att$livn, att$cap, att$nver.livros, att$nver.biblia)
      xml2::write_xml(xml2::as_xml_document(usx),
                      paste0("inst/usx/template/release/USX_1/", att$liv, ".usx"))
    })
  }
}
