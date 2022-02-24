#' Faz a traducao (conversao) dos versos contidos em um
#' livro especifico em JSON para o formato .usx
#' dado pelo template
#'
#' @param traduzir objeto usx lido pelo pacote xml2
#' @param tradutor lista contendo o livro da biblia,
#' lido pelo pacote jsonlite
#' @param traducao sigla da versao da biblia utilizada
#'
#' @return uma \code{lista}
#'
#' @export
traduzir_livro <- function(traduzir, tradutor, traducao) {
  styles <- sapply(traduzir$usx, function(el) attr(el, "style"))
  for(ind in 1:length(styles)) {
    style <- styles[ind]
    if(style == "id") {
      traduzir$usx[[ind]][[1]] <- paste0(tradutor$name, " (", traducao, ")")
      next
    }
    if(style %in% c("h", "toc1", "toc2", "mt1")) {
      traduzir$usx[[ind]][[1]] <- tradutor$name
      next
    }
    if(style == "toc3") {
      traduzir$usx[[ind]][[1]] <- stringr::str_replace(tradutor$abbrev,
                                                       "(?<=\\d|)[a-z]", toupper)
      next
    }
    if(style == "c") {
      cap <- as.integer(attr(traduzir$usx[[ind]], "number"))
      next
    }
    if(style == "p") {
      ver <- as.integer(attr(traduzir$usx[[ind]]$verse, "number"))
      traduzir$usx[[ind]][[2]] <- tradutor$chapters[[cap]][[ver]]
    }
  }
  return(xml2::as_xml_document(traduzir))
}

#' Faz a traducao (conversao) dos versos contidos em um JSON
#' para o formato .usx dado pelo template
#'
#' @export
traduzir_biblias <- function() {
  livros_nome <- xml2::read_xml("inst/usx/template/metadata.xml") %>%
    xml2::xml_find_all(xpath = "//publications//structure//content") %>%
    xml2::xml_attrs() %>% sapply(function(el) el[["role"]])
  traducoes <- c(#"ACF", "ARA", "ARC", "AS21", "JFAA", "KJA", "KJF",
                 #"NAA", "NBV", "NTLH", "NVI", "NVT",
    "TB")
  sb <- cli::cli_status(paste0("{cli::symbol$arrow_right} Traduzindo ",
                               "{.pkg {length(traducoes)}} B\u00edblias em ",
                               "formato {.emph JSON} para {.emph USX}, ",
                               "utilizando o template em '/inst/usx/template'."))
  purrr::iwalk(traducoes, function(trad, ix) {
    suppressWarnings(dir.create(paste0("inst/usx/", trad)))
    purrr::iwalk(livros_nome, function(livro, indice) {
      bib <- ler_biblia(trad)
      usx <- xml2::read_xml(paste0("inst/usx/template/", livro, ".usx")) %>%
        xml2::as_list()
      corrigir_usx(trad)
      usx_trad <- traduzir_livro(usx, bib[[indice]], trad)
      xml2::write_xml(usx_trad, paste0("inst/usx/", trad, "/", livro, ".usx"))
      msg <- paste0("{cli::symbol$arrow_right} Vers\u00e3o: {.pkg {trad}} ",
                    "Livro: {.pkg {livro}} traduzido, ",
                    "restando {.pkg {length(livros_nome)-indice}} livros ",
                    "e {.pkg {length(traducoes)-ix}} vers\u00f5es")
      cli::cli_status_update(sb, msg)
    })
  })
  cli::cli_status_clear(id = sb)
  cli::cli_alert_success("Tradu\u00e7\u00f5es finalizadas.")
}

#' Faz a compressao em .zip de todos os arquivos .usx
#' de todas biblias
#'
#' @export
zipar_biblias <- function() {
  traducoes <- c("ACF", "ARA", "ARC", "AS21", "JFAA", "KJA", "KJF",
                 "NAA", "NBV", "NTLH", "NVI", "NVT", "TB")
  purrr::walk(traducoes, function(traducao) {
    usx_path <- Sys.glob(paths = paste0("inst/usx/", traducao, "/*.usx"))
    zip::zip(zipfile = paste0("inst/usx/", traducao, ".zip"),
             files = usx_path, mode = 'cherry-pick')
  })
}
