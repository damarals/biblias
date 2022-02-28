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
  nome_pastas <- c("135c3fbc-0a74-48ae-9615-7385fa389ceb", "2e711cc2-e43c-4f40-a8b3-cbefbcb8d159",
                   "3ce6eccc-2cfd-4762-a574-1445ef95b84b", "519fdf5f-7768-4f84-8af6-0b68ba709d24",
                   "62092417-5f69-44cf-ad0f-f9796e97c0da", "73a3e50d-a50a-4db6-a5f8-be1a70c56998",
                   "87f40c9a-2696-417a-aa63-8a90b1f11655", "b5b81f73-6e09-4e32-a62f-9e7508241e8a",
                   "baa11128-c17f-4e52-bce0-4b14ff6c659d", "c6b99c20-ea9c-426b-a3f0-4f823318c54b",
                   "c6d682bc-725e-40bc-993e-78cf3b2744d1", "e803cc0c-9f47-4c47-a8f7-4dc384d74e4d",
                   "eadf34dd-f4ee-4b3b-924a-7ee72a936f71")
  siglas_original <- c("WBP", "CBT", "MAL10RO", "BB", "BOB", "MG1865", "ALBB",
                       "HUNK", "BJB", "AVB", "BPB", "SRP1865", "NMV")
  traducoes <- c("ACF", "ARA", "ARC", "AS21", "JFAA", "KJA", "KJF",
                 "NAA", "NBV", "NTLH", "NVI", "NVT", "TB")
  nomes <- c("Almeida Corrigida e Fiel", "Almeida Revista e Atualizada",
             "Almeida Revista e Corrigida", "Almeida S\u00e9culo XXI",
             "Almeida Atualizada", "King James Atualizada", "King James Fiel",
             "Nova Almeida Atualizada", "Nova B\u00edblia Viva",
             "Nova Tradu\u00e7\u00e3o na Linguagem de Hoje",
             "Nova Vers\u00e3o Internacional", "Nova Vers\u00e3o Transformadora",
             "Tradu\u00e7\u00e3o Brasileira")
  sb <- cli::cli_status(paste0("{cli::symbol$arrow_right} Traduzindo ",
                               "{.pkg {length(traducoes)}} B\u00edblias em ",
                               "formato {.emph JSON} para {.emph USX}, ",
                               "utilizando o template em '/inst/usx/template'."))
  purrr::iwalk(traducoes, function(trad, ix) {
    invisible(dir.create(paste0("inst/usx/traducao/", trad, "/", nome_pastas[ix]),
                         recursive = TRUE))
    path_template <- list.files("inst/usx/template/", full.names = TRUE) %>%
      stringr::str_subset("BibleData", negate = TRUE)
    file.copy(path_template, paste0("inst/usx/traducao/",
                                    trad, "/", nome_pastas[ix]), recursive = TRUE)
    metadata <- traduzir_metadata()
    xml2::write_xml(metadata, paste0("inst/usx/traducao/", trad, "/",
                                     nome_pastas[ix], "/metadata.xml"))
    rvmetadata <- traduzir_rvmetadata(nomes[ix], trad, siglas_original[ix])
    xml2::write_xml(rvmetadata, paste0("inst/usx/traducao/", trad, "/",
                                       nome_pastas[ix], "/rvmetadata.xml"))
    purrr::iwalk(livros_nome, function(livro, indice) {
      bib <- ler_biblia(trad)
      usx <- xml2::read_xml(paste0("inst/usx/template/release/USX_1/",
                                   livro, ".usx")) %>%
        xml2::as_list()
      corrigir_usx(trad)
      usx_trad <- traduzir_livro(usx, bib[[indice]], trad)
      xml2::write_xml(usx_trad, paste0("inst/usx/traducao/", trad, "/", nome_pastas[ix],
                                       "/release/USX_1/", livro, ".usx"))
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

#' Faz a traducao do arquivo metadata com informacoes da versao
#' da biblia utilizada
#'
#' @return uma \code{lista}
#'
#' @export
traduzir_metadata <- function() {
  abreviacoes <- c("Gn", "\u00cax", "Lv", "Nm", "Dt", "Js", "Jz", "Rt", "1Sm", "2Sm",
                   "1Rs", "2Rs", "1Cr", "2Cr", "Ed", "Ne", "Et", "J\u00f3", "Sl",
                   "Pv", "Ec", "Ct", "Is", "Jr", "Lm", "Ez", "Dn", "Os", "Jl",
                   "Am", "Ob", "Jn", "Mq", "Na", "Hc", "Sf", "Ag", "Zc", "Ml",
                   "Mt", "Mc", "Lc", "Jo", "At", "Rm", "1Co", "2Co", "Gl", "Ef",
                   "Fp", "Cl", "1Ts", "2Ts", "1Tn", "2Tm", "Tt", "Fm", "Hb",
                   "Tg", "1Pe", "2Pe", "1Jo", "2Jo", "3Jo", "Jd", "Ap")
  livros <- c("G\u00eanesis", "\u00caxodo", "Lev\u00edtico", "N\u00fameros",
              "Deuteron\u00f4mio", "Josu\u00e9", "Ju\u00edzes", "Rute", "1 Samuel",
              "2 Samuel", "1 Reis", "2 Reis", "1 Cr\u00f4nicas", "2 Cr\u00f4nicas",
              "Esdras", "Neemias", "Ester", "J\u00f3", "Salmos", "Prov\u00e9rbios",
              "Eclesiastes", "C\u00e2nticos", "Isa\u00edas", "Jeremias",
              "Lamenta\u00e7\u00f5es de Jeremias", "Ezequiel", "Daniel", "Os\u00e9ias",
              "Joel", "Am\u00f3s", "Obadias", "Jonas", "Miqu\u00e9ias", "Naum",
              "Habacuque", "Sofonias", "Ageu", "Zacarias", "Malaquias", "Mateus",
              "Marcos", "Lucas", "Jo\u00e3o", "Atos", "Romanos", "1 Cor\u00edntios",
              "2 Cor\u00edntios", "G\u00e1latas", "Ef\u00e9sios", "Filipenses",
              "Colossenses", "1 Tessalonicenses", "2 Tessalonicenses", "1 Tim\u00f3teo",
              "2 Tim\u00f3teo", "Tito", "Filemom", "Hebreus", "Tiago", "1 Pedro",
              "2 Pedro", "1 Jo\u00e3o", "2 Jo\u00e3o", "3 Jo\u00e3o", "Judas", "Apocalipse")
  metadata <- xml2::read_xml("inst/usx/template/metadata.xml") %>%
    xml2::as_list()
  attr(metadata$DBLMetadata, "id") <- gerar_id_biblia()
  for(ix in 1:66) {
    metadata$DBLMetadata$names[[ix]]$abbr[[1]] <- abreviacoes[ix]
    metadata$DBLMetadata$names[[ix]]$short[[1]] <- livros[ix]
    metadata$DBLMetadata$names[[ix]]$long[[1]] <- livros[ix]
  }
  return(xml2::as_xml_document(metadata))
}

#' Faz a traducao do arquivo rvmetadata com informacoes da versao
#' da biblia utilizada
#'
#' @param nome nome completo da versao da biblia utilizada
#' @param abbr abreviatura da versao da biblia utilizada
#' @param abbr_original abreviatura da versao da biblia do propresenter
#' utilizada como template
#'
#' @return uma \code{lista}
#'
#' @export
traduzir_rvmetadata <- function(nome, abbr, abbr_original) {
  rvmetadata <- xml2::read_xml("inst/usx/template/rvmetadata.xml") %>%
    xml2::as_list()
  rvmetadata$RVBibleMetdata$name[[1]] <- nome
  rvmetadata$RVBibleMetdata$displayAbbreviation[[1]] <- abbr
  rvmetadata$RVBibleMetdata$abbreviation[[1]] <- abbr_original
  return(xml2::as_xml_document(rvmetadata))
}

#' Gera um id aleatorio para uma versao qualquer da biblia
#' para ser colocada no arquivo de metadados da versao
#'
#' @export
gerar_id_biblia <- function() {
  paste0(sample(c(letters, 0:9), 16), collapse = "")
}

#' Faz a compressao em .zip de todos os arquivos .usx
#' de todas biblias
#'
#' @export
zipar_biblias <- function() {
  traducoes <- c("ACF", "ARA", "ARC", "AS21", "JFAA", "KJA", "KJF",
                 "NAA", "NBV", "NTLH", "NVI", "NVT", "TB")
  pastas <- paste0("inst/usx/traducao/", traducoes, "/*") %>%
    purrr::map_chr(Sys.glob)
  zip::zip(zipfile = paste0("inst/usx/traducao/biblias-propresenter.zip"),
           files = c(pastas, "inst/usx/template/BibleData.proPref"),
           mode = 'cherry-pick')
  purrr::walk(traducoes, function(traducao) {
    usx_path <- Sys.glob(paths = paste0("inst/usx/traducao/", traducao, "/*"))
    zip::zip(zipfile = paste0("inst/usx/traducao/", traducao, ".zip"),
             files = usx_path, mode = 'cherry-pick')
  })
}
