#' Faz o download das biblias em formato SQLite neste repositório
#'
#' @export
baixar_biblias <- function() {
  u <- 'http://altamiro.comunidades.net/biblias'
  nodes <- rvest::read_html(u) %>%
    rvest::html_elements(xpath = "//font//td//a")
  sb <- cli::cli_status(paste0("{cli::symbol$arrow_right} Fazendo Download ",
                               "de {.pkg {length(nodes)}} B\u00edblias."))
  purrr::iwalk(nodes, function(node, ix) {
      title <- rvest::html_attr(node, "title")
      url <- rvest::html_attr(node, "href")
      utils::download.file(url, "inst/sql/temp.zip",
                           quiet = TRUE, mode = "wb")
      utils::unzip("inst/sql/temp.zip", exdir = "inst/sql")
      unlink("inst/sql/temp.zip")
      msg <- paste0("{cli::symbol$arrow_right} Download da B\u00edblia ",
                   "{.pkg {title}} conclu\u00eddo, ",
                   "restando {.pkg {length(nodes)-ix}}")
      cli::cli_status_update(sb, msg)
    })
  cli::cli_status_clear(id = sb)
  cli::cli_alert_success("Downloads finalizados.")
}

#' Converte uma versao da biblia no formato SQLite para JSON
#'
#' @param versao versao da traducao biblica: ACF, ARA, ARC, AS21, JFAA,
#' KJA, KJF, NAA, NBV, NTLH, NVI, NVT ou TB
#'
#' @export
biblia_sql2json <- function(versao) {
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0("inst/sql/", versao, ".sqlite"))
  biblia <- DBI::dbReadTable(con, "verse")
  DBI::dbDisconnect(con)

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

  books <- biblia %>%
    dplyr::distinct(book_id) %>%
    dplyr::arrange(book_id) %>%
    dplyr::pull(book_id)
  biblia_json <- purrr::map(books, function(book) {
    caps <- biblia %>%
      dplyr::filter(book_id == book) %>%
      dplyr::distinct(chapter) %>%
      dplyr::arrange(chapter) %>%
      dplyr::pull(chapter)
    chapters <- purrr::map(caps, function(cap) {
      verses <- biblia %>%
        dplyr::filter(book_id == book, chapter == cap) %>%
        dplyr::distinct(verse) %>%
        dplyr::arrange(verse) %>%
        dplyr::pull(verse)
      purrr::map(verses, function(ver) {
        biblia %>%
          dplyr::filter(book_id == book, chapter == cap, verse == ver) %>%
          dplyr::pull(text)
      })
    })
    list(abbrev = abreviacoes[book], chapters = chapters, name = livros[book])
  })

  jsonlite::write_json(biblia_json, paste0("inst/json/", versao, ".json"),
                       auto_unbox = TRUE)
}

#' Converte todas as versoes das biblias no formato SQLite para JSON
#'
#' @export
conversao_biblias <- function() {
  versoes <- c("ACF", "ARA", "ARC", "AS21", "JFAA", "KJA", "KJF",
               "NAA", "NBV", "NTLH", "NVI", "NVT", "TB")
  sb <- cli::cli_status(paste0("{cli::symbol$arrow_right} Convertendo ",
                               "{.pkg {length(versoes)}} B\u00edblias em ",
                               "formato {.emph SQLite} para {.emph JSON}."))
  purrr::iwalk(versoes, function(versao, ix) {
    biblia_sql2json(versao)
    msg <- paste0("{cli::symbol$arrow_right} B\u00edblia ",
                  "{.pkg {versao}} convertida, ",
                  "restando {.pkg {length(versoes)-ix}}")
    cli::cli_status_update(sb, msg)
  })
  cli::cli_status_clear(id = sb)
  cli::cli_alert_success("Convers\u00f5es finalizadas.")
}

#' Leitura da Biblia em formato JSON como uma lista aninhada
#'
#' @param versao versao da traducao biblica: ACF, ARA, ARC, AS21, JFAA,
#' KJA, KJF, NBV, NTLH, NVI, NVT ou TB
#'
#' @return uma \code{lista}
#'
#' @export
ler_biblia <- function(versao) {
  jsonlite::read_json(paste0("inst/json/", versao, ".json"))
}
