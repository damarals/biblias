con <- DBI::dbConnect(RSQLite::SQLite(), "inst/sql/NAA.sqlite")
biblia <- DBI::dbReadTable(con, "verse")
biblia[15871:15872, 3] <- c(117, 117)
DBI::dbWriteTable(con, "verse", biblia, overwrite = TRUE)
DBI::dbDisconnect(con)

biblia[15871:15872, 3] <- c(117, 117)
