if (file.exists("R/tract_geography.R")) source("R/tract_geography.R") else {
  asu_load_tract_geography <- getFromNamespace("asu_load_tract_geography", "ASUbuildR")
  asu_join_tract_data <- getFromNamespace("asu_join_tract_data", "ASUbuildR")
}
square <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE)))
shapes <- function(ids) sf::st_sf(GEOID=ids, geometry=sf::st_sfc(rep(list(square), length(ids)), crs=4326))
data <- data.frame(GEOID=c("09001000100", "06001000100"), tract_pop_cur=c(110L, 220L),
                   tract_ASU_emp=c(70L,140L), tract_ASU_unemp=c(7L,14L), tract_ASU_clf=c(77L,154L))
calls <- list()
fetch <- function(state, year) {
  calls[[length(calls)+1L]] <<- c(state, year)
  shapes(if (state == "09") "09001000100" else c("06001000200", "06001000100"))
}
geo <- asu_load_tract_geography(data, 2024, fetch=fetch, log=function(...) NULL)
stopifnot(identical(calls, list(c("09","2021"),c("06","2024"))), nrow(geo)==2L)
geo$continuous <- list(2L,1L)
joined <- asu_join_tract_data(geo, data[2:1,])
stopifnot(identical(joined$tract_pop_cur,c(110L,220L)), identical(joined$continuous,list(2L,1L)),
          identical(sf::st_geometry(joined),sf::st_geometry(geo)), sum(joined$tract_ASU_unemp)==21L)
expect_error <- function(expr, pattern) {
  e <- tryCatch({force(expr); NULL}, error=identity)
  stopifnot(inherits(e,"error"), grepl(pattern,conditionMessage(e)))
}
expect_error(asu_load_tract_geography(data,2024,fetch=function(...) shapes("09001000999"),log=function(...) NULL),
             "1 uploaded tracts have no matching")
expect_error(asu_load_tract_geography(rbind(data,data[1,]),2024),"unique")
expect_error(asu_join_tract_data(geo,data[1,]),"one-to-one")
bad <- data; bad$tract_ASU_unemp[1] <- NA_real_
expect_error(asu_join_tract_data(geo,bad),"Invalid or missing tract_ASU_unemp")
calls <- list()
asu_load_tract_geography(data,2024,new_england_2021=TRUE,fetch=fetch,log=function(...) NULL)
stopifnot(identical(calls,list(c("09","2021"),c("06","2024"))))
modern <- data[1,]; modern$GEOID <- "09110000100"
calls <- list()
asu_load_tract_geography(modern,2024,fetch=function(state,year) {
  stopifnot(year==2024); shapes(modern$GEOID)
}, log=function(...) NULL)
cat("Geography coverage, vintage, join and adjacency alignment tests passed.\n")
