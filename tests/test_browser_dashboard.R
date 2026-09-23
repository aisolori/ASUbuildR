# Exercise production reactive definitions with state switches and assignment edits.
if (file.exists("R/browser_data.R")) {
  source("R/browser_data.R")
} else {
  for (nm in c("asu_state_slice", "asu_display_geometry", "asu_display_bounds", "asu_map_source"))
    assign(nm, getFromNamespace(nm, "ASUbuildR"))
}
run_browser_dashboard_tests <- function() {
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  dashboard <- "inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd"
  if (!file.exists(dashboard)) dashboard <- system.file("shiny_app", "ASU_Flexdashboard_mapgl.Rmd", package = "ASUbuildR")
  knitr::purl(dashboard, output = path, quiet = TRUE)
  expressions <- as.list(parse(path))
  lhs <- function(e) if (is.call(e) && identical(e[[1L]], as.name("<-")))
    paste(deparse(e[[2L]]), collapse = "") else ""
  names <- vapply(expressions, lhs, character(1))
  first <- which(names == "available_display_states")
  last <- which(names == "map_source")
  helpers <- expressions[names %in% c("ASU_COLORS", "asu_palette", "calculate_tract_colors")]
  renders <- expressions[names %in% c("output$initial_map", "output$edit_map")]
  stopifnot(length(first) == 1, length(last) == 1, length(renders) == 2)
  square <- function(x) sf::st_polygon(list(matrix(c(x,0,x+.01,0,x+.01,.01,x,.01,x,0),ncol=2,byrow=TRUE)))
  fixture <- sf::st_sf(GEOID=c("01001000100","06001000100"),asunum=c(0L,2L),
    tract_pop_cur=c(100L,200L),tract_ASU_clf=c(70L,140L),tract_ASU_urate=c(10,10),
    tract_ASU_unemp=c(7L,14L),geometry=sf::st_sfc(square(-87),square(-121),crs=4326))
  shiny::testServer(function(input,output,session) {
    # MockShinySession returns one placeholder URL for every data object.
    session$registerDataObj <- function(name, data, filterFunc) paste0("session/test/dataobj/", name)
    geometry_builds <- new.env(parent=emptyenv())
    geometry_builds$n <- 0L
    simplify <- asu_display_geometry
    asu_display_geometry <- function(data) {
      geometry_builds$n <- geometry_builds$n + 1L
      simplify(data)
    }
    full_data <- shiny::reactiveVal(fixture)
    uploaded_data <- shiny::reactiveVal(sf::st_drop_geometry(fixture))
    display_state <- shiny::reactiveVal("01")
    selected_tracts <- shiny::reactiveVal(NULL)
    highlighted_tracts <- shiny::reactiveVal(character(0))
    for (e in c(expressions[first:last], helpers, renders)) eval(e, envir=environment())
  }, {
    session$flushReact()
    revision <- map_revision()
    stopifnot(identical(display_data()$GEOID,fixture$GEOID[1]), nrow(full_data())==2L)
    initial <- jsonlite::fromJSON(output$initial_map,simplifyVector=FALSE)
    stopifnot(length(initial$x$sources)==1L,
              !grepl("FeatureCollection",output$initial_map),
              all(vapply(initial$x$layers,function(x) identical(x$source,"tracts-source"),logical(1))))
    stopifnot("ALL" %in% available_display_states())
    first_source <- map_source()$source
    edited <- fixture; edited$asunum[1] <- 3L
    full_data(edited); session$flushReact()
    stopifnot(map_revision()==revision, identical(map_source()$source,first_source),map_geom()$asunum==3L,
              identical(sf::st_geometry(full_data()),sf::st_geometry(fixture)))
    session$setInputs(display_state_edit="06")
    stopifnot(display_state()=="06",identical(display_data()$GEOID,fixture$GEOID[2]),nrow(full_data())==2L,
              map_revision()>revision,!identical(map_source()$source,first_source))
    session$setInputs(display_state_initial="01")
    stopifnot(display_state()=="01",map_geom()$asunum==3L)
    if (isTRUE(getOption("ASUbuildR.use_pmtiles", TRUE)) && requireNamespace("freestiler", quietly=TRUE)) {
      session$setInputs(display_state_edit="ALL")
      stopifnot(display_state()=="ALL",nrow(display_data())==2L,nrow(map_geom())==2L,
                identical(map_source()$source_layer,"tracts"))
      national_source <- map_source()$source
      national_revision <- map_revision()
      builds_before_switch <- geometry_builds$n
      edited <- full_data(); edited$asunum[2] <- 4L; full_data(edited); session$flushReact()
      stopifnot(map_revision()==national_revision,identical(map_source()$source,national_source),
                identical(map_geom()$asunum,c(3L,4L)))
      session$setInputs(display_state_initial="06")
      stopifnot(nrow(display_data())==1L,map_geom()$asunum==4L)
      session$setInputs(display_state_upload="ALL")
      stopifnot(nrow(display_data())==2L,identical(map_geom()$asunum,c(3L,4L)))
      stopifnot(geometry_builds$n==builds_before_switch+1L)
      session$setInputs(display_state_edit="01")
    }
    # Same GEOIDs with revised geometry must create a new map/source.
    revision <- map_revision()
    moved <- full_data(); sf::st_geometry(moved)[[1]] <- square(-86)
    full_data(moved); session$flushReact()
    stopifnot(map_revision()>revision)
    session$setInputs(display_state_upload="99")
    stopifnot(display_state()=="01")
  })
  cat("Dashboard state switching, source reuse, assignment and geometry tests passed.\n")
}
run_browser_dashboard_tests()
