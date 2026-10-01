# Real MapLibre/Shiny hover regression using synthetic, local tract geometry.
# Opens an isolated headless browser and a temporary localhost app, never an
# existing user browser or solver session. No external basemap is required.
# Opt in from the project root with ASU_RUN_TOOLTIP_BROWSER_TESTS=true.
run_tract_tooltip_browser_test <- function(use_tiles = FALSE) {
  root <- normalizePath('.', winslash = '/')
  dashboard <- file.path(root, 'inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd')
  port <- httpuv::randomPort()
  process <- callr::r_bg(function(root, dashboard, port, use_tiles) {
    source(file.path(root, 'R/browser_data.R'), local = TRUE)
    text <- paste(readLines(dashboard, warn = FALSE), collapse = '\n')
    script <- regmatches(text, regexec('(?s)<script>(.*?)</script>', text, perl = TRUE))[[1]][2]
    parsed <- tempfile(fileext = '.R')
    knitr::purl(dashboard, output = parsed, quiet = TRUE)
    expressions <- as.list(parse(parsed))
    unlink(parsed)
    lhs <- function(e) if (is.call(e) && identical(e[[1L]], as.name('<-')))
      paste(deparse(e[[2L]]), collapse = '') else ''
    names <- vapply(expressions, lhs, character(1))
    helper <- expressions[[which(names == 'attach_tract_tooltip')]]
    eval(helper)
    square <- sf::st_polygon(list(matrix(c(-119,39,-118.9,39,-118.9,39.1,-119,39.1,-119,39),
                                          ncol = 2, byrow = TRUE)))
    tracts <- sf::st_sf(GEOID = '32001000100', asunum = 0L, tract_pop_cur = 1000L,
      tract_ASU_clf = 500L, tract_ASU_urate = 6.5, tract_ASU_unemp = 32L,
      geometry = sf::st_sfc(square, crs = 4326))
    options(ASUbuildR.use_pmtiles = use_tiles,
            asu.pmtiles_cache_dir = tempfile('asu-tooltip-browser-tiles-'))
    ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$script(shiny::HTML(script))),
      shiny::actionButton('refresh', 'Replace map'),
      shiny::actionButton('assign', 'Assign ASU 7'),
      mapgl::maplibreOutput('initial_map', height = '600px'))
    server <- function(input, output, session) {
      output$initial_map <- mapgl::renderMaplibre({
        revision <- input$refresh
        source <- asu_map_source(tracts, session, revision)
        if (use_tiles) stopifnot(identical(source$source_layer, 'tracts'))
        blank <- list(version = 8L, sources = list(empty = list(type = 'geojson',
          data = list(type = 'FeatureCollection', features = list()))), layers = list())
        map <- mapgl::maplibre(style = blank,
                              center = c(-118.95, 39.05), zoom = 11)
        map$x$sources <- list(source$source,
          list(id = 'pending-background', type = 'geojson', data = '/asu-tooltip-pending'))
        map <- mapgl::add_fill_layer(map, id = 'basemap', source = 'tracts-source',
          source_layer = source$source_layer, fill_color = '#3399ff', tooltip = NULL)
        attach_tract_tooltip(map)
      })
      shiny::observeEvent(input$assign, {
        session$sendCustomMessage('asu-tooltip-state', list(id = 'initial_map',
          updates = list(list(geoid = '32001000100', asunum = 7L))))
      })
    }
    shiny::runApp(shiny::shinyApp(ui, server), host = '127.0.0.1', port = port,
                  launch.browser = FALSE, quiet = TRUE)
  }, args = list(root, dashboard, port, use_tiles), supervise = TRUE)
  on.exit(process$kill(), add = TRUE)
  browser <- chromote::Chromote$new(browser = chromote::Chrome$new(args = c(
    setdiff(chromote::default_chrome_args(), '--disable-gpu'),
    '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader')))
  on.exit(browser$close(), add = TRUE)
  b <- chromote::ChromoteSession$new(parent = browser)
  on.exit(b$close(), add = TRUE, after = FALSE)
  errors <- character()
  b$Runtime$enable()
  b$Runtime$exceptionThrown(function(p) {
    details <- p$exceptionDetails
    # Shiny can throw a string instead of an Error object.
    errors <<- c(errors, details$exception$description, details$exception$value, details$text)
  })
  # Hold an unrelated source request open. The tract layer can be visible
  # while isStyleLoaded() remains false, as with slow basemap/tile resources.
  b$Fetch$requestPaused(function(p) {
    if (!grepl('/asu-tooltip-pending$', p$request$url))
      b$Fetch$continueRequest(requestId = p$requestId, wait_ = FALSE)
    invisible(NULL)
  })
  # Register first: Chromote automatically enables an event's domain, which
  # otherwise overwrites an earlier Fetch filter and pauses the whole page.
  b$Fetch$enable(patterns = list(list(urlPattern = '*asu-tooltip-pending*')))
  js <- function(code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
  wait <- function(code, seconds = 30) {
    start <- Sys.time()
    while (!isTRUE(js(code))) {
      if (!process$is_alive() || as.numeric(difftime(Sys.time(), start, units = 'secs')) > seconds)
        stop('Browser condition failed: ', code, '\n', paste(errors, collapse = '\n'),
             '\n', paste(process$read_error_lines(), collapse = '\n'),
             '\nBrowser state: ', jsonlite::toJSON(js("({shiny:!!window.Shiny, widgets:!!window.HTMLWidgets, popup:!!window.maplibregl?.Popup, ensure:typeof window._asuEnsureTooltip, installed:!!window.HTMLWidgets?.find('#initial_map')?.getMap()?._asuDynamicTooltipInstalled, body:document.body.innerText.slice(0,500)})"), auto_unbox = TRUE))
      Sys.sleep(.1)
    }
  }
  # Wait for the server before navigating; no browser connection refusal race.
  start <- Sys.time()
  repeat {
    ready <- tryCatch({
      connection <- suppressWarnings(socketConnection('127.0.0.1', port, open = 'r+', timeout = 1))
      close(connection); TRUE
    }, error = function(e) FALSE)
    if (ready) break
    if (!process$is_alive() || difftime(Sys.time(), start, units = 'secs') > 30)
      stop(paste(process$read_error_lines(), collapse = '\n'))
    Sys.sleep(.1)
  }
  b$Page$navigate(paste0('http://127.0.0.1:', port), wait_ = FALSE)
  wait("!!window.HTMLWidgets?.find('#initial_map')?.getMap()?.getLayer('basemap')")
  wait("HTMLWidgets.find('#initial_map').getMap().isSourceLoaded('tracts-source')")
  stopifnot(!isTRUE(js("HTMLWidgets.find('#initial_map').getMap().isStyleLoaded()")))
  hover <- function() {
    location <- js("(()=>{const m=HTMLWidgets.find('#initial_map').getMap(); const p=m.project([-118.95,39.05]); const r=m.getCanvas().getBoundingClientRect(); return {x:r.left+p.x,y:r.top+p.y};})()")
    b$Input$dispatchMouseEvent(type = 'mouseMoved', x = 1, y = 1)
    b$Input$dispatchMouseEvent(type = 'mouseMoved', x = location$x, y = location$y)
  }
  wait("!!HTMLWidgets.find('#initial_map').getMap()._asuDynamicTooltipInstalled")
  hover()
  wait("!!document.querySelector('.maplibregl-popup-content')")
  stopifnot(isTRUE(js("(()=>{const p=document.querySelector('.maplibregl-popup'); const r=p.getBoundingClientRect(); const s=getComputedStyle(p); return r.width>0 && r.height>0 && s.display!=='none' && s.visibility!=='hidden';})()")))
  popup <- js("document.querySelector('.maplibregl-popup-content').innerText")
  stopifnot(all(vapply(c('ASU Number: 0', '32001000100', '1,000', '500', '6.5%', '32'),
                        grepl, logical(1), x = popup, fixed = TRUE)))
  js("document.querySelector('#assign').click()")
  wait("window._asuTooltipState.initial_map['32001000100']===7")
  hover()
  wait("document.querySelector('.maplibregl-popup-content')?.innerText.includes('ASU Number: 7')")
  js("window.oldTooltipMap=HTMLWidgets.find('#initial_map').getMap(); document.querySelector('#refresh').click()")
  wait("HTMLWidgets.find('#initial_map').getMap()!==window.oldTooltipMap && !!HTMLWidgets.find('#initial_map').getMap()._asuDynamicTooltipInstalled")
  wait("HTMLWidgets.find('#initial_map').getMap().isSourceLoaded('tracts-source')")
  hover()
  wait("document.querySelector('.maplibregl-popup-content')?.innerText.includes('ASU Number: 7')")
  b$Input$dispatchMouseEvent(type = 'mouseMoved', x = 1, y = 1)
  wait("!document.querySelector('.maplibregl-popup-content') && HTMLWidgets.find('#initial_map').getMap().getCanvas().style.cursor===''")
  stopifnot(length(errors) == 0L)
  cat(if (use_tiles) 'PMTiles:' else 'GeoJSON:',
      'real browser initial-map hover, assignment, replacement, and mouseleave checks passed.\n')
}
if (identical(Sys.getenv('ASU_RUN_TOOLTIP_BROWSER_TESTS'), 'true')) {
  stopifnot(requireNamespace('chromote', quietly = TRUE),
            requireNamespace('callr', quietly = TRUE))
  run_tract_tooltip_browser_test()
  if (requireNamespace('freestiler', quietly = TRUE)) {
    run_tract_tooltip_browser_test(use_tiles = TRUE)
  } else {
    cat('SKIP PMTiles browser path: freestiler is not installed.\n')
  }
} else {
  cat('SKIP live tooltip test: set ASU_RUN_TOOLTIP_BROWSER_TESTS=true.\n')
}
