# Optional live smoke test. Launch the dashboard on ASU_TEST_URL first.
# Uses a separate headless Chrome process, not a user's browser/session.
run_upload_e2e <- function() {
  url <- Sys.getenv("ASU_TEST_URL")
  workbook <- Sys.getenv("ASU_NATIONAL_TEST_FILE")
  if (!nzchar(url) || !file.exists(workbook)) {
    cat("SKIP live upload test: set ASU_TEST_URL and ASU_NATIONAL_TEST_FILE.\n")
    return(invisible(NULL))
  }
  b <- chromote::ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  eval <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE)$result$value
  wait <- function(js, seconds=90) {
    start <- Sys.time()
    while (!isTRUE(eval(js))) {
      if (as.numeric(difftime(Sys.time(), start, units="secs")) > seconds)
        stop("Timed out: ", js, "\n", substr(eval("document.body.innerText"),1,2500))
      Sys.sleep(.25)
    }
  }
  largest <- 0L
  messages <- 0L
  disconnects <- 0L
  geometry_requests <- 0L
  b$Network$enable()
  b$Network$webSocketFrameReceived(function(p) {
    if (p$response$opcode == 1) {
      largest <<- max(largest,nchar(enc2utf8(p$response$payloadData),type="bytes"))
      messages <<- messages+1L
    }
  })
  b$Network$webSocketClosed(function(p) disconnects <<- disconnects+1L)
  b$Network$requestWillBeSent(function(p) {
    if (grepl("/dataobj/asu-tracts-", p$request$url, fixed=TRUE))
      geometry_requests <<- geometry_requests+1L
  })
  b$Page$navigate(url)
  wait("!!document.querySelector('input#file') && !!window.Shiny && Shiny.shinyapp.isConnected()")
  root <- b$DOM$getDocument()$root$nodeId
  node <- b$DOM$querySelector(root,"input#file")$nodeId
  b$DOM$setFileInputFiles(files=list(normalizePath(workbook,winslash="/")),nodeId=node)
  wait("(document.querySelector('#upload_status')?.innerText || '').includes('84,414')")
  stopifnot(isTRUE(eval("Shiny.shinyapp.isConnected()")),disconnects==0L)
  cat("Upload:",eval("document.querySelector('#upload_status').innerText"),"\n")
  eval("document.querySelector('#display_state_upload').selectize.setValue('06')")
  wait("(document.querySelector('#selected_state')?.innerText || '').includes('Display FIPS: 06')")
  wait("document.querySelector('#data_preview tbody tr td')?.innerText.startsWith('06')")
  stopifnot(isTRUE(eval("Shiny.shinyapp.isConnected()")),disconnects==0L)
  cat("Preview switched to California; max received WebSocket message bytes=",largest,
      " messages=",messages," disconnects=",disconnects,"\n",sep="")
  invalid <- tempfile(fileext=".xlsx")
  writeLines("Invalid workbook fixture",invalid)
  on.exit(unlink(invalid),add=TRUE)
  b$DOM$setFileInputFiles(files=list(normalizePath(invalid,winslash="/")),nodeId=node)
  wait("(document.querySelector('#upload_status')?.innerText || '').includes('Upload rejected:')")
  stopifnot(isTRUE(eval("Shiny.shinyapp.isConnected()")),
    isTRUE(eval("document.querySelector('#data_preview tbody tr td').innerText.startsWith('06')")))
  geometry <- Sys.getenv("ASU_TEST_GEOMETRY_RDS")
  if (nzchar(geometry) && file.exists(geometry)) {
    node <- b$DOM$querySelector(root,"input#load_data")$nodeId
    b$DOM$setFileInputFiles(files=list(normalizePath(geometry,winslash="/")),nodeId=node)
    wait("document.body.innerText.includes('Your data has been successfully loaded')")
    eval("$('.modal').modal('hide'); [...document.querySelectorAll('a')].find(a=>a.textContent.trim()==='Modify ASU Selections').click()")
    map_ready <- function(state) paste0("(()=>{const m=HTMLWidgets.find('#edit_map')?.getMap(); return !!m && m.isStyleLoaded() && m.isSourceLoaded('tracts-source') && m.querySourceFeatures('tracts-source',{sourceLayer:'tracts'}).length>0 && m.querySourceFeatures('tracts-source',{sourceLayer:'tracts'}).every(f=>String(f.properties.GEOID).startsWith('",state,"'));})()")
    wait(map_ready("01"))
    if (identical(Sys.getenv("ASU_TEST_EXPECT_PMTILES"), "true"))
      stopifnot(isTRUE(eval("HTMLWidgets.find('#edit_map').getMap().getStyle().sources['tracts-source'].url.startsWith('pmtiles://')")))
    eval("document.querySelector('#display_state_edit').selectize.setValue('06')")
    wait(map_ready("06"))
    geoid <- eval("HTMLWidgets.find('#edit_map').getMap().querySourceFeatures('tracts-source',{sourceLayer:'tracts'})[0].properties.GEOID")
    before <- geometry_requests
    source_before <- eval("JSON.stringify(HTMLWidgets.find('#edit_map').getMap().getStyle().sources['tracts-source'])")
    eval(paste0("Shiny.setInputValue('manual_geoid',",jsonlite::toJSON(geoid,auto_unbox=TRUE),
                "); Shiny.setInputValue('select_geoid',1,{priority:'event'})"))
    wait("(document.querySelector('#summary_count')?.innerText || '').includes('1')")
    eval("Shiny.setInputValue('new_asu',7); Shiny.setInputValue('update',1,{priority:'event'})")
    wait(paste0("window._asuTooltipState?.edit_map?.[",jsonlite::toJSON(geoid,auto_unbox=TRUE),"]===7"))
    stopifnot(identical(source_before,
      eval("JSON.stringify(HTMLWidgets.find('#edit_map').getMap().getStyle().sources['tracts-source'])")))
    # Selecting a tract zooms the map; vector tiles may fetch additional ranges
    # for that viewport without replacing or resending the source.
    if (!grepl("pmtiles://", source_before, fixed=TRUE)) stopifnot(geometry_requests==before)
    eval("document.querySelector('#display_state_initial').selectize.setValue('01')")
    wait(map_ready("01"))
    eval("document.querySelector('#display_state_edit').selectize.setValue('06')")
    wait(map_ready("06"))
    stopifnot(isTRUE(eval("Shiny.shinyapp.isConnected()")),disconnects==0L,
      isTRUE(eval(paste0("window._asuTooltipState.edit_map[",jsonlite::toJSON(geoid,auto_unbox=TRUE),"]===7"))))
    cat("Real state maps switched; assignment retained; geometry source unchanged on edit. Max WS bytes=",
        largest," disconnects=",disconnects,"\n",sep="")
  }
  single <- Sys.getenv("ASU_SINGLE_STATE_TEST_FILE")
  if (nzchar(single) && file.exists(single)) {
    eval("[...document.querySelectorAll('a')].find(a=>a.textContent.trim()==='Data Initialization').click()")
    node <- b$DOM$querySelector(root,"input#file")$nodeId
    b$DOM$setFileInputFiles(files=list(normalizePath(single,winslash="/")),nodeId=node)
    wait("(document.querySelector('#upload_status')?.innerText || '').includes('across 1 states')")
    stopifnot(isTRUE(eval("Shiny.shinyapp.isConnected()")),disconnects==0L)
    cat("Single-state replacement passed:",eval("document.querySelector('#upload_status').innerText"),"\n")
  }
}
run_upload_e2e()
