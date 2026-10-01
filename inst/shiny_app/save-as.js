/* Save Shiny downloads on the browser's computer, including remote R sessions. */
(function () {
  "use strict";

  function notice(message, type) {
    if (window.Shiny) {
      window.Shiny.setInputValue("asu_save_notice", { message: message, type: type },
                                { priority: "event" });
    }
  }

  document.addEventListener("click", async function (event) {
    const link = event.target.closest("a.asu-save-as");
    if (!link || event.defaultPrevented || event.button > 0) return;
    const href = link.getAttribute("href");
    if (!href || href === "#" || link.classList.contains("disabled")) return;
    if (link.getAttribute("aria-busy") === "true") {
      event.preventDefault();
      event.stopImmediatePropagation();
      return;
    }
    if (typeof window.showSaveFilePicker !== "function") {
      notice("Your browser will download this file. To choose a folder each time, enable 'Ask where to save each file' in your browser's download settings.", "message");
      return; // Keep Shiny's normal download link as the browser fallback.
    }

    event.preventDefault();
    event.stopImmediatePropagation();
    link.setAttribute("aria-busy", "true");
    let writable;
    try {
      const name = link.dataset.asuSaveName;
      const extension = name.endsWith(".rds") ? ".rds" : ".log";
      const mime = extension === ".rds" ? "application/octet-stream" : "text/plain";
      // Open synchronously from the click gesture, before requesting the data.
      const handle = await window.showSaveFilePicker({
        suggestedName: name,
        types: [{ description: extension === ".rds" ? "R data file" : "Solver log",
                  accept: { [mime]: [extension] } }]
      });
      const response = await window.fetch(href, { credentials: "same-origin" });
      if (!response.ok || (response.headers.get("content-type") || "").includes("text/html")) {
        throw new Error("The file could not be prepared. Check that data or a solver log is available, then try again.");
      }
      writable = await handle.createWritable();
      if (response.body) {
        await response.body.pipeTo(writable);
      } else {
        await writable.write(await response.blob());
        await writable.close();
      }
      writable = null;
      notice("Saved " + handle.name + ".", "message");
    } catch (error) {
      if (writable) {
        try { await writable.abort(); } catch (_) { /* The stream may already be closed. */ }
      }
      if (error.name !== "AbortError") notice("Save failed: " + error.message, "error");
    } finally {
      link.removeAttribute("aria-busy");
    }
  }, true);
}());
