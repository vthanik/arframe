// arframe -- the Galley bridge (v1). Mode switching, focus routing, disabled
// sync, and the report-title click-to-edit flip. Sortable + region-click
// handlers are appended in Tasks 7/9; card pin in Task 10. See
// docs/superpowers/specs/2026-07-02-arframe-galley-design-system.md #3.

$(document).on("click", "[data-ar-mode]", function () {
  Shiny.setInputValue("frame-mode", this.getAttribute("data-ar-mode"), {
    priority: "event",
  });
});

Shiny.addCustomMessageHandler("ar-mode", function (m) {
  var ws = document.querySelector(".ar-workspace");
  if (!ws) return;
  // Swap ONLY the mode class -- a wholesale className reset would wipe the
  // v5 collapse classes (ar-rail-collapsed / ar-insp-collapsed) on every
  // mode switch.
  ws.className = ws.className.replace(/\bar-mode-[a-z]+\b/, "ar-mode-" + m);
});

// v5 panel collapse (decision #8): any element carrying data-ar-collapse
// ("rail" | "insp") posts to the frame; the store flips, and ar-collapse
// mirrors both booleans back as workspace classes for the CSS.
$(document).on("click", "[data-ar-collapse]", function () {
  Shiny.setInputValue(
    "frame-collapse",
    this.getAttribute("data-ar-collapse"),
    { priority: "event" }
  );
});

Shiny.addCustomMessageHandler("ar-collapse", function (m) {
  var ws = document.querySelector(".ar-workspace");
  if (!ws) return;
  ws.classList.toggle("ar-rail-collapsed", !!m.rail);
  ws.classList.toggle("ar-insp-collapsed", !!m.insp);
});

Shiny.addCustomMessageHandler("ar-focus", function (m) {
  var el = document.getElementById(m.id);
  if (el) {
    el.scrollIntoView({ block: "nearest" });
    el.focus();
  }
});

Shiny.addCustomMessageHandler("ar-disable", function (m) {
  var el = document.getElementById(m.id);
  if (el) el.toggleAttribute("disabled", !!m.disabled);
});

// The report title: click the wrapper to flip into edit mode (CSS shows the
// text input, hides the static span); Enter or blur flips back and lets the
// input's native change event (which Shiny's text-input binding already
// listens for) carry the value into input$name. The input's value is copied
// from the display span on every open (not synced server-side) so it always
// starts from what is currently rendered, with zero reactive round-trip.
$(document).on("click", ".ar-title-wrap", function (e) {
  if ($(this).hasClass("ar-title-editing")) return;
  var $input = $(this).find("input[type=text]");
  $input.val($(this).find(".ar-title").text());
  $(this).addClass("ar-title-editing");
  $input.trigger("focus").trigger("select");
});

$(document).on("keydown", ".ar-title-wrap input[type=text]", function (e) {
  if (e.key === "Enter") this.blur();
});

$(document).on("blur", ".ar-title-wrap input[type=text]", function () {
  $(this).closest(".ar-title-wrap").removeClass("ar-title-editing");
});

// The SortableJS bridge (Task 7): any container marked `data-ar-sortable`
// gets a Sortable instance. On drag end, the container posts its current
// item order (read off `data-ar-sortable-attr` on each item) to the input
// named by `data-ar-sortable-input`, plus any extra JSON payload from
// `data-ar-sortable-extra`. Re-run on every Shiny render (a fresh renderUI
// replaces the DOM nodes, so a stale Sortable instance would still be bound
// to detached elements) -- `_arSortable` guards against double-binding the
// same live element twice within one render.
function arInitSortables() {
  document.querySelectorAll("[data-ar-sortable]").forEach(function (el) {
    if (el._arSortable) return;
    el._arSortable = new Sortable(el, {
      animation: 150,
      handle: el.getAttribute("data-ar-sortable-handle") || undefined,
      draggable: el.getAttribute("data-ar-sortable-item"),
      ghostClass: "ar-sortable-ghost",
      chosenClass: "ar-sortable-chosen",
      dragClass: "ar-sortable-drag",
      // `document.body.dataset.arDragging` is a cheap "a drag is physically
      // in progress" flag, set here and cleared in onEnd below. Nothing
      // reads it today -- no re-render suppression is wired up, because
      // nothing in the app can currently commit to a store's `rv$report`
      // concurrently with a drag (see the renderUI comment in
      // mod_contents.R). It is left here so the first concurrent-mutator
      // task (Task 9/10) has a ready-made signal to defer its re-render on,
      // instead of having to rediscover the need for one.
      onStart: function () {
        document.body.dataset.arDragging = "true";
      },
      onEnd: function () {
        delete document.body.dataset.arDragging;
        var attr = el.getAttribute("data-ar-sortable-attr");
        var order = Array.prototype.map.call(
          el.querySelectorAll(el.getAttribute("data-ar-sortable-item")),
          function (it) {
            return it.getAttribute(attr);
          }
        );
        var extra = el.getAttribute("data-ar-sortable-extra");
        var payload = { order: order, nonce: Date.now() };
        if (extra) Object.assign(payload, JSON.parse(extra));
        Shiny.setInputValue(el.getAttribute("data-ar-sortable-input"), payload, {
          priority: "event",
        });
      },
    });
  });
}
$(document).on("shiny:value shiny:idle", function () {
  setTimeout(arInitSortables, 50);
});
document.addEventListener("DOMContentLoaded", arInitSortables);

// The kebab popover portal: `.ar-toc` scrolls (`overflow-y: auto`), which
// clips an absolutely-positioned popover opened on a row near the bottom of
// a scrolled list regardless of z-index. The three popovers
// (`.ar-pop-menu`, `.ar-pop-rename`, `.ar-pop-remove`) are `position: fixed`
// in CSS -- relative to the viewport, so no scrollable ancestor can clip
// them -- and this MutationObserver computes their on-screen position from
// the trigger's own `getBoundingClientRect()` whenever mod_contents.R's
// inline onclick handlers toggle one of the three `-open` classes onto its
// `.ar-toc-kebab-wrap`. Watching the class attribute (rather than adding a
// second click handler) needs no change to those onclick strings and reacts
// identically whichever of the three popovers just opened. Right-aligned to
// the wrap's right edge (matching the old `right: 0` layout); flips to open
// upward, above the trigger, when there is not enough room below it in the
// viewport.
function arPositionPopover(wrap) {
  // Which of the three popovers is open is read straight off which `-open`
  // class `wrap` currently carries -- at most one is ever set at a time
  // (every onclick that adds one first removes the other two).
  var active = null;
  if (wrap.classList.contains("ar-pop-menu-open")) {
    active = wrap.querySelector(".ar-pop-menu");
  } else if (wrap.classList.contains("ar-pop-rename-open")) {
    active = wrap.querySelector(".ar-pop-rename");
  } else if (wrap.classList.contains("ar-pop-remove-open")) {
    active = wrap.querySelector(".ar-pop-remove");
  }
  if (!active) return;
  var rect = wrap.getBoundingClientRect();
  var margin = 4;
  active.style.top = "";
  active.style.bottom = "";
  active.style.right = window.innerWidth - rect.right + "px";
  active.style.left = "auto";
  // Measure after display:flex is applied (the -open class is already on
  // wrap by the time the observer callback runs) so offsetHeight reflects
  // the real popover, not the display:none 0-height default.
  var popHeight = active.offsetHeight;
  var spaceBelow = window.innerHeight - rect.bottom;
  if (spaceBelow < popHeight + margin && rect.top > popHeight + margin) {
    active.style.bottom = window.innerHeight - rect.top + margin + "px";
  } else {
    active.style.top = rect.bottom + margin + "px";
  }
}
var arPopoverObserver = new MutationObserver(function (mutations) {
  mutations.forEach(function (m) {
    if (m.target.classList.contains("ar-toc-kebab-wrap")) {
      arPositionPopover(m.target);
    }
  });
});
// Unlike arInitSortables, this observer is attached ONCE and never needs
// re-attaching on renderUI: `.ar-toc` is the OUTER static wrapper div from
// mod_contents_ui() (see arframe.css #03), and Shiny's renderUI only ever
// replaces the INNER `uiOutput("toc")` div's contents -- the `.ar-toc` node
// itself is never removed or replaced, so `subtree: true` keeps observing
// every row's kebab wrap across every reorder/rename/duplicate/remove.
document.addEventListener("DOMContentLoaded", function () {
  var toc = document.querySelector(".ar-toc");
  if (!toc) return;
  arPopoverObserver.observe(toc, {
    attributes: true,
    attributeFilter: ["class"],
    subtree: true,
  });
});
// Reposition an already-open popover if the window resizes under it (the
// TOC's own scroll never needs this: `position: fixed` does not move with
// scroll of a non-fixed ancestor, so the row can scroll away while the
// popover stays put, exactly like a native OS context menu).
window.addEventListener("resize", function () {
  document
    .querySelectorAll(
      ".ar-toc-kebab-wrap.ar-pop-menu-open, .ar-toc-kebab-wrap.ar-pop-rename-open, .ar-toc-kebab-wrap.ar-pop-remove-open"
    )
    .forEach(arPositionPopover);
});

// The command-palette hint is platform-specific and can only be resolved in
// the browser -- the server's OS is not the client's. Mac shows the Command
// glyph (U+2318); every other platform shows "Ctrl K".
function arSetShortcutHint() {
  var plat =
    (navigator.userAgentData && navigator.userAgentData.platform) ||
    navigator.platform ||
    "";
  var mac = /mac|iphone|ipad|ipod/i.test(plat);
  document.querySelectorAll(".ar-bar-hint").forEach(function (el) {
    el.textContent = mac ? "⌘K" : "Ctrl K";
  });
}
document.addEventListener("DOMContentLoaded", arSetShortcutHint);
$(document).on("shiny:idle", arSetShortcutHint);

// The Add-output overlay (Task 8b): Esc closes it, same as a backdrop
// click -- both post to the module's own `dismiss` input (namespaced
// under whatever id app.R gave mod_add_output_server(), read off the open
// dialog's own id attribute rather than hardcoded, so this keeps working
// if the module is ever mounted under a different namespace). A no-op
// when the dialog is not present (rv$adding FALSE unmounts its content).
$(document).on("keydown", function (e) {
  if (e.key !== "Escape") return;
  var dialog = document.querySelector(".ar-add-card");
  if (!dialog) return;
  var ns = dialog.id.replace(/-dialog$/, "");
  Shiny.setInputValue(ns + "-dismiss", Date.now(), { priority: "event" });
});

// Keyboard navigation (Task 17): in Report mode, Up/Down walk the TOC
// selection and Enter opens the inspector on the selected output's first gap.
// Suppressed while focus is in a form field, a button, or a link, so typing
// and native activation are never hijacked -- the map only acts when focus is
// on the page chrome itself. Namespaced to the fixed "contents" module id,
// matching the data-mode delegated handlers' convention.
$(document).on("keydown", function (e) {
  if (e.key !== "ArrowUp" && e.key !== "ArrowDown" && e.key !== "Enter") {
    return;
  }
  var ws = document.querySelector(".ar-workspace");
  if (!ws || !ws.classList.contains("ar-mode-report")) return;
  // `e.target` is normally the focused Element; guard `.closest` in case it is
  // the document/documentElement (which lack it) so the handler never throws.
  if (
    e.target &&
    typeof e.target.closest === "function" &&
    e.target.closest(
      "input, textarea, select, button, a, [contenteditable], .selectize-input"
    )
  ) {
    return;
  }
  if (e.key === "Enter") {
    Shiny.setInputValue("contents-activate", Date.now(), { priority: "event" });
  } else {
    e.preventDefault();
    Shiny.setInputValue(
      "contents-nav",
      { dir: e.key === "ArrowUp" ? "up" : "down", nonce: Date.now() },
      { priority: "event" }
    );
  }
});

// Move focus into the dialog the moment it appears (design spec #6's
// "focus != selection" rule). Client-driven, NOT a server "ar-focus"
// message: `output$overlay`'s first mount of the search box triggers
// Shiny's normal "a freshly bound input echoes its own value back once"
// behavior, which (since that render depends on the search input for
// live filtering) fires ONE extra, unavoidable renderUI cycle right
// after open -- a server message sent from the observer that flips
// rv$adding has no ordering guarantee against that second cycle
// replacing the dialog DOM node out from under it. Watching for the
// element's own appearance sidesteps the race regardless of how many
// render cycles happen to fire. `.ar-add-overlay-slot` is the STABLE
// `uiOutput()` wrapper (mod_add_output_ui()) -- renderUI only ever
// replaces its CONTENTS, matching the `.ar-toc`/arPopoverObserver
// pattern above, so this observer is attached once and never needs
// re-binding.
document.addEventListener("DOMContentLoaded", function () {
  var slot = document.querySelector(".ar-add-overlay-slot");
  if (!slot) return;
  var seen = null;
  new MutationObserver(function () {
    var dialog = slot.querySelector(".ar-add-card");
    if (dialog && dialog !== seen) {
      seen = dialog;
      dialog.focus();
    } else if (!dialog) {
      seen = null;
    }
  }).observe(slot, { childList: true, subtree: true });
});

// The paper is a READ-ONLY tabular preview: no region-click delegation and
// no editable margin-marks -- editing happens entirely in the right rail.
// Only the table/figure class-flip message handler remains.

Shiny.addCustomMessageHandler("ar-paper-kind", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.classList.remove("ar-paper-kind-table", "ar-paper-kind-figure");
  if (m.kind === "table") el.classList.add("ar-paper-kind-table");
  if (m.kind === "figure") el.classList.add("ar-paper-kind-figure");
});

// The docked inspector (v5, decision #8): the active-tab class flip on the
// card root. Sent by mod_card.R on every rv$insp_tab change (a direct tab
// click OR open_card()'s region routing) -- swapping a class never
// remounts pane state, mirroring the ar-mode handler's technique.
Shiny.addCustomMessageHandler("ar-insp-tab", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.className = el.className.replace(
    /\bar-insp-tab-[a-z]+\b/,
    "ar-insp-tab-" + m.tab
  );
});

// The code view (v5): flip the desk between artifact and reproduction
// script. A class toggle on the desk column, so neither surface remounts.
Shiny.addCustomMessageHandler("ar-code-view", function (m) {
  var el = document.getElementById(m.id);
  if (el) el.classList.toggle("ar-showing-code", !!m.on);
});

// Programmatic click relay: a server observer can trigger a hidden button
// (e.g. the tree "Add folder" CTA firing the toolbar's shinyFiles chooser).
Shiny.addCustomMessageHandler("ar-click", function (m) {
  var el = document.getElementById(m.id);
  if (el) el.click();
});

// Data mode (v5, decision #8): the SOURCES tree and the explorer rows post
// through delegated handlers scoped to the data body, so no per-node/row
// input is registered. Namespace is read off the mounted `.ar-data-main`
// ancestor's own descendant ids -- the module id is a fixed "data".
$(document).on("click", "[data-ar-source]", function () {
  Shiny.setInputValue("data-source", this.getAttribute("data-ar-source"), {
    priority: "event",
  });
});

$(document).on("click", ".ar-dx-row", function () {
  Shiny.setInputValue("data-focus", this.getAttribute("data-ar-name"), {
    priority: "event",
  });
});

$(document).on("dblclick", ".ar-dx-row", function () {
  Shiny.setInputValue("data-open", this.getAttribute("data-ar-name"), {
    priority: "event",
  });
});

// Client-side dataset filter: hide explorer rows whose name/folder does not
// contain the typed text. Kept in JS (no round-trip) since it is pure view.
$(document).on("input", ".ar-dx-filter", function () {
  var q = this.value.toLowerCase();
  document.querySelectorAll(".ar-dx-row").forEach(function (tr) {
    var hay = (
      tr.getAttribute("data-ar-name") + " " + tr.getAttribute("data-ar-lib")
    ).toLowerCase();
    tr.style.display = hay.indexOf(q) === -1 ? "none" : "";
  });
});

// Copy the reproduction script: a [data-ar-copy] button whose value is the
// id of the <pre> to copy. Falls back to a manual selection when the async
// clipboard API is unavailable (older browsers / insecure origin).
$(document).on("click", "[data-ar-copy]", function () {
  var pre = document.getElementById(this.getAttribute("data-ar-copy"));
  if (!pre) return;
  var text = pre.textContent;
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text);
  } else {
    var r = document.createRange();
    r.selectNodeContents(pre);
    var sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(r);
    document.execCommand("copy");
  }
});

// Inspector rail resize (Part C): drag the handle on the rail's left edge to
// set the rail width. Client-side only -- the inline flex-basis persists for
// the session; `ar-resizing` disables the width transition + text selection
// during the drag.
// Generalized (2026-07-04): data-ar-resize="insp" drags the inspector's
// LEFT edge (width measured from the right); data-ar-resize="left" drags
// a left panel's RIGHT edge (contents rail, data sources rail).
(function () {
  var dragging = null;
  var side = null;
  document.addEventListener("mousedown", function (e) {
    var h = e.target.closest ? e.target.closest("[data-ar-resize]") : null;
    if (!h) return;
    side = h.getAttribute("data-ar-resize");
    dragging =
      side === "insp"
        ? h.closest(".ar-card")
        : h.closest("[data-ar-resizable]");
    if (!dragging) return;
    dragging.classList.add("ar-resizing");
    e.preventDefault();
  });
  document.addEventListener("mousemove", function (e) {
    if (!dragging) return;
    var w;
    if (side === "insp") {
      w = Math.max(220, Math.min(640, window.innerWidth - e.clientX));
    } else {
      var left = dragging.getBoundingClientRect().left;
      w = Math.max(180, Math.min(520, e.clientX - left));
    }
    dragging.style.flexBasis = w + "px";
  });
  document.addEventListener("mouseup", function () {
    if (!dragging) return;
    dragging.classList.remove("ar-resizing");
    dragging = null;
  });
})();

// Run-from-anywhere (2026-07-04): Cmd/Ctrl+Enter fires the canvas
// toolbar's Run, in report mode only, and never while the Add-output
// overlay is open. The namespace is read off the mounted [data-ar-toolbar]
// element (matching the Esc/dismiss pattern above), never hardcoded.
// Deliberately NOT suppressed in form fields -- Run-from-anywhere is the
// point of the shortcut.
$(document).on("keydown", function (e) {
  if (e.key !== "Enter" || !(e.metaKey || e.ctrlKey)) return;
  var ws = document.querySelector(".ar-workspace");
  if (!ws || !ws.classList.contains("ar-mode-report")) return;
  if (document.querySelector(".ar-add-card")) return;
  var mount = document.querySelector("[data-ar-toolbar]");
  if (!mount) return;
  e.preventDefault();
  Shiny.setInputValue(
    mount.getAttribute("data-ar-toolbar") + "-run",
    Date.now(),
    { priority: "event" }
  );
});

// The canvas context menu (2026-07-04): right-click on the desk offers
// Add output (always) and Delete output (only when a TOC row is active).
// Items post to mod_paper's add_first / ctx_remove inputs; the namespace
// is read off [data-ar-paper], never hardcoded. Esc, any click, or a
// second right-click elsewhere dismisses it.
(function () {
  var menu = null;
  function hideCtxMenu() {
    if (menu) {
      menu.remove();
      menu = null;
    }
  }
  function ctxItem(label, danger, onPick) {
    var b = document.createElement("button");
    b.type = "button";
    b.className = "ar-ctx-item" + (danger ? " ar-ctx-item-danger" : "");
    b.textContent = label;
    b.addEventListener("click", function () {
      hideCtxMenu();
      onPick();
    });
    return b;
  }
  document.addEventListener("contextmenu", function (e) {
    var desk = e.target.closest ? e.target.closest(".ar-desk-col") : null;
    if (!desk) return hideCtxMenu();
    var paper = document.querySelector("[data-ar-paper]");
    if (!paper) return;
    e.preventDefault();
    hideCtxMenu();
    var ns = paper.getAttribute("data-ar-paper");
    menu = document.createElement("div");
    menu.className = "ar-ctx-menu";
    menu.setAttribute("role", "menu");
    menu.appendChild(
      ctxItem("Add output", false, function () {
        Shiny.setInputValue(ns + "-add_first", Date.now(), {
          priority: "event",
        });
      })
    );
    if (document.querySelector(".ar-toc-row-active")) {
      menu.appendChild(
        ctxItem("Delete output", true, function () {
          Shiny.setInputValue(ns + "-ctx_remove", Date.now(), {
            priority: "event",
          });
        })
      );
    }
    document.body.appendChild(menu);
    menu.style.left =
      Math.min(e.clientX, window.innerWidth - menu.offsetWidth - 8) + "px";
    menu.style.top =
      Math.min(e.clientY, window.innerHeight - menu.offsetHeight - 8) + "px";
  });
  document.addEventListener("click", hideCtxMenu);
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") hideCtxMenu();
  });
})();
