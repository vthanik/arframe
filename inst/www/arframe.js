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
      // `arFlushDeferredRegionClicks` (defined further down, in the Task-9
      // paper section) is called here -- NOT via a `shiny:inputchanged`
      // listener keyed on the input's name -- so the flush is deterministic
      // and does not depend on guessing every current/future sortable
      // input's naming pattern. `typeof` guards the call because this
      // function runs before the paper section's declaration is READ in
      // source order is irrelevant (function declarations hoist), but
      // keeping the guard costs nothing and protects a future refactor that
      // splits this file.
      onEnd: function () {
        delete document.body.dataset.arDragging;
        if (typeof arFlushDeferredRegionClicks === "function") {
          arFlushDeferredRegionClicks();
        }
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

// The paper (Task 9): region-click delegation, the class-flip message
// handlers (table/figure kind, fit/page width), and click-to-select
// styling on the active region.

// THE CONCURRENT-MUTATOR DRAG GUARD (deferred from Task 7, landing here --
// see the `document.body.dataset.arDragging` comment in `arInitSortables()`
// above). mod_paper is the first SECOND module that re-renders off
// `store$rv$report`/`store$rv$selected` while a Contents TOC drag can be
// physically in progress (`arInitSortables()`'s `onStart`/`onEnd` sets and
// clears the flag for exactly that window). The concrete risk this guards
// against: a region click (real furniture, a ghost slot, or an error-
// summary jump link) firing `Shiny.setInputValue` WHILE the mouse button is
// still down on a TOC row -- e.g. a drag gesture that clips a paper region
// as it starts, or a synthetic/automated click landing mid-gesture in a
// test. `open_card()` only touches `rv$region`/`rv$card` (never
// `rv$report`), so this is not today's most dangerous race, but the guard
// is written for the general contract the brief asks for: NO Shiny input
// that could drive a report-triggered re-render fires while `arDragging` is
// set, queued instead and flushed the instant the flag clears. `arRegionClick()`
// is the ONE function both the real click delegation below and any future
// paper-triggered input (fit/page toggle is local-only CSS, so it does not
// need the guard) route through, so the guard lives in one place.
var arDeferredRegionClicks = [];
function arRegionClick(host, region) {
  if (document.body.dataset.arDragging) {
    // Queue rather than drop: a click physically landed, the user's intent
    // should not silently vanish just because it raced a drag -- it fires
    // the instant the drag's `onEnd` clears the flag (see the flush call
    // appended to `onEnd` below).
    arDeferredRegionClicks.push({ host: host, region: region });
    return;
  }
  Shiny.setInputValue(host + "-region", region, { priority: "event" });
}
function arFlushDeferredRegionClicks() {
  if (arDeferredRegionClicks.length === 0) return;
  var queued = arDeferredRegionClicks;
  arDeferredRegionClicks = [];
  queued.forEach(function (c) {
    Shiny.setInputValue(c.host + "-region", c.region, { priority: "event" });
  });
}
// The flush itself is called directly from `arInitSortables()`'s `onEnd`
// (above, right after it clears `document.body.dataset.arDragging`) -- a
// direct call is deterministic and needs no assumption about a reorder
// input's name pattern, unlike listening for `shiny:inputchanged`.

$(document).on("click", "[data-ar-region]", function (e) {
  e.stopPropagation();
  var host = $(this).closest("[data-ar-paper]").attr("data-ar-paper");
  if (!host) return;
  arRegionClick(host, this.getAttribute("data-ar-region"));
  document
    .querySelectorAll(".ar-region-active")
    .forEach(function (el) {
      el.classList.remove("ar-region-active");
    });
  this.classList.add("ar-region-active");
});

// A ghost slot carries `role="button"` + `tabindex="0"` (utils_ghost.R) so
// it is keyboard-focusable and announced as a button -- but a plain `div`,
// unlike a real `<button>`/`<a>`, does not natively fire `click` on
// Enter/Space, so that promise needs an explicit handler. Real (non-ghost)
// furniture regions are NOT focusable (no `tabindex`), so this only ever
// matches a ghost slot; `e.preventDefault()` stops Space from also
// scrolling the page, the standard `role="button"` keyboard contract.
$(document).on("keydown", "[data-ar-region][tabindex]", function (e) {
  if (e.key !== "Enter" && e.key !== " ") return;
  e.preventDefault();
  $(this).trigger("click");
});

Shiny.addCustomMessageHandler("ar-paper-kind", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.classList.remove("ar-paper-kind-table", "ar-paper-kind-figure");
  if (m.kind === "table") el.classList.add("ar-paper-kind-table");
  if (m.kind === "figure") el.classList.add("ar-paper-kind-figure");
});

Shiny.addCustomMessageHandler("ar-paper-width", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.classList.remove("ar-paper--fit", "ar-paper--page");
  el.classList.add(m.mode === "page" ? "ar-paper--page" : "ar-paper--fit");
});

// The galley card (Task 10): the floating/pinned class flip, and Esc-to-
// close when unpinned. `ar-card-pin`'s `pinned` flag ALSO drives whether
// Esc should act -- both read off `.ar-card`'s own current class rather
// than a second server-tracked flag, so the two can never disagree about
// what "currently pinned" means.
Shiny.addCustomMessageHandler("ar-card-pin", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.classList.toggle("ar-pinned", !!m.pinned);
});

// Esc closes the card, same as the Add-output overlay's own Esc handler
// (mirroring its "read the namespace off the mounted element's own id"
// technique) -- but ONLY when the card is not pinned, matching
// `close_card()`'s own no-op-while-pinned contract server-side (this is a
// CLIENT-side mirror of that rule so an unnecessary round-trip never
// fires for a keypress that would be dropped anyway).
$(document).on("keydown", function (e) {
  if (e.key !== "Escape") return;
  var card = document.querySelector(".ar-card");
  if (!card || card.classList.contains("ar-pinned")) return;
  var ns = card.id.replace(/-card$/, "");
  Shiny.setInputValue(ns + "-close", Date.now(), { priority: "event" });
});
