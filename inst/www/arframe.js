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
  ws.className = "ar-workspace ar-mode-" + m;
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
      onEnd: function () {
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
