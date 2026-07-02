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
