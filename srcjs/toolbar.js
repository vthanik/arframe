// The canvas toolbar -- the first Preact component (2026-07-04). Renders
// into mod_toolbar_ui()'s [data-ar-toolbar] mount div and talks to R only
// through Shiny inputs (<ns>-view / <ns>-run / <ns>-rtf_click) and the
// "ar-toolbar" custom message (display state pushed by mod_toolbar.R).
// All application state stays in the store server-side; this component
// holds nothing but the last pushed display state.
import { h, render } from "preact";
import { useState, useEffect } from "preact/hooks";
import htm from "htm";

const html = htm.bind(h);

// The last state pushed per mount id, buffered so a message that arrives
// before the component mounts (Shiny binds messages before DOMContentLoaded
// work finishes) is not lost -- the component reads it on mount.
const lastState = {};
const listeners = {};

if (window.Shiny) {
  Shiny.addCustomMessageHandler("ar-toolbar", function (m) {
    lastState[m.id] = m.state;
    if (listeners[m.id]) listeners[m.id](m.state);
  });
}

function macLike() {
  const plat =
    (navigator.userAgentData && navigator.userAgentData.platform) ||
    navigator.platform ||
    "";
  return /mac|iphone|ipad|ipod/i.test(plat);
}

function Toolbar({ ns, mountId }) {
  const [state, setState] = useState(
    lastState[mountId] || {
      code_view: false,
      ready: false,
      stale: false,
      running: false,
    }
  );
  useEffect(() => {
    listeners[mountId] = setState;
    if (lastState[mountId]) setState(lastState[mountId]);
    return () => delete listeners[mountId];
  }, [mountId]);

  const set = (name, value) =>
    Shiny.setInputValue(ns + "-" + name, value, { priority: "event" });
  const view = state.code_view ? "code" : "output";
  const canRtf = state.ready && !state.stale;

  return html`
    <div class="ar-tb-row">
      <div class="ar-tb-seg" role="tablist" aria-label="Desk view">
        <button
          type="button"
          role="tab"
          aria-selected=${view === "output"}
          class=${"ar-tb-seg-btn" + (view === "output" ? " ar-tb-on" : "")}
          onClick=${() => set("view", "output")}
        >
          <svg width="13" height="13" viewBox="0 0 24 24" fill="none"
            stroke="currentColor" stroke-width="1.3" stroke-linecap="round"
            stroke-linejoin="round" aria-hidden="true">
            <path d="M2 12s3.5-6 10-6 10 6 10 6-3.5 6-10 6-10-6-10-6z" />
            <circle cx="12" cy="12" r="3" />
          </svg>
          Output
        </button>
        <button
          type="button"
          role="tab"
          aria-selected=${view === "code"}
          class=${"ar-tb-seg-btn" + (view === "code" ? " ar-tb-on" : "")}
          onClick=${() => set("view", "code")}
        >
          <svg width="13" height="13" viewBox="0 0 24 24" fill="none"
            stroke="currentColor" stroke-width="1.3" stroke-linecap="round"
            stroke-linejoin="round" aria-hidden="true">
            <polyline points="16 18 22 12 16 6" />
            <polyline points="8 6 2 12 8 18" />
          </svg>
          Code
        </button>
      </div>
      <div class="ar-tb-spacer"></div>
      ${state.stale &&
      html`<span class="ar-tb-stale ar-mono">stale — run to re-typeset</span>`}
      <button
        type="button"
        class="ar-tb-btn"
        disabled=${!canRtf}
        aria-label="Download RTF"
        onClick=${() => canRtf && set("rtf_click", Date.now())}
      >
        <svg width="13" height="13" viewBox="0 0 24 24" fill="none"
          stroke="currentColor" stroke-width="1.3" stroke-linecap="round"
          stroke-linejoin="round" aria-hidden="true">
          <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
          <polyline points="7 10 12 15 17 10" />
          <line x1="12" y1="15" x2="12" y2="3" />
        </svg>
        .rtf
      </button>
      <button
        type="button"
        class="ar-tb-run"
        onClick=${() => set("run", Date.now())}
      >
        <svg width="12" height="12" viewBox="0 0 24 24" fill="currentColor"
          stroke="none" aria-hidden="true">
          <polygon points="6 3 20 12 6 21 6 3" />
        </svg>
        Run
        <span class="ar-tb-kbd ar-mono">
          ${macLike() ? "⌘↵" : "Ctrl ↵"}
        </span>
      </button>
    </div>
  `;
}

export function mountToolbars() {
  document.querySelectorAll("[data-ar-toolbar]").forEach(function (el) {
    if (el._arToolbar) return;
    el._arToolbar = true;
    render(
      html`<${Toolbar} ns=${el.getAttribute("data-ar-toolbar")} mountId=${el.id} />`,
      el
    );
  });
}

document.addEventListener("DOMContentLoaded", mountToolbars);
