/* Translate marked UI text without replacing inputs or changing their values. */
(() => {
  "use strict";
  function initialise() {
    const catalog = JSON.parse(document.getElementById("i18n-catalog").textContent);
    let language = "en";
    let scheduled = false;
    const text = key => language === "de" && catalog[key] !== undefined ? catalog[key] : key;
    function apply() {
      scheduled = false;
      document.documentElement.lang = language;
      document.title = text("OGRS 3 · Reoffending estimates");
      document.querySelectorAll("[data-i18n]").forEach(node => {
        const value = text(node.dataset.i18n);
        if (node.textContent !== value) node.textContent = value;
      });
      ["placeholder", "title", "aria-label"].forEach(attribute => {
        document.querySelectorAll(`[data-i18n-${attribute}]`).forEach(node => {
          const value = text(node.getAttribute(`data-i18n-${attribute}`));
          if (node.getAttribute(attribute) !== value) node.setAttribute(attribute, value);
        });
      });
      document.querySelectorAll("select[data-i18n-options]").forEach(select => {
        const options = JSON.parse(select.getAttribute("data-i18n-options"));
        options.forEach(option => {
          const label = text(option.text);
          Array.from(select.options).filter(item => item.value === option.value).forEach(item => {
            if (item.textContent !== label) item.textContent = label;
          });
          const control = select.selectize;
          if (control) {
            const existing = control.options[option.value];
            const field = control.settings.labelField;
            if (existing && existing[field] !== label) {
              control.updateOption(option.value, Object.assign({}, existing, {[field]: label}));
            }
            if (option.value === "") {
              // Selectize hides prompts once a value is selected and sizes the
              // empty input to fit its prompt. Preserve both behaviours.
              const placeholder = control.items.length ? undefined : label;
              if (control.settings.placeholder !== label ||
                  control.$control_input.attr("placeholder") !== placeholder) {
                control.settings.placeholder = label;
                control.updatePlaceholder();
              }
            }
          }
        });
      });
      // Shiny and Selectize create these fixed interface strings in the browser.
      // Restrict translation to their controls, never arbitrary data table cells.
      const uploadStates = ["Upload complete", "Uploading...", "Upload failed"];
      document.querySelectorAll(".shiny-file-input-progress .progress-bar").forEach(bar => {
        const current = bar.textContent.trim();
        const key = uploadStates.find(value => current === value || current === catalog[value]);
        if (key && bar.textContent !== text(key)) bar.textContent = text(key);
      });
      document.querySelectorAll(".selectize-control .remove[title]").forEach(button => {
        if (["Remove", catalog.Remove].includes(button.title) && button.title !== text("Remove")) {
          button.title = text("Remove");
        }
      });
    }
    function schedule() {
      if (!scheduled) { scheduled = true; requestAnimationFrame(apply); }
    }
    const dropdown = document.getElementById("language");
    dropdown.addEventListener("change", () => {
      language = dropdown.value === "de" ? "de" : "en";
      apply();
    });
    // Shiny also emits this event for programmatic dropdown updates.
    window.jQuery(document).on("shiny:inputchanged.ogrsLanguage", event => {
      if (event.name === "language") {
        language = event.value === "de" ? "de" : "en";
        schedule();
      }
    });
    new MutationObserver(schedule).observe(document.body, {childList: true, subtree: true});
    apply();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", initialise);
  else initialise();
})();
