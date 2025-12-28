"use strict";

const state = {
  searchQuery: "",
  selectedStatuses: [],
  selectedKeywords: [],
  showAbstracts: false,
  sortColumn: "number",
  sortOrder: "desc"
};

let allCards = [];

function parseURL() {
  const params = new URLSearchParams(window.location.search);

  state.searchQuery = params.get("q") || "";
  state.showAbstracts = params.has("abstracts");
  state.selectedStatuses = params.get("statuses")?.split(",").filter(Boolean) || [];
  state.selectedKeywords = params.get("keywords")?.split(",").filter(Boolean) || [];

  const sort = params.get("sort");

  if (sort) {
    const [column, order] = sort.split("-");

    if (column && order) {
      state.sortColumn = column;
      state.sortOrder = order;
    }
  }
}

function updateURL() {
  const params = new URLSearchParams();

  if (state.searchQuery) params.set("q", state.searchQuery);
  if (state.showAbstracts) params.set("abstracts", "");
  if (state.selectedStatuses.length) params.set("statuses", state.selectedStatuses.join(","));
  if (state.selectedKeywords.length) params.set("keywords", state.selectedKeywords.join(","));
  if (state.sortColumn !== "number" || state.sortOrder !== "desc") {
    params.set("sort", `${state.sortColumn}-${state.sortOrder}`);
  }

  const url = params.toString() ? `?${params}` : window.location.pathname;

  history.replaceState(null, "", url);
}

function updateDisplay() {
  const query = state.searchQuery.toLowerCase().trim();

  allCards.forEach(card => {
    let visible = true;

    if (query) {
      const text = card.textContent.toLowerCase();
      const words = query.split(/\s+/);
      visible = visible && words.every(word => text.includes(word));
    }

    if (state.selectedStatuses.length) {
      const status = card.querySelector("[data-status]").dataset.status;

      visible = visible && state.selectedStatuses.includes(status);
    }

    if (state.selectedKeywords.length) {
      const keywords = card.querySelector("[data-keywords]").dataset.keywords.split(",");

      visible = visible && keywords.some(k => state.selectedKeywords.includes(k));
    }

    card.style.display = visible ? "" : "none";
  });

  sortCards();
}

function naturalCompare(a, b) {
  return a.localeCompare(b, undefined, {numeric: true, sensitivity: "base"});
}

function sortCards() {
  const container = document.querySelector(".list");

  if (!container) return;

  const sorted = [...allCards].sort((a, b) => {
    let aVal = "", bVal = "";

    switch(state.sortColumn) {
      case "number":
        aVal = a.querySelector(".number")?.textContent || "";
        bVal = b.querySelector(".number")?.textContent || "";
        break;
      case "name":
        aVal = a.querySelector(".name")?.textContent || "";
        bVal = b.querySelector(".name")?.textContent || "";
        break;
      case "authors":
        aVal = a.querySelector(".authors")?.textContent || "";
        bVal = b.querySelector(".authors")?.textContent || "";
        break;
      case "date":
        aVal = a.querySelector(".date")?.textContent || "";
        bVal = b.querySelector(".date")?.textContent || "";
        break;
      case "status":
        aVal = a.querySelector("[data-status]").dataset.status;
        bVal = b.querySelector("[data-status]").dataset.status;
        break;
    }

    const compare = naturalCompare(aVal, bVal);

    return state.sortOrder === "asc" ? compare : -compare;
  });

  sorted.forEach(card => container.appendChild(card));
}

function updateMultiSelect(selectElement, values) {
  const options = selectElement.querySelectorAll("option");
  const anyOption = selectElement.querySelector("[value=\"any\"]");

  options.forEach(o => {
    o.selected = values.includes(o.value);
  });

  if (anyOption) {
    anyOption.selected = values.length === 0;
  }

  const chosen = selectElement.parentElement.querySelector(".chosen");

  if (chosen) {
    if (values.length === 0) {
      chosen.textContent = "any";
    } else {
      const labels = values.map(v => {
        const opt = selectElement.querySelector(`[value="${v}"]`);

        return opt ? opt.textContent : v;
      });
      chosen.textContent = labels.join(", ");
    }
  }
}

function init() {
  parseURL();

  allCards = Array.from(document.querySelectorAll(".list > li"));

  const abstractsControl = document.getElementById("abstracts-control");
  const listElement = document.querySelector(".list");

  if (abstractsControl && listElement) {
    abstractsControl.checked = state.showAbstracts;
    listElement.classList.toggle("detailed", state.showAbstracts);
    listElement.classList.toggle("summary", !state.showAbstracts);

    abstractsControl.addEventListener("change", () => {
      state.showAbstracts = abstractsControl.checked;
      listElement.classList.toggle("detailed", state.showAbstracts);
      listElement.classList.toggle("summary", !state.showAbstracts);
      updateURL();
    });
  }

  const searchControl = document.getElementById("search");

  if (searchControl) {
    searchControl.value = state.searchQuery;
    searchControl.addEventListener("input", () => {
      state.searchQuery = searchControl.value;
      updateDisplay();
      updateURL();
    });
  }

  document.querySelectorAll(".sort").forEach(button => {
    const column = button.dataset.sort;

    if (!column) return;

    if (column === state.sortColumn) {
      button.classList.add(state.sortOrder);
    }

    button.addEventListener("click", () => {
      if (column === state.sortColumn) {
        state.sortOrder = state.sortOrder === "asc" ? "desc" : "asc";
      } else {
        state.sortColumn = column;
        state.sortOrder = "asc";
      }

      document.querySelectorAll(".sort").forEach(b => {
        b.classList.remove("asc", "desc");
      });
      button.classList.add(state.sortOrder);

      updateDisplay();
      updateURL();
    });
  });

  function setupDropdown(dropdown) {
    const button = dropdown.querySelector("button");
    const select = dropdown.querySelector("select");

    if (!button || !select) return;

    const isStatus = dropdown.id === "statuses";
    const isKeywords = dropdown.id === "keywords";

    if (isStatus) {
      updateMultiSelect(select, state.selectedStatuses);
    } else if (isKeywords) {
      updateMultiSelect(select, state.selectedKeywords);
    }

    button.addEventListener("click", (e) => {
      e.preventDefault();
      e.stopPropagation();

      document.querySelectorAll(".dropdown").forEach(d => {
        if (d !== dropdown) d.classList.remove("show-options");
      });

      dropdown.classList.toggle("show-options");
    });

    select.addEventListener("change", (e) => {
      e.preventDefault();
      e.stopPropagation();

      const selected = Array.from(select.options)
        .filter(o => o.selected && o.value !== "any")
        .map(o => o.value);

      if (isStatus) {
        state.selectedStatuses = selected;
        updateMultiSelect(select, selected);
      } else if (isKeywords) {
        state.selectedKeywords = selected;
        updateMultiSelect(select, selected);
      }

      updateDisplay();
      updateURL();
    });
  }

  const statusesDropdown = document.getElementById("statuses");
  const keywordsDropdown = document.getElementById("keywords");

  if (statusesDropdown) setupDropdown(statusesDropdown);
  if (keywordsDropdown) setupDropdown(keywordsDropdown);

  document.addEventListener("click", () => {
    document.querySelectorAll(".dropdown").forEach(d => {
      d.classList.remove("show-options");
    });
  });

  const parameters = document.getElementById("parameters");

  if (parameters) {
    parameters.classList.remove("invisible");
  }

  updateDisplay();
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", init);
} else {
  init();
}