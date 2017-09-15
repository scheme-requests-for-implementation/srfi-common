function sortFunction(itemA, itemB, options) {
  let column = options.valueName;
  let sort = srfiList.utils.naturalSort;
  let primary = sort(itemA.values()[column],
                     itemB.values()[column]);

  if (primary == 0) {
    if (column === "number") {
      return 0;
    }
    return sort(itemA.values()["number"],
                itemB.values()["number"]);
  }
  return primary;

};

let options = {
  sortFunction,
  valueNames: [ "abstract",
                "authors",
                "date",
                "name",
                "number",
                { name: "status", attr: "data-status" }
              ]};

let srfiList = new List("srfis", options);

let abstractsControl = document.querySelector("#abstracts-control");
let listElement = document.querySelector(".list");
let searchControl = document.querySelector("#search");
let sortControls = Array.from(document.querySelectorAll(".sort"));
let statusesControl = document.querySelector("#statuses");
let keywordsControl = document.querySelector("#keywords");

function assert(expression) {
  if (! expression) {
    throw new Error();
  }
}

function assertMember(candidate, array) {
  assert(array.find(x => x === candidate));
}

function assertValidColumn(column) {
  assertMember(column,
               ["abstract", "authors", "date", "name", "number", "status"]);
}

function assertValidSortOrder(order) {
  assertMember(order, ["asc", "desc"]);
}

function decodeSortParameter(sp) {
  if (sp) {
    let [column, order] =  sp.split("-", 2);

    assertValidColumn(column);
    assertValidSortOrder(order);
    return { column, order };
  } else {
    return { column: "number", order: "desc" };
  }
}

function decodeMultiParameter(string) {
  return string && string.split(",");
}

let Choice = {
  YES: 1,
  NO: 2
};

function choice(boolean) {
  return boolean ? Choice.YES : Choice.NO;
}

function decodeURL(url) {
  let parameters = url.searchParams;
  let abstracts = parameters.has("abstracts") ? Choice.YES : Choice.NO;
  let keywords = decodeMultiParameter(parameters.get("keywords"));
  let query = parameters.get("q") || "";
  let sort = decodeSortParameter(parameters.get("sort"));
  let statuses = decodeMultiParameter(parameters.get("statuses"));

  return { abstracts, keywords, query, sort, statuses };
}

function encodeWithCommas(name, values) {
  return values && values.length > 0
    ? [`${name}=${values.join(",")}`]
    : [];
}

function encodeURL(abstracts, keywords, query, sort, statuses) {
  // Drop defaults.
  let elements =
      [].concat(
        abstracts == Choice.YES ? ["abstracts"] : [],
        query ? [`q=${encodeURIComponent(query)}`] : [],
        (sort && (sort.column !== "number" || sort.order !== "desc"))
          ? [`sort=${sort.column}-${sort.order}`]
          : [],
        encodeWithCommas("keywords", keywords),
        encodeWithCommas("statuses", statuses));

  return elements.length == 0
    ? ""
    : "?" + elements.join("&");
}

function updateURL(url, { abstracts=null,
                          keywords=null,
                          query=null,
                          sort=null,
                          statuses=null }) {
  let { abstracts: oldAbstracts,
        keywords: oldKeywords,
        query: oldQuery,
        sort: oldSort,
        statuses: oldStatuses }
      = decodeURL(url);

  return url.protocol
    + "//"
    + url.host
    + url.pathname
    + encodeURL(abstracts || oldAbstracts,
                keywords || oldKeywords,
                query === null ? oldQuery : query,
                sort || oldSort,
                statuses || oldStatuses);
}

function obeyAbstracts(abstracts) {
  abstractsControl.checked = abstracts == Choice.YES;
  if (abstractsControl.checked) {
    listElement.classList.remove("summary");
    listElement.classList.add("detailed");
  } else {
    listElement.classList.remove("detailed");
    listElement.classList.add("summary");
  }
}

function updateMultiSelect(control, values) {
  let options = Array.from(control.querySelectorAll("option"));
  let set = new Set(values);
  let names = [];

  for (let o of options) {
    o.selected = set.has(o.value);
    if (o.selected) {
      names.push(o.innerHTML);
    }
  };
  control.querySelector(".chosen").innerHTML =
    choiceEnglish(names);
}

function srfiCards() {
  return Array.from(srfiList.items).map(i => i.elm);
}

function filterMulti(control, name, values) {
  let invisibleClass = `invisible-${name}`;

  for (let i of Array.from(document.querySelectorAll("." + invisibleClass))) {
    i.classList.remove(invisibleClass);
  }
  if (values.length > 0) {
    let set = new Set(values);

    for (let c of srfiCards()) {
      let cardValues = c.querySelector(`.${name}`)
          .dataset[name].split(",");

      if (! cardValues.find(v => set.has(v))) {
        c.classList.add(invisibleClass);
      }
    }
  }}

function obeyKeywords(keywords) {
  updateMultiSelect(keywordsControl, keywords);
  filterMulti(keywordsControl, "keywords", keywords);
}

function obeySearch(query) {
  searchControl.value = query;
  srfiList.search(query);
}

function obeySort(sort) {
  srfiList.sort(sort.column, { order: sort.order });
}

function obeyStatuses(statuses) {
  updateMultiSelect(statusesControl, statuses);
  filterMulti(statusesControl, "status", statuses);
}

function obeyQueryParameters(_) {
  let { abstracts, keywords, query, sort, statuses }
      = decodeURL(new URL(document.location));

  obeyAbstracts(abstracts);
  obeyKeywords(keywords || []);
  obeySearch(query);
  obeySort(sort);
  obeyStatuses(statuses || []);
}

window.onpopstate = obeyQueryParameters;

function changeURL(replace, components) {
  let existing = new URL(document.location);
  let newURL = updateURL(existing, components);

  if (existing.toString() !== newURL) {
    if (replace) {
      history.replaceState(newURL, "home", newURL);
    } else {
      history.pushState(newURL, "home", newURL);
    }
  }
}

abstractsControl.addEventListener(
  "change",
  function(event) {
    obeyAbstracts(choice(this.checked));
    changeURL(false, { abstracts: choice(this.checked) });
    event.preventDefault();
    event.stopPropagation();
  });

searchControl.addEventListener(
  "input",
  function(event) {
    changeURL(true, { query: this.value });
  });

function choiceEnglish(choices) {
  if (choices.length == 0) {
    return "any";
  }

  let size = choices.length;
  let last = choices[size - 1];

  if (size == 1) {
    return last;
  }
  if (size == 2) {
    return choices[0] + " or " + last;
  }
  return choices.slice(0, -1).join(", ") + ", or " + last;
}

function enableMultiSelect(control, update) {
  let popper = control.querySelector("button");
  let select = control.querySelector("select");

  popper.addEventListener(
    "click",
    function(event) {
      event.preventDefault();
      for (let s of Array.from(document.querySelectorAll(".show-options"))) {
        if (s != control) {
          s.classList.remove("show-options");
        }
      }
      control.classList.toggle("show-options");
    }
  );
  select.addEventListener(
    "mousedown",
    function(event) {
      event.preventDefault();
      event.stopPropagation();

      let scroll = select.scrollTop;

      event.target.selected = !event.target.selected;
      // setTimeout(() => select.scrollTop = scroll, 0);
      select.focus();

      let values = Array.from(select.options)
          .filter(s => s.selected)
          .map(s => s.value);

      update(values);
    });
  select.addEventListener("mousemove", event => event.preventDefault());
}

enableMultiSelect(statusesControl,
                   function(choices) {
                     changeURL(false, { statuses: choices });
                     obeyStatuses(choices);
                   });

enableMultiSelect(keywordsControl,
                   function(choices) {
                     changeURL(false, { keywords: choices });
                     obeyKeywords(choices);
                   });

let observer = new MutationObserver(
  function(mutationRecords, observer) {
    for (let r of mutationRecords) {
      let button = r.target;
      let classes = button.classList;
      let column = button.textContent;

      assertValidColumn(column);

      if (classes.contains("asc")) {
        changeURL(false, { sort: { column: column, order: "asc" } });
      } else if (classes.contains("desc")) {
        changeURL(false, { sort: { column: column, order: "desc" } });
      }
    }
  });

for (let button of sortControls) {
  observer.observe(button, { attributes: true });
}

obeyQueryParameters(null);
document.querySelector(".invisible").classList.remove("invisible");