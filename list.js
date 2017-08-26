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
let listControl = document.querySelector(".list");
let searchControl = document.querySelector("#search");
let sortControls = Array.from(document.querySelectorAll(".sort"));

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
  let query = parameters.get("q") || "";
  let sort = decodeSortParameter(parameters.get("sort"));

  return { abstracts, query, sort };
}

function encodeURL(abstracts, query, sort) {
  // Drop defaults.
  let elements =
      [].concat(
        abstracts == Choice.YES ? ["abstracts"] : [],
        query ? [`q=${encodeURIComponent(query)}`] : [],
        (sort && (sort.column !== "number" || sort.order !== "desc"))
          ? [`sort=${sort.column}-${sort.order}`]
          : []);

  return elements.length == 0
    ? ""
    : "?" + elements.join("&");
}

function updateURL(url, { abstracts=null, query=null, sort=null }) {
  let { abstracts: oldAbstracts, query: oldQuery, sort: oldSort }
      = decodeURL(url);

  return url.protocol
    + "//"
    + url.host
    + "/"
    + url.pathname
    + encodeURL(abstracts || oldAbstracts,
                query === null ? oldQuery : query,
                sort || oldSort);
}

function obeyAbstracts(abstracts) {
  abstractsControl.checked = abstracts == Choice.YES;
  if (abstractsControl.checked) {
    listControl.classList.remove("summary");
    listControl.classList.add("detailed");
  } else {
    listControl.classList.remove("detailed");
    listControl.classList.add("summary");
  }
}

function obeySearch(query) {
  searchControl.value = query;
  srfiList.search(query);
}

function obeySort(sort) {
  srfiList.sort(sort.column, { order: sort.order });
}

function obeyQueryParameters(_) {
  let { abstracts, query, sort } = decodeURL(new URL(document.location));

  obeyAbstracts(abstracts);
  obeySearch(query);
  obeySort(sort);
}

window.onpopstate = obeyQueryParameters;

function changeURL(replace, components) {
  let newURL = updateURL(new URL(document.location), components);

  if (replace) {
    history.replaceState(newURL, "home", newURL);
  } else {
    history.pushState(newURL, "home", newURL);
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