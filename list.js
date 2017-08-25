let options = {
  sortFunction: function(itemA, itemB, options) {
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

  },
  valueNames: [ "authors",
                "date",
                "name",
                "number",
                { name: "status", attr: "data-status" }
              ]};

let srfiList = new List("srfis", options);

srfiList.sort("number", { order: "desc" });