function addCloser(div) {
  div.querySelector(".close").addEventListener(
    "click",
    function(event) {
      event.preventDefault();
      event.stopPropagation();
      for (let button of div.querySelectorAll("button")) {
        button.classList.remove("invisible");
      }
      event.target.classList.add("invisible");
      for (let form of div.querySelectorAll("form")) {
        form.classList.add("invisible");
      }
    });
}
function addRevealer(div, selector) {
  div.querySelector("button" + selector).addEventListener(
    "click",
    function(event) {
      event.preventDefault();
      event.stopPropagation();
      div.querySelector("form" + selector).classList.remove("invisible");
      for (let button of div.querySelectorAll("button")) {
        if (button.classList.contains("close")) {
          button.classList.remove("invisible");
        } else {
          button.classList.add("invisible");
        }
      }
    });
}

for (let div of document.querySelectorAll(".subunsub")) {
  for (let button of div.querySelectorAll("button.sub")) {
    button.classList.remove("invisible");
  }
  for (let button of div.querySelectorAll("button.unsub")) {
    button.classList.remove("invisible");
  }
  for (let form of div.querySelectorAll("form")) {
    form.classList.add("invisible");
  }
  addCloser(div);
  addRevealer(div, ".sub");
  addRevealer(div, ".unsub");
}