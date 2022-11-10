$(document).ready(function () {
  let glideObj = new Glide('.pathway-grid', {
    type: 'carousel',
    perView: 8
  });
  glideObj.mount();
});

function glideSelected(id, glideId) {
  Shiny.setInputValue(glideId, id);
}

function toggleSubplot(id) {
  var x = document.getElementById(id);
  console.log(id);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}
