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
