$(document).ready(function () {
  var glideObj = new Glide('.pathway-grid', {
    type: 'carousel',
    perView: 8
  });
  glideObj.mount();
})

function glide_selected(id, glide_id) {
  Shiny.setInputValue(glide_id, id);
  console.log(glide_id + ": " + id)
}
