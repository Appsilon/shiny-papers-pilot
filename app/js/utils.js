$(document).ready(function () {
  let glideObj = new Glide('.pathway-grid', {
    type: 'carousel',
    perView: 8
  });
  glideObj.mount();
});

function glideSelected(id, glideId) {
  let cards = document.getElementsByClassName('pathway-card');
  cards.forEach(function (e) {
    if (e.id == id) {
     e.style.opacity = 1;
    } else {
     e.style.opacity = 0.7;
    }
  });

  Shiny.setInputValue(glideId, id);
}

function toggleSubplot(id) {
  var e = document.getElementById(id);
  if (e.style.display === 'none') {
    e.style.display = 'block';
  } else {
    e.style.display = 'none';
  }
}

function updateCollapseIcon(id) {
  var e = document.getElementById(id);

  if (e.classList.contains('fa-plus')) {
    e.classList.remove('fa-plus')
    e.classList.add('fa-minus')
  } else {
    e.classList.remove('fa-minus')
    e.classList.add('fa-plus')
  }


}
