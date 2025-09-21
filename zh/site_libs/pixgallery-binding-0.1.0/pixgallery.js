HTMLWidgets.widget({

  name: 'pixgallery',
  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {
      renderValue: function(opts) {
        pixgallery_base(el,opts);
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});

function shuffle(obj1, obj2) {
  var index = obj1.length;
  var rnd, tmp1, tmp2;

  while (index) {
    rnd = Math.floor(Math.random() * index);
    index -= 1;
    tmp1 = obj1[index];
    tmp2 = obj2[index];
    obj1[index] = obj1[rnd];
    obj2[index] = obj2[rnd];
    obj1[rnd] = tmp1;
    obj2[rnd] = tmp2;
  }
}

function pixgallery_base(el,x){
  if(x.shuffle) shuffle(x.path, x.caption);
  if(x.layout === "grid") {
    var fixed = false;
    pixgallery_grid(el,x,fixed);
  }
  if(x.layout === "fixed") {
    var fixed = true;
    pixgallery_grid(el,x,fixed);
  }
  if(x.layout === "masonry") pixgallery_masonry(el,x);
  if(x.layout === "justified") pixgallery_justified(el,x);
  if(x.layout === "elastic") pixgallery_elastic(el,x);
  if(x.layout === "mosaic") pixgallery_mosaic(el,x);
  if(x.layout === "rhombus") pixgallery_rhombus(el,x);
}

/* grid -- fixed ------------------------------------------------------------ */

function pixgallery_grid(el,x,fixed){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var captionValign = x.caption_valign;
  var captionHalign = x.caption_halign;
  var link = x.link;
  var h = x.h;
  var w = x.w;
  var gap = x.gap;
  var borderRadius = x.border_radius;

  if(typeof link[0] !== 'boolean') urls = x.link
  if(h === null) h = "200px";
  if(w === null) w = "200px";

  // initialize template
  if(caption.length === 0){

    // if captions are not used
    var temp = '<div class="pixgallery-child pixgallery-grid-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image" style="border-radius:{borderRadius};height:{h};" src="{src}"></a></div>';

  } else {
    if(captionValign == "none"){

      // if captions are not displayed
      var temp = '<div class="pixgallery-child pixgallery-grid-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image" style="border-radius:{borderRadius};height:{h};" src="{src}"></a></div>';

    } else if(captionValign == "below") {

      if(fixed) {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-below" id="pixgallery-{id}" style="grid-template-rows:{h} 1fr;"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-below" style="border-radius:{borderRadius} {borderRadius} 0 0;height:{h};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-below" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      } else {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-grid-child-below" id="pixgallery-{id}" style="grid-template-rows:{h} 1fr;"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-below" style="border-radius:{borderRadius} {borderRadius} 0 0;height:{h};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-below" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      }
      
    } else if(captionValign == "top") {

      if(fixed) {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:{h};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-top" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      } else {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-top" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      }
      
    } else if(captionValign == "center") {

      if(fixed) {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:{h};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-center" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      } else {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-center" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      }

    } else {

      if(fixed) {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:{h};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-bottom" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      } else {
        var temp = '<div class="pixgallery-child pixgallery-grid-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-grid-image pixgallery-grid-image-over" style="border-radius:{borderRadius};height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-bottom" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      }
    }
  }

  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{h\}/g, h).replace(/\{w\}/g, w).replace(/\{borderRadius\}/g, borderRadius).replace(/\{src\}/g, src[i]).replace(/\{min-height\}/g, h)

    // if link is false or url is null, disable link
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i]).replace(/\{captionHalign\}/g, captionHalign)
    }

    newValues += newValue
  }

  if(fixed) {
    document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-grid" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, ' + w + ');' + '">' + newValues + '</div>';
  } else {
    if(captionValign == "none") {
      document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-grid" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, minmax(' + w + ',1fr));grid-auto-rows:' + h + ';">' + newValues + '</div>';
    } else if(captionValign == "below") {
      document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-grid" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, minmax(' + w + ',1fr));">' + newValues + '</div>';
    } else {
      document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-grid" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, minmax(' + w + ',1fr));grid-auto-rows: minmax(' + h + ',1fr);">' + newValues + '</div>';
    }
  }

  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}

/* mosaic ------------------------------------------------------------------- */

function pixgallery_mosaic(el,x){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var captionValign = x.caption_valign;
  var captionHalign = x.caption_halign;
  var link = x.link;
  var h = x.h;
  var w = x.w;
  var gap = x.gap;
  var borderRadius = x.border_radius;

  if(typeof link[0] !== 'boolean') urls = x.link
  if(h === null) h = "200px";
  if(w === null) w = "200px";

  // initialize template
  if(caption.length === 0){

    // if captions are not used
    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';

  } else {
    if(captionValign == "none"){

    // if captions are not displayed
    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';

    } else if(captionValign == "below") {

    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child pixgallery-mosaic-child-below" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius} {borderRadius} 0 0;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-below" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';

      
    } else if(captionValign == "top") {

    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-top" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
      
    } else if(captionValign == "center") {

    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-center" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else {

    var temp = '<div class="pixgallery-child pixgallery-mosaic-child pixgallery-mosaic-child pixgallery-child-over" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-mosaic-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-bottom" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    }
  }

  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{h\}/g, h).replace(/\{w\}/g, w).replace(/\{borderRadius\}/g, borderRadius).replace(/\{src\}/g, src[i]).replace(/\{min-height\}/g, h)

    // if link is false or url is null, disable link
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i]).replace(/\{captionHalign\}/g, captionHalign)
    }

    newValues += newValue
  }

  if(captionValign == "none") {
    document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-mosaic" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, minmax(' + w + ',1fr));grid-auto-rows:' + h + ';">' + newValues + '</div>';
  } else {
    document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-mosaic" style="gap:' + gap + ';grid-template-columns: repeat(auto-fit, minmax(' + w + ',1fr));">' + newValues + '</div>';
  }

  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}

/* masonry ------------------------------------------------------------------ */

function pixgallery_masonry(el,x){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var captionValign = x.caption_valign;
  var captionHalign = x.caption_halign;
  var link = x.link;
  var w = x.w;
  var gap = x.gap;
  var borderRadius = x.border_radius;

  if(typeof link[0] !== 'boolean') urls = x.link
  if(w === null) w = "200px";

    // initialize template
  if(caption.length === 0){

    // if captions are not used
    var temp = '<div class="pixgallery-child pixgallery-masonry-child" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';

  } else {

    if(captionValign == "none"){

      // if captions are not displayed
      var temp = '<div class="pixgallery-child pixgallery-masonry-child" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';

    } else if(captionValign == "below") {

      var temp = '<div class="pixgallery-child pixgallery-masonry-child pixgallery-child-below" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius} {borderRadius} 0 0;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-below" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else if(captionValign == "top") {

      var temp = '<div class="pixgallery-child pixgallery-masonry-child pixgallery-child-over" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius}; height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-top" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else if(captionValign == "center") {

      var temp = '<div class="pixgallery-child pixgallery-masonry-child pixgallery-child-over" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius}; height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-center" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else {

      var temp = '<div class="pixgallery-child pixgallery-masonry-child pixgallery-child-over" id="pixgallery-{id}" style="margin-bottom:{gap};"><a href="{url}"><img class="pixgallery-masonry-image" style="border-radius:{borderRadius}; height:100%;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-bottom" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
    }
  }

  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{w\}/g, w).replace(/\{borderRadius\}/g, borderRadius).replace(/\{src\}/g, src[i]).replace(/\{gap\}/g, gap)

    // if link is false or url is null, disable a
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i]).replace(/\{captionHalign\}/g, captionHalign)
    }

    newValues += newValue
  }

	document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-masonry" style="column-gap:' + gap + ';column-width:' + w + ';">' + newValues + '</div>';
  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}

/* justified ---------------------------------------------------------------- */

function pixgallery_justified(el,x){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var captionValign = x.caption_valign;
  var captionHalign = x.caption_halign;
  var link = x.link;
  var h = x.h;
  var gap = x.gap;
  var borderRadius = x.border_radius;

  if(typeof link[0] !== 'boolean') urls = x.link
  if(h === null) h = "200px";

  // initialize template
  if(caption.length === 0){

    // if captions are not used
    var temp = '<div class="pixgallery-child pixgallery-justified-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-justified-image" style="height:{h};border-radius:{borderRadius};" src="{src}"></a></div>';

  } else {
    if(captionValign == "none"){

      // if captions are not displayed
      var temp = '<div class="pixgallery-child pixgallery-justified-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-justified-image" style="height:{h};border-radius:{borderRadius};" src="{src}"></a></div>';

    } else  if(captionValign == "below") {

      var temp = '<div class="pixgallery-child pixgallery-justified-child pixgallery-child-below" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-justified-image" style="height:{h};border-radius:{borderRadius} {borderRadius} 0 0;" src="{src}"></a><div class="pixgallery-caption pixgallery-caption-below" style="border-radius:0 0 {borderRadius} {borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else if(captionValign == "top") {

      var temp = '<div class="pixgallery-child pixgallery-justified-over-child" id="pixgallery-{id}"><a href="{url}" style="height:{h};"><img class="pixgallery-justified-over-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-justified-caption-top" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else if(captionValign == "center") {

      var temp = '<div class="pixgallery-child pixgallery-justified-over-child" id="pixgallery-{id}"><a href="{url}" style="height:{h};"><img class="pixgallery-justified-over-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-justified-caption-center" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';

    } else {

      var temp = '<div class="pixgallery-child pixgallery-justified-over-child" id="pixgallery-{id}"><a href="{url}" style="height:{h};"><img class="pixgallery-justified-over-image" style="border-radius:{borderRadius};" src="{src}"></a><div class="pixgallery-caption pixgallery-justified-caption-bottom" style="border-radius:{borderRadius};text-align:{captionHalign};">{caption}</div></div>';
    }
  }
  
  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{h\}/g, h).replace(/\{borderRadius\}/g, borderRadius).replace(/\{src\}/g, src[i]).replace(/\{gap\}/g, gap)

    // if link is false or url is null, disable a
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i]).replace(/\{captionHalign\}/g, captionHalign)
    }

    newValues += newValue
  }

	document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-justified" style="gap:' + gap + ';">' + newValues + '</div>';
  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}

/* elastic ------------------------------------------------------------------ */

function pixgallery_elastic(el,x){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var link = x.link;
  var h = x.h;
  var gap = x.gap;
  var borderRadius = x.border_radius;

  if(typeof link[0] !== 'boolean') urls = x.link
  if(h === null) h = "200px";

    // initialize template
  if(caption.length === 0){
    var temp = '<div class="pixgallery-child pixgallery-elastic-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-elastic-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';
  } else {
    var temp = '<div class="pixgallery-child pixgallery-elastic-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-elastic-image" style="border-radius:{borderRadius};" src="{src}"></a></div>';
  }
  
  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{h\}/g, h).replace(/\{borderRadius\}/g, borderRadius).replace(/\{src\}/g, src[i]).replace(/\{gap\}/g, gap)

    // if link is false or url is null, disable a
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i])
    }

    newValues += newValue
  }

	document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-elastic" style="height:' + h + ';gap:' + gap + ';">' + newValues + '</div>';
  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}

/* rhombus ------------------------------------------------------------------ */

function pixgallery_rhombus(el,x){

  var urls = x.path;
  var src = x.path;
  var caption = x.caption;
  var link = x.link;
  var gap = x.gap;
  if(typeof link[0] !== 'boolean') urls = x.link

  // initialize template
  if(caption.length === 0){
    var temp = '<div class="pixgallery-child pixgallery-rhombus-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-rhombus-image" src="{src}"></a></div>';
  } else {
    var temp = '<div class="pixgallery-child pixgallery-rhombus-child" id="pixgallery-{id}"><a href="{url}"><img class="pixgallery-rhombus-image" src="{src}"></a></div>';
  }
  
  var newValues = '', limitItem = urls.length;
  for (let i = 0; i < limitItem; ++i) {

    let newValue = '';
    newValue = temp.replace("{id}",el.id).replace(/\{src\}/g, src[i])

    // if link is false or url is null, disable a
    if(urls[i]== null || link[0] === false){
      newValue = newValue.replace('href="{url}"', 'class="disabled"').replace("title=\"{caption}\"","")
    } else {
      newValue = newValue.replace(/\{url\}/g, urls[i])
    }

    // if caption is not empty
    if(caption.length !== 0){
      if(caption[i] === null || caption[i] === undefined) caption[i] = "";
      newValue = newValue.replace(/\{caption\}/g, caption[i])
    }

    newValues += newValue
  }

  document.getElementById(el.id).innerHTML = '<div class="pixgallery-gallery pixgallery-rhombus" style="gap:' + gap + ';">' + newValues + '</div>';
  if(link[0] === true) var lightbox = new SimpleLightbox({elements: '#pixgallery-' + el.id + ' a'});
}
