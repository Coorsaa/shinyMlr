$("#top-nav a[data-value]").each(function() {
  // this references the DOM element and we change its 'href' attribute.
  if(this.getAttribute('data-value').substring(0,4) == 'java') {
    this.setAttribute('onClick', this.getAttribute('data-value'))
    this.setAttribute('data-toggle', null);
  } else if(this.getAttribute('data-value').substring(0,4) == 'http'){
    this.setAttribute('href', this.getAttribute('data-value'));
    this.setAttribute('target', '_blank');
    this.setAttribute('data-toggle', null);
  }
});

$('a:contains("hide_me")').parent().parent().parent().css("width", "100%");

var space_width = (
$('.container-fluid').innerWidth() -
$('.brand').outerWidth() -
eval($('.navbar a').map(function(){
    return $(this).parent().width();
}).get().join("+")) +
$('a:contains("hide_me")').parent().width() - 130);

//spacing of navbar
$('a:contains("hide_me")').css("visibility", "hidden");
$('a:contains("hide_me")').parent().css("width", space_width);