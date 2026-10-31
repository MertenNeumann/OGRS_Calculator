$(document).ready(function(){
  $('input[name=checkItem6]').on('click', function(event){
    if($('input[name=checkItem6]:checked').length > 5){
      $(this).prop('checked', false);
    }
  });
  $('input[name=checkItem6]').on('click', function(event){
    if($('input[name=checkItem6]:checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});