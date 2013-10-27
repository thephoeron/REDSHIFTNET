$(document).ready(function() {
 	
	//------------- ToDo -------------//
	//toDo 
    function toDo () {
        var todos = $('.toDo');
        var items = todos.find('.task-item');
        var chboxes = items.find('input[type="checkbox"]');
        var close = items.find('.act');

        chboxes.change(function() {
           if ($(this).is(':checked')) {
                $(this).closest('.task-item').addClass('done');
            } else {
                $(this).closest('.task-item').removeClass('done');
            }
        });

        items.hover(
          function () {
            $(this).addClass('show');
          },
          function () {
            $(this).removeClass('show');
          }
        );

        close.click(function() {
            $(this).closest('.task-item').fadeOut('500');
            //Do other stuff here..
        });

    }

    toDo();

	//sortable
	$(function() {
	    $( "#today, #tomorrow" ).sortable({
	      connectWith: ".todo-list"
	    }).disableSelection();
	});

	//------------- Spark stats in widget box title -------------//
	$('.spark>.positive').sparkline('html', { type:'bar', barColor:'#42b449'});
	$('.spark-line>.positive').sparkline('html', { type:'line', lineColor:'#42b449'});


	//------------- Custom scroll in widget box  -------------//

	$(".scroll").niceScroll({
		cursoropacitymax: 0.8,
        cursorborderradius: 0,
        cursorwidth: "10px"
	});

	//------------- Contact widet list nav plugin -------------//
	$('#contact-list').listnav({ 
	    includeNums: false,  
	    noMatchText: 'There are no matching entries.',
	    showCounts: false
	 });

	 	
});