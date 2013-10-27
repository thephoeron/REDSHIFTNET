$(document).ready(function() {

	//------------- Bootstrap image gallery -------------//
 	
 	$(function () {
    'use strict';

	    // Load images via flickr for demonstration purposes:
	    $.ajax({
	        url: 'http://api.flickr.com/services/rest/',
	        data: {
	            format: 'json',
	            method: 'flickr.interestingness.getList',
	            api_key: '7617adae70159d09ba78cfec73c13be3'
	        },
		    dataType: 'jsonp',
	        jsonp: 'jsoncallback'
	    }).done(function (data) {
	        var gallery = $('#gallery'),
	            url;
	        $.each(data.photos.photo, function (index, photo) {
	            url = 'http://farm' + photo.farm + '.static.flickr.com/' +
	                photo.server + '/' + photo.id + '_' + photo.secret;
	            $('<a data-gallery="gallery"/>')
	                .append($('<img>').prop('src', url + '_s.jpg'))
	                .prop('href', url + '_b.jpg')
	                .prop('title', photo.title)
	                .appendTo(gallery);
	        });

	        //activate magnificPopup
	        $('#gallery').magnificPopup({
			    delegate: 'a', // child items selector, by clicking on it popup will open
			    type: 'image',
			    gallery: {
			    	enabled: true
			    },
			});
	    });
	});
 	
});