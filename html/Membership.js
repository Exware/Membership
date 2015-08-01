$(document).ready(function() {
	$('a.member_off').each(function() {
		var $dialog = $('<div></div>');
		var $link = $(this).one('click', function() {
			$dialog
				.load($link.attr('href'))
				.dialog({
					title: $link.attr('title'),
					width: 640,
					height: 480
				});

			$link.click(function() {
				$dialog.dialog('open');
				return false;
			});
			return false;
		});
	});
	
	$('.load_ajax').each(function() {
		var $link = $(this);
		$(this).parent().load($link.attr('href'));
	});
});

