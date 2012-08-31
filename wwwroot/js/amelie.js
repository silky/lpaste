window.jQuery(function (){var var_1 = 0;window.jQuery('.amelie-code').each(function (){var_1 = Math.max(500,Math.max(window.jQuery(this).width() + 50,var_1));return true;});window.jQuery('.amelie-code').each(function (){window.jQuery('.amelie-wrap').width(Math.max(var_1,500));return true;});window.jQuery('.amelie-latest-pastes').each(function (){window.jQuery('.amelie-wrap').width(Math.max(window.jQuery(this).width(),500));return true;});window.jQuery('.amelie-hint').each(function (){var var_6 = window.jQuery(this);var_6.css('height','1em');var_6.css('overflow','hidden');var_6.parent().css('cursor','pointer');var_6.parent().toggle(function (){var_6.css('height','auto');return false;},function (){var_6.css('height','1em');var_6.css('overflow','hidden');return false;});return true;});window.jQuery('.amelie-paste-nav').each(function (){var var_10 = window.jQuery('<a href="">Expand</a>');var var_11 = window.jQuery(this);var_11.prepend(' - ');var_11.prepend(var_10);var var_12 = var_11.siblings('.amelie-paste-specs');var_12.css('display','none');var_10.text('Expand');var_10.toggle(function (){var_12.css('display','block');var_10.text('Collapse');return false;},function (){var_12.css('display','none');var_10.text('Expand');return false;});return true;});});


/*jshint eqnull:true */
/*!
 * jQuery Cookie Plugin v1.1
 * https://github.com/carhartl/jquery-cookie
 *
 * Copyright 2011, Klaus Hartl
 * Dual licensed under the MIT or GPL Version 2 licenses.
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.opensource.org/licenses/GPL-2.0
 */
(function($, document) {

    var pluses = /\+/g;
    function raw(s) {
	return s;
    }
    function decoded(s) {
	return decodeURIComponent(s.replace(pluses, ' '));
    }

    $.cookie = function(key, value, options) {

	// key and at least value given, set cookie...
	if (arguments.length > 1 && (!/Object/.test(Object.prototype.toString.call(value)) || value == null)) {
	    options = $.extend({}, $.cookie.defaults, options);

	    if (value == null) {
		options.expires = -1;
	    }

	    if (typeof options.expires === 'number') {
		var days = options.expires, t = options.expires = new Date();
		t.setDate(t.getDate() + days);
	    }

	    value = String(value);

	    return (document.cookie = [
				       encodeURIComponent(key), '=', options.raw ? value : encodeURIComponent(value),
				       options.expires ? '; expires=' + options.expires.toUTCString() : '', // use expires attribute, max-age is not supported by IE
				       options.path    ? '; path=' + options.path : '',
				       options.domain  ? '; domain=' + options.domain : '',
				       options.secure  ? '; secure' : ''
				       ].join(''));
	}

	// key and possibly options given, get cookie...
	options = value || $.cookie.defaults || {};
	var decode = options.raw ? raw : decoded;
	var cookies = document.cookie.split('; ');
	for (var i = 0, parts; (parts = cookies[i] && cookies[i].split('=')); i++) {
	    if (decode(parts.shift()) === key) {
		return decode(parts.join('='));
	    }
	}
	return null;
    };

    $.cookie.defaults = {};

})(jQuery, document);

$(document).ready(function(){
	$('form').each(function(){
		$(this).find('input[name=author]')
		    .val($.cookie('author'))
		    .change(function(){
			$.cookie('author', $(this).val(), { expires: 365, path: '/' });
		    });
	    });
});