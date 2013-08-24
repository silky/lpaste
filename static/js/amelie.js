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

/*******************************************************************************
 *  Date utilities
 */

Date.prototype.relative = function(t2,fix){
  var t1 = this;
  var diff = t1 - t2;
  var minute = 60, hour = minute * 60, day = hour * 24,
  week = day * 7, month = day * 30, year = month * 12;
  return inRange(
    [0,'just now'],
    [5,'% seconds',1],
    [minute,'a minute'],
    [minute*2,'% minutes',minute],
    [minute*30,'half an hour'],
    [minute*31,'% minutes',minute],
    [hour,'an hour'],
    [hour*2,'% hours',hour],
    [hour*3,'a few hours'],
    [hour*4,'% hours',hour],
    [day,'a day'],
    [day*2,'% days',day],
    [week,'a week'],
    [week*2,'% weeks',week],
    [month,'a month'],
    [month*2,'% months',month],
    [year,'a year'],
    [year*2,'% years',year]
  );
  function inRange() {
    var span = Math.abs(diff/1000);
    for (var i = arguments.length-1; i >= 0; i--) {
      var range = arguments[i];
      if (span >= range[0]) {
        return (
          (fix&& diff>0?'in ':'') +
            (range[1].match(/%/)?
             range[1].replace(/%/g,Math.round(span/(range[2]? range[2] : 1)))
             : range[1]) +
            (fix&& diff<0?' ago':'')
        );
      }
    }
  }
};

function refreshDates(){
  $('.relative-time').each(function(){
    var t = (new Date($(this).attr('data-epoch') * 1000));
    var now = new Date();
    $(this).text(t.relative(now,true));
  });
}

/*******************************************************************************
 * Main code.
 */

$(function(){
  $('form').each(function(){
    $(this).find('input[name=author]')
      .val($.cookie('author'))
      .change(function(){
	$.cookie('author', $(this).val(), { expires: 365, path: '/' });
      });
  });
  $('.hint').each(function (){
    var var_2 = $(this);
    var_2.css('height','1em');
    var_2.css('overflow','hidden');
    var_2.parent().css('cursor','pointer');
    var_2.parent().toggle(function (){
      var_2.css('height','auto');
      return false;
    },function (){
      var_2.css('height','1em');
      var_2.css('overflow','hidden');
      return false;

    });
    return true;
  });
  $('.paste-nav').each(function (){
    var var_6 = $('<a href="">Expand</a>');
    var var_7 = $(this);
    var_7.prepend(' - ');
    var_7.prepend(var_6);
    var var_8 = var_7.siblings('.paste-specs');
    var_8.css('display','none');
    var_6.text('Expand');
    var_6.toggle(function (){
      var_8.css('display','block');
      var_6.text('Collapse');
      return false;

    },function (){
      var_8.css('display','none');
      var_6.text('Expand');
      return false;

    });
    return true;
  });
  refreshDates();
  setInterval(refreshDates,1000);
});
