// module Router

'use strict';

exports.decodeURIComponent = function(component) {
	return decodeURIComponent(component);
};

exports.encodeURIComponent = function(component) {
	return encodeURIComponent(component);
};

exports.parseIntImpl = function(just) {
	return function(nothing) {
		return function(s) {
			var i = Number.parseInt(s);

			if (i) {
				return just(i);
			} else {
				return nothing(null);
			}
		};
	};
};
