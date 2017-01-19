'use strict'

exports['dataTransfer'] = function (e) { return e.dataTransfer; }

exports['dataTransferR'] = function (e) { return e.dataTransfer; }

exports['path'] = function (file) { return file.path; }

exports['intToString'] = function (n) { return n + ''; }


exports['onceReadable'] = function (s) {
    return function (f) {
	return function () {
	    s.once('readable', f );
	}
    }
}
