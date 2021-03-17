/* global exports, require */

"use strict";

exports.node = "node";
exports.browser = "browser";

exports.jsEngine = typeof window !== 'undefined'? exports.browser : exports.node;

