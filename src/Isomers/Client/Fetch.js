
var JSEngine = require('../Isomers.JSEngine/foreign');

exports.unsafePatch = (function() {
  if(JSEngine.jsEngine == JSEngine.node) {
    var fetch = require('node-fetch');
    global.Headers = fetch.Headers;
    global.Request = fetch.Request;
    global.Response = fetch.Response;
    global.FetchError = fetch.FetchError;
    global.isRedirect = fetch.isRedirect;
  }
})();
