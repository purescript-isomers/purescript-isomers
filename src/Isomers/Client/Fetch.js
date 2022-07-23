exports.unsafePatch = (function() {
  if(typeof window === 'undefined') {
    var fetch = require('node-fetch');
    global.fetch = fetch;
    global.Headers = fetch.Headers;
    global.Request = fetch.Request;
    global.Response = fetch.Response;
    global.FetchError = fetch.FetchError;
    global.isRedirect = fetch.isRedirect;
  }
})();
