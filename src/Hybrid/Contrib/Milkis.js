"use strict";

var Milkis = require("../Milkis/foreign.js");

exports.node = "node";
exports.browser = "browser";

exports.jsEngine = typeof window !== 'undefined'? exports.browser : exports.node;

exports.fetchImpl = (function() {
  switch (exports.jsEngine) {
    case exports.browser:
      return window.fetch;
    case exports.node:
      return eval('require')("node-fetch").default;
  }
})();

exports.saveAttachmentImpl = (function() {
  var falsePromise = new Promise(function(resolve) {
      resolve(false);
  });

  var browserDownload = function(filename, contentType, arr) {
    blob = new Blob([arr], { type: contentType });

    if (typeof window.navigator.msSaveBlob !== 'undefined') {
        // IE workaround for "HTML7007: One or more blob URLs were revoked by closing the blob for which they were created. These URLs will no longer resolve as the data backing the URL has been freed."
        window.navigator.msSaveBlob(blob, filename);
    } else {
      var URL = window.URL || window.webkitURL;
      var downloadUrl = URL.createObjectURL(blob);
      if (filename) {
          // use HTML5 a[download] attribute to specify filename
          var a = document.createElement("a");
          // safari doesn't support this yet
          if (typeof a.download === 'undefined') {
              window.location.href = downloadUrl;
          } else {
              a.href = downloadUrl;
              a.download = filename;
              document.body.appendChild(a);
              a.click();
              a.remove();
          }
      } else {
          window.location.href = downloadUrl;
      }

      setTimeout(function () {
        URL.revokeObjectURL(downloadUrl);
      }, 100); // cleanup
    }
    return true;
  };

  var nodeDownload = function(filename, contentType, arr) {
    throw new Error("Attachment download in node not implemented yet!");
  };

  var download = exports.jsEngine === exports.browser? browserDownload : nodeDownload;

  return function(response) {
    return function() {
      // check for a filename
      var filename = "";
      var blob, contentType;
      var headers = Milkis.headersImpl(response);
      var chunks = [], reader;

      var disposition = headers['content-disposition'];
      if (disposition && disposition.indexOf('attachment') !== -1) {
          var filenameRegex = /filename[^;=\n]*=((['"]).*?\2|[^;\n]*)/;
          var matches = filenameRegex.exec(disposition);
          if (matches != null && matches[1]) filename = matches[1].replace(/['"]/g, '');
      } else {
        return falsePromise;
      }
      return response.arrayBuffer().then(function(arr) {
        var contentType = headers['content-type'];
        return download(filename, contentType, arr);
      });
    };
  };
})();

