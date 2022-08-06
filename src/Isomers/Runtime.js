// Use this to detect deno as well:
// https://github.com/flexdinesh/browser-or-node


exports.caseRuntime = (function() {
  let browser = "browser";
  let nodeJS = "nodeJS";
  let deno = "deno";

  const isNode =
    typeof process !== "undefined" &&
    process.versions != null &&
    process.versions.node != null;

  const isBrowser =
    typeof window !== "undefined" && typeof window.document !== "undefined";

  const isDeno = typeof Deno !== "undefined" && typeof Deno.core !== "undefined";

  return function(onNode, onBrowser, onDeno, onSomethingElse) {
    if(isNode) {
      return onNode(nodeJS);
    } else if (isBrowser) {
      return onBrowser(browser);
    } else if (isDeno) {
      return onDeno(deno);
    } else {
      return onSomethingElse;
    }
  }
})();

