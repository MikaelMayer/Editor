
const fs = require("fs");
//const https = require('https');
const http = require('http');
/*var options = {
  key: fs.readFileSync('keys/key.pem'),
  cert: fs.readFileSync('keys/cert.pem')
};*/
const url = require('url');
const hostname = '127.0.0.1';
const port = 3000;

const sns = require("sketch-n-sketch");

clientscript = function(editdelay) {
  return [
  ["script",
  [],
  [["TEXT", `
  function initSigninV2() {
    console.log("platform.js loaded");
  }
  
  function domNodeToNativeValue(n) {
      if(n.nodeType == "3") {
        return ["TEXT", n.textContent];
      } else {
        var attributes = [];
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          var value = n.attributes[i].value;
          if(key == "style") {
            value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2);
          }
          attributes.push([key, value]);
        }
        var children = [];
        for(i = 0; i < n.childNodes.length; i++) {
          children.push(domNodeToNativeValue(n.childNodes[i]));
        }
        return [n.tagName.toLowerCase(), attributes, children];
      }
    }
    function replaceContent(NC) {
      document.open();
      document.write(NC);
      document.close();
    }
    
    var t = undefined;
  
    function handleMutations(mutations) {
      // Send in post the new HTML along with the URL
      console.log("mutations", mutations);
      if(typeof t !== "undefined") {
        clearTimeout(t);
      }
      t = setTimeout(function() {
        t = undefined;
        console.log("sending post request");
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.onreadystatechange = function () {
            if (xmlhttp.readyState == XMLHttpRequest.DONE) {
              //console.log("going to replace with");
              //console.log(xmlhttp.responseText);
              replaceContent(xmlhttp.responseText);
              var newQueryStr = xmlhttp.getResponseHeader("New-Query");
              if(newQueryStr !== null) {
                var newQuery = JSON.parse(newQueryStr);
                var newQueryKeys = Object.keys(newQuery);
                var strQuery = "";
                for(var i = 0; i < newQueryKeys.length; i++) {
                  var key = newQueryKeys[i];
                  strQuery = strQuery + (i == 0 ? "?" : "&") + key + "=" + newQuery[key];
                }
                window.history.replaceState({}, "Current page", strQuery);
              }
            }
        };
        xmlhttp.open("POST", location.pathname + location.search);
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));
      }, ${editdelay})
    }
  
    if (typeof outputValueObserver !== "undefined") {
      // console.log("outputValueObserver.disconnect()");
      outputValueObserver.disconnect();
      
    }
    

    setTimeout(function() {
      outputValueObserver = new MutationObserver(handleMutations);
      outputValueObserver.observe
       ( document.body.parentElement
       , { attributes: true
         , childList: true
         , characterData: true
         , attributeOldValue: true
         , characterDataOldValue: true
         , subtree: true
         }
       )
     }, 10)`]]]];
}

// Returns a [Result of string containing the requested page, new overrides]
// If newvalue is defined, performs an update before returning the page.
function loadpage(name, overrides, newvalue) {
  // __dirname = path.resolve(); // If in the REPL
  var source = "";
  if(typeof overrides != "object") overrides = {};
  var env = { vars: overrides, clientscript: clientscript(1000), sourcefile: name };
  var envToOverrides = function (env) {
    return env.vars;
  }
  try {
    source =  fs.readFileSync(__dirname + "/" + name, "utf8");  
  } catch (err) {
    return [{ ctor: "Err", _0: `File ${name} does not exists`}, overrides];
  }
  function evaluate(env, source) {
    var result = sns.evaluateEnv(env)(source);
    if(result.ctor == "Ok") {
      var out = sns.valToHTMLSource(result._0)
      if(out.ctor == "Ok") {
        return out;
      } else {
        return { ctor: "Err", _0: "Error while converting the result to HTML source file: " + out._0}
      }
    } else {
      return { ctor: "Err", _0: `Error while interpreting ${name}: ` + result._0}
    }
  }
  
  if(typeof newvalue == "undefined") {
    return [evaluate(env, source), overrides];
  } else { // We update the page and re-render it.
    var newVal = sns.nativeToVal(newvalue);
    var result = sns.updateEnv(env)(source)(newVal);
    if(result.ctor == "Ok") {
      var newEnvSource = result._0._0; // TODO: If toolbar, interact to choose ambiguity
      var newEnv = newEnvSource._0;
      var newSource = newEnvSource._1;
      if(newSource != source) {
        fs.writeFileSync(__dirname + "/" + name, newSource, "utf8");
      }
      try {
        newSource =  fs.readFileSync(__dirname + "/" + name, "utf8");  
      } catch (err) {
        return [{ ctor: "Err", _0: `File ${name} does not exists`}, overrides];
      }
      return [evaluate(newEnv, newSource), envToOverrides(newEnv)];
    } else return [result, overrides];
  }
}

const server = http.createServer((request, response) => {
  if(request.method == "GET") {
    var urlParts = url.parse(request.url, parseQueryString=true);
    var pathname = urlParts.pathname.substring(1); // Without the slash.
    if(pathname == "") pathname = "index.elm";
    var [htmlContent, newQueryDiscarded] = loadpage(pathname, urlParts.query);
    response.statusCode = 200;
    response.setHeader('Content-Type', 'text/html; charset=utf-8');
    if(htmlContent.ctor == "Err") {
      response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
    } else {
      response.end(htmlContent._0);
    }
  } else if(request.method == "POST") {
    var urlParts = url.parse(request.url, parseQueryString=true);
    var pathname = urlParts.pathname.substring(1); // Without the slash.
    if(pathname == "") pathname = "index.elm";
    var body = '';
    request.on('data', function (data) {
        body += data;
    });
    request.on('end', function () {
        var pushedValue = JSON.parse(body);
        response.statusCode = 200;
        var [htmlContent, newQuery] = loadpage(pathname, urlParts.query, pushedValue);
        response.setHeader('Content-Type', 'text/html; charset=utf-8');
        response.setHeader('New-Query', JSON.stringify(newQuery));
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.end(htmlContent._0);
        }
        });
    // Later, deal with update.
  } else {
    response.statusCode = 200;
    response.end("Unknown method");
  }
});


// Load the Elm program into our namespace.
console.log("Sketch-n-sketch Server ready !")
server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
  });