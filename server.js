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

const serverFile = __dirname + "/bin/server.elm"
const htaccessFile = __dirname + "/bin/htaccess.elm"

const sns = require("sketch-n-sketch");
sns.params = sns.params || {};
sns.params.delayedWrite = true;
sns.fileOperations = sns.fileOperations || [];

function evaluateToHtml(name, env, source) {
  var result = sns.objEnv.string.evaluate(env)(source);
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

// Returns a [Result of string containing the requested page, new overrides]
// If newvalue is defined, performs an update before returning the page.
function loadpage(name, overrides, newvalue) {
  // __dirname = path.resolve(); // If in the REPL
  var source = "";
  if(typeof overrides != "object") overrides = {};
  try {
    source =  fs.readFileSync(serverFile, "utf8");  
  } catch (err) {
    return [{ ctor: "Err", _0: `File bin/server.elm does not exist.`}, overrides];
  }
  var env = { vars: overrides, pagename: name };
  var envToOverrides = function (env) {
    return env.vars;
  }
  
  if(typeof newvalue == "undefined") {
    return [evaluateToHtml(name, env, source), overrides];
  } else { // We update the page and re-render it.
    var newVal = sns.nativeToVal(newvalue);
    var result = sns.objEnv.string.update(env)(source)(newVal);
    if(result.ctor == "Ok") {
      var newEnvSource = result._0._0; // TODO: If toolbar, interact to choose ambiguity
      var newEnv = newEnvSource._0;
      var newSource = newEnvSource._1;
      if(newSource != source) { // source modified from the update method
        fs.writeFileSync(serverFile, newSource, "utf8");
      }
      console.log(sns.fileOperations);
      try { // The source might have been modified by actually writing to the file.
        newSource =  fs.readFileSync(__dirname + "/bin/server.elm", "utf8");  
      } catch (err) {
        return [{ ctor: "Err", _0: `File ${serverFile} does not exists`}, overrides];
      }
      return [evaluateToHtml(name, newEnv, newSource), envToOverrides(newEnv)];
    } else return [result, overrides];
  }
}

const server = http.createServer((request, response) => {
  var urlParts = url.parse(request.url, parseQueryString=true);
  var pathname = urlParts.pathname.substring(1); // Without the slash.
  if(pathname == "") pathname = "index.elm";
  var accessResult = sns.objEnv.string.evaluate({pagename:pathname,method:request.method})(fs.readFileSync(htaccessFile, "utf8"));
  var access = sns.process(accessResult)(sns.valToNative);
  response.setHeader('Content-Type', 'text/html; charset=utf-8');
  if(access.ctor == "Err") {
    console.log("Error in htaccess", access._0);
    response.statusCode = 500;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>htaccess.elm internal Error report</h1><pre style="white-space:pre-wrap">${access._0}</pre></div></body></html>`);
  } else if(access._0) {
    if(request.method == "GET") {
      var q = urlParts.query;
      if(pathname.endsWith(".elm")) {
        var [htmlContent, newQueryDiscarded] = loadpage(pathname, urlParts.query);
        response.statusCode = 200;
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.end(htmlContent._0);
        }
      } else {
        var content = fs.readFileSync(__dirname + "/" + pathname);
        response.statusCode = 200;
        response.end(content);
      }
    } else if(request.method == "POST") {
    var body = '';
    request.on('data', function (data) {
        body += data;
    });
    request.on('end', function () {
        var pushedValue = JSON.parse(body);
        response.statusCode = 201;
        var [htmlContent, newQuery] = loadpage(pathname, urlParts.query, pushedValue);
        response.setHeader('Content-Type', 'text/html; charset=utf-8');
        response.setHeader('New-Query', JSON.stringify(newQuery));
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.end(htmlContent._0);
        }
        });
    } else {
      response.statusCode = 200;
      response.end("Unknown method");
    }
  } else {
    response.statusCode = 401;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Unauthorized access to ${pathname}</h1></div></body></html>`);
  }
});


// Load the Elm program into our namespace.
console.log("Sketch-n-sketch Server ready !")
server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
  });