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

const serverFile = "./server.elm"
const htaccessFile = "./htaccess.elm"

// Don't modify, this will be replaced by the content of 'server.elm'
const defaultServerContent = "<html><head></head><body>Server not available.</body></html>";

const defaultHtAccessFileContent = `if Regex.matchIn """\\.\\.(?:/|\\\\)|(?:/|\\\\)\\.\\.|^\\.\\.$""" path then False else True`

function readServerFile() {
  if(fs.existsSync(serverFile)) {
    return fs.readFileSync(serverFile, "utf8");
  } else
    return defaultServerContent;
}

function readHtAccessFile() {
  if(fs.existsSync(htaccessFile)) {
    return fs.readFileSync(htaccessFile, "utf8");
  } else
    return defaultHtAccessFileContent;
}

const sns = require("sketch-n-sketch");
sns.params = sns.params || {};
sns.params.delayedWrite = true;
sns.fileOperations = sns.fileOperations || [];

function evaluateToHtml(path, env, source) {
  var result = sns.objEnv.string.evaluate(env)(source);
  if(result.ctor == "Ok") {
    var out = sns.valToHTMLSource(result._0)
    if(out.ctor == "Ok") {
      return out;
    } else {
      return { ctor: "Err", _0: "Error while converting the result to HTML source file: " + out._0}
    }
  } else {
    return { ctor: "Err", _0: `Error while interpreting ${path}: ` + result._0}
  }
}

function lazyListToList(ll) {
  var x = [];
  while(sns.lazyList.nonEmpty(ll)) {
    x.push(sns.lazyList.head(ll));
    ll = sns.lazyList.tail(ll);
  }
  return x;
}

cachedSolutions = {
  key: {
     timestamp: 0,
     computed: [["html page", "environment overrides", "fileOperations"],
                ["html page 2", "environment overrides 2", "fileOperations 2..."]],
     remaining: "false, or a LazyList whose head element is last computed solution. Call getOneSolution(this.path, this.source, sns.lazyList.tail(remaining)) on it to get one more solution if it exists",
     path: "The path that is being computed",
     source: "The original source file of the server. Maybe be overwritten in fileOperations"
  }
}

// Retrieves the given solution by 1-based index from the set of solutions
// If the solution is the last computed, computes remaining solutions
function getSolutionByNum(solutionSet, num) {
  if(solutionSet.computed.length >= num && num >= 1) {
    if(solutionSet.computed.length == num) { // If we select the last computed solution, we checked if we can compute more solutions.
      if(solutionSet.remaining !== false) {
        sns.fileOperations = [];
        console.log("Checking for ambiguity #" + (num + 1));
        var lt = sns.lazyList.tail(solutionSet.remaining);
        var newSolution = getOneSolution(solutionSet.path, solutionSet.source, lt);
        if(newSolution === false) {
          console.log("No more ambiguity");
          solutionSet.remaining = false;
        } else {
          console.log("Ambiguity #" + (num + 1) + " found");
          solutionSet.remaining = lt;
          solutionSet.computed.push(newSolution);
        }
      }
    }
    return solutionSet.computed[num - 1];
  } else {
    console.log(`Requested invalid solution number ${num}. Returning the first`)
    return solutionSet.computed[0];
  }
}

function uniqueKey() {
  var potentialkey = +(new Date());
  while(typeof cachedSolutions["" + potentialkey] != "undefined") {
    potentialkey++;
  }
  return "" + potentialkey;
}

function envToOverrides(env) { return env.vars; }

function getOneSolution(path, source, allSolutions) {
  if(sns.lazyList.isEmpty(allSolutions)) return false;
  var {_0: newEnv, _1: newSource} = sns.lazyList.head(allSolutions);
  if(newSource != source) { // server file itself modified from the update method
    sns.fileOperations = sns.fileOperations || [];
    sns.fileOperations.push([ 'write',
      { _1: serverFile,
        _2: newSource}]);
  }
  var fo = sns.fileOperations;
  var evaluated = evaluateToHtml(path, newEnv, newSource);
  sns.fileOperations = [];
  return [evaluated, envToOverrides(newEnv), fo]; 
  // Evaluates everything given the temporary context of writing the files.
}

// Apply the given operations to the file system. TODO: Merge different writes to a single file.
function applyOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var [kind, action] = operations[i];
    if(kind == "write") {
      var {_1: path, _2: content} = action;
      fs.writeFileSync(path, content, "utf8");
    } else if (kind == "delete") {
      var path = action;
      fs.unlinkSync(path);
    }
  }
}

// Returns a [Result of string containing the requested page, new overrides]
// If newvalue is defined, performs an update before returning the page.
function loadpage(path, overrides, newvalue) {
  // __dirname = path.resolve(); // If in the REPL
  if(typeof overrides != "object") overrides = {};
  var source =  readServerFile();
  var env = { vars: overrides, path: path };
  
  if(typeof newvalue == "undefined") {
    return [evaluateToHtml(path, env, source), overrides];
  } else { // We update the page and re-render it.
    var newVal = sns.nativeToVal(newvalue);
    sns.fileOperations = [];
    console.log("Started to update...");
    var result = sns.objEnv.string.update(env)(source)(newVal);
    console.log("Update finished (first solution)");
    if(result.ctor == "Ok") {
      var allSolutions = result._0;
      // Instead of iterating through all the solutions, just detect if there is an ambiguity.
      var solution = getOneSolution(path, source, allSolutions);
      if(solution === false) {
        return [{ctor: "Err", _0: "Empty list of solutions"}];
      } else {
        sns.fileOperations = []; // Always do this before taking the tail of a stream of update solutions.
        console.log("Checking for ambiguities");
        var allSolutionsTail = sns.lazyList.tail(allSolutions);
        var solution2 = getOneSolution(path, source, allSolutionsTail);
        if(solution2 === false) { // No ambiguity, we can immediately process the change.
          console.log("No ambiguity");
          return solution;
        } else {
          console.log("Ambiguity found");
          var solutionKey = uniqueKey();
          var cachedSolution = {
              timestamp: (+ new Date()),
              computed: [solution, solution2],
              remaining: allSolutionsTail,
              path: path,
              source: source
          }
          cachedSolutions[solutionKey] = cachedSolution;
          return solution.concat(solutionKey);
        }
      }
    } else return [result];
  }
}

const server = http.createServer((request, response) => {
  var urlParts = url.parse(request.url, parseQueryString=true);
  var path = urlParts.pathname.substring(1); // Without the slash.
  var accessResult = sns.objEnv.string.evaluate({path:path,method:request.method})(readHtAccessFile());
  var access = sns.process(accessResult)(sns.valToNative);
  var header = 'text/html; charset=utf-8';
  if(access.ctor == "Err") {
    console.log("Error in htaccess", access._0);
    response.setHeader('Content-Type', header);
    response.statusCode = 500;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>htaccess.elm internal Error report</h1><pre style="white-space:pre-wrap">${access._0}</pre></div></body></html>`);
  } else if(access._0) {
    if(request.method == "GET") {
      var header = path.endsWith(".ico") ? "image/ico" : header;
      var header = path.endsWith(".jpg") ? "image/jpg" : header;
      var header = path.endsWith(".gif") ? "image/gif" : header;
      var header = path.endsWith(".png") ? "image/png" : header;
      var header = path.endsWith(".svg") ? "image/svg+xml" : header;
      var header = path.endsWith(".css") ? "text/css; charset=utf-8" : header;
      if(!header.startsWith("image/") && !header.startsWith("text/css")) {
        var [htmlContent] = loadpage(path, urlParts.query);
        response.setHeader('Content-Type', header);
        response.statusCode = 200;
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.end(htmlContent._0);
        }
      } else {
        response.setHeader('Content-Type', header);
        var content = fs.readFileSync("./" + path);
        response.statusCode = 200;
        response.end(content);
      }
    } else if(request.method == "POST") {
    var body = '';
    request.on('data', function (data) {
        body += data;
    });
    request.on('end', function () {
        var ambiguityKey = request.headers["ambiguity-key"];
        var numberOfSolutionsSoFar = 2; // Only if Ambiguity-Key is set.
        var numSolutionSelected = 1;
        var htmlContent = {ctor:"Err", _0: "Not yet defined"};
        var newQuery = "{}";
        var fileOperations = [];
        if(ambiguityKey !== null && typeof ambiguityKey !== "undefined") {
          var selectAmbiguityStr = request.headers["select-ambiguity"];
          if(selectAmbiguityStr !== null && typeof selectAmbiguityStr !== "undefined") {
            numSolutionSelected = JSON.parse(selectAmbiguityStr);
            var solutionSet = cachedSolutions[ambiguityKey];
            if(typeof solutionSet != "undefined") {
              [htmlContent, newQuery, fileOperations] = getSolutionByNum(solutionSet, numSolutionSelected);
              numberOfSolutionsSoFar = solutionSet.computed.length;
            } else {
              htmlContent = {ctor:"Err", _0: "Solution set not found"};
            }
          } else {
            var acceptAmbiguityStr = request.headers["accept-ambiguity"];
            if(acceptAmbiguityStr !== null && typeof acceptAmbiguityStr !== "undefined") {
              var acceptAmbiguity = JSON.parse(acceptAmbiguityStr);
              var solutionSet = cachedSolutions[ambiguityKey];
              [htmlContent, newQuery, fileOperations] = getSolutionByNum(solutionSet, acceptAmbiguity);
              ambiguityKey = undefined;
            } else {
              htmlContent = {ctor:"Err", _0: "Solution set not found."};
            }
          }
        } else {
          var pushedValue = JSON.parse(body);
          var [htmlContent, newQuery, fileOperations, ambiguityKey] = loadpage(path, urlParts.query, pushedValue);
        }
        response.statusCode = 201;
        response.setHeader('Content-Type', 'text/html; charset=utf-8');
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.setHeader('New-Query', JSON.stringify(newQuery));
          if(ambiguityKey != null && typeof ambiguityKey != "undefined" &&
             !path.endsWith(".html") && 
             urlParts.query["edit"] == "true") {
            response.setHeader('Ambiguity-Key', ambiguityKey);
            response.setHeader('Ambiguity-Number', JSON.stringify(numberOfSolutionsSoFar));
            response.setHeader('Ambiguity-Selected', JSON.stringify(numSolutionSelected));
          } else {
            applyOperations(fileOperations);
          }
          sns.fileOperations = [];
          response.end(htmlContent._0);
        }
        });
    } else {
      response.statusCode = 200;
      response.end("Unknown method");
    }
  } else {
    response.statusCode = 401;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Unauthorized access to ${path}</h1></div></body></html>`);
  }
});


// Load the Elm program into our namespace.
console.log("Editor Server ready!")
server.listen(port, hostname, () => {
    console.log(`Point your browser at http://${hostname}:${port}/?edit=true`);
  });