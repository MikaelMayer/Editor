
var params = process.argv.slice(2);

function getParam(x, defaultValue) {
  var param = params.find(elem => elem.startsWith(x));
  if(typeof param !== "undefined") {
    var returnValue = param.substring(x.length + 1);
    if(returnValue == "") return defaultValue;
    return returnValue;
  } else {
    return defaultValue;
  }
}

function existsParam(x) {
  var param = params.find(elem => elem == x);
  return typeof param !== "undefined";
}

function isBoolParamTrue(x) {
  return existsParam(x) || getParam(x, "false") == true;
}

function getNonParam() {
  return params.find(elem => !elem.startsWith("-"))
}

const fs = require("fs");
const https = require('https');
//const http = require('http');
const url = require('url');
const hostname = getParam("hostname", 'localhost');
var port = parseInt(getParam("port", "3000"));

// The default client id is suitable only for localhost:3000
var googleClientId = getParam("google-client-id", "844835838734-2iphm3ff20ephn906md1ru8vbkpu4mg8.apps.googleusercontent.com");

const getPort = require('get-port');

const serverFile = "./server.elm";
const htaccessFile = "./htaccess.elm";
var path =  getParam("--path",    "");
var question = getParam("--question", "true") == "true";
var autosave = getParam("--autosave", "false") == "true";

var fileToOpen = getNonParam();
if(fileToOpen) {
  if(fileToOpen.indexOf("/") == -1 && fileToOpen.indexOf("\\") == -1) {
    path = "";
  } else {
    path = fileToOpen.replace(/[\/\\][^\/\\]*$/g, "");
  }
  fileToOpen = fileToOpen.replace(/.*[\/\\](?=[^\/\\]*$)/g, "");
  question = false; // When opening a file, by default we should not ask questions.
  autosave = false; // When opening a file, by default we want to let the user save it.
}
var timeBeforeExit = 2000; // Number of ms after receiving the closing signal (if file open) to kill the server.

var defaultOptions = {
  edit:     getParam("--edit",     "true") == "true",
  autosave: autosave,
  question: question,
  admin:    isBoolParamTrue("--admin"),
  production:    isBoolParamTrue("--production"),
  path:     path,
  closeable: !(!(fileToOpen)),
  openbrowser: isBoolParamTrue("--openbrowser"),
  key: "localhost-key.pem",
  cert: "localhost.pem"
};

async function start() {

// Don't modify, this will be replaced by the content of 'server.elm'
const defaultServerContent = "<html><head></head><body>Server not available.</body></html>";

const defaultHtAccessFileContent = `if Regex.matchIn """\\.\\.(?:/|\\\\)|(?:/|\\\\)\\.\\.|^\\.\\.$""" path then False else
    not (Regex.matchIn """.*\\.pem""" path)`

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

function evaluateToHtml(path, env, serverFileContent) {
  var result = sns.objEnv.string.evaluate(env)(serverFileContent);
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
     computed: [["html page", "environment vars", "fileOperations"],
                ["html page 2", "environment vars 2", "fileOperations 2..."]],
     remaining: "false, or a LazyList whose head element is last computed solution. Call getOneSolution(this.path, this.serverFileContent, sns.lazyList.tail(remaining)) on it to get one more solution if it exists",
     path: "The path that is being computed",
     serverFileContent: "The original source file of the server. Maybe be overwritten in fileOperations"
  }
}

// Retrieves the given solution by 1-based index from the set of solutions
// If the solution is the last computed, computes remaining solutions
function getSolutionByNum(solutionSet, num) {
  if(solutionSet.computed.length >= num && num >= 1) {
    if(solutionSet.computed.length == num) { // If we select the last computed solution, we checked if we can compute more solutions.
      if(solutionSet.remaining !== false) {
        console.log("Checking for ambiguity #" + (num + 1));
        var lt = sns.lazyList.tail(solutionSet.remaining);
        var newSolution = getOneSolution(solutionSet.path, solutionSet.serverFileContent, lt);
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

function envToVars(env) { return env.vars; }

function getOneSolution(path, serverFileContent, allSolutions) {
  if(sns.lazyList.isEmpty(allSolutions)) return false;
  var {_0: newEnv, _1: newServerFileContent} = sns.lazyList.head(allSolutions);
  if(newServerFileContent != serverFileContent) { // server file itself modified from the update method
    var d =
      sns.process(sns.objEnv.string.evaluate({x: serverFileContent, y: newServerFileContent})(`__diff__ x y`))(sns.valToNative)
    var diffsServerFileContent = 
      d.ctor == "Ok" ? d._0 ? d._0.args ? d._0.args._1 ? d._0.args._1.args ? d._0.args._1.args._1 ? d._0.args._1.args._1 :
        false : false : false : false : false : false;

    newEnv.fileOperations.unshift(
      {"$t_ctor": "Tuple2",
       _1: serverFile,
       _2: {"$d_ctor": "Write",
       args: {_1: serverFileContent, _2: newServerFileContent, _3: diffsServerFileContent}}
       });
  }
  var fo = newEnv.fileOperations;
  var evaluated = evaluateToHtml(path, newEnv, newServerFileContent);
  return [evaluated, envToVars(newEnv), fo]; 
  // Evaluates everything given the temporary context of writing the files.
}

// Apply the given operations to the file system. TODO: Merge different writes to a single file.
function applyOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var {_1: path, _2: action} = operations[i];
    if(action["$d_ctor"] == "Write") {
      fs.writeFileSync(path, action.args._2, "utf8");
    } else if(action["$d_ctor"] == "Create") {
      // TODO: Create the path if necessary
      fs.writeFileSync(path, action.args._1, "utf8");
    } else if(action["$d_ctor"] == "Rename") {
      if(path.startsWith("/")) path = path.substring(1);
      var newName = action.args._1;
      if(newName.startsWith("/")) newName = newName.substring(1);
      fs.renameSync(path, newName);
    } else if(action["$d_ctor"] == "Delete") {
      fs.unlinkSync(path);
    } else {
      console.log("unrecognized action:", action);
    }
  }
}

function stringDiffSummary(oldString, newString, stringDiffs) {
  if(stringDiffs["$d_ctor"] == "Nothing") return "";
  if(stringDiffs["$d_ctor"] == "Just") stringDiffs = stringDiffs.args._1;
  var listStringDiffs = stringDiffs.args._1; // It's a VStringDiffs
  var offset = 0;
  var summary = "";
  for(var i = 0; i < listStringDiffs.length; i++) {
    var {args: {_1: start, _2: end, _3: replaced}} = listStringDiffs[i];
    var removed = oldString.substring(start, end);
    var inserted = newString.substring(start + offset, start + offset + replaced);
    var beforeRemoved = oldString.substring(0, start);
    var linesBeforeRemoved = beforeRemoved.split(/\r\n|\r|\n/);
    var lineNumber = linesBeforeRemoved.length;
    var charNumber = linesBeforeRemoved[linesBeforeRemoved.length - 1].length + 1;
    summary += "L" + lineNumber + "C" + charNumber + ", "
    if(removed == "")
      summary += "inserted '" + inserted + "'";
    else if(inserted == "")
      summary += "removed '" + removed + "'";
    else
      summary += "removed '" + removed + "', inserted '"+ inserted +"'";
    offset += replaced - (end - start);
  }
  return summary;
}

function relativePath(filepath) {
  return filepath.substring(path.length);
}

function fileOperationSummary(operations) {
  if(operations == null) return "";
  var summary = "";
  for(var i = 0; i < operations.length; i++) {
    var {_1: filepath, _2: action} = operations[i];
    if(summary != "") summary += "\n";
    if(action["$d_ctor"] == "Write") {
      summary += "Modify " + relativePath(filepath) + ", " + stringDiffSummary(action.args._1, action.args._2, action.args._3);
    } else if(action["$d_ctor"] == "Create") {
      summary += "Created " + relativePath(filepath);
    } else if(action["$d_ctor"] == "Rename") {
      summary += "Renamed " + relativePath(filepath) + " to " + action.args._1;
    } else if(action["$d_ctor"] == "Delete") {
      summary += "Deleted " + relativePath(filepath);
    } else {
      console.log("unrecognized action:", action);
    }
  }
  return summary;
}

// Returns a [Result of string containing the requested page, new overrides]
// If newvalue is defined, performs an update before returning the page.
function loadpage(path, vars, user, newvalue) {
  // __dirname = path.resolve(); // If in the REPL
  if(typeof vars != "object") vars = {};
  var serverFileContent = readServerFile();
  var env = {
      googleClientId: googleClientId,
      vars: vars,
      user: toLeoQuery(user || {}),
      defaultOptions: toLeoQuery(defaultOptions),
      path: path,
      fileOperations: [] };
  
  if(typeof newvalue == "undefined") {
    return [evaluateToHtml(path, env, serverFileContent), vars];
  } else { // We update the page and re-render it.
    var newVal = sns.nativeToVal(newvalue);
    console.log("Started to update...");
    var result = sns.objEnv.string.update(env)(serverFileContent)(newVal);
    console.log("Update finished (first solution)");
    if(result.ctor == "Ok") {
      var allSolutions = result._0;
      // Instead of iterating through all the solutions, just detect if there is an ambiguity.
      var solution = getOneSolution(path, serverFileContent, allSolutions);
      if(solution === false) {
        return [{ctor: "Err", _0: "Empty list of solutions"}];
      } else {
        console.log("Checking for ambiguities");
        var allSolutionsTail = sns.lazyList.tail(allSolutions);
        var solution2 = getOneSolution(path, serverFileContent, allSolutionsTail);
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
              serverFileContent: serverFileContent
          }
          cachedSolutions[solutionKey] = cachedSolution;
          return solution.concat(solutionKey);
        }
      }
    } else return [result];
  }
}

function toLeoQuery(query) {
  var result = [];
  for(key in query) {
    result.push({"$t_ctor": "Tuple2", _1: key, _2: query[key]});
  }
  return result;
}

var willkill = undefined;

var key = fs.existsSync(defaultOptions.key) ? fs.readFileSync(defaultOptions.key) : null;
var cert = fs.existsSync(defaultOptions.cert) ? fs.readFileSync(defaultOptions.cert) : null;
var httpsOptions = { key: key, cert: cert };
var protocol = key && cert ? "https" : "http";
var httpOrHttps = require(protocol);
if(protocol == "http") {
  console.log(`${defaultOptions.key} (--key) and/or ${defaultOptions.cert} (--cert) files missing. Starting http server instead of https.`);
}

const server = httpOrHttps.createServer(httpsOptions, (request, response) => {
  var urlParts = url.parse(request.url, parseQueryString=true);
  var query = toLeoQuery(urlParts.query);
  var path = decodeURIComponent(urlParts.pathname.substring(1)); // Without the slash.
  var accessResult = sns.objEnv.string.evaluate({path:path,method:request.method})(readHtAccessFile());
  var access = sns.process(accessResult)(sns.valToNative);
  var header = 'text/html; charset=utf-8';
  if(access.ctor == "Err") {
    console.log("Error in htaccess", access._0);
    response.setHeader('Content-Type', header);
    response.statusCode = 500;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>htaccess.elm internal Error report</h1><pre style="white-space:pre-wrap">${access._0}</pre></div></body></html>`);
  } else if(access._0) {
    if(typeof willkill != "undefined") {
      clearTimeout(willkill);
      willkill = undefined;
    }
    if(request.method == "GET") {
      var header = path.endsWith(".ico") ? "image/ico" : header;
      var header = path.endsWith(".jpg") ? "image/jpg" : header;
      var header = path.endsWith(".gif") ? "image/gif" : header;
      var header = path.endsWith(".png") ? "image/png" : header;
      var header = path.endsWith(".svg") ? "image/svg+xml" : header;
      var header = path.endsWith(".css") ? "text/css; charset=utf-8" : header;
      if(!header.startsWith("image/") && !header.startsWith("text/css")) {
        var [htmlContent] = loadpage(path, query, undefined);
        response.setHeader('Content-Type', header);
        response.statusCode = 200;
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`);
        } else {
          response.end(htmlContent._0);
        }
      } else {
        response.setHeader('Content-Type', header);
        let expectedFilePath = (defaultOptions.path == "" ? "." : defaultOptions.path) + "/" + path;
        if(fs.existsSync(expectedFilePath)) {
          var content = fs.readFileSync(expectedFilePath);
          response.statusCode = 200;
          response.end(content);
        } else {
          response.statusCode = 404;
          response.end(`<html>body>${path} not found</body></html>`);
        }
      }
    } else if(request.method == "POST") {
      const chunks = [];
      request.on('data', chunk => chunks.push(chunk));
      request.on('end', function () {
        var allChunks = Buffer.concat(chunks);
        if(defaultOptions.closeable && request.headers["close"]) {
          willkill = setTimeout(() => process.exit(), timeBeforeExit); // Kills the server after 2 seconds if the webpage is not requested.
          response.statusCode = 200;
          response.end('');
          return;
        } else if(request.headers["write-file"]) { // With this and with the correct permissions, we can overwrite any file.
          console.log("going to write file");
          // Just a file that we write on disk.
          var imageType = request.headers["write-file"];
          var imageLocation = path;
          console.log("going to write image file to ", imageLocation);
          fs.writeFileSync(imageLocation, allChunks);
          response.statusCode = 201;
          response.end('');
          return;
        }
        
        var continueWithLoading = (userdata) => {
          var canAskQuestion = (request.headers["question"] || urlParts.query["question"] || (defaultOptions.questions ? "true" : "false")) == "true";
          var body =  allChunks.toString();
          var ambiguityKey = request.headers["ambiguity-key"];
          var numberOfSolutionsSoFar = 2; // Only if Ambiguity-Key is set.
          var numSolutionSelected = 1;
          var htmlContent = {ctor:"Err", _0: "Not yet defined"};
          var newQuery = [];
          var fileOperations = [];
          var ambiguitiesSummary = [];
          if(ambiguityKey !== null && typeof ambiguityKey !== "undefined") {
            var selectAmbiguityStr = request.headers["select-ambiguity"];
            if(selectAmbiguityStr != null) {
              numSolutionSelected = JSON.parse(selectAmbiguityStr);
              var solutionSet = cachedSolutions[ambiguityKey];
              if(typeof solutionSet != "undefined") {
                [htmlContent, newQuery, fileOperations] = getSolutionByNum(solutionSet, numSolutionSelected);
                numberOfSolutionsSoFar = solutionSet.computed.length;
                ambiguitiesSummary = solutionSet.computed.map(a => fileOperationSummary(a[2]));
              } else {
                htmlContent = {ctor:"Err", _0: "Solution set not found"};
              }
            } else {
              var acceptAmbiguityStr = request.headers["accept-ambiguity"];
              if(acceptAmbiguityStr != null) {
                var acceptAmbiguity = JSON.parse(acceptAmbiguityStr);
                var solutionSet = cachedSolutions[ambiguityKey];
                [htmlContent, newQuery, fileOperations] = getSolutionByNum(solutionSet, acceptAmbiguity);
                ambiguityKey = undefined;
              } else {
                var cancelAmbiguityStr = request.headers["cancel-ambiguity"];
                if(cancelAmbiguityStr != null) {
                  ambiguityKey = undefined;
                  [htmlContent, newQuery, fileOperations] = loadpage(path, query, userdata);
                  fileOperations = [];
                } else {
                  htmlContent = {ctor:"Err", _0: "Solution set not found."};
                }
              }
            }
          } else if(request.headers["reload"]) {
            [htmlContent, newQuery, fileOperations] = loadpage(path, query, userdata);
          } else {
            var pushedValue = JSON.parse(body);
            [htmlContent, newQuery, fileOperations, ambiguityKey] = loadpage(path, query, userdata, pushedValue);
          }
          response.statusCode = 201;
          response.setHeader('Content-Type', 'text/html; charset=utf-8');
          if(htmlContent.ctor == "Err") {
            response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
          } else {
            response.setHeader('New-Query', JSON.stringify(newQuery));
            if(ambiguityKey != null && typeof ambiguityKey != "undefined" &&
               !path.endsWith(".html") && canAskQuestion &&
               (urlParts.query["edit"] == "true" || (urlParts.query["edit"] == null && defaultOptions.edit))) {
              var solutionSet = cachedSolutions[ambiguityKey];
              var ambiguityEnd = solutionSet.remaining === false;
              ambiguitiesSummary = solutionSet.computed.map(a => fileOperationSummary(a[2]));
              response.setHeader('Ambiguity-Key', ambiguityKey);
              response.setHeader('Ambiguity-Number', JSON.stringify(numberOfSolutionsSoFar));
              response.setHeader('Ambiguity-Selected', JSON.stringify(numSolutionSelected));
              response.setHeader('Ambiguity-Summaries', JSON.stringify(ambiguitiesSummary));
              response.setHeader('Ambiguity-End', ambiguityEnd ? "true" : "false");
            } else {
              if(fileOperations) {
                applyOperations(fileOperations);
                response.setHeader('Operations-Summary', encodeURI(fileOperationSummary(fileOperations)));
              }
            }
            response.end(htmlContent._0);
          }
        }
        
        var token = request.headers["id-token"];
        if(token) {
          const {OAuth2Client} = require('google-auth-library');
          const client = new OAuth2Client(googleClientId);
 
          async function verify() {
            const ticket = await client.verifyIdToken({
                idToken: token,
                audience: googleClientId // Array of client ids if multiple clients access the backend.
            });
            const userdata = ticket.getPayload();
            continueWithLoading(userdata); // Only authenticated users can access their information. Great!
          }
          verify().catch(err => {
            console.log(err);
            continueWithLoading(undefined);            
          });
          return;
        } else {
          continueWithLoading(undefined);
        }
      });
    } else {
      response.statusCode = 400;
      response.end("Unknown method");
    }
  } else {
    response.statusCode = 401;
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Unauthorized access to ${path}</h1></div></body></html>`);
  }
});

port = await getPort({host: hostname, port: [port, 3000, 3001, 3002, 3003, 3004]})

// Load the Elm program into our namespace.
server.listen(port, hostname, () => {
  console.log("Editor Server serving path " + defaultOptions.path);
  if(defaultOptions.edit) {
    console.log("Edit mode:     activated.    (toggle option: 'edit=false')");
  } else {
    console.log("Edit mode:     deactivated.  (toggle option: 'edit=true')");
  }
  if(defaultOptions.autosave) {
    console.log("Autosave mode: activated.    (toggle option: 'autosave=false')");
  } else {
    console.log("Autosave mode: deactivated.  (toggle option: 'autosave=true')");
  }
  if(defaultOptions.question) {
    console.log("Questions:     activated.    (toggle option: 'question=false')");
  } else {
    console.log("Questions:     deactivated.  (toggle option: 'question=true')");
  }
  console.log("To toggle any of these options in the browser, join these toggle options using '&', prefix this with '?', and append the result to any URL, ");
  console.log(`Point your browser at ${protocol}://${hostname}:${port}`);
});

if(fileToOpen) {
  var opn = require('opn');

  // opens the url in the default browser 
  opn(protocol + "://" + hostname + ":" + port + "/" + fileToOpen);
} else if(defaultOptions.openbrowser) {
  var opn = require('opn');

  // opens the url in the default browser 
  opn(protocol + "://" + hostname + ":" + port);
}
} // async declaration of start()

// Never called when starting the server from command-line.
module.exports = function(requireOptions) {
  if(!requireOptions) {
    start();
    return;
  } else {
    for(var k in requireOptions) {
      defaultOptions[k] = requireOptions[k];
    }
    start();
    return;
  }
}

/*REMOVE_FOR_NPM_INCLUDE*/
start();
/*END_REMOVE*/
