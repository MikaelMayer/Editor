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

const defaultServerContent = "-- input: pagename  The file to serve.\n-- input: vars:     URL query vars.\n-- output: the page, either raw or augmented with the toolbar and edit scripts.\npreludeEnv = __CurrentEnv__\n\neditdelay = 1000\n\nuserpermissions = {pageowner= True, admin= True}\n\npermissionToCreate = userpermissions.admin\n\npermissionToEditServer = userpermissions.admin -- should be possibly get from user authentication\n\ncanEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == \"true\"; _ -> False)\n\nserverOwned = Update.conditionalFreeze (not permissionToEditServer)\n\nsourcecontent = if pagename == \"server.elm\" || pagename == \"bin/server.elm\" then\n    \"\"\"<html><head></head><body>Sample server Elm</body></html>\"\"\"\n  else\n    nodejs.fileread pagename\n  |> Maybe.withDefaultReplace (\n    serverOwned \"\"\"<html><body>@(\n        if permissionToCreate then \"\"\"<span>@pagename does not exist yet. Modify this page to create it!</span>\"\"\" else \"\"\"<span>Error 404, @pagename does not exist</span>\"\"\"\n      )</body></html>\"\"\"\n  )\n\ncanEvaluate = vars |> case of {evaluate} -> evaluate; _ -> \"true\" \n  \nmain = (if canEvaluate == \"true\" then\n      if Regex.matchIn \"\"\"\\.html$\"\"\" pagename then\n        case Regex.extract \"\"\"^(?:(?!<html).)*([\\s\\S]*</html>)\\s*$\"\"\" sourcecontent of\n          Just [interpretableHtml] ->\n            __evaluate__ [] interpretableHtml\n          _ ->  Err \"\"\"@pagename is not a valid html file.\"\"\"\n      else if Regex.matchIn \"\"\"\\.elm$\"\"\" pagename then\n        __evaluate__ ((\"vars\", vars)::(\"pagename\", pagename)::preludeEnv) sourcecontent \n      else\n        Err \"\"\"Serving only .html and .elm files. Got @pagname\"\"\"\n    else\n    Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>\n  ) |> (case of\n  Err msg -> serverOwned <|\n    <html><head></head><body style=\"color:#cc0000\"><div style=\"max-width:600px;margin-left:auto;margin-right:auto\"><h1>Error report</h1><pre style=\"white-space:pre-wrap\">@msg</pre></div></body></html>\n  Ok page -> page)\n  |> case of\n      [\"html\", htmlattrs, htmlchildren] -> [\"html\", htmlattrs, htmlchildren |>\n        List.filter (case of\n          [_, _] -> False\n          _ -> True) |>\n        List.mapWithReverse identity (case of\n          [\"body\", bodyattrs, bodychildren] ->\n            [\"body\",\n              (if canEditPage then serverOwned [[\"contenteditable\", \"true\"]] else freeze []) ++\n                bodyattrs,\n              (if canEditPage then serverOwned [editionmenu, codepreview sourcecontent] else freeze []) ++ bodychildren ++ Update.sizeFreeze (serverOwned [<script>@editionscript</script>])]\n          x -> x\n        )]\n  --|> Update.debug \"main\"\n\neditionmenu = <menu id=\"themenu\" ignore-modifications=\"true\" class=\"edittoolbar\" contenteditable=\"false\">\n<style>\nmenu.edittoolbar {\n  display: block;\n  background: black;\n  color: white;\n  padding: 2px;\n}\nmenuitem.disabled {\n  color: #BBB;\n}\n#editor_codepreview {\n  display: none;\n}\n#editor_codepreview[ghost-visible=true] {\n  display: block;\n}\n</style>\n<menuitem>@pagename</menuitem>\n<menuitem class=\"disabled\"><button onclick=\"\"\"\nvar cp = document.getElementById(\"editor_codepreview\");\nif(cp !== null) {\n   cp.setAttribute(\"ghost-visible\", cp.getAttribute(\"ghost-visible\") == \"true\" ? \"false\": \"true\")\n}\"\"\">Display/hide source</button></menuitem>\n</menu>\n\ncodepreview sourcecontent = \n<div class=\"codepreview\" id=\"editor_codepreview\">\n  <textarea id=\"editor_codepreview_textarea\" save-attributes=\"scrollTop\"\n    style=\"width:100%;height:200px\" v=sourcecontent onchange=\"this.setAttribute('v', this.value)\">@sourcecontent</textarea>\n</div>\n    \neditionscript = \"\"\"function initSigninV2() {\n    console.log(\"platform.js loaded\");\n  }\n\n  function isGhostNode(elem) {\n    return elem.nodeType == 1 &&\n      (elem.tagName == \"GHOST\" || elem.getAttribute(\"isghost\") == \"true\");\n  }\n  function hasGhostAncestor(htmlElem) {\n    if(htmlElem == null) return false;\n    if(isGhostNode(htmlElem)) return true;\n    return hasGhostAncestor(htmlElem.parentNode);\n  }\n  function isGhostAttributeKey(name) {\n    return name.startsWith(\"ghost-\");\n  }\n  \n  // Save / Load ghost attributes after a page is reloaded.\n  // Same for some attributes\n  function saveGhostAttributes() {\n    var ghostModified = document.querySelectorAll(\"[ghost-visible]\");\n    var idGhostAttributes = [];\n    for(var i = 0; i < ghostModified.length; i++) {\n      var id = ghostModified[i].getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        idGhostAttributes.push([id,\n          {\"ghost-visible\": ghostModified[i].getAttribute(\"ghost-visible\")}]);\n      }\n    }\n    var elemsWithAttributesToSave = document.querySelectorAll(\"[save-attributes]\");\n    var idDynamicAttributes = [];\n    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {\n      var elem = elemsWithAttributesToSave[i];\n      var id = elem.getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        var toSave = elem.getAttribute(\"save-attributes\").split(\" \");\n        for(i in toSave) {\n          var key = toSave[i];\n          idDynamicAttributes.push([id, key, elem[key]])\n        }\n      }\n    }\n    return [idGhostAttributes, idDynamicAttributes];\n  }\n  function applyGhostAttributes(attrs) {\n    var [idGhostAttributes, idDynamicAttributes] = attrs;\n    for(var i in idGhostAttributes) {\n      var [id, attrs] = idGhostAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem !== null && typeof elem !== \"undefined\") {\n        for(key in attrs) {\n          elem.setAttribute(key, attrs[key]);\n        }\n      }\n    }\n    for(var i in idDynamicAttributes) {\n      var [id, key, value] = idDynamicAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem !== null && typeof elem !== \"undefined\") {\n        elem[key] = value;\n      }\n    }\n  }\n  \n  function domNodeToNativeValue(n) {\n      if(n.nodeType == \"3\") {\n        return [\"TEXT\", n.textContent];\n      } else {\n        var attributes = [];\n        for(var i = 0; i < n.attributes.length; i++) {\n          var key = n.attributes[i].name;\n          var value = n.attributes[i].value;\n          if(!isGhostAttributeKey(key)) {\n            if(key == \"style\") {\n              value = value.split(\";\").map(x => x.split(\":\")).filter(x => x.length == 2);\n            }\n            attributes.push([key, value]);\n          }\n        }\n        var children = [];\n        for(i = 0; i < n.childNodes.length; i++) {\n          if(!isGhostNode(n.childNodes[i])) {\n            children.push(domNodeToNativeValue(n.childNodes[i]));\n          }\n        }\n        return [n.tagName.toLowerCase(), attributes, children];\n      }\n    }\n    function replaceContent(NC) {\n      document.open();\n      document.write(NC);\n      document.close();\n    }\n    \n    var t = undefined;\n    \n    \n    function handleMutations(mutations) {\n      var onlyGhosts = true;\n      for(var i = 0; i < mutations.length && onlyGhosts; i++) {\n        // A mutation is a ghost if either\n        // -- The attribute starts with 'ghost-'\n        // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\n        // -- It is the modification of a node or an attribute inside a ghost node.\n        var mutation = mutations[i];\n        if(hasGhostAncestor(mutation.target)) continue;\n        if(mutation.type == \"attributes\") {\n          if(isGhostAttributeKey(mutation.attributeName)) {\n          } else {\n            onlyGhosts = false;\n          }\n        } else if(mutation.type == \"childList\") {\n          for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {\n            if(!hasGhostAncestor(mutation.addedNodes[j])) {\n              onlyGhosts = false;\n            }\n          }\n        } else {\n          onlyGhosts = false;\n        }\n      }\n      if(onlyGhosts) {\n        console.log(\"mutations are only ghosts, skipping\", mutations);\n        return;\n      } // Send in post the new HTML along with the URL\n      console.log(\"mutations\", mutations);\n      if(typeof t !== \"undefined\") {\n        clearTimeout(t);\n      }\n      t = setTimeout(function() {\n        t = undefined;\n        console.log(\"sending post request\");\n        var xmlhttp = new XMLHttpRequest();\n        xmlhttp.onreadystatechange = function () {\n            if (xmlhttp.readyState == XMLHttpRequest.DONE) {\n              //console.log(\"Received new content. Replacing the page.\");\n              var saved = saveGhostAttributes()\n              replaceContent(xmlhttp.responseText);\n              applyGhostAttributes(saved);\n              \n              var newQueryStr = xmlhttp.getResponseHeader(\"New-Query\");\n              var ambiguityKey = xmlhttp.getResponseHeader(\"Other-Solutions\");\n              if(ambiguityKey !== null && typeof ambiguityKey != \"undefined\") {\n                var newMenu = document.createElement(\"menuitem\");\n                newMenu.innerHTML = `<span style=\"color:red\" id=\"ambiguity-id\" v=\"${ambiguityKey}\">Ambiguity detected.</span> Ambiguity resolution coming soon.`;\n                newMenu.setAttribute(\"isghost\", \"true\")\n                document.getElementById(\"themenu\").append(newMenu);\n              }\n              if(newQueryStr !== null) {\n                var newQuery = JSON.parse(newQueryStr);\n                var newQueryKeys = Object.keys(newQuery);\n                var strQuery = \"\";\n                for(var i = 0; i < newQueryKeys.length; i++) {\n                  var key = newQueryKeys[i];\n                  strQuery = strQuery + (i == 0 ? \"?\" : \"&\") + key + \"=\" + newQuery[key];\n                }\n                window.history.replaceState({}, \"Current page\", strQuery);\n              }\n            }\n        };\n        console.log(\"sending modifications\");\n        xmlhttp.open(\"POST\", location.pathname + location.search);\n        xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\n        xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));\n      }, @editdelay)\n    }\n  \n    if (typeof outputValueObserver !== \"undefined\") {\n      // console.log(\"outputValueObserver.disconnect()\");\n      outputValueObserver.disconnect();\n      \n    }\n    \n\n    setTimeout(function() {\n      outputValueObserver = new MutationObserver(handleMutations);\n      outputValueObserver.observe\n       ( document.body.parentElement\n       , { attributes: true\n         , childList: true\n         , characterData: true\n         , attributeOldValue: true\n         , characterDataOldValue: true\n         , subtree: true\n         }\n       )\n     }, 10)\"\"\"\n\nmain";

function readServerFile() {
  if(fs.existsSync(serverFile)) {
    return fs.readFileSync(serverFile, "utf8");
  } else
    return defaultServerContent;
}

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
     computed: ["html page", "environment overrides", "fileOperations"],
     remaining: "lazyList of solutions. Call getOneSolution(this.name, this.source) on it to get one more solution",
     name: "The name that is being computed",
     source: "The source file of the server"
  }
}

function uniqueKey() {
  var potentialkey = 1000;
  while(typeof cachedSolutions[potentialkey] != "undefined") {
    potentialkey++;
  }
  return potentialkey;
}

function envToOverrides(env) { return env.vars; }

function getOneSolution(name, source, allSolutions) {
  if(sns.lazyList.isEmpty(allSolutions)) return false;
  var {_0: newEnv, _1: newSource} = sns.lazyList.head(allSolutions);
  if(newSource != source) { // server file itself modified from the update method
    sns.fileOperations = sns.fileOperations || [];
    sns.fileOperations.push([ 'write',
      { _1: serverFile,
        _2: newSource}]);
  }
  var fo = sns.fileOperations;
  var evaluated = evaluateToHtml(name, newEnv, newSource);
  sns.fileOperations = [];
  return [[evaluated, envToOverrides(newEnv), fo], sns.lazyList.tail(allSolutions)]; 
  // Evaluates everything given the temporary context of writing the files.
}

// Apply the given operations to the file system. TODO: Merge different writes to a single file.
function applyOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var [kind, action] = operations[i];
    if(kind == "write") {
      var {_1: name, _2: content} = action;
      fs.writeFileSync(name, content, "utf8");
    } else if (kind == "delete") {
      var name = action;
      fs.unlinkSync(name);
    }
  }
}

// Returns a [Result of string containing the requested page, new overrides]
// If newvalue is defined, performs an update before returning the page.
function loadpage(name, overrides, newvalue) {
  // __dirname = path.resolve(); // If in the REPL
  var source = "";
  if(typeof overrides != "object") overrides = {};
  try { source =  readServerFile(); }
    catch (err) { return [{ ctor: "Err", _0: `File bin/server.elm does not exist.`}, overrides]; }
  var env = { vars: overrides, pagename: name };
  
  if(typeof newvalue == "undefined") {
    return [evaluateToHtml(name, env, source), overrides];
  } else { // We update the page and re-render it.
    var newVal = sns.nativeToVal(newvalue);
    sns.fileOperations = sns.fileOperations || [];
    var result = sns.objEnv.string.update(env)(source)(newVal);
    console.log("update finished");
    if(result.ctor == "Ok") {
      console.log("update succeeded");
      var allSolutions = result._0;
      // Instead of iterating through all the solutions, just detect if there is an ambiguity.
      var mbSolution = getOneSolution(name, source, allSolutions);
      if(mbSolution === false) {
        return [{ctor: "Err", _0: "Empty list of solutions"}];
      } else {
        [solution, tailSolutions] = mbSolution;
        var mbSolution2 = getOneSolution(name, source, tailSolutions);
        if(mbSolution2 === false) { // No ambiguity, we can immediately process the change.
          return solution;
        } else {
          console.log("ambiguity detected");
          var solutionKey = uniqueKey();
          [solution2, tailSolutions2] = mbSolution2;
          var cachedSolution = {
              timestamp: (+ new Date()),
              computed: [solution, solution2],
              remaining: tailSolutions2,
              name: name,
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
  var pathname = urlParts.pathname.substring(1); // Without the slash.
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
      if(pathname.endsWith(".elm") || pathname.endsWith(".html")) {
        var [htmlContent] = loadpage(pathname, urlParts.query);
        response.statusCode = 200;
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.end(htmlContent._0);
        }
      } else {
        var content = fs.readFileSync("./" + pathname);
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
        var [htmlContent, newQuery, fileOperations, otherSolutionsKey] = loadpage(pathname, urlParts.query, pushedValue);
        response.setHeader('Content-Type', 'text/html; charset=utf-8');
        if(htmlContent.ctor == "Err") {
          response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Internal Error report</h1><pre style="white-space:pre-wrap">${htmlContent._0}</pre></div></body></html>`)
        } else {
          response.setHeader('New-Query', JSON.stringify(newQuery));
          if(typeof otherSolutionsKey != "undefined") {
            response.setHeader('Other-Solutions', JSON.stringify(otherSolutionsKey));
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
    response.end(`<html><body style="color:#cc0000"><div   style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Unauthorized access to ${pathname}</h1></div></body></html>`);
  }
});


// Load the Elm program into our namespace.
console.log("Sketch-n-sketch Server ready !")
server.listen(port, hostname, () => {
    console.log(`Server running at http://${hostname}:${port}/`);
  });