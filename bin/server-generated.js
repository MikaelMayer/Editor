#!/usr/bin/env node

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
const defaultServerContent = "-- input: path  The file to serve.\n-- input: vars:     URL query vars.\n-- output: the page, either raw or augmented with the toolbar and edit scripts.\npreludeEnv = __CurrentEnv__\n\neditdelay = 1000\n\nuserpermissions = {pageowner= True, admin= True}\n\npermissionToCreate = userpermissions.admin\n\npermissionToEditServer = userpermissions.admin -- should be possibly get from user authentication\n\ncanEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == \"true\"; _ -> False)\n\nserverOwned = Update.conditionalFreeze (not permissionToEditServer)\n\npath = if nodejs.isdir path then\n       if nodejs.isfile <| path + \"index.html\" then path + \"index.html\"\n  else if nodejs.isfile <| path + \"/index.html\" then path + \"/index.html\"\n  else if nodejs.isfile <| path + \"index.elm\" then path + \"index.elm\"\n  else if nodejs.isfile <| path + \"/index.elm\" then path + \"/index.elm\"\n  else if nodejs.isfile <| path + \"README.md\" then path + \"README.md\"\n  else if nodejs.isfile <| path + \"/README.md\" then path + \"/README.md\"\n  else path\n  else path\n\nsourcecontent = if path == \"server.elm\" then\n    \"\"\"<html><head></head><body>Sample server Elm</body></html>\"\"\"\n  else\n    if nodejs.isdir path then\n      \"\"\"<html><head></head><body><h1><a href=''>/@path</a></h1>\n      <ul>@@(List.map (\\name -> <li><a href=(path + \"/\" + name)>@@name</li>) (nodejs.listdir path))</ul>\n      Hint: place an <a href=(path + \"/index.html\")>index.html</a> or <a href=(path + \"/index.elm\")>index.elm</a> file to display something else than this page.</body></html>\"\"\"\n    else\n      if nodejs.isfile path && Regex.matchIn \"\"\"\\.(png|jpg|ico|gif|jpeg)$\"\"\" path then\n        \"\"\"<html><head><title>@path</title></head><body><img src=\"@path\"></body></html>\"\"\"\n      else\n        nodejs.fileread path\n      |> Maybe.withDefaultReplace (\n        serverOwned \"\"\"<html><head></head><body>@(\n            if permissionToCreate then \"\"\"<span>@path does not exist yet. Modify this page to create it!</span>\"\"\" else \"\"\"<span>Error 404, @path does not exist</span>\"\"\"\n          )</body></html>\"\"\"\n      )\n\ncanEvaluate = vars |> case of {evaluate} -> evaluate; _ -> \"true\" \n  \nmain = (if canEvaluate == \"true\" then\n      if Regex.matchIn \"\"\"\\.html$\"\"\" path then\n        case Regex.extract \"\"\"^(?:(?!<html)[\\s\\S])*((?=<html)[\\s\\S]*</html>)\\s*$\"\"\" sourcecontent of\n          Just [interpretableHtml] ->\n            __evaluate__ preludeEnv \"\"\"<raw>@interpretableHtml</raw>\"\"\"\n            |> Result.andThen (case of\n              [\"raw\", _, [htmlNode]] -> Ok htmlNode\n              result -> Err \"\"\"Html interpretation error: The interpretation of raw html did not work but produced @result\"\"\"\n            )\n          x ->  Err \"\"\"@path is not a valid html file. Interpreation got: @x\"\"\"\n      else if Regex.matchIn \"\"\"\\.md$\"\"\" path then\n        let markdownized = String.markdown sourcecontent in\n          case Html.parseViaEval markdownized of\n            x -> Ok <html><head></head><body>@x</body></html>\n      else if Regex.matchIn \"\"\"\\.elm$\"\"\" path || nodejs.isdir path then\n        __evaluate__ ((\"vars\", vars)::(\"path\", path)::preludeEnv) sourcecontent\n      else\n        Err \"\"\"Serving only .html, .md and .elm files. Got @path\"\"\"\n    else\n      Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>\n  ) |> (case of\n  Err msg -> serverOwned <|\n    <html><head></head><body style=\"color:#cc0000\"><div style=\"max-width:600px;margin-left:auto;margin-right:auto\"><h1>Error report</h1><pre style=\"white-space:pre-wrap\">@msg</pre></div></body></html>\n  Ok page -> page)\n  |> case of\n      [\"html\", htmlattrs, htmlchildren] -> [\"html\", htmlattrs, htmlchildren |>\n        List.filter (case of\n          [_, _] -> False\n          _ -> True) |>\n        List.mapWithReverse identity (case of\n          [\"body\", bodyattrs, bodychildren] ->\n            [\"body\",\n              (if canEditPage then serverOwned [[\"contenteditable\", \"true\"]] else freeze []) ++\n                bodyattrs,\n              (if canEditPage then serverOwned (editionmenu ++ [codepreview sourcecontent]) else freeze []) ++ bodychildren ++ Update.sizeFreeze (serverOwned [<script>@editionscript</script>])]\n          x -> x\n        )]\n      x-> <html><head></head><body>Not a valid html page: @(\"\"\"@x\"\"\")</body></html>\n  --|> Update.debug \"main\"\n\neditionmenu = [\n<menu id=\"themenu\" ignore-modifications=\"true\" class=\"edittoolbar\" contenteditable=\"false\">\n<style>\n#menumargin {\n  padding-top: 1em;\n}\nmenu {\n  position: fixed;\n  margin-top: 0px;\n  z-index: 10000;\n}\nmenu.edittoolbar {\n  display: block;\n  background: black;\n  color: white;\n  padding: 2px;\n}\nmenuitem.disabled {\n  color: #BBB;\n}\nmenuitem > .solution {\n  \n}\nmenuitem > .solution.selected {\n  outline: white 2px solid;\n}\nmenuitem > .solution:not(.selected):hover {\n  outline: #CCC 2px solid;\n  cursor: pointer;\n}\n#editor_codepreview {\n  display: none;\n  z-index: 9999;\n}\n#editor_codepreview[ghost-visible=true] {\n  display: block;\n}\n</style>\n<menuitem>@path</menuitem>\n<menuitem class=\"disabled\"><button onclick=\"\"\"\nvar cp = document.getElementById(\"editor_codepreview\");\nif(cp !== null) {\n   cp.setAttribute(\"ghost-visible\", cp.getAttribute(\"ghost-visible\") == \"true\" ? \"false\": \"true\")\n}\"\"\">Display/hide source</button></menuitem>\n</menu>,\n<div id=\"menumargin\"></div>]\n\ncodepreview sourcecontent = \n<div class=\"codepreview\" id=\"editor_codepreview\">\n  <textarea id=\"editor_codepreview_textarea\" save-attributes=\"scrollTop\"\n    style=\"width:100%;height:200px\" v=sourcecontent onchange=\"this.setAttribute('v', this.value)\">@(Update.softFreeze (if Regex.matchIn \"^\\r?\\n\" sourcecontent then \"\\n\" + sourcecontent else sourcecontent))</textarea>\n</div>\n    \neditionscript = \"\"\"function initSigninV2() {\n    console.log(\"platform.js loaded\");\n  }\n\n  function isGhostNode(elem) {\n    return elem.nodeType == 1 &&\n      (elem.tagName == \"GHOST\" || elem.getAttribute(\"isghost\") == \"true\");\n  }\n  function hasGhostAncestor(htmlElem) {\n    if(htmlElem == null) return false;\n    if(isGhostNode(htmlElem)) return true;\n    return hasGhostAncestor(htmlElem.parentNode);\n  }\n  function isGhostAttributeKey(name) {\n    return name.startsWith(\"ghost-\");\n  }\n  \n  // Save / Load ghost attributes after a page is reloaded.\n  // Same for some attributes\n  function saveGhostAttributes() {\n    var ghostModified = document.querySelectorAll(\"[ghost-visible]\");\n    var idGhostAttributes = [];\n    for(var i = 0; i < ghostModified.length; i++) {\n      var id = ghostModified[i].getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        idGhostAttributes.push([id,\n          {\"ghost-visible\": ghostModified[i].getAttribute(\"ghost-visible\")}]);\n      }\n    }\n    var elemsWithAttributesToSave = document.querySelectorAll(\"[save-attributes]\");\n    var idDynamicAttributes = [];\n    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {\n      var elem = elemsWithAttributesToSave[i];\n      var id = elem.getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        var toSave = elem.getAttribute(\"save-attributes\").split(\" \");\n        for(i in toSave) {\n          var key = toSave[i];\n          idDynamicAttributes.push([id, key, elem[key]])\n        }\n      }\n    }\n    return [idGhostAttributes, idDynamicAttributes];\n  }\n  function applyGhostAttributes(attrs) {\n    var [idGhostAttributes, idDynamicAttributes] = attrs;\n    for(var i in idGhostAttributes) {\n      var [id, attrs] = idGhostAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem !== null && typeof elem !== \"undefined\") {\n        for(key in attrs) {\n          elem.setAttribute(key, attrs[key]);\n        }\n      }\n    }\n    for(var i in idDynamicAttributes) {\n      var [id, key, value] = idDynamicAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem !== null && typeof elem !== \"undefined\") {\n        elem[key] = value;\n      }\n    }\n  }\n  \n  function domNodeToNativeValue(n) {\n      if(n.nodeType == \"3\") {\n        return [\"TEXT\", n.textContent];\n      } else if(n.nodeType == \"8\") {\n        return [\"COMMENT\", n.textContent];\n      } else {\n        var attributes = [];\n        for(var i = 0; i < n.attributes.length; i++) {\n          var key = n.attributes[i].name;\n          var value = n.attributes[i].value;\n          if(!isGhostAttributeKey(key)) {\n            if(key == \"style\") {\n              value = value.split(\";\").map(x => x.split(\":\")).filter(x => x.length == 2);\n            }\n            attributes.push([key, value]);\n          }\n        }\n        var children = [];\n        for(i = 0; i < n.childNodes.length; i++) {\n          if(!isGhostNode(n.childNodes[i])) {\n            children.push(domNodeToNativeValue(n.childNodes[i]));\n          }\n        }\n        return [n.tagName.toLowerCase(), attributes, children];\n      }\n    }\n    function replaceContent(NC) {\n      document.open();\n      document.write(NC);\n      document.close();\n    }\n    \n    var t = undefined;\n    \n    handleServerPOSTResponse = xmlhttp => function () {\n        if (xmlhttp.readyState == XMLHttpRequest.DONE) {\n          //console.log(\"Received new content. Replacing the page.\");\n          var saved = saveGhostAttributes();\n          replaceContent(xmlhttp.responseText);\n          applyGhostAttributes(saved);\n          \n          var newQueryStr = xmlhttp.getResponseHeader(\"New-Query\");\n          var ambiguityKey = xmlhttp.getResponseHeader(\"Ambiguity-Key\");\n          var ambiguityNumber = xmlhttp.getResponseHeader(\"Ambiguity-Number\");\n          var ambiguitySelected = xmlhttp.getResponseHeader(\"Ambiguity-Selected\");\n          if(ambiguityKey !== null && typeof ambiguityKey != \"undefined\" &&\n             ambiguityNumber !== null && typeof ambiguityNumber != \"undefined\" &&\n             ambiguitySelected !== null && typeof ambiguitySelected != \"undefined\") {\n             \n            var n = JSON.parse(ambiguityNumber);\n            var selected = JSON.parse(ambiguitySelected);\n            var newMenu = document.createElement(\"menuitem\");\n            var disambiguationMenu = `<span style=\"color:red\" id=\"ambiguity-id\" v=\"${ambiguityKey}\">Ambiguity.</span> Solutions `;\n            for(var i = 1; i <= n; i++) {\n              if(i == selected) {\n                disambiguationMenu += ` <span class=\"solution selected\">#${i}</span>`\n              } else {\n                disambiguationMenu += ` <a class=\"solution\" onclick=\"selectAmbiguity('${ambiguityKey}', ${i})\">#${i}</a>`\n              }\n            }\n            disambiguationMenu += ` <button onclick='acceptAmbiguity(\"${ambiguityKey}\", ${selected})'>Accept current</button>`;\n            newMenu.innerHTML = disambiguationMenu;\n            newMenu.setAttribute(\"isghost\", \"true\")\n            document.getElementById(\"themenu\").append(newMenu);\n          }\n          if(newQueryStr !== null) {\n            var newQuery = JSON.parse(newQueryStr);\n            var newQueryKeys = Object.keys(newQuery);\n            var strQuery = \"\";\n            for(var i = 0; i < newQueryKeys.length; i++) {\n              var key = newQueryKeys[i];\n              strQuery = strQuery + (i == 0 ? \"?\" : \"&\") + key + \"=\" + newQuery[key];\n            }\n            window.history.replaceState({}, \"Current page\", strQuery);\n          }\n        }\n    }\n    \n    function selectAmbiguity(key, num) {\n      var xmlhttp = new XMLHttpRequest();\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\n      xmlhttp.open(\"POST\", location.pathname + location.search);\n      xmlhttp.setRequestHeader(\"ambiguity-key\", key);\n      xmlhttp.setRequestHeader(\"select-ambiguity\", JSON.stringify(num));\n      xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\n      xmlhttp.send(\"{\\\"a\\\":1}\");\n    }\n    \n    function acceptAmbiguity(key, num) {\n      var xmlhttp = new XMLHttpRequest();\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\n      xmlhttp.open(\"POST\", location.pathname + location.search);\n      xmlhttp.setRequestHeader(\"ambiguity-key\", key);\n      xmlhttp.setRequestHeader(\"accept-ambiguity\", JSON.stringify(num));\n      xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\n      xmlhttp.send(\"{\\\"a\\\":1}\");\n    }\n    \n    function handleMutations(mutations) {\n      var onlyGhosts = true;\n      for(var i = 0; i < mutations.length && onlyGhosts; i++) {\n        // A mutation is a ghost if either\n        // -- The attribute starts with 'ghost-'\n        // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\n        // -- It is the modification of a node or an attribute inside a ghost node.\n        var mutation = mutations[i];\n        if(hasGhostAncestor(mutation.target)) continue;\n        if(mutation.type == \"attributes\") {\n          if(isGhostAttributeKey(mutation.attributeName)) {\n          } else {\n            onlyGhosts = false;\n          }\n        } else if(mutation.type == \"childList\") {\n          for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {\n            if(!hasGhostAncestor(mutation.addedNodes[j])) {\n              onlyGhosts = false;\n            }\n          }\n        } else {\n          onlyGhosts = false;\n        }\n      }\n      if(onlyGhosts) {\n        console.log(\"mutations are only ghosts, skipping\", mutations);\n        return;\n      } // Send in post the new HTML along with the URL\n      console.log(\"mutations\", mutations);\n      if(typeof t !== \"undefined\") {\n        clearTimeout(t);\n      }\n      t = setTimeout(function() {\n        t = undefined;\n        \n        var newMenu = document.createElement(\"menuitem\");\n        var notification = `Sending modifications to server...`;\n        newMenu.innerHTML = notification;\n        newMenu.setAttribute(\"isghost\", \"true\")\n        document.getElementById(\"themenu\").append(newMenu);\n        \n        var xmlhttp = new XMLHttpRequest();\n        xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\n        xmlhttp.open(\"POST\", location.pathname + location.search);\n        xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\n        xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));\n      }, @editdelay)\n    }\n  \n    if (typeof outputValueObserver !== \"undefined\") {\n      // console.log(\"outputValueObserver.disconnect()\");\n      outputValueObserver.disconnect();\n      \n    }\n    \n\n    setTimeout(function() {\n      outputValueObserver = new MutationObserver(handleMutations);\n      outputValueObserver.observe\n       ( document.body.parentElement\n       , { attributes: true\n         , childList: true\n         , characterData: true\n         , attributeOldValue: true\n         , characterDataOldValue: true\n         , subtree: true\n         }\n       )\n     }, 10)\"\"\"\n\nmain";

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
        var lt = sns.lazyList.tail(solutionSet.remaining);
        var newSolution = getOneSolution(solutionSet.path, solutionSet.source, lt);
        if(newSolution === false) {
          solutionSet.remaining = false;
        } else {
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
        var allSolutionsTail = sns.lazyList.tail(allSolutions);
        var solution2 = getOneSolution(path, source, allSolutionsTail);
        if(solution2 === false) { // No ambiguity, we can immediately process the change.
          return solution;
        } else {
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