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
const defaultServerContent = "-- input: path  The file to serve.\r\n-- input: vars:     URL query vars.\r\n-- output: the page, either raw or augmented with the toolbar and edit scripts.\r\npreludeEnv = __CurrentEnv__\r\n\r\neditdelay = 1000\r\n\r\nuserpermissions = {pageowner= True, admin= True}\r\n\r\npermissionToCreate = userpermissions.admin\r\n\r\npermissionToEditServer = userpermissions.admin -- should be possibly get from user authentication\r\n\r\ncanEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == \"true\"; _ -> False)\r\n\r\nserverOwned = Update.conditionalFreeze (not permissionToEditServer)\r\n\r\npath = if nodejs.isdir path then\r\n       if nodejs.isfile <| path + \"index.html\" then path + \"index.html\"\r\n  else if nodejs.isfile <| path + \"/index.html\" then path + \"/index.html\"\r\n  else if nodejs.isfile <| path + \"index.elm\" then path + \"index.elm\"\r\n  else if nodejs.isfile <| path + \"/index.elm\" then path + \"/index.elm\"\r\n  else if nodejs.isfile <| path + \"README.md\" then path + \"README.md\"\r\n  else if nodejs.isfile <| path + \"/README.md\" then path + \"/README.md\"\r\n  else path\r\n  else path\r\n\r\nsourcecontent = if path == \"server.elm\" then\r\n    \"\"\"<html><head></head><body>Sample server Elm</body></html>\"\"\"\r\n  else\r\n    if nodejs.isdir path then\r\n      \"\"\"<html><head></head><body><h1><a href=''>/@path</a></h1>\r\n      <ul>@@(List.map (\\name -> <li><a href=(path + \"/\" + name)>@@name</li>) (nodejs.listdir path))</ul>\r\n      Hint: place an <a href=(path + \"/index.html\")>index.html</a> or <a href=(path + \"/index.elm\")>index.elm</a> file to display something else than this page.</body></html>\"\"\"\r\n    else\r\n      if nodejs.isfile path && Regex.matchIn \"\"\"\\.(png|jpg|ico|gif|jpeg)$\"\"\" path then\r\n        \"\"\"<html><head><title>@path</title></head><body><img src=\"@path\"></body></html>\"\"\"\r\n      else\r\n        nodejs.fileread path\r\n      |> Maybe.withDefaultReplace (\r\n        serverOwned \"\"\"<html><head></head><body>@(\r\n            if permissionToCreate then \"\"\"<span>@path does not exist yet. Modify this page to create it!</span>\"\"\" else \"\"\"<span>Error 404, @path does not exist</span>\"\"\"\r\n          )</body></html>\"\"\"\r\n      )\r\n\r\ncanEvaluate = vars |> case of {evaluate} -> evaluate; _ -> \"true\" \r\n  \r\nmain = (if canEvaluate == \"true\" then\r\n      if Regex.matchIn \"\"\"\\.html$\"\"\" path then\r\n        case Regex.extract \"\"\"^(?:(?!<html)[\\s\\S])*((?=<html)[\\s\\S]*</html>)\\s*$\"\"\" sourcecontent of\r\n          Just [interpretableHtml] ->\r\n            __evaluate__ preludeEnv \"\"\"<raw>@interpretableHtml</raw>\"\"\"\r\n            |> Result.andThen (case of\r\n              [\"raw\", _, [htmlNode]] -> Ok htmlNode\r\n              result -> Err \"\"\"Html interpretation error: The interpretation of raw html did not work but produced @result\"\"\"\r\n            )\r\n          x ->  Err \"\"\"@path is not a valid html file. Interpreation got: @x\"\"\"\r\n      else if Regex.matchIn \"\"\"\\.md$\"\"\" path then\r\n        let markdownized = String.markdown sourcecontent in\r\n          case Html.parseViaEval markdownized of\r\n            x -> Ok <html><head></head><body>@x</body></html>\r\n      else if Regex.matchIn \"\"\"\\.elm$\"\"\" path || nodejs.isdir path then\r\n        __evaluate__ ((\"vars\", vars)::(\"path\", path)::preludeEnv) sourcecontent\r\n      else\r\n        Err \"\"\"Serving only .html, .md and .elm files. Got @path\"\"\"\r\n    else\r\n      Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>\r\n  ) |> (case of\r\n  Err msg -> serverOwned <|\r\n    <html><head></head><body style=\"color:#cc0000\"><div style=\"max-width:600px;margin-left:auto;margin-right:auto\"><h1>Error report</h1><pre style=\"white-space:pre-wrap\">@msg</pre></div></body></html>\r\n  Ok page -> page)\r\n  |> case of\r\n      [\"html\", htmlattrs, htmlchildren] -> [\"html\", htmlattrs, htmlchildren |>\r\n        List.filter (case of\r\n          [_, _] -> False\r\n          _ -> True) |>\r\n        List.mapWithReverse identity (case of\r\n          [\"body\", bodyattrs, bodychildren] ->\r\n            [\"body\",\r\n              (if canEditPage then serverOwned [[\"contenteditable\", \"true\"]] else freeze []) ++\r\n                bodyattrs,\r\n              (if canEditPage then serverOwned (editionmenu ++ [codepreview sourcecontent]) else freeze []) ++ bodychildren ++ Update.sizeFreeze (serverOwned [<script>@editionscript</script>])]\r\n          x -> x\r\n        )]\r\n      x-> <html><head></head><body>Not a valid html page: @(\"\"\"@x\"\"\")</body></html>\r\n  --|> Update.debug \"main\"\r\n\r\neditionmenu = [\r\n<menu id=\"themenu\" ignore-modifications=\"true\" class=\"edittoolbar\" contenteditable=\"false\">\r\n<style>\r\n#menumargin {\r\n  padding-top: 1em;\r\n}\r\nmenu {\r\n  position: fixed;\r\n  margin-top: 0px;\r\n  z-index: 10000;\r\n}\r\nmenu.edittoolbar {\r\n  display: block;\r\n  background: black;\r\n  color: white;\r\n  padding: 2px;\r\n}\r\nmenuitem.disabled {\r\n  color: #BBB;\r\n}\r\nmenuitem > .solution {\r\n  \r\n}\r\nmenuitem > .solution.selected {\r\n  outline: white 2px solid;\r\n}\r\nmenuitem > .solution:not(.selected):hover {\r\n  outline: #CCC 2px solid;\r\n  cursor: pointer;\r\n}\r\n#editor_codepreview {\r\n  display: none;\r\n  z-index: 9999;\r\n}\r\n#editor_codepreview[ghost-visible=true] {\r\n  display: block;\r\n}\r\n</style>\r\n<menuitem>@path</menuitem>\r\n<menuitem class=\"disabled\"><button onclick=\"\"\"\r\nvar cp = document.getElementById(\"editor_codepreview\");\r\nif(cp !== null) {\r\n   cp.setAttribute(\"ghost-visible\", cp.getAttribute(\"ghost-visible\") == \"true\" ? \"false\": \"true\")\r\n}\"\"\">Display/hide source</button></menuitem>\r\n</menu>,\r\n<script>\r\nfunction isGhostNode(elem) {\r\n  return elem.nodeType == 1 &&\r\n    (elem.tagName == \"GHOST\" || elem.getAttribute(\"isghost\") == \"true\");\r\n}\r\nfunction hasGhostAncestor(htmlElem) {\r\n  if(htmlElem == null) return false;\r\n  if(isGhostNode(htmlElem)) return true;\r\n  return hasGhostAncestor(htmlElem.parentNode);\r\n}\r\nfunction isGhostAttributeKey(name) {\r\n  return name.startsWith(\"ghost-\");\r\n}\r\n\r\nfunction handleScriptInsertion(mutations) {\r\n  for(var i = 0; i < mutations.length; i++) {\r\n    // A mutation is a ghost if either\r\n    // -- The attribute starts with 'ghost-'\r\n    // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\r\n    // -- It is the modification of a node or an attribute inside a ghost node.\r\n    var mutation = mutations[i];\r\n    if(hasGhostAncestor(mutation.target)) continue;\r\n    if(mutation.type == \"childList\") {\r\n      for(var j = 0; j < mutation.addedNodes.length; j++) {\r\n        var addedNode = mutation.addedNodes[j];\r\n        if(addedNode.tagName == \"SCRIPT\" && typeof addedNode.getAttribute(\"src\") == \"string\" &&\r\n           addedNode.getAttribute(\"src\").indexOf(\"google-analytics.com/analytics.js\") &gt;= 0 &&\r\n           addedNode.getAttribute(\"isghost\") != \"true\") {\r\n         addedNode.setAttribute(\"isghost\", \"true\");\r\n        }\r\n      }\r\n    }\r\n  }\r\n}\r\n\r\nif (typeof analyticsScriptNeutralizer !== \"undefined\") {\r\n  // console.log(\"analyticsScriptNeutralizer.disconnect()\");\r\n  analyticsScriptNeutralizer.disconnect();\r\n}\r\n\r\nanalyticsScriptNeutralizer = new MutationObserver(handleScriptInsertion);\r\nanalyticsScriptNeutralizer.observe\r\n ( document.body\r\n , { attributes: false\r\n   , childList: true\r\n   , characterData: false\r\n   , attributeOldValue: false\r\n   , characterDataOldValue: false\r\n   , subtree: true\r\n   }\r\n )\r\n</script>,\r\n<div id=\"menumargin\"></div>]\r\n\r\ncodepreview sourcecontent = \r\n<div class=\"codepreview\" id=\"editor_codepreview\">\r\n  <textarea id=\"editor_codepreview_textarea\" save-attributes=\"scrollTop\"\r\n    style=\"width:100%;height:200px\" v=sourcecontent onchange=\"this.setAttribute('v', this.value)\">@(Update.softFreeze (if Regex.matchIn \"^\\r?\\n\" sourcecontent then \"\\n\" + sourcecontent else sourcecontent))</textarea>\r\n</div>\r\n    \r\neditionscript = \"\"\"\r\n  \r\n  // Save / Load ghost attributes after a page is reloaded.\r\n  // Same for some attributes\r\n  function saveGhostAttributes() {\r\n    var ghostModified = document.querySelectorAll(\"[ghost-visible]\");\r\n    var idGhostAttributes = [];\r\n    for(var i = 0; i < ghostModified.length; i++) {\r\n      var id = ghostModified[i].getAttribute(\"id\");\r\n      if(id !== null && typeof id !== \"undefined\") {\r\n        idGhostAttributes.push([id,\r\n          {\"ghost-visible\": ghostModified[i].getAttribute(\"ghost-visible\")}]);\r\n      }\r\n    }\r\n    var elemsWithAttributesToSave = document.querySelectorAll(\"[save-attributes]\");\r\n    var idDynamicAttributes = [];\r\n    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {\r\n      var elem = elemsWithAttributesToSave[i];\r\n      var id = elem.getAttribute(\"id\");\r\n      if(id !== null && typeof id !== \"undefined\") {\r\n        var toSave = elem.getAttribute(\"save-attributes\").split(\" \");\r\n        for(i in toSave) {\r\n          var key = toSave[i];\r\n          idDynamicAttributes.push([id, key, elem[key]])\r\n        }\r\n      }\r\n    }\r\n    return [idGhostAttributes, idDynamicAttributes];\r\n  }\r\n  function applyGhostAttributes(attrs) {\r\n    var [idGhostAttributes, idDynamicAttributes] = attrs;\r\n    for(var i in idGhostAttributes) {\r\n      var [id, attrs] = idGhostAttributes[i];\r\n      var elem = document.getElementById(id);\r\n      if(elem !== null && typeof elem !== \"undefined\") {\r\n        for(key in attrs) {\r\n          elem.setAttribute(key, attrs[key]);\r\n        }\r\n      }\r\n    }\r\n    for(var i in idDynamicAttributes) {\r\n      var [id, key, value] = idDynamicAttributes[i];\r\n      var elem = document.getElementById(id);\r\n      if(elem !== null && typeof elem !== \"undefined\") {\r\n        elem[key] = value;\r\n      }\r\n    }\r\n  }\r\n  \r\n  function domNodeToNativeValue(n) {\r\n      if(n.nodeType == \"3\") {\r\n        return [\"TEXT\", n.textContent];\r\n      } else if(n.nodeType == \"8\") {\r\n        return [\"COMMENT\", n.textContent];\r\n      } else {\r\n        var attributes = [];\r\n        for(var i = 0; i < n.attributes.length; i++) {\r\n          var key = n.attributes[i].name;\r\n          var value = n.attributes[i].value;\r\n          if(!isGhostAttributeKey(key)) {\r\n            if(key == \"style\") {\r\n              value = value.split(\";\").map(x => x.split(\":\")).filter(x => x.length == 2);\r\n            }\r\n            attributes.push([key, value]);\r\n          }\r\n        }\r\n        var children = [];\r\n        for(i = 0; i < n.childNodes.length; i++) {\r\n          if(!isGhostNode(n.childNodes[i])) {\r\n            children.push(domNodeToNativeValue(n.childNodes[i]));\r\n          }\r\n        }\r\n        return [n.tagName.toLowerCase(), attributes, children];\r\n      }\r\n    }\r\n    function replaceContent(NC) {\r\n      document.open();\r\n      document.write(NC);\r\n      document.close();\r\n    }\r\n    \r\n    var t = undefined;\r\n    \r\n    handleServerPOSTResponse = xmlhttp => function () {\r\n        if (xmlhttp.readyState == XMLHttpRequest.DONE) {\r\n          //console.log(\"Received new content. Replacing the page.\");\r\n          var saved = saveGhostAttributes();\r\n          replaceContent(xmlhttp.responseText);\r\n          applyGhostAttributes(saved);\r\n          \r\n          var newQueryStr = xmlhttp.getResponseHeader(\"New-Query\");\r\n          var ambiguityKey = xmlhttp.getResponseHeader(\"Ambiguity-Key\");\r\n          var ambiguityNumber = xmlhttp.getResponseHeader(\"Ambiguity-Number\");\r\n          var ambiguitySelected = xmlhttp.getResponseHeader(\"Ambiguity-Selected\");\r\n          if(ambiguityKey !== null && typeof ambiguityKey != \"undefined\" &&\r\n             ambiguityNumber !== null && typeof ambiguityNumber != \"undefined\" &&\r\n             ambiguitySelected !== null && typeof ambiguitySelected != \"undefined\") {\r\n             \r\n            var n = JSON.parse(ambiguityNumber);\r\n            var selected = JSON.parse(ambiguitySelected);\r\n            var newMenu = document.createElement(\"menuitem\");\r\n            var disambiguationMenu = `<span style=\"color:red\" id=\"ambiguity-id\" v=\"${ambiguityKey}\">Ambiguity.</span> Solutions `;\r\n            for(var i = 1; i <= n; i++) {\r\n              if(i == selected) {\r\n                disambiguationMenu += ` <span class=\"solution selected\">#${i}</span>`\r\n              } else {\r\n                disambiguationMenu += ` <a class=\"solution\" onclick=\"selectAmbiguity('${ambiguityKey}', ${i})\">#${i}</a>`\r\n              }\r\n            }\r\n            disambiguationMenu += ` <button onclick='acceptAmbiguity(\"${ambiguityKey}\", ${selected})'>Accept current</button>`;\r\n            newMenu.innerHTML = disambiguationMenu;\r\n            newMenu.setAttribute(\"isghost\", \"true\")\r\n            document.getElementById(\"themenu\").append(newMenu);\r\n          }\r\n          if(newQueryStr !== null) {\r\n            var newQuery = JSON.parse(newQueryStr);\r\n            var newQueryKeys = Object.keys(newQuery);\r\n            var strQuery = \"\";\r\n            for(var i = 0; i < newQueryKeys.length; i++) {\r\n              var key = newQueryKeys[i];\r\n              strQuery = strQuery + (i == 0 ? \"?\" : \"&\") + key + \"=\" + newQuery[key];\r\n            }\r\n            window.history.replaceState({}, \"Current page\", strQuery);\r\n          }\r\n        }\r\n    }\r\n    \r\n    function selectAmbiguity(key, num) {\r\n      var xmlhttp = new XMLHttpRequest();\r\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\r\n      xmlhttp.open(\"POST\", location.pathname + location.search);\r\n      xmlhttp.setRequestHeader(\"ambiguity-key\", key);\r\n      xmlhttp.setRequestHeader(\"select-ambiguity\", JSON.stringify(num));\r\n      xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\r\n      xmlhttp.send(\"{\\\"a\\\":1}\");\r\n    }\r\n    \r\n    function acceptAmbiguity(key, num) {\r\n      var xmlhttp = new XMLHttpRequest();\r\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\r\n      xmlhttp.open(\"POST\", location.pathname + location.search);\r\n      xmlhttp.setRequestHeader(\"ambiguity-key\", key);\r\n      xmlhttp.setRequestHeader(\"accept-ambiguity\", JSON.stringify(num));\r\n      xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\r\n      xmlhttp.send(\"{\\\"a\\\":1}\");\r\n    }\r\n    \r\n    function handleMutations(mutations) {\r\n      var onlyGhosts = true;\r\n      for(var i = 0; i < mutations.length && onlyGhosts; i++) {\r\n        // A mutation is a ghost if either\r\n        // -- The attribute starts with 'ghost-'\r\n        // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\r\n        // -- It is the modification of a node or an attribute inside a ghost node.\r\n        var mutation = mutations[i];\r\n        if(hasGhostAncestor(mutation.target)) continue;\r\n        if(mutation.type == \"attributes\") {\r\n          if(isGhostAttributeKey(mutation.attributeName)) {\r\n          } else {\r\n            onlyGhosts = false;\r\n          }\r\n        } else if(mutation.type == \"childList\") {\r\n          for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {\r\n            if(!hasGhostAncestor(mutation.addedNodes[j])) {\r\n              onlyGhosts = false;\r\n            }\r\n          }\r\n          if(mutation.removedNodes.length > 0) {\r\n            onlyGhosts = false;\r\n          }\r\n        } else {\r\n          onlyGhosts = false;\r\n        }\r\n      }\r\n      if(onlyGhosts) {\r\n        console.log(\"mutations are only ghosts, skipping\", mutations);\r\n        return;\r\n      } // Send in post the new HTML along with the URL\r\n      console.log(\"mutations\", mutations);\r\n      if(typeof t !== \"undefined\") {\r\n        clearTimeout(t);\r\n      }\r\n      t = setTimeout(function() {\r\n        t = undefined;\r\n        \r\n        var newMenu = document.createElement(\"menuitem\");\r\n        var notification = `Sending modifications to server...`;\r\n        newMenu.innerHTML = notification;\r\n        newMenu.setAttribute(\"isghost\", \"true\")\r\n        document.getElementById(\"themenu\").append(newMenu);\r\n        \r\n        var xmlhttp = new XMLHttpRequest();\r\n        xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\r\n        xmlhttp.open(\"POST\", location.pathname + location.search);\r\n        xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\r\n        xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));\r\n      }, @editdelay)\r\n    }\r\n  \r\n    if (typeof outputValueObserver !== \"undefined\") {\r\n      // console.log(\"outputValueObserver.disconnect()\");\r\n      outputValueObserver.disconnect();\r\n    }\r\n    \r\n\r\n    setTimeout(function() {\r\n      outputValueObserver = new MutationObserver(handleMutations);\r\n      outputValueObserver.observe\r\n       ( document.body.parentElement\r\n       , { attributes: true\r\n         , childList: true\r\n         , characterData: true\r\n         , attributeOldValue: true\r\n         , characterDataOldValue: true\r\n         , subtree: true\r\n         }\r\n       )\r\n     }, 10)\"\"\"\r\n\r\nmain";

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