
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
var autosave = getParam("--autosave", "true") == "true";

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
  admin:    getParam("--admin",    "false") == "true",
  production:    getParam("--production",    "false") == "true",
  path:     path,
  closeable: !(!(fileToOpen)),
  openbrowser: getParam("--openbrowser", "false") == "true",
  key: "localhost-key.pem",
  cert: "localhost.pem"
};

async function start() {

// Don't modify, this will be replaced by the content of 'server.elm'
const defaultServerContent = "-- input: path            The file to serve.\n-- input: vars:           URL query vars.\n-- input: defaultOptions  default options (options that vars can override for certain parts).\n-- input: fileOperations  The current set of delayed file disk operations.\n--    Note that Elm pages are given in context the path, the vars, and the file system (fs) to read other files\n-- output: The page, either raw or augmented with the toolbar and edit scripts.\nlistGetOrElse key listDict default = listDict.get key listDict |> Maybe.withDefault default\n\npreludeEnv = let _ = googlesigninbutton in -- Force googlesigninbutton to be evaluated before preludeEnv\n  __CurrentEnv__\n\nmbApplyPrefix = case listDict.get \"path\" defaultOptions of\n  Just \"\" -> Nothing\n  Nothing -> Nothing\n  Just prefix -> Just (\\name -> if name == \"\" then prefix\n      else if Regex.matchIn \"/$\" prefix then prefix + name\n      else prefix + \"/\" + name)\n\nfs = nodejs.delayedFS nodejs.nodeFS fileOperations\n\nfs = case mbApplyPrefix of\n  Nothing -> fs\n  Just applyPrefix -> { fs |\n    read name = fs.read (applyPrefix name)\n    listdir name = fs.listdir (applyPrefix name)\n    listdircontent name = fs.listdircontent (applyPrefix name)\n    isdir name = fs.isdir (applyPrefix name)\n    isfile name = fs.isfile (applyPrefix name)\n  }\n\neditdelay = 1000\n\nboolVar name resDefault =\n  listDict.get name vars |>\n  Maybe.map (Update.bijection (case of \"true\" -> True; _ -> False) (case of True -> \"true\"; _ -> \"false\")) |>\n  Maybe.withDefaultReplace (\n    listDict.get name defaultOptions |> Maybe.withDefault resDefault |> freeze)\n\nvaradmin = boolVar \"admin\" False\nvaredit = boolVar \"edit\" True\nvarproduction = listDict.get \"production\" defaultOptions |> Maybe.withDefault (freeze False)\niscloseable = listDict.get \"closeable\" defaultOptions |> Maybe.withDefault (freeze False)\n\nuserpermissions = {pageowner= True, admin= varadmin}\npermissionToCreate = userpermissions.admin\npermissionToEditServer = userpermissions.admin -- should be possibly get from user authentication\n-- List.contains (\"sub\", \"102014571179481340426\") user -- That's my Google user ID.\n\ncanEditPage = userpermissions.pageowner && varedit\n\n{freezeWhen} = Update\n\nserverOwned what = freezeWhen (not permissionToEditServer) (\\od -> \"\"\"You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>\n\nIf you really intended to modify this, add ?admin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.<br><br>\n\nFor debugging purposes, below is the new value that was pushed:\n<pre>@(Regex.replace \"<\" (always \"&lt;\") \"\"\"@od\"\"\")</pre>\n\"\"\")\n\npath =\n  if fs.isdir path then\n   if listDict.get \"ls\" vars /= Just \"true\" then\n         if fs.isfile <| path + \"index.html\" then path + \"index.html\"\n    else if fs.isfile <| path + \"/index.html\" then path + \"/index.html\"\n    else if fs.isfile <| path + \"index.elm\" then path + \"index.elm\"\n    else if fs.isfile <| path + \"/index.elm\" then path + \"/index.elm\"\n    else if fs.isfile <| path + \"README.md\" then path + \"README.md\"\n    else if fs.isfile <| path + \"/README.md\" then path + \"/README.md\"\n    else path\n   else path\n  else path\n\nsourcecontent = String.newlines.toUnix <|\n  if path == \"server.elm\" then\n    \"\"\"<html><head></head><body>Sample server Elm</body></html>\"\"\"\n  else\n    if fs.isdir path then\n      \"\"\"\n      let pathprefix = if path == \"\" then path else path + \"/\" in\n      <html><head></head><body><h1><a href=''>/@path</a></h1>\n      <ul>@@(case Regex.extract \"^(.*)/.*$\" path of\n        Just [prev] -> [<li><a href=(\"/\" + prev)>..</li>]\n        _ -> if path == \"\" then [] else [<li><a href=\"/\" contenteditable=\"false\">..</li>])@@(List.map (\\name -> <li><a href=(\"/\" + pathprefix + name)>@@name</li>) (fs.listdir path))</ul>\n      Hint: place a <a href=(\"/\" + pathprefix + \"README.md?edit=true\") contenteditable=\"false\">README.md</a>, <a href=(\"/\" + pathprefix + \"index.html?edit=true\") contenteditable=\"false\">index.html</a> or <a href=(\"/\" + pathprefix + \"index.elm?edit=true\") contenteditable=\"false\">index.elm</a> file to display something else than this page.</body></html>\"\"\"\n    else\n      if fs.isfile path && Regex.matchIn \"\"\"\\.(png|jpg|ico|gif|jpeg)$\"\"\" path then -- Normally not called because server.js takes care of these cases.\n        \"\"\"<html><head><title>@path</title></head><body><img src=\"@path\"></body></html>\"\"\"\n      else\n        fs.read path\n      |> Maybe.withDefaultReplace (\n        serverOwned \"404 page\" \"\"\"<html><head></head><body>@(\n            if permissionToCreate then \"\"\"<span>@path does not exist yet. Modify this page to create it!</span>\"\"\" else \"\"\"<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>\"\"\"\n          )</body></html>\"\"\"\n      )\n\ncanEvaluate = listDict.get \"evaluate\" vars |> Maybe.withDefaultReplace (serverOwned \"default value of evaluate\" \"true\")\n  \nmain = (if canEvaluate == \"true\" then\n      if Regex.matchIn \"\"\"\\.html$\"\"\" path then\n        let interpretableData =\n          case Regex.extract \"\"\"^(?:(?!<html)[\\s\\S])*((?=<html)[\\s\\S]*</html>)\\s*$\"\"\" sourcecontent of\n            Just [interpretableHtml] -> serverOwned \"begin raw tag\" \"<raw>\" + interpretableHtml + serverOwned \"end raw tag\" \"</raw>\"\n            _ -> serverOwned \"raw display of html - beginning\" \"\"\"<raw><html><head></head><body>\"\"\" + sourcecontent + serverOwned \"raw display of html - end\" \"\"\"</body></html></raw>\"\"\"\n        in\n        __evaluate__ preludeEnv interpretableData\n        |> Result.andThen (case of\n          [\"raw\", _, [htmlNode]] -> Ok htmlNode\n          result -> Err \"\"\"Html interpretation error: The interpretation of raw html did not work but produced @result\"\"\"\n        )\n      else if Regex.matchIn \"\"\"\\.md$\"\"\" path then\n        let markdownized = String.markdown sourcecontent in\n          case Html.parseViaEval markdownized of\n            x -> \n              let markdownstyle = fs.read \"markdown.css\" |> Maybe.withDefaultReplace \"\"\"img {\n  max-width: 100%;\n}\npre {\n  padding: 10px 0 10px 30px;\n  color: cornflowerblue;\n}\na {\n  text-decoration: none;\n  font-weight: bold;\n  color: #0268cd;\n}\np {\n  margin: 1.0em 0 1.0em 0;\n}\nbody {\n  text-align: justify;\n  font-family: Geneva, Verdana, sans-serif;\n  line-height: 1.75em;\n  background-color: #C9CFCD;\n}\nh1, h2, h3, h4 {\n  letter-spacing: -1px;\n  font-weight: normal;\n  color: #171717;\n}\nh2 {\n\tfont-size: 2.25em;\n}\nh3 {\n  padding: 25px 0 0 0;\n\tfont-size: 1.75em;\n}\nh4 {\n\tfont-size: 1.25em;\n  margin-top: 1.25em;\n}\n.wrapper {\n  margin-left: auto;\n  margin-right: auto;\n  margin-top: 10px;\n  max-width: 900px;\n  padding-left: 20px;\n  padding-right: 20px;\n  padding-top: 20px;\n  background-color: white;\n}\"\"\" in\n              Ok <html><head></head><body><style title=\"If you modify me, I'll create a custom markdwon.css that will override the default CSS for markdown rendering\">@markdownstyle</style><div class=\"wrapper\">@x</div></body></html>\n      else if Regex.matchIn \"\"\"\\.(elm|leo)$\"\"\" path || fs.isdir path then\n        __evaluate__ ((\"vars\", vars)::(\"path\", path)::(\"fs\", fs)::preludeEnv) sourcecontent\n      else\n        Err \"\"\"Serving only .html, .md and .elm files. Got @path\"\"\"\n    else\n      Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>\n  ) |> (case of\n  Err msg -> serverOwned \"Error Report\" <|\n    <html><head></head><body style=\"color:#cc0000\"><div style=\"max-width:600px;margin-left:auto;margin-right:auto\"><h1>Error report</h1><pre style=\"white-space:pre-wrap\">@msg</pre></div></body></html>\n  Ok page -> page)\n  |> case of\n      [\"html\", htmlattrs, htmlchildren] -> [\"html\", htmlattrs, htmlchildren |>\n        List.filter (case of\n          [_, _] -> False\n          _ -> True) |>\n        List.mapWithReverse identity (case of\n          [\"body\", bodyattrs, bodychildren] ->\n            [\"body\",\n              (if canEditPage then serverOwned \"contenteditable attribute of the body due to edit=true\" [[\"contenteditable\", \"true\"]] else freeze []) ++\n                bodyattrs,\n              (if canEditPage then (serverOwned \"edition menu\" editionmenu ++ Update.sizeFreeze [(serverOwned \"code preview box\" codepreview) sourcecontent]) else\n               if not varedit && not iscloseable && not varproduction then serverOwned \"open edit box\" [openEditBox] else\n               serverOwned \"edit prelude when not in edit mode\" []) ++ serverOwned \"initial script\" initialScript ++ bodychildren ++ Update.sizeFreeze (serverOwned \"synchronization script\" [<script>@editionscript</script>])]\n          x -> x\n        )]\n      x-> <html><head></head><body>Not a valid html page: @(\"\"\"@x\"\"\")</body></html>\n  --|> Update.debug \"main\"\n\n-- Box to switch to edit mode.\nswitchEditBox toEdit = \n  let prev = if toEdit then \"false\" else \"true\"\n      next = if toEdit then \"true\" else \"false\"\n      msg = if toEdit then \"edit\" else \"x\"\n      title = if toEdit then \"Reload the page in edit mode\" else \"Reload the page without edit mode\" in\n<div id=\"editbox\" title=@title onclick=\"\"\"\n if(location.search.indexOf(\"edit=@prev\") == -1) {\n   location.search = location.search.startsWith(\"?\") ? location.search + \"&edit=@next\" : \"?edit=@next\"\n } else {\n   location.search = location.search.replace(/edit=@prev/, \"edit=@next\");\n }\n\"\"\">\n<style>#editbox {\n  @(if toEdit then \"\"\"position: fixed;\n  margin-top: 2px;\n  margin-left: 2px;\n  background: white;\n  padding: 2px;\n  border-radius: 10px;\n  transform: scale(0.6);\n  \"\"\" else \"\"\"position: absolute;\n  color: white;\n  background: black;\n  font-family: 'Helvetica', 'Arial', sans-serif;\n  font-size: 2em;\n  font-weight: bold;\n  text-align: center;\n  width: 40px;\n  height: 40px;\n  border-radius: 5px;\n  transform: translate(-0.7em, -0.7em) scale(0.3);\n  \"\"\"\n )z-index: 20000;\n  opacity: 0.5;\n  cursor: pointer;\n}\n#editbox:hover {\n  opacity: 1;\n}\n</style>@msg\n</div>\n\nopenEditBox = switchEditBox True\ncloseEditBox = switchEditBox False\n\nboolToCheck = Update.bijection (case of \"true\" -> [[\"checked\", \"\"]]; _ -> []) (case of [[\"checked\", \"\"]] -> \"true\"; _ -> \"false\")\n  \neditionmenu = [\n<div id=\"docslinkbubble\" class=\"docs-bubble docs-linkbubble-bubble\" list-ghost-attributes=\"style\" help=\"Modify or delete a link\" tabindex=\"0\" contenteditable=\"false\"><a rel=\"noreferrer\" id=\"docslinkbubble-linkpreview\" list-ghost-attributes=\"href contenteditable\" children-are-ghosts=\"true\"></a><span> – <button id=\"docslinkbubble-modify\" class=\"docs-bubble-link\" tabindex=\"0\">Modify</button> | <button id=\"docslinkbubble-delete\" class=\"docs-bubble-link\" tabindex=\"0\">Delete</button></span></div>,\n<menu id=\"themenu\" ignore-modifications=\"true\" class=\"edittoolbar\" contenteditable=\"false\">\n@(if iscloseable then [] else closeEditBox)\n<style>\n.docs-linkbubble-bubble {\n  z-index: 1503;\n}\n\n.docs-bubble {\n  background-color: #fff;\n  border-radius: 2px;\n  border: 1px solid;\n  border-color: #bbb #bbb #a8a8a8;\n  box-shadow: 0 1px 3px rgba(0,0,0,.2);\n  color: #666;\n  cursor: default;\n  padding: 12px 20px;\n  position: absolute;\n  z-index: 1502;\n  white-space: nowrap;\n}\n\n.docs-bubble-link, .docs-bubble a {\n  color: #15c!important;\n  cursor: pointer;\n  text-decoration: none!important;\n}\n\n.docs-bubble-link:hover, .docs-bubble a:hover {\n  text-decoration: underline!important;\n}\n\n.docs-bubble-link[contenteditable=true], .docs-bubble a[contenteditable=true] {\n  cursor: text;\n  text-decoration: none!important;\n}\n\n.docs-bubble-link[contenteditable=true] + button, .docs-bubble a[contenteditable=true] + span > button:first-child {\n  outline: 1px solid black;\n}\n\nmenu .editor-logo {\n  display: inline-block;\n  margin-right: 5px;\n  font-weight: bold;\n}\n\nmenuitem.filename {\n  color: #777;\n  padding-left: 3px;\n}\n.editor-menu {\n  display: initial !important;\n}\n#menumargin {\n  padding-top: 2em;\n}\nmenu {\n  position: fixed;\n  margin-top: 0px;\n  z-index: 1000;\n  min-height: 1.5em;\n  font-family: sans-serif;\n  border: 1px solid #888;\n}\nmenu.edittoolbar {\n  display: block;\n  color: black;\n  background-color: #d5daff;\n  padding: 3px;\n  border-radius: 10px;\n}\nmenuitem.disabled {\n  color: #BBB;\n}\nmenu input[type=checkbox] {\n  display: none;\n}\nmenu input[type=checkbox] + .label-checkbox {\n  color: #888;\n}\nmenu input[type=checkbox]:checked + .label-checkbox {\n  background: #bcbbff;\n  color: #000;\n}\n/*\nmenu input[type=checkbox]:not(:checked) + .label-checkbox::after {\n  content: \": off\";\n}\nmenu input[type=checkbox]:checked + .label-checkbox::after {\n  content: \": on\";\n}\n*/\n.label-checkbox {\n  padding: 2px;\n  border-radius: 10px;\n}\n.label-checkbox:hover {\n  background-color: rgba(0,0,0,0.06);\n  cursor: pointer;\n}\n.menu-separator {\n  display: inline-block;\n  border-left: 1px solid #828282;\n  margin: 0 3px;\n  height: 1.5em;\n  padding: 0;\n  vertical-align: top;\n  line-height: normal;\n  outline: none;\n  overflow: hidden;\n  text-decoration: none;\n  width: 0;\n}\n\nmenuitem > .solution.selected {\n  outline: black 2px solid;\n}\n.to-be-selected {\n  outline: #FCC 2px solid;\n  animation:spin 1s linear infinite;\n}\n@@keyframes spin{\n\tfrom {\n    outline-color: #FAA;\n  }\n  33% {\n    outline-color: #AFA;\n  }\n  66% {\n    outline-color: #AAF;\n  }\n\tto {\n    outline-color: #FAA;\n  }\t\n}\nmenuitem > .solution:not(.selected):hover {\n  outline: #999 2px solid;\n  cursor: pointer;\n}\nmenuitem > .solution.notfinal {\n  color: #666;\n}\n#editor_codepreview, #manualsync-menuitem {\n  display: none;\n  z-index: 999;\n}\n#docslinkbubble {\n  display: none;\n}\n[ghost-visible=true] {\n  display: initial !important;\n}\n[ghost-disabled=true] {\n  opacity: 0.5 !important;\n  cursor: initial;\n  pointer-events: none !important;\n}\n[ghost-disabled=false] {\n  opacity: 1  !important;\n  pointer-events: auto !important;\n}\n#manualsync-menuitem[ghost-disabled=false] > button {\n  cursor:pointer !important;\n  opacity: 1  !important;\n  pointer-events: auto !important;\n}\n#manualsync-menuitem[force-visible=true] {\n  display: initial;\n}\n[ghost-visible=false] {\n  display: none !important;\n}\n#manualsync-menuitem> button {\n  vertical-align: top;\n  opacity: 0.5;\n  cursor: initial;\n  pointer-events: none;\n}\n.summary {\n  color: green;\n}\n</style>\n<div class= \"editor-logo\">Editor <a href= \"https://github.com/MikaelMayer/Editor/issues\">(report issue)</a></div><div class=\"menu-separator\"></div><menuitem class= \"filename\" title= \"the path of the file you are currently viewing\">@(if path == \"\" then serverOwned \"empty path\" \"[root folder]\" else path)</menuitem>\n<div class=\"menu-separator\"></div><menuitem>\n<label title=\"Display the source code of this pagge below\"><input id=\"input-showsource\" type=\"checkbox\" save-properties=\"checked\"\n  onchange=\"\"\"\nvar cp = document.getElementById(\"editor_codepreview\");\nif(cp !== null) {\n   cp.setAttribute(\"ghost-visible\", this.checked ? \"true\": \"false\")\n}\"\"\"><span class= \"label-checkbox\">Source</span></label>\n</menuitem><div class=\"menu-separator\"></div>\n<menuitem>\n<label title=\"If off, ambiguities are resolved automatically. Does not apply for HTML pages\"><input id=\"input-question\" type=\"checkbox\" save-properties=\"checked\" @(case listDict.get \"question\" vars of\n                       Just questionattr -> boolToCheck questionattr\n                       _ -> serverOwned \"initial checked attribute (use &question=false in query parameters to modify it)\" (\n                              if boolVar \"question\" True then [[\"checked\", \"\"]] else []))><span class= \"label-checkbox\">Ask questions</span></label>\n</menuitem>\n<div class=\"menu-separator\"></div>\n<menuitem>\n<label title=\"If on, changes are automatically propagated 1 second after the last edit\"><input id=\"input-autosave\" type=\"checkbox\" save-properties=\"checked\" onchange=\"document.getElementById('manualsync-menuitem').setAttribute('ghost-visible', this.checked ? 'false' : 'true')\" @(case listDict.get \"autosave\" vars of\n                      Just autosaveattr -> boolToCheck autosaveattr\n                      _ -> serverOwned \"initial checked attribute (use &autosave=true or false in query parameters to modify it)\"  (\n                             if boolVar \"autosave\" True then [[\"checked\", \"\"]] else []))><span class= \"label-checkbox\">Auto-save</span></label>\n</menuitem>\n<menuitem id=\"manualsync-menuitem\" @(if boolVar \"autosave\" True then [] else [[\"force-visible\", \"true\"]])>\n<button onclick=\"sendModificationsToServer()\" title= \"Sends modifications to the server\">Save</button>\n</menuitem>\n</menu>,\n<div id=\"menumargin\"></div>]\n\ninitialScript = [\n<script>\nif(typeof googleAuthIdToken == \"undefined\") {\n  var googleAuthIdToken = undefined;\n}\n\nfunction isGhostNode(elem) {\n  return elem && elem.nodeType == 1 &&\n    (elem.tagName == \"GHOST\" || elem.getAttribute(\"isghost\") == \"true\");\n}\n\nfunction areChildrenGhosts(n) {\n  return n && n.getAttribute && n.getAttribute(\"children-are-ghosts\") == \"true\";\n}\nfunction hasGhostAncestor(htmlElem) {\n  if(htmlElem == null) return false;\n  if(isGhostNode(htmlElem)) return true;\n  return areChildrenGhosts(htmlElem.parentNode) || hasGhostAncestor(htmlElem.parentNode);\n}\nfunction isGhostAttributeKey(name) {\n  return name.startsWith(\"ghost-\");\n}\n\nglobalGhostAttributeKeysFromNode = [];\n(globalGhostAttributeKeysFromNode || []).push(n =>\n  ((n && n.getAttribute && n.getAttribute(\"list-ghost-attributes\")) || \"\").split(\" \").concat(\n    ((n && n.getAttribute && n.getAttribute(\"save-ghost-attributes\")) || \"\").split(\" \")).filter(a => a != \"\")\n);\n(globalGhostAttributeKeysFromNode || []).push(n =>\n  n && n.tagName == \"HTML\" ? [\"class\"] : []\n);\n(globalGhostAttributeKeysFromNode || []).push(n =>\n  n && n.tagName == \"BODY\" ? [\"data-gr-c-s-loaded\"] : []\n);\n\nfunction isSpecificGhostAttributeKeyFromNode(n) {\n  var additionalGhostAttributes = [];\n  for(var k in globalGhostAttributeKeysFromNode) {\n    additionalGhostAttributes = additionalGhostAttributes.concat(globalGhostAttributeKeysFromNode[k](n))\n  }\n  return (a => name => a.indexOf(name) != -1)(additionalGhostAttributes);\n}\n\nsetGhostOnInserted = [];\n\n// Analytics scripts\n(setGhostOnInserted || []).push(insertedNode =>\n  insertedNode.tagName == \"SCRIPT\" && typeof insertedNode.getAttribute(\"src\") == \"string\" &&\n     insertedNode.getAttribute(\"src\").indexOf(\"google-analytics.com/analytics.js\") != -1\n);\n\n// For for ace styles in header\n(setGhostOnInserted || []).push(insertedNode => {\n    if(insertedNode.tagName == \"STYLE\" && typeof insertedNode.getAttribute(\"id\") == \"string\" &&\n     (insertedNode.getAttribute(\"id\").startsWith(\"ace-\") ||\n      insertedNode.getAttribute(\"id\").startsWith(\"ace_\"))) {\n      insertedNode.setAttribute(\"save-ghost\", \"true\"); \n      return true;\n    } else {\n      return false;\n    }\n  }\n);\n// For Google sign-in buttons and i-frames\n(setGhostOnInserted || []).push(insertedNode =>\n  (insertedNode.tagName == \"DIV\" &&\n    insertedNode.classList.contains(\"abcRioButton\")) ||\n  (insertedNode.tagName == \"IFRAME\" &&\n    insertedNode.getAttribute(\"id\") == \"ssIFrame_google\")\n);\n// For anonymous styles inside HEAD (e.g. ace css themes and google sign-in)\n(setGhostOnInserted || []).push(insertedNode => \n  insertedNode.tagName == \"STYLE\" && insertedNode.getAttribute(\"id\") == null &&\n  insertedNode.parentElement.tagName == \"HEAD\" && (insertedNode.setAttribute(\"save-ghost\", \"true\") || true)\n);\n// For ace script for syntax highlight\n(setGhostOnInserted || []).push(insertedNode =>\n  insertedNode.tagName == \"SCRIPT\" && typeof insertedNode.getAttribute(\"src\") == \"string\" &&\n     insertedNode.getAttribute(\"src\").startsWith(\"https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.2/mode-j\")\n);\n// For ace script for syntax highlight\n(setGhostOnInserted || []).push(insertedNode =>\n  insertedNode.tagName == \"ACE_OUTER\"\n);\n\nfunction handleScriptInsertion(mutations) {\n  for(var i = 0; i < mutations.length; i++) {\n    // A mutation is a ghost if either\n    // -- The attribute starts with 'ghost-'\n    // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\n    // -- It is the modification of a node or an attribute inside a ghost node.\n    var mutation = mutations[i];\n    if(hasGhostAncestor(mutation.target)) continue;\n    if(mutation.type == \"childList\") {\n      for(var j = 0; j < mutation.addedNodes.length; j++) {\n        var insertedNode = mutation.addedNodes[j];\n        if(!hasGhostAncestor(insertedNode) && (insertedNode.nodeType == 1 && insertedNode.getAttribute(\"isghost\") != \"true\" || insertedNode.noteType == 3 && !insertedNode.isghost) && setGhostOnInserted.find(pred => pred(insertedNode))) {\n         if(insertedNode.nodeType == 1) insertedNode.setAttribute(\"isghost\", \"true\");\n         insertedNode.isghost = true;\n        }\n      }\n    }\n  }\n}\n\nif (typeof analyticsScriptNeutralizer !== \"undefined\") {\n  // console.log(\"analyticsScriptNeutralizer.disconnect()\");\n  analyticsScriptNeutralizer.disconnect();\n}\n\nanalyticsScriptNeutralizer = new MutationObserver(handleScriptInsertion);\nanalyticsScriptNeutralizer.observe\n ( document.body.parentElement\n , { attributes: false\n   , childList: true\n   , characterData: false\n   , attributeOldValue: false\n   , characterDataOldValue: false\n   , subtree: true\n   }\n )\n\n// Self-editing capabilities\nfunction getSelectionStart() {\n   var node = document.getSelection().anchorNode;\n   return (node != null && node.nodeType == 3 ? node.parentNode : node);\n}\nfunction getEnclosingCaret(tagName) {\n  var w = getSelectionStart();\n  console.log(\"where\", w);\n  while(w != null && w.tagName.toLowerCase() != tagName.toLowerCase()) {\n    w = w.parentNode;\n  }\n  return w;\n}\nfunction emptyTextContent(node) {\n  if(node != null) {\n    if(node.nodeType == 3) {\n      node.textContent = \"\";\n    } else {\n      for(i in node.childNodes) {\n        emptyTextContent(node.childNodes[i]);\n      }\n    }\n  }\n  return node;\n}\nfunction insertBefore(parent, node, beforeNode) {\n  if(beforeNode == null) {\n    parent.append(node);\n  } else {\n    parent.insertBefore(node, beforeNode);\n  }\n}\n\nfunction duplicate(node, options) {\n  if(typeof options == \"undefined\") options = {}\n  if(typeof options.onBeforeInsert != \"function\") options.onBeforeInsert = e => e;\n  if(node != null && node.parentNode != null) {\n    var insertBeforeNode = options.after ? node.nextSibling : node;\n    if(node.previousSibling != null) {\n      var next = node.nextSibling;\n      if(next.nodeType == 3 && next.nextSibling != null &&\n         next.nextSibling.tagName == node.tagName && (node.tagName == \"TR\" || node.tagName == \"LI\" || node.tagName == \"TD\")) {\n        var textElement = next.cloneNode(true);\n        insertBefore(node.parentNode, textElement, options.after ? node.nextSibling : node);\n        if(options.after) {\n          insertBeforeNode = textElement.nextSibling;\n        } else {\n          insertBeforeNode = textElement\n        }\n      }\n    }\n    var cloned = options.onBeforeInsert(node.cloneNode(true));\n    insertBefore(node.parentNode, cloned, insertBeforeNode);\n  }\n}\nfunction remove(node) {\n  if(node.previousSibling != null) { // Remove whitespace as well\n    var next = node.nextSibling;\n    if(next.nodeType == 3 && next.nextSibling != null &&\n       next.nextSibling.tagName == node.tagName && (node.tagName == \"TR\" || node.tagName == \"LI\" || node.tagName == \"TD\")) {\n      next.remove();\n    }\n  }\n  node.remove();\n}\n</script>\n]\n\ncodepreview sourcecontent = \n<div class=\"codepreview\" id=\"editor_codepreview\">\n  <textarea id=\"editor_codepreview_textarea\" save-properties=\"scrollTop\"\n    style=\"width:100%;height:200px\" v=sourcecontent onchange=\"this.setAttribute('v', this.value)\">@(Update.softFreeze (if Regex.matchIn \"^\\r?\\n\" sourcecontent then \"\\n\" + sourcecontent else sourcecontent))</textarea>\n</div>\n    \neditionscript = \"\"\"\n  \n  // Save / Load ghost attributes after a page is reloaded.\n  // Same for some attributes\n  function saveGhostAttributes() {\n    var ghostModified = document.querySelectorAll(\"[ghost-visible]\");\n    var idGhostAttributes = [];\n    for(var i = 0; i < ghostModified.length; i++) {\n      var id = ghostModified[i].getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        idGhostAttributes.push([id,\n          \"ghost-visible\", ghostModified[i].getAttribute(\"ghost-visible\")]);\n      }\n    }\n    var ghostAttributesModified = document.querySelectorAll(\"[save-ghost-attributes]\");\n    console.log(ghostAttributesModified);\n    for(var i = 0; i < ghostAttributesModified.length; i++) {\n      var elem = ghostAttributesModified[i];\n      var id = elem.getAttribute(\"id\");\n      if(id != null) {\n        var toSave = elem.getAttribute(\"save-ghost-attributes\").split(\" \");\n        for(j in toSave) {\n          var key = toSave[j];\n          idGhostAttributes.push([id, key, elem.getAttribute(key)]);\n        }\n      }\n    }\n    \n    var elemsWithAttributesToSave = document.querySelectorAll(\"[save-properties]\");\n    var idDynamicAttributes = [];\n    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {\n      var elem = elemsWithAttributesToSave[i];\n      var id = elem.getAttribute(\"id\");\n      if(id !== null && typeof id !== \"undefined\") {\n        var toSave = elem.getAttribute(\"save-properties\").split(\" \");\n        for(j in toSave) {\n          var key = toSave[j];\n          idDynamicAttributes.push([id, key, elem[key]])\n        }\n      }\n    }\n    var ghostElemsToReinsert = document.querySelectorAll(\"[save-ghost]\");\n    var parentsGhostNodes = [];\n    for(var i = 0; i < ghostElemsToReinsert.length; i++) {\n      var elem = ghostElemsToReinsert[i];\n      if(elem.parentNode.tagName == \"HEAD\") {\n        parentsGhostNodes.push({parentSelector: \"HEAD\", node: elem});\n      } else {\n        var id =  elem.parentNode.getAttribute(\"id\");\n        if(id != null) {\n          parentsGhostNodes.push({parentSelector: \"#\"+id, node: elem});\n        }\n      }\n    }\n    return [idGhostAttributes, idDynamicAttributes, parentsGhostNodes];\n  }\n  function applyGhostAttributes(attrs) {\n    var [idGhostAttributes, idDynamicAttributes, parentsGhostNodes] = attrs;\n    for(var i in idGhostAttributes) {\n      var [id, key, attr] = idGhostAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem != null) {\n        elem.setAttribute(key, attr);\n      }\n    }\n    for(var i in idDynamicAttributes) {\n      var [id, key, value] = idDynamicAttributes[i];\n      var elem = document.getElementById(id);\n      if(elem != null) {\n        elem[key] = value;\n      }\n    }\n    for(var i in parentsGhostNodes) {\n      var {parentSelector: selector, node: elem} = parentsGhostNodes[i];\n      var parent = document.querySelector(selector);\n      if(parent != null) {\n        if(!elem.getAttribute(\"id\") || !document.getElementById(elem.getAttribute(\"id\"))) {\n          parent.appendChild(elem);\n        }\n      }\n    }\n  }\n  \n  function domNodeToNativeValue(n) {\n      if(n.nodeType == \"3\") {\n        return [\"TEXT\", n.textContent];\n      } else if(n.nodeType == \"8\") {\n        return [\"COMMENT\", n.textContent];\n      } else {\n        var attributes = [];\n        var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(n);\n        for(var i = 0; i < n.attributes.length; i++) {\n          var key = n.attributes[i].name;\n          var value = n.attributes[i].value;\n          if(!isGhostAttributeKey(key) && !isSpecificGhostAttributeKey(key)) {\n            if(key == \"style\") {\n              value = value.split(\";\").map(x => x.split(\":\")).filter(x => x.length == 2);\n            }\n            attributes.push([key, value]);\n          }\n        }\n        var children = [];\n        if(!areChildrenGhosts(n)) {\n          for(i = 0; i < n.childNodes.length; i++) {\n            if(!isGhostNode(n.childNodes[i])) {\n              children.push(domNodeToNativeValue(n.childNodes[i]));\n            }\n          }\n        }\n        return [n.tagName.toLowerCase(), attributes, children];\n      }\n    }\n    function replaceContent(NC) {\n      document.open();\n      document.write(NC);\n      document.close();\n    }\n    \n    var t = undefined;\n    \n    handleServerPOSTResponse = (xmlhttp, onBeforeUpdate) => function () {\n        if (xmlhttp.readyState == XMLHttpRequest.DONE) {\n          //console.log(\"Received new content. Replacing the page.\");\n          if(typeof onBeforeUpdate !== \"undefined\") onBeforeUpdate();\n          var saved = saveGhostAttributes();\n          replaceContent(xmlhttp.responseText);\n          applyGhostAttributes(saved);\n          \n          var newQueryStr = xmlhttp.getResponseHeader(\"New-Query\");\n          var ambiguityKey = xmlhttp.getResponseHeader(\"Ambiguity-Key\");\n          var ambiguityNumber = xmlhttp.getResponseHeader(\"Ambiguity-Number\");\n          var ambiguitySelected = xmlhttp.getResponseHeader(\"Ambiguity-Selected\");\n          var ambiguityEnd = xmlhttp.getResponseHeader(\"Ambiguity-End\");\n          var ambiguitySummaries = xmlhttp.getResponseHeader(\"Ambiguity-Summaries\");\n          if(ambiguityKey !== null && typeof ambiguityKey != \"undefined\" &&\n             ambiguityNumber !== null && typeof ambiguityNumber != \"undefined\" &&\n             ambiguitySelected !== null && typeof ambiguitySelected != \"undefined\") {\n             \n            var n = JSON.parse(ambiguityNumber);\n            var selected = JSON.parse(ambiguitySelected);\n            var summaries = JSON.parse(ambiguitySummaries);\n            var newMenu = document.createElement(\"menuitem\");\n            var disambiguationMenu = `<span style=\"color:red\" id=\"ambiguity-id\" v=\"${ambiguityKey}\">Ambiguity.</span> Solutions `;\n            for(var i = 1; i <= n; i++) {\n              var summary = summaries[i-1].replace(/\"/g,'&quot;');\n              if(i == selected) {\n                disambiguationMenu += ` <span class=\"solution selected\" title=\"${summary}\">#${i}</span>`\n              } else {\n                disambiguationMenu += ` <span class=\"solution${i == n && ambiguityEnd != 'true' ? ' notfinal' : ''}\" title=\"${summary}\" onclick=\"this.classList.add('to-be-selected'); selectAmbiguity('${ambiguityKey}', ${i})\">#${i}</span>`\n              }\n            }\n            disambiguationMenu += ` <button id=\"saveambiguity\" onclick='acceptAmbiguity(\"${ambiguityKey}\", ${selected})'>Save</button>`;\n            disambiguationMenu += ` <button id=\"cancelAmbiguity\" onclick='cancelAmbiguity(\"${ambiguityKey}\", ${selected})'>Cancel</button>`;\n            newMenu.innerHTML = disambiguationMenu;\n            newMenu.setAttribute(\"isghost\", \"true\");\n            if(document.getElementById(\"themenu\"))\n              document.getElementById(\"themenu\").append(newMenu);\n          } else {\n            var opSummaryEncoded = xmlhttp.getResponseHeader(\"Operations-Summary\");\n            if(opSummaryEncoded) {\n              var opSummary = decodeURI(opSummaryEncoded);\n              var log = document.createElement(\"span\");\n              log.setAttribute(\"class\", \"summary\");\n              log.innerText = \"Last action: \" + opSummary;\n              var newMenu = document.createElement(\"menuitem\");\n              newMenu.append(log);\n              newMenu.setAttribute(\"id\", \"lastaction\");\n              newMenu.setAttribute(\"isghost\", \"true\");\n              if(document.getElementById(\"themenu\"))\n                document.getElementById(\"themenu\").append(newMenu);\n            }\n          }\n          if(newQueryStr !== null) {\n            var newQuery = JSON.parse(newQueryStr);\n            var strQuery = \"\";\n            for(var i = 0; i < newQuery.length; i++) {\n              var {_1: key, _2: value} = newQuery[i];\n              strQuery = strQuery + (i == 0 ? \"?\" : \"&\") + key + \"=\" + value\n            }\n            window.history.replaceState({}, \"Current page\", strQuery);\n          }\n        }\n    }\n    \n    notifyServer = callback => {\n      var xmlhttp = new XMLHttpRequest();\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp, () => {\n        if(document.getElementById(\"input-autosave\") && !document.getElementById(\"input-autosave\").checked) {\n          document.getElementById(\"manualsync-menuitem\").setAttribute(\"ghost-visible\", \"true\") // Because it will be saved\n        }\n      });\n      xmlhttp.open(\"POST\", location.pathname + location.search);\n      xmlhttp.setRequestHeader(\"Content-Type\", \"application/json\");\n      if(googleAuthIdToken) {\n        xmlhttp.setRequestHeader(\"id-token\", googleAuthIdToken)\n      }\n      var result = callback(xmlhttp);\n      xmlhttp.send(result || \"{\\\"a\\\":1}\");\n    }\n    \n    function reloadPage() { // Use this only after a successful login\n      notifyServer(xmlhttp => {\n        xmlhttp.setRequestHeader(\"reload\", \"true\");\n      })\n    }\n    \n    function selectAmbiguity(key, num) {\n      notifyServer(xmlhttp => {\n        xmlhttp.setRequestHeader(\"ambiguity-key\", key);\n        xmlhttp.setRequestHeader(\"select-ambiguity\", JSON.stringify(num));\n        xmlhttp.setRequestHeader(\"question\", \"true\");\n      });\n    }\n    \n    function acceptAmbiguity(key, num) {\n      notifyServer(xmlhttp => {\n        xmlhttp.setRequestHeader(\"ambiguity-key\", key);\n        xmlhttp.setRequestHeader(\"accept-ambiguity\", JSON.stringify(num));\n      });\n    }\n    \n    function cancelAmbiguity(key, num) {\n      notifyServer(xmlhttp => {\n        xmlhttp.setRequestHeader(\"ambiguity-key\", key);\n        xmlhttp.setRequestHeader(\"cancel-ambiguity\", JSON.stringify(num));\n      });\n    }\n    \n    function sendModificationsToServer() {\n      if(document.getElementById(\"notification-menu\") != null) {\n        //document.getElementById(\"notification-menu\").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`\n        return;\n      }\n      var newMenu = document.createElement(\"menuitem\");\n      var notification = `Updating the source files...`;\n      newMenu.innerHTML = notification;\n      newMenu.setAttribute(\"isghost\", \"true\");\n      newMenu.setAttribute(\"id\", \"notification-menu\");\n      newMenu.classList.add(\"to-be-selected\");\n      if(document.getElementById('lastaction')) {\n        document.getElementById('lastaction').remove();\n      }\n      if(document.getElementById(\"themenu\") && document.getElementById(\"manualsync-menuitem\")) {\n        document.getElementById(\"themenu\").append(newMenu);\n        document.getElementById(\"manualsync-menuitem\").setAttribute(\"ghost-visible\", \"false\");\n      }\n      notifyServer(xmlhttp => {\n        xmlhttp.setRequestHeader(\"question\", document.getElementById(\"input-question\") && document.getElementById(\"input-question\").checked ? \"true\" : \"false\");\n        return JSON.stringify(domNodeToNativeValue(document.body.parentElement));\n      })\n    }\n    \n    function handleMutations(mutations) {\n      var onlyGhosts = true;\n      for(var i = 0; i < mutations.length && onlyGhosts; i++) {\n        // A mutation is a ghost if either\n        // -- The attribute starts with 'ghost-'\n        // -- It is the insertion of a node whose tag is \"ghost\" or that contains an attribute \"isghost=true\"\n        // -- It is the modification of a node or an attribute inside a ghost node.\n        var mutation = mutations[i];\n        if(hasGhostAncestor(mutation.target)) {\n          continue;\n        }\n        if(mutation.type == \"attributes\") {\n          var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(mutation.target);\n          if(isGhostAttributeKey(mutation.attributeName) || isSpecificGhostAttributeKey(mutation.attributeName)) {\n          } else {\n            onlyGhosts = false;\n            console.log(\"Attribute is not ghost\", mutation);\n          }\n        } else if(mutation.type == \"childList\") {\n          if(!areChildrenGhosts(mutation.target)) {\n            for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {\n              if(!hasGhostAncestor(mutation.addedNodes[j])) {\n                onlyGhosts = false;\n                console.log(`Added node ${j} does not have a ghost ancestor`, mutation);\n              }\n            }\n            for(var j = 0; j < mutation.removedNodes.length && onlyGhosts; j++) {\n              if(!isGhostNode(mutation.removedNodes[j])) {\n                onlyGhosts = false;\n                console.log(`Removed node ${j} was not a ghost`, mutation);\n              }\n            }\n          }\n        } else {\n          onlyGhosts = false;\n          console.log(\"mutations other than attributes and childList are not ghosts\", mutations);\n        }\n      }\n      if(onlyGhosts) {\n        console.log(\"mutations are only ghosts, skipping\");\n        return;\n      } // Send in post the new HTML along with the URL\n      if(document.getElementById(\"input-autosave\") && !document.getElementById(\"input-autosave\").checked) {\n        if(document.getElementById(\"manualsync-menuitem\")) {\n          document.getElementById(\"manualsync-menuitem\").setAttribute(\"ghost-disabled\", \"false\");\n        }\n        return;\n      }\n      \n      if(typeof t !== \"undefined\") {\n        clearTimeout(t);\n      }\n      t = setTimeout(function() {\n        t = undefined;\n        \n        sendModificationsToServer();\n      }, @editdelay)\n    }\n  \n    if (typeof outputValueObserver !== \"undefined\") {\n      // console.log(\"outputValueObserver.disconnect()\");\n      outputValueObserver.disconnect();\n    }\n    \n\n    setTimeout(function() {\n      outputValueObserver = new MutationObserver(handleMutations);\n      outputValueObserver.observe\n       ( document.body.parentElement\n       , { attributes: true\n         , childList: true\n         , characterData: true\n         , attributeOldValue: true\n         , characterDataOldValue: true\n         , subtree: true\n         }\n       )\n     }, 10)\n    \n    function pasteHtmlAtCaret(html) {\n      var sel, range;\n      if (window.getSelection) {\n          // IE9 and non-IE\n          sel = window.getSelection();\n          if (sel.getRangeAt && sel.rangeCount) {\n              range = sel.getRangeAt(0);\n              range.deleteContents();\n\n              // Range.createContextualFragment() would be useful here but is\n              // only relatively recently standardized and is not supported in\n              // some browsers (IE9, for one)\n              var el = document.createElement(\"div\");\n              el.innerHTML = html;\n              var frag = document.createDocumentFragment(), node, lastNode;\n              while ( (node = el.firstChild) ) {\n                  lastNode = frag.appendChild(node);\n              }\n              range.insertNode(frag);\n\n              // Preserve the selection\n              if (lastNode) {\n                  range = range.cloneRange();\n                  range.setStartAfter(lastNode);\n                  range.collapse(true);\n                  sel.removeAllRanges();\n                  sel.addRange(range);\n              }\n          }\n      } else if (document.selection && document.selection.type != \"Control\") {\n          // IE < 9\n          document.selection.createRange().pasteHTML(html);\n      }\n    }\n     \n    function handleFileSelect(evt) {\n      evt.stopPropagation();\n      evt.preventDefault();\n\n      var insertRelative = true;\n      \n      var files = evt.dataTransfer.files; // FileList object.\n      // files is a FileList of File objects. List some properties.\n      var output = [];\n      for (var i = 0, f; f = files[i]; i++) {\n        if(f.type.indexOf(\"image\") == 0 && f.size < 30000000) {\n          // process image files under 30Mb\n          var xhr = new XMLHttpRequest();\n          var tmp = location.pathname.split(\"/\");\n          tmp = tmp.slice(0, tmp.length - 1);\n          var storageFolder = tmp.join(\"/\");\n          var storageLocation =  storageFolder + \"/\" + f.name;\n          xhr.onreadystatechange = ((xhr, path, name) => () => {\n            if (xhr.readyState == XMLHttpRequest.DONE) {\n              pasteHtmlAtCaret(`<img src=\"${path}\" alt=\"${name}\">`);\n            } else {\n              console.log(\"Error while uploading picture\", xhr);\n            }\n          })(xhr, insertRelative ? f.name : storageLocation, f.name)\n          xhr.open(\"POST\", storageLocation, true);\n          xhr.setRequestHeader(\"write-file\", f.type);\n          xhr.send(f);\n        }\n      }\n    }\n\n    function handleDragOver(evt) {\n      evt.stopPropagation();\n      evt.preventDefault();\n      evt.dataTransfer.dropEffect = 'copy'; // Explicitly show this is a copy.\n    }\n\n    if(@(if varedit then \"true\" else \"false\")) {\n      var dropZone = document.body;\n      dropZone.addEventListener('dragover', handleDragOver, false);\n      dropZone.addEventListener('drop', handleFileSelect, false);\n    }\n    \n    // Shortcuts\n    document.onkeydown = function(e) {\n      var key = e.which || e.keyCode;\n      if (e.which == 83 && (e.ctrlKey || e.metaKey)) { // CTRL+S or CMD+S: Save\n        closeLinkWindow();\n        if(document.getElementById(\"saveambiguity\")) {\n          eval(document.getElementById(\"saveambiguity\").getAttribute(\"onclick\") || \"console.log('no onclick for saveambiguity')\")\n        } else {\n          sendModificationsToServer();\n        }\n        e.preventDefault();\n      }\n      if(e.which == 75 && (e.ctrlKey || e.metaKey)) { // CTRL+K: Insert link\n        document.execCommand('createLink', false, 'http://');\n        e.preventDefault();\n      }\n    };\n    document.onkeyup = document.onkeydown\n    \n    observeTargetA = null;\n    \n    addEditEqualToUrl = function(href, what) {\n      if(href.indexOf(\"://\") == -1) { // Instrument the relative link so that it is edit=true\n        if(href.indexOf(\"?\") >= 0) {\n          if(href.endsWith(\"?\")) {\n            href = href + \"edit=\" + what\n          } else {\n            href = href + \"&edit=\" + what\n          }\n        } else {\n          href = href + \"?edit=\" + what\n        }\n      }\n      return href;\n    }\n    \n    onClickOnLink = function (event) {\n      var clickedElem = event.target;\n      if(clickedElem && clickedElem.tagName == \"A\" && clickedElem.getAttribute(\"id\") != \"docslinkbubble-linkpreview\") {\n        var href = clickedElem.getAttribute(\"href\");\n        if(href) {\n          var rect = clickedElem.getBoundingClientRect();\n          var bottomX = window.scrollX + rect.left;\n          var bottomY = window.scrollY + rect.top + clickedElem.offsetHeight;\n          var d = document.getElementById(\"docslinkbubble\");\n          d.setAttribute(\"style\", \"left: \" + bottomX + \"; top: \" + bottomY);\n          var targetA = document.getElementById(\"docslinkbubble-linkpreview\");\n          var updateHref = function(href) {\n            href = @(if listDict.get \"edit\" defaultOptions |> Maybe.withDefault False |> not then \"\"\"\n            addEditEqualToUrl(href, \"true\");\"\"\" else \"href;\")\n            targetA.setAttribute(\"href\", href);\n          }\n          updateHref(href);\n          targetA.innerText = href;\n          d.setAttribute(\"ghost-visible\", \"true\");\n          \n          var deleteButton = document.getElementById(\"docslinkbubble-delete\");\n          deleteButton.onclick = () => {\n            clickedElem.outerHTML = clickedElem.innerHTML;\n            d.setAttribute(\"ghost-visible\", \"false\");\n          }\n          \n          var modifyButton = document.getElementById(\"docslinkbubble-modify\");\n          modifyButton.onclick = () => {\n            if (observeTargetA != null) {\n              observeTargetA.disconnect();\n              observeTargetA = null;\n            }\n            if(targetA.getAttribute(\"contenteditable\") == \"true\") {\n              targetA.setAttribute(\"contenteditable\", \"false\");\n            } else {\n              targetA.setAttribute(\"contenteditable\", \"true\");\n              targetA.focus();\n              console.log(\"reconnect observeTargetA\", targetA);\n              observeTargetA = new MutationObserver(function(mutations) {\n                console.log(\"targetA modified\");\n                clickedElem.setAttribute(\"href\", targetA.innerText);\n                updateHref(targetA.innerText); // Instrumented link\n              });\n              observeTargetA.observe\n               ( targetA\n               , { attributes: false\n                 , childList: true\n                 , characterData: true\n                 , attributeOldValue: false\n                 , characterDataOldValue: true\n                 , subtree: true\n                 }\n               )\n            }\n          }\n        }\n      } else if(clickedElem.getAttribute &&\n           (clickedElem.getAttribute(\"id\") == \"docslinkbubble\" ||\n            (clickedElem.parentNode && clickedElem.parentNode.getAttribute && clickedElem.parentNode.getAttribute(\"id\") == \"docslinkbubble\") ||\n            (clickedElem.parentNode && clickedElem.parentNode.parentNode &&\n             clickedElem.parentNode.parentNode.getAttribute && clickedElem.parentNode.parentNode.getAttribute(\"id\") == \"docslinkbubble\")\n            )) {\n      } else {\n        closeLinkWindow();\n      }\n      // Check if the event.target matches some selector, and do things...\n    }\n    \n    // Links edition - Might be empty.\n    @(if varedit == False && (listDict.get \"edit\" defaultOptions |> Maybe.withDefault False) == True then\n      -- Special case when ?edit=false but the default behavior is edit=true if nothing is set.\n      \"\"\"document.onclick = function (e) {\n          e = e ||  window.event;\n          var node = e.target || e.srcElement;\n          while(node) {\n            if(node.tagName == \"A\" && node.getAttribute(\"href\") && !node.onclick && !node.getAttribute(\"onclick\")) {\n             var newLocation = addEditEqualToUrl(node.getAttribute(\"href\"), \"false\");\n             console.log(newLocation);\n             window.location.href = newLocation;\n             e.stopPropagation();\n             return false;\n            } else {\n              node = node.parentNode;\n            }\n          }\n        }\"\"\"\n    else if varedit then\n      \"\"\"document.addEventListener('click', onClickOnLink, false);\"\"\"\n    else \"\")\n    \n    function closeLinkWindow() {\n      var d = document.getElementById(\"docslinkbubble\");\n      if(d && d.getAttribute(\"ghost-visible\") == \"true\") {\n        d.setAttribute(\"ghost-visible\", \"false\");\n        var targetA = document.getElementById(\"docslinkbubble-linkpreview\");\n        targetA.setAttribute(\"contenteditable\", \"false\");\n      }\n    }\n@(if iscloseable then \"\"\"\nwindow.onbeforeunload = function (e) {\n    e = e || window.event;\n\n    var askConfirmation = document.getElementById(\"manualsync-menuitem\") &&\n         document.getElementById(\"manualsync-menuitem\").getAttribute(\"ghost-disabled\") == \"false\";\n    const confirmation = 'You have unsaved modifications. Do you still want to exit?';\n\n    // For IE and Firefox prior to version 4\n    if (e) {\n      if(askConfirmation) {\n        e.returnValue = confirmation;\n      }\n    }\n    if(askConfirmation) {\n      // For Safari\n      return confirmation;\n    } else {\n      var xmlhttp = new XMLHttpRequest();\n      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);\n      xmlhttp.open(\"POST\", location.pathname + location.search);\n      xmlhttp.setRequestHeader(\"close\", \"true\");\n      xmlhttp.send(\"{\\\"a\\\":1}\");\n    }\n};\n\"\"\" else \"\")\n\"\"\"\n\ngooglesigninbutton = serverOwned \"the google sign-in button\" [\n<style>\na.closeGoogleSignIn {\n  margin-left: 2px;\n  padding-left: 4px;\n  padding-right: 5px;\n}\na.closeGoogleSignIn:hover {\n  background: #AAA;\n  color: white;\n  border-radius: 10px;\n}\n</style>,\n<div class=\"g-signin2\" data-onsuccess=\"onGoogleSignIn\" list-ghost-attributes=\"data-gapiscan data-onload\" children-are-ghost=\"true\"></div>,\n<script>\nfunction onGoogleSignIn(googleUser) {\n  var profile = googleUser.getBasicProfile();\n  \n  var wasSignedIn = googleAuthIdToken ? true : false;\n  // When set, will be used throughout \n  googleAuthIdToken = googleUser.getAuthResponse().id_token;\n  var addSignout = (name) => {\n    var signin = document.querySelector(\".abcRioButtonContents\").children[1];\n    signin.setAttribute(\"title\", \"Signed in as \" + name);\n    var signout = document.createElement(\"a\");\n    signout.classList.add(\"closeGoogleSignIn\");\n    signout.setAttribute(\"title\", \"Sign out\");\n    signout.innerText = \"x\";\n    signout.onclick = () => {\n      var auth2 = gapi.auth2.getAuthInstance();\n      auth2.signOut().then(() => {\n        auth2.disconnect();\n        googleAuthIdToken = undefined;\n        console.log('User signed out.');\n      });\n    }\n    signin.append(signout);\n  }\n  addSignout(profile.getName());\n  if(!wasSignedIn) { // Necessary to ensure that we don't reload the page the second time it is loaded.\n    reloadPage();\n  }\n}\n</script>,\n<script id=\"googlesigninscript\" src=\"https://apis.google.com/js/platform.js\" async defer save-ghost-attributes=\"gapi_processed\"></script>\n]\n\nmain";

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

function fileOperationSummary(operations) {
  if(operations == null) return "";
  var summary = "";
  for(var i = 0; i < operations.length; i++) {
    var {_1: path, _2: action} = operations[i];
    if(summary != "") summary += "\n";
    if(action["$d_ctor"] == "Write") {
      summary += "Modify " + path + ", " + stringDiffSummary(action.args._1, action.args._2, action.args._3);
    } else if(action["$d_ctor"] == "Create") {
      summary += "Created " + path;
    } else if(action["$d_ctor"] == "Rename") {
      summary += "Renamed " + path + " to " + action.args._1;
    } else if(action["$d_ctor"] == "Delete") {
      summary += "Deleted " + path;
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
          willkill = setTimeout(() => process.exit(), timeBeforeExit); // Kills the server after 2 seconds
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
  opn(protocol + "http://" + hostname + ":" + port);
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


