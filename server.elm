-- input: path            The file to serve.
-- input: vars:           URL query vars.
-- input: defaultOptions  default options (options that vars can override for certain parts).
-- input: fileOperations  The current set of delayed file disk operations.
--    Note that Elm pages are given in context the path, the vars, and the file system (fs) to read other files
-- output: The page, either raw or augmented with the toolbar and edit scripts.
preludeEnv = __CurrentEnv__

mbApplyPrefix = case listDict.get "path" defaultOptions of
  Just "" -> Nothing
  Nothing -> Nothing
  Just prefix -> Just (\name -> if name == "" then prefix
      else if Regex.matchIn "/$" prefix then prefix + name
      else prefix + "/" + name)

fs = nodejs.delayedFS nodejs.nodeFS fileOperations

fs = case mbApplyPrefix of
  Nothing -> fs
  Just applyPrefix -> { fs |
    read name = fs.read (applyPrefix name)
    listdir name = fs.listdir (applyPrefix name)
    listdircontent name = fs.listdircontent (applyPrefix name)
    isdir name = fs.isdir (applyPrefix name)
    isfile name = fs.isfile (applyPrefix name)
  }

editdelay = 1000

boolVar name resDefault =
  listDict.get name vars |>
  Maybe.map (Update.bijection (case of "true" -> True; _ -> False) (case of True -> "true"; _ -> "false")) |>
  Maybe.withDefaultReplace (
    listDict.get name defaultOptions |> Maybe.withDefault resDefault |> freeze)

varadmin = boolVar "admin" False
varedit = boolVar "edit" True
varproduction = listDict.get "production" defaultOptions |> Maybe.withDefault (freeze False)

userpermissions = {pageowner= True, admin= varadmin}
permissionToCreate = userpermissions.admin
permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication

canEditPage = userpermissions.pageowner && varedit

freezeWhen notPermission lazyMessage = Update.lens {
  apply x = x
  update {outputNew, diffs} =
    if notPermission then
      Err (lazyMessage (outputNew, diffs))
    else
      Ok (InputsWithDiffs [(outputNew, Just diffs)])
}

serverOwned what = freezeWhen (not permissionToEditServer) (\od -> """You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>

If you really intended to modify this, add ?admin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.<br><br>

For debugging purposes, below is the new value that was pushed:
<pre>@(Regex.replace "<" (always "&lt;") """@od""")</pre>
""")

path =
  if fs.isdir path then
   if listDict.get "ls" vars /= Just "true" then
         if fs.isfile <| path + "index.html" then path + "index.html"
    else if fs.isfile <| path + "/index.html" then path + "/index.html"
    else if fs.isfile <| path + "index.elm" then path + "index.elm"
    else if fs.isfile <| path + "/index.elm" then path + "/index.elm"
    else if fs.isfile <| path + "README.md" then path + "README.md"
    else if fs.isfile <| path + "/README.md" then path + "/README.md"
    else path
   else path
  else path

sourcecontent = String.newlines.toUnix <|
  if path == "server.elm" then
    """<html><head></head><body>Sample server Elm</body></html>"""
  else
    if fs.isdir path then
      """
      let pathprefix = if path == "" then path else path + "/" in
      <html><head></head><body><h1><a href=''>/@path</a></h1>
      <ul>@@(case Regex.extract "^(.*)/.*$" path of
        Just [prev] -> [<li><a href=("/" + prev)>..</li>]
        _ -> if path == "" then [] else [<li><a href="/" contenteditable="false">..</li>])@@(List.map (\name -> <li><a href=("/" + pathprefix + name)>@@name</li>) (fs.listdir path))</ul>
      Hint: place a <a href=("/" + pathprefix + "README.md?edit=true") contenteditable="false">README.md</a>, <a href=("/" + pathprefix + "index.html?edit=true") contenteditable="false">index.html</a> or <a href=("/" + pathprefix + "index.elm?edit=true") contenteditable="false">index.elm</a> file to display something else than this page.</body></html>"""
    else
      if fs.isfile path && Regex.matchIn """\.(png|jpg|ico|gif|jpeg)$""" path then -- Normally not called because server.js takes care of these cases.
        """<html><head><title>@path</title></head><body><img src="@path"></body></html>"""
      else
        fs.read path
      |> Maybe.withDefaultReplace (
        serverOwned "404 page" """<html><head></head><body>@(
            if permissionToCreate then """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>"""
          )</body></html>"""
      )

canEvaluate = listDict.get "evaluate" vars |> Maybe.withDefaultReplace (serverOwned "default value of evaluate" "true")
  
main = (if canEvaluate == "true" then
      if Regex.matchIn """\.html$""" path then
        let interpretableData =
          case Regex.extract """^(?:(?!<html)[\s\S])*((?=<html)[\s\S]*</html>)\s*$""" sourcecontent of
            Just [interpretableHtml] -> serverOwned "begin raw tag" "<raw>" + interpretableHtml + serverOwned "end raw tag" "</raw>"
            _ -> serverOwned "raw display of html - beginning" """<raw><html><head></head><body>""" + sourcecontent + serverOwned "raw display of html - end" """</body></html></raw>"""
        in
        __evaluate__ preludeEnv interpretableData
        |> Result.andThen (case of
          ["raw", _, [htmlNode]] -> Ok htmlNode
          result -> Err """Html interpretation error: The interpretation of raw html did not work but produced @result"""
        )
      else if Regex.matchIn """\.md$""" path then
        let markdownized = String.markdown sourcecontent in
          case Html.parseViaEval markdownized of
            x -> 
              let markdownstyle = fs.read "markdown.css" |> Maybe.withDefaultReplace """pre {
  padding: 10px 0 10px 30px;
  color: cornflowerblue;
}
a {
  text-decoration: none;
  font-weight: bold;
  color: #0268cd;
}
p {
  margin: 1.0em 0 1.0em 0;
}
body {
  text-align: justify;
  font-family: Geneva, Verdana, sans-serif;
  line-height: 1.75em;
  background-color: #C9CFCD;
}
h1, h2, h3, h4 {
  letter-spacing: -1px;
  font-weight: normal;
  color: #171717;
}
h2 {
	font-size: 2.25em;
}
h3 {
  padding: 25px 0 0 0;
	font-size: 1.75em;
}
h4 {
	font-size: 1.25em;
  margin-top: 1.25em;
}
.wrapper {
  margin-left: auto;
  margin-right: auto;
  margin-top: 10px;
  max-width: 900px;
  padding-left: 20px;
  padding-right: 20px;
  padding-top: 20px;
  background-color: white;
}""" in
              Ok <html><head></head><body><style title="If you modify me, I'll create a custom markdwon.css that will override the default CSS for markdown rendering">@markdownstyle</style><div class="wrapper">@x</div></body></html>
      else if Regex.matchIn """\.(elm|leo)$""" path || fs.isdir path then
        __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::preludeEnv) sourcecontent
      else
        Err """Serving only .html, .md and .elm files. Got @path"""
    else
      Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>
  ) |> (case of
  Err msg -> serverOwned "Error Report" <|
    <html><head></head><body style="color:#cc0000"><div style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">@msg</pre></div></body></html>
  Ok page -> page)
  |> case of
      ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
        List.filter (case of
          [_, _] -> False
          _ -> True) |>
        List.mapWithReverse identity (case of
          ["body", bodyattrs, bodychildren] ->
            ["body",
              (if canEditPage then serverOwned "contenteditable attribute of the body due to edit=true" [["contenteditable", "true"]] else freeze []) ++
                bodyattrs,
              (if canEditPage then (serverOwned "edition menu" editionmenu ++ Update.sizeFreeze [(serverOwned "code preview box" codepreview) sourcecontent]) else
               if not varedit && not varproduction then serverOwned "open edit box" [openEditBox] else
               serverOwned "edit prelude when not in edit mode" []) ++ serverOwned "initial script" initialScript ++ bodychildren ++ Update.sizeFreeze (serverOwned "synchronization script" [<script>@editionscript</script>])]
          x -> x
        )]
      x-> <html><head></head><body>Not a valid html page: @("""@x""")</body></html>
  --|> Update.debug "main"

-- Box to switch to edit mode.
switchEditBox toEdit = 
  let prev = if toEdit then "false" else "true"
      next = if toEdit then "true" else "false"
      msg = if toEdit then "edit" else "x"
      title = if toEdit then "Reload the page in edit mode" else "Reload the page without edit mode" in
<div id="editbox" title=@title onclick="""
 if(location.search.indexOf("edit=@prev") == -1) {
   location.search = location.search.startsWith("?") ? location.search + "&edit=@next" : "?edit=@next"
 } else {
   location.search = location.search.replace(/edit=@prev/, "edit=@next");
 }
""">
<style>#editbox {
  @(if toEdit then """position: fixed;
  margin-top: 2px;
  margin-left: 2px;
  background: white;
  padding: 2px;
  border-radius: 10px;
  transform: scale(0.6);
  """ else """position: absolute;
  color: white;
  background: black;
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 2em;
  font-weight: bold;
  text-align: center;
  width: 40px;
  height: 40px;
  border-radius: 5px;
  transform: translate(-0.7em, -0.7em) scale(0.3);
  """
 )z-index: 20000;
  opacity: 0.5;
  cursor: pointer;
}
#editbox:hover {
  opacity: 1;
}
</style>@msg
</div>

openEditBox = switchEditBox True
closeEditBox = switchEditBox False
  
boolToCheck = Update.bijection (case of "true" -> [["checked", ""]]; _ -> []) (case of [["checked", ""]] -> "true"; _ -> "false")
  
editionmenu = [
<div id="docslinkbubble" class="docs-bubble docs-linkbubble-bubble" list-ghost-attributes="style" help="Modify or delete a link" tabindex="0" contenteditable="false"><a rel="noreferrer" id="docslinkbubble-linkpreview" list-ghost-attributes="href contenteditable" children-are-ghosts="true"></a><span> – <button id="docslinkbubble-modify" class="docs-bubble-link" tabindex="0">Modify</button> | <button id="docslinkbubble-delete" class="docs-bubble-link" tabindex="0">Delete</button></span></div>,
<menu id="themenu" ignore-modifications="true" class="edittoolbar" contenteditable="false">
@closeEditBox
<style>
.docs-linkbubble-bubble {
  z-index: 1503;
}

.docs-bubble {
  background-color: #fff;
  border-radius: 2px;
  border: 1px solid;
  border-color: #bbb #bbb #a8a8a8;
  box-shadow: 0 1px 3px rgba(0,0,0,.2);
  color: #666;
  cursor: default;
  padding: 12px 20px;
  position: absolute;
  z-index: 1502;
  white-space: nowrap;
}

.docs-bubble-link, .docs-bubble a {
  color: #15c!important;
  cursor: pointer;
  text-decoration: none!important;
}

.docs-bubble-link:hover, .docs-bubble a:hover {
  text-decoration: underline!important;
}

.docs-bubble-link[contenteditable=true], .docs-bubble a[contenteditable=true] {
  cursor: text;
  text-decoration: none!important;
}

.docs-bubble-link[contenteditable=true] + button, .docs-bubble a[contenteditable=true] + span > button:first-child {
  outline: 1px solid black;
}

menu .editor-logo {
  display: inline-block;
  margin-right: 5px;
  font-weight: bold;
}

menuitem.filename {
  color: #777;
  padding-left: 3px;
}
.editor-menu {
  display: initial !important;
}
#menumargin {
  padding-top: 2em;
}
menu {
  position: fixed;
  margin-top: 0px;
  z-index: 1000;
  min-height: 1.5em;
  font-family: sans-serif;
  border: 1px solid #888;
}
menu.edittoolbar {
  display: block;
  color: black;
  background-color: #d5daff;
  padding: 3px;
  border-radius: 10px;
}
menuitem.disabled {
  color: #BBB;
}
menu input[type=checkbox] {
  display: none;
}
menu input[type=checkbox] + .label-checkbox {
  color: #888;
}
menu input[type=checkbox]:checked + .label-checkbox {
  background: #bcbbff;
  color: #000;
}
/*
menu input[type=checkbox]:not(:checked) + .label-checkbox::after {
  content: ": off";
}
menu input[type=checkbox]:checked + .label-checkbox::after {
  content: ": on";
}
*/
.label-checkbox {
  padding: 2px;
  border-radius: 10px;
}
.label-checkbox:hover {
  background-color: rgba(0,0,0,0.06);
  cursor: pointer;
}
.menu-separator {
  display: inline-block;
  border-left: 1px solid #828282;
  margin: 0 3px;
  height: 1.5em;
  padding: 0;
  vertical-align: top;
  line-height: normal;
  outline: none;
  overflow: hidden;
  text-decoration: none;
  width: 0;
}

menuitem > .solution.selected {
  outline: black 2px solid;
}
.to-be-selected {
  outline: #FCC 2px solid;
  animation:spin 1s linear infinite;
}
@@keyframes spin{
	from {
    outline-color: #FAA;
  }
  33% {
    outline-color: #AFA;
  }
  66% {
    outline-color: #AAF;
  }
	to {
    outline-color: #FAA;
  }	
}
menuitem > .solution:not(.selected):hover {
  outline: #999 2px solid;
  cursor: pointer;
}
menuitem > .solution.notfinal {
  color: #666;
}
#editor_codepreview, #manualsync-menuitem {
  display: none;
  z-index: 999;
}
#docslinkbubble {
  display: none;
}
[ghost-visible=true] {
  display: initial !important;
}
[ghost-disabled=true] {
  opacity: 0.5 !important;
  cursor: initial;
  pointer-events: none !important;
}
[ghost-disabled=false] {
  opacity: 1  !important;
  pointer-events: auto !important;
}
#manualsync-menuitem[ghost-disabled=false] > button {
  cursor:pointer !important;
  opacity: 1  !important;
  pointer-events: auto !important;
}
#manualsync-menuitem[force-visible=true] {
  display: initial;
}
[ghost-visible=false] {
  display: none !important;
}
#manualsync-menuitem> button {
  vertical-align: top;
  opacity: 0.5;
  cursor: initial;
  pointer-events: none;
}
.summary {
  color: green;
}
</style>
<div class= "editor-logo">Editor <a href= "https://github.com/MikaelMayer/Editor/issues">(report issue)</a></div><div class="menu-separator"></div><menuitem class= "filename" title= "the path of the file you are currently viewing">@(if path == "" then serverOwned "empty path" "[root folder]" else path)</menuitem>
<div class="menu-separator"></div><menuitem>
<label title="Display the source code of this pagge below"><input id="input-showsource" type="checkbox" save-attributes="checked"
  onchange="""
var cp = document.getElementById("editor_codepreview");
if(cp !== null) {
   cp.setAttribute("ghost-visible", this.checked ? "true": "false")
}"""><span class= "label-checkbox">Source</span></label>
</menuitem><div class="menu-separator"></div>
<menuitem>
<label title="If off, ambiguities are resolved automatically. Does not apply for HTML pages"><input id="input-question" type="checkbox" save-attributes="checked" @(case listDict.get "question" vars of
                       Just questionattr -> boolToCheck questionattr
                       _ -> serverOwned "initial checked attribute (use &question=false in query parameters to modify it)" (
                              if boolVar "question" True then [["checked", ""]] else []))><span class= "label-checkbox">Ask questions</span></label>
</menuitem>
<div class="menu-separator"></div>
<menuitem>
<label title="If on, changes are automatically propagated 1 second after the last edit"><input id="input-autosave" type="checkbox" save-attributes="checked" onchange="document.getElementById('manualsync-menuitem').setAttribute('ghost-visible', this.checked ? 'false' : 'true')" @(case listDict.get "autosave" vars of
                      Just autosaveattr -> boolToCheck autosaveattr
                      _ -> serverOwned "initial checked attribute (use &autosave=true or false in query parameters to modify it)"  (
                             if boolVar "autosave" True then [["checked", ""]] else []))><span class= "label-checkbox">Auto-save</span></label>
</menuitem>
<menuitem id="manualsync-menuitem" @(if boolVar "autosave" True then [] else [["force-visible", "true"]])>
<button onclick="sendModificationsToServer()" title= "Sends modifications to the server">Save</button>
</menuitem>
</menu>,
<div id="menumargin"></div>]

initialScript = [
<script>
function isGhostNode(elem) {
  return elem && elem.nodeType == 1 &&
    (elem.tagName == "GHOST" || elem.getAttribute("isghost") == "true");
}

function areChildrenGhosts(n) {
  return n && n.getAttribute && n.getAttribute("children-are-ghosts") == "true";
}
function hasGhostAncestor(htmlElem) {
  if(htmlElem == null) return false;
  if(isGhostNode(htmlElem)) return true;
  return areChildrenGhosts(htmlElem.parentNode) || hasGhostAncestor(htmlElem.parentNode);
}
function isGhostAttributeKey(name) {
  return name.startsWith("ghost-");
}

globalGhostAttributeKeysFromNode = [];
(globalGhostAttributeKeysFromNode || []).push(n =>
  ((n && n.getAttribute && n.getAttribute("list-ghost-attributes")) || "").split(" ").filter(a => a != "")
);
(globalGhostAttributeKeysFromNode || []).push(n =>
  n && n.tagName == "HTML" ? ["class"] : []
);
(globalGhostAttributeKeysFromNode || []).push(n =>
  n && n.tagName == "BODY" ? ["data-gr-c-s-loaded"] : []
);

function isSpecificGhostAttributeKeyFromNode(n) {
  var additionalGhostAttributes = [];
  for(var k in globalGhostAttributeKeysFromNode) {
    additionalGhostAttributes = additionalGhostAttributes.concat(globalGhostAttributeKeysFromNode[k](n))
  }
  return (a => name => a.indexOf(name) != -1)(additionalGhostAttributes);
}

setGhostOnInserted = [];

// Analytics scripts
(setGhostOnInserted || []).push(insertedNode =>
  insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
     insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1
);

// For for ace styles in header
(setGhostOnInserted || []).push(insertedNode => {
    if(insertedNode.tagName == "STYLE" && typeof insertedNode.getAttribute("id") == "string" &&
     (insertedNode.getAttribute("id").startsWith("ace-") ||
      insertedNode.getAttribute("id").startsWith("ace_"))) {
      insertedNode.setAttribute("save-ghost", "true"); 
      return true;
    } else {
      return false;
    }
  }
);
// For ace css themes
(setGhostOnInserted || []).push(insertedNode => {
  if(insertedNode.tagName == "STYLE" && insertedNode.getAttribute("id") == null) {
    if(insertedNode.nextSibling && insertedNode.nextSibling.getAttribute &&
      insertedNode.nextSibling.getAttribute("id") &&
      insertedNode.nextSibling.getAttribute("id").startsWith("ace-")) {
      insertedNode.setAttribute("id", "loaded-aced-theme");
      insertedNode.setAttribute("save-ghost", "true");
      return true;
    }
    return false;
  }
  return false;
  }
);
// For ace script for syntax highlight
(setGhostOnInserted || []).push(insertedNode =>
  insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
     insertedNode.getAttribute("src").startsWith("https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.2/mode-j")
);
// For ace script for syntax highlight
(setGhostOnInserted || []).push(insertedNode =>
  insertedNode.tagName == "ACE_OUTER"
);

function handleScriptInsertion(mutations) {
  for(var i = 0; i < mutations.length; i++) {
    // A mutation is a ghost if either
    // -- The attribute starts with 'ghost-'
    // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
    // -- It is the modification of a node or an attribute inside a ghost node.
    var mutation = mutations[i];
    if(hasGhostAncestor(mutation.target)) continue;
    if(mutation.type == "childList") {
      for(var j = 0; j < mutation.addedNodes.length; j++) {
        var insertedNode = mutation.addedNodes[j];
        if(!hasGhostAncestor(insertedNode) && (insertedNode.nodeType == 1 && insertedNode.getAttribute("isghost") != "true" || insertedNode.noteType == 3 && !insertedNode.isghost) && setGhostOnInserted.find(pred => pred(insertedNode))) {
         if(insertedNode.nodeType == 1) insertedNode.setAttribute("isghost", "true");
         insertedNode.isghost = true;
        }
      }
    }
  }
}

if (typeof analyticsScriptNeutralizer !== "undefined") {
  // console.log("analyticsScriptNeutralizer.disconnect()");
  analyticsScriptNeutralizer.disconnect();
}

analyticsScriptNeutralizer = new MutationObserver(handleScriptInsertion);
analyticsScriptNeutralizer.observe
 ( document.body.parentElement
 , { attributes: false
   , childList: true
   , characterData: false
   , attributeOldValue: false
   , characterDataOldValue: false
   , subtree: true
   }
 )

// Self-editing capabilities
function getSelectionStart() {
   var node = document.getSelection().anchorNode;
   return (node != null && node.nodeType == 3 ? node.parentNode : node);
}
function getEnclosingCaret(tagName) {
  var w = getSelectionStart();
  console.log("where", w);
  while(w != null && w.tagName.toLowerCase() != tagName.toLowerCase()) {
    w = w.parentNode;
  }
  return w;
}
function emptyTextContent(node) {
  if(node != null) {
    if(node.nodeType == 3) {
      node.textContent = "";
    } else {
      for(i in node.childNodes) {
        emptyTextContent(node.childNodes[i]);
      }
    }
  }
  return node;
}
function insertBefore(parent, node, beforeNode) {
  if(beforeNode == null) {
    parent.append(node);
  } else {
    parent.insertBefore(node, beforeNode);
  }
}

function duplicate(node, options) {
  if(typeof options == "undefined") options = {}
  if(typeof options.onBeforeInsert != "function") options.onBeforeInsert = e => e;
  if(node != null && node.parentNode != null) {
    var insertBeforeNode = options.after ? node.nextSibling : node;
    if(node.previousSibling != null) {
      var next = node.nextSibling;
      if(next.nodeType == 3 && next.nextSibling != null &&
         next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "LI" || node.tagName == "TD")) {
        var textElement = next.cloneNode(true);
        insertBefore(node.parentNode, textElement, options.after ? node.nextSibling : node);
        if(options.after) {
          insertBeforeNode = textElement.nextSibling;
        } else {
          insertBeforeNode = textElement
        }
      }
    }
    var cloned = options.onBeforeInsert(node.cloneNode(true));
    insertBefore(node.parentNode, cloned, insertBeforeNode);
  }
}
function remove(node) {
  if(node.previousSibling != null) { // Remove whitespace as well
    var next = node.nextSibling;
    if(next.nodeType == 3 && next.nextSibling != null &&
       next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "LI" || node.tagName == "TD")) {
      next.remove();
    }
  }
  node.remove();
}
</script>
]

codepreview sourcecontent = 
<div class="codepreview" id="editor_codepreview">
  <textarea id="editor_codepreview_textarea" save-attributes="scrollTop"
    style="width:100%;height:200px" v=sourcecontent onchange="this.setAttribute('v', this.value)">@(Update.softFreeze (if Regex.matchIn "^\r?\n" sourcecontent then "\n" + sourcecontent else sourcecontent))</textarea>
</div>
    
editionscript = """
  
  // Save / Load ghost attributes after a page is reloaded.
  // Same for some attributes
  function saveGhostAttributes() {
    var ghostModified = document.querySelectorAll("[ghost-visible]");
    var idGhostAttributes = [];
    for(var i = 0; i < ghostModified.length; i++) {
      var id = ghostModified[i].getAttribute("id");
      if(id !== null && typeof id !== "undefined") {
        idGhostAttributes.push([id,
          {"ghost-visible": ghostModified[i].getAttribute("ghost-visible")}]);
      }
    }
    var elemsWithAttributesToSave = document.querySelectorAll("[save-attributes]");
    var idDynamicAttributes = [];
    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {
      var elem = elemsWithAttributesToSave[i];
      var id = elem.getAttribute("id");
      if(id !== null && typeof id !== "undefined") {
        var toSave = elem.getAttribute("save-attributes").split(" ");
        for(j in toSave) {
          var key = toSave[j];
          idDynamicAttributes.push([id, key, elem[key]])
        }
      }
    }
    var ghostElemsToReinsert = document.querySelectorAll("[save-ghost]");
    var parentsGhostNodes = [];
    for(var i = 0; i < ghostElemsToReinsert.length; i++) {
      var elem = ghostElemsToReinsert[i];
      if(elem.parentNode.tagName == "HEAD") {
        parentsGhostNodes.push({parentSelector: "HEAD", node: elem});
      } else {
        var id =  elem.parentNode.getAttribute("id");
        if(id != null) {
          parentsGhostNodes.push({parentSelector: "#"+id, node: elem});
        }
      }
    }
    return [idGhostAttributes, idDynamicAttributes, parentsGhostNodes];
  }
  function applyGhostAttributes(attrs) {
    var [idGhostAttributes, idDynamicAttributes, parentsGhostNodes] = attrs;
    for(var i in idGhostAttributes) {
      var [id, attrs] = idGhostAttributes[i];
      var elem = document.getElementById(id);
      if(elem !== null && typeof elem !== "undefined") {
        for(key in attrs) {
          elem.setAttribute(key, attrs[key]);
        }
      }
    }
    for(var i in idDynamicAttributes) {
      var [id, key, value] = idDynamicAttributes[i];
      var elem = document.getElementById(id);
      if(elem != null) {
        elem[key] = value;
      }
    }
    for(var i in parentsGhostNodes) {
      var {parentSelector: selector, node: elem} = parentsGhostNodes[i];
      var parent = document.querySelector(selector);
      if(parent != null) {
        if(!elem.getAttribute("id") || !document.getElementById(elem.getAttribute("id"))) {
          parent.appendChild(elem);
        }
      }
    }
  }
  
  function domNodeToNativeValue(n) {
      if(n.nodeType == "3") {
        return ["TEXT", n.textContent];
      } else if(n.nodeType == "8") {
        return ["COMMENT", n.textContent];
      } else {
        var attributes = [];
        var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(n);
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          var value = n.attributes[i].value;
          if(!isGhostAttributeKey(key) && !isSpecificGhostAttributeKey(key)) {
            if(key == "style") {
              value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2);
            }
            attributes.push([key, value]);
          }
        }
        var children = [];
        if(!areChildrenGhosts(n)) {
          for(i = 0; i < n.childNodes.length; i++) {
            if(!isGhostNode(n.childNodes[i])) {
              children.push(domNodeToNativeValue(n.childNodes[i]));
            }
          }
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
    
    handleServerPOSTResponse = (xmlhttp, onBeforeUpdate) => function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          //console.log("Received new content. Replacing the page.");
          if(typeof onBeforeUpdate !== "undefined") onBeforeUpdate();
          var saved = saveGhostAttributes();
          replaceContent(xmlhttp.responseText);
          applyGhostAttributes(saved);
          
          var newQueryStr = xmlhttp.getResponseHeader("New-Query");
          var ambiguityKey = xmlhttp.getResponseHeader("Ambiguity-Key");
          var ambiguityNumber = xmlhttp.getResponseHeader("Ambiguity-Number");
          var ambiguitySelected = xmlhttp.getResponseHeader("Ambiguity-Selected");
          var ambiguityEnd = xmlhttp.getResponseHeader("Ambiguity-End");
          var ambiguitySummaries = xmlhttp.getResponseHeader("Ambiguity-Summaries");
          if(ambiguityKey !== null && typeof ambiguityKey != "undefined" &&
             ambiguityNumber !== null && typeof ambiguityNumber != "undefined" &&
             ambiguitySelected !== null && typeof ambiguitySelected != "undefined") {
             
            var n = JSON.parse(ambiguityNumber);
            var selected = JSON.parse(ambiguitySelected);
            var summaries = JSON.parse(ambiguitySummaries);
            var newMenu = document.createElement("menuitem");
            var disambiguationMenu = `<span style="color:red" id="ambiguity-id" v="${ambiguityKey}">Ambiguity.</span> Solutions `;
            for(var i = 1; i <= n; i++) {
              var summary = summaries[i-1].replace(/"/g,'&quot;');
              if(i == selected) {
                disambiguationMenu += ` <span class="solution selected" title="${summary}">#${i}</span>`
              } else {
                disambiguationMenu += ` <span class="solution${i == n && ambiguityEnd != 'true' ? ' notfinal' : ''}" title="${summary}" onclick="this.classList.add('to-be-selected'); selectAmbiguity('${ambiguityKey}', ${i})">#${i}</span>`
              }
            }
            disambiguationMenu += ` <button onclick='acceptAmbiguity("${ambiguityKey}", ${selected})'>Save</button>`;
            disambiguationMenu += ` <button onclick='cancelAmbiguity("${ambiguityKey}", ${selected})'>Cancel</button>`;
            newMenu.innerHTML = disambiguationMenu;
            newMenu.setAttribute("isghost", "true");
            if(document.getElementById("themenu"))
              document.getElementById("themenu").append(newMenu);
          } else {
            var opSummary = decodeURI(xmlhttp.getResponseHeader("Operations-Summary"));
            var log = document.createElement("span");
            log.setAttribute("class", "summary");
            log.innerText = "Last action: " + opSummary;
            var newMenu = document.createElement("menuitem");
            newMenu.append(log);
            newMenu.setAttribute("isghost", "true");
            if(document.getElementById("themenu"))
              document.getElementById("themenu").append(newMenu);
          }
          if(newQueryStr !== null) {
            var newQuery = JSON.parse(newQueryStr);
            var strQuery = "";
            for(var i = 0; i < newQuery.length; i++) {
              var {_1: key, _2: value} = newQuery[i];
              strQuery = strQuery + (i == 0 ? "?" : "&") + key + "=" + value
            }
            window.history.replaceState({}, "Current page", strQuery);
          }
        }
    }
    
    function selectAmbiguity(key, num) {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("ambiguity-key", key);
      xmlhttp.setRequestHeader("select-ambiguity", JSON.stringify(num));
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      xmlhttp.setRequestHeader("question", "true");
      xmlhttp.send("{\"a\":1}");
    }
    
    function acceptAmbiguity(key, num) {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("ambiguity-key", key);
      xmlhttp.setRequestHeader("accept-ambiguity", JSON.stringify(num));
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      xmlhttp.send("{\"a\":1}");
    }
    
    function cancelAmbiguity(key, num) {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("ambiguity-key", key);
      xmlhttp.setRequestHeader("cancel-ambiguity", JSON.stringify(num));
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      xmlhttp.send("{\"a\":1}");
    }
    
    function sendModificationsToServer() {
      if(document.getElementById("notification-menu") != null) {
        //document.getElementById("notification-menu").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`
        return;
      }
      var newMenu = document.createElement("menuitem");
      var notification = `Updating the source files...`;
      newMenu.innerHTML = notification;
      newMenu.setAttribute("isghost", "true");
      newMenu.setAttribute("id", "notification-menu");
      newMenu.classList.add("to-be-selected");
      if(document.getElementById("themenu") && document.getElementById("manualsync-menuitem")) {
        document.getElementById("themenu").append(newMenu);
        document.getElementById("manualsync-menuitem").setAttribute("ghost-visible", "false");
      }
      
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp, () => {
        if(document.getElementById("input-autosave") && !document.getElementById("input-autosave").checked) {
          document.getElementById("manualsync-menuitem").setAttribute("ghost-visible", "true") // Because it will be saved
        }
      });
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      xmlhttp.setRequestHeader("question", document.getElementById("input-question") && document.getElementById("input-question").checked ? "true" : "false");
      xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));
    }
    
    function handleMutations(mutations) {
      var onlyGhosts = true;
      for(var i = 0; i < mutations.length && onlyGhosts; i++) {
        // A mutation is a ghost if either
        // -- The attribute starts with 'ghost-'
        // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
        // -- It is the modification of a node or an attribute inside a ghost node.
        var mutation = mutations[i];
        if(hasGhostAncestor(mutation.target)) {
          continue;
        }
        if(mutation.type == "attributes") {
          var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(mutation.target);
          if(isGhostAttributeKey(mutation.attributeName) || isSpecificGhostAttributeKey(mutation.attributeName)) {
          } else {
            onlyGhosts = false;
            console.log("Attribute is not ghost", mutation);
          }
        } else if(mutation.type == "childList") {
          if(!areChildrenGhosts(mutation.target)) {
            for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {
              if(!hasGhostAncestor(mutation.addedNodes[j])) {
                onlyGhosts = false;
                console.log(`Added node ${j} does not have a ghost ancestor`, mutation);
              }
            }
            for(var j = 0; j < mutation.removedNodes.length && onlyGhosts; j++) {
              if(!isGhostNode(mutation.removedNodes[j])) {
                onlyGhosts = false;
                console.log(`Removed node ${j} was not a ghost`, mutation);
              }
            }
          }
        } else {
          onlyGhosts = false;
          console.log("mutations other than attributes and childList are not ghosts", mutations);
        }
      }
      if(onlyGhosts) {
        console.log("mutations are only ghosts, skipping");
        return;
      } // Send in post the new HTML along with the URL
      if(document.getElementById("input-autosave") && !document.getElementById("input-autosave").checked) {
        if(document.getElementById("manualsync-menuitem")) {
          document.getElementById("manualsync-menuitem").setAttribute("ghost-disabled", "false");
        }
        return;
      }
      
      if(typeof t !== "undefined") {
        clearTimeout(t);
      }
      t = setTimeout(function() {
        t = undefined;
        
        sendModificationsToServer();
      }, @editdelay)
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
     }, 10)
    
    function pasteHtmlAtCaret(html) {
      var sel, range;
      if (window.getSelection) {
          // IE9 and non-IE
          sel = window.getSelection();
          if (sel.getRangeAt && sel.rangeCount) {
              range = sel.getRangeAt(0);
              range.deleteContents();

              // Range.createContextualFragment() would be useful here but is
              // only relatively recently standardized and is not supported in
              // some browsers (IE9, for one)
              var el = document.createElement("div");
              el.innerHTML = html;
              var frag = document.createDocumentFragment(), node, lastNode;
              while ( (node = el.firstChild) ) {
                  lastNode = frag.appendChild(node);
              }
              range.insertNode(frag);

              // Preserve the selection
              if (lastNode) {
                  range = range.cloneRange();
                  range.setStartAfter(lastNode);
                  range.collapse(true);
                  sel.removeAllRanges();
                  sel.addRange(range);
              }
          }
      } else if (document.selection && document.selection.type != "Control") {
          // IE < 9
          document.selection.createRange().pasteHTML(html);
      }
    }
     
    function handleFileSelect(evt) {
      evt.stopPropagation();
      evt.preventDefault();

      var insertRelative = true;
      
      var files = evt.dataTransfer.files; // FileList object.
      // files is a FileList of File objects. List some properties.
      var output = [];
      for (var i = 0, f; f = files[i]; i++) {
        if(f.type.indexOf("image") == 0 && f.size < 30000000) {
          // process image files under 30Mb
          var xhr = new XMLHttpRequest();
          var tmp = location.pathname.split("/");
          tmp = tmp.slice(0, tmp.length - 1);
          var storageFolder = tmp.join("/");
          var storageLocation =  storageFolder + "/" + f.name;
          xhr.onreadystatechange = ((xhr, path, name) => () => {
            if (xhr.readyState == XMLHttpRequest.DONE) {
              pasteHtmlAtCaret(`<img src="${path}" alt="${name}">`);
            } else {
              console.log("Error while uploading picture", xhr);
            }
          })(xhr, insertRelative ? f.name : storageLocation, f.name)
          xhr.open("POST", storageLocation, true);
          xhr.setRequestHeader("write-file", f.type);
          xhr.send(f);
        }
      }
    }

    function handleDragOver(evt) {
      evt.stopPropagation();
      evt.preventDefault();
      evt.dataTransfer.dropEffect = 'copy'; // Explicitly show this is a copy.
    }

    if(@(if varedit then "true" else "false")) {
      var dropZone = document.body;
      dropZone.addEventListener('dragover', handleDragOver, false);
      dropZone.addEventListener('drop', handleFileSelect, false);
    }
    
    // Shortcuts
    document.onkeydown = function(e) {
      var key = e.which || e.keyCode;
      if (e.which == 83 && (e.ctrlKey || e.metaKey)) { // CTRL+S or CMD+S: Save
        closeLinkWindow();
        sendModificationsToServer();
        e.preventDefault();
      }
      if(e.which == 75 && (e.ctrlKey || e.metaKey)) { // CTRL+K: Insert link
        document.execCommand('createLink', false, 'http://');
        e.preventDefault();
      }
    };
    document.onkeyup = document.onkeydown
    
    observeTargetA = null;
    
    addEditEqualToUrl = function(href, what) {
      if(href.indexOf("://") == -1) { // Instrument the relative link so that it is edit=true
        if(href.indexOf("?") >= 0) {
          if(href.endsWith("?")) {
            href = href + "edit=" + what
          } else {
            href = href + "&edit=" + what
          }
        } else {
          href = href + "?edit=" + what
        }
      }
      return href;
    }
    
    onClickOnLink = function (event) {
      var clickedElem = event.target;
      if(clickedElem && clickedElem.tagName == "A" && clickedElem.getAttribute("id") != "docslinkbubble-linkpreview") {
        var href = clickedElem.getAttribute("href");
        if(href) {
          var bottomX = clickedElem.offsetLeft;
          var bottomY = clickedElem.offsetTop + clickedElem.offsetHeight;
          var d = document.getElementById("docslinkbubble");
          d.setAttribute("style", "left: " + bottomX + "; top: " + bottomY);
          var targetA = document.getElementById("docslinkbubble-linkpreview");
          var updateHref = function(href) {
            href = @(if listDict.get "edit" defaultOptions |> Maybe.withDefault False |> not then """
            addEditEqualToUrl(href, "true");""" else "href;")
            targetA.setAttribute("href", href);
          }
          updateHref(href);
          targetA.innerText = href;
          d.setAttribute("ghost-visible", "true");
          
          var deleteButton = document.getElementById("docslinkbubble-delete");
          deleteButton.onclick = () => {
            clickedElem.outerHTML = clickedElem.innerHTML;
            d.setAttribute("ghost-visible", "false");
          }
          
          var modifyButton = document.getElementById("docslinkbubble-modify");
          modifyButton.onclick = () => {
            if (observeTargetA != null) {
              observeTargetA.disconnect();
              observeTargetA = null;
            }
            if(targetA.getAttribute("contenteditable") == "true") {
              targetA.setAttribute("contenteditable", "false");
            } else {
              targetA.setAttribute("contenteditable", "true");
              targetA.focus();
              console.log("reconnect observeTargetA", targetA);
              observeTargetA = new MutationObserver(function(mutations) {
                console.log("targetA modified");
                clickedElem.setAttribute("href", targetA.innerText);
                updateHref(targetA.innerText); // Instrumented link
              });
              observeTargetA.observe
               ( targetA
               , { attributes: false
                 , childList: true
                 , characterData: true
                 , attributeOldValue: false
                 , characterDataOldValue: true
                 , subtree: true
                 }
               )
            }
          }
        }
      } else if(clickedElem.getAttribute &&
           (clickedElem.getAttribute("id") == "docslinkbubble" ||
            (clickedElem.parentNode && clickedElem.parentNode.getAttribute && clickedElem.parentNode.getAttribute("id") == "docslinkbubble") ||
            (clickedElem.parentNode && clickedElem.parentNode.parentNode &&
             clickedElem.parentNode.parentNode.getAttribute && clickedElem.parentNode.parentNode.getAttribute("id") == "docslinkbubble")
            )) {
      } else {
        closeLinkWindow();
      }
      // Check if the event.target matches some selector, and do things...
    }
    
    // Links edition - Might be empty.
    @(if varedit == False && (listDict.get "edit" defaultOptions |> Maybe.withDefault False) == True then
      -- Special case when ?edit=false but the default behavior is edit=true if nothing is set.
      """document.onclick = function (e) {
          e = e ||  window.event;
          var node = e.target || e.srcElement;
          while(node) {
            if(node.tagName == "A" && node.getAttribute("href") && !node.onclick && !node.getAttribute("onclick")) {
             var newLocation = addEditEqualToUrl(node.getAttribute("href"), "false");
             console.log(newLocation);
             window.location.href = newLocation;
             e.stopPropagation();
             return false;
            } else {
              node = node.parentNode;
            }
          }
        }"""
    else if varedit then
      """document.addEventListener('click', onClickOnLink, false);"""
    else "")
    
    function closeLinkWindow() {
      var d = document.getElementById("docslinkbubble");
      if(d && d.getAttribute("ghost-visible") == "true") {
        d.setAttribute("ghost-visible", "false");
        var targetA = document.getElementById("docslinkbubble-linkpreview");
        targetA.setAttribute("contenteditable", "false");
      }
    }
"""

main