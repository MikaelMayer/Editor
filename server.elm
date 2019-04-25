-- input: path            The file to serve.
-- input: vars:           URL query vars.
-- input: defaultOptions  default options (options that vars can override for certain parts).
--                        If nodefs is set, will use it instead of nodejs.nodeFS
--                        If browserSide is set, will use a different kind of request. 
-- input: fileOperations  The current set of delayed file disk operations.
--    Note that Elm pages are given in context the path, the vars, and the file system (fs) to read other files
-- output: The page, either (almost) uninstrumented or augmented with the toolbar and edit scripts.

{--------------------------------------------------------
    Permission handling, file system, options processing
---------------------------------------------------------}
listGetOrElse key listDict default = listDict.get key listDict |> Maybe.withDefault default

preludeEnv = let _ = googlesigninbutton in -- Forces googlesigninbutton to be evaluated before preludeEnv
  __CurrentEnv__

mbApplyPrefix = case listDict.get "path" defaultOptions of
  Just "" -> Nothing
  Nothing -> Nothing
  Just prefix -> Just (\name -> if name == "" then prefix
      else if Regex.matchIn "/$" prefix then prefix + name
      else prefix + "/" + name)

directReadFileSystem =
  listDict.get "nodefs" defaultOptions |> Maybe.withDefault nodejs.nodeFS

fs = nodejs.delayedFS directReadFileSystem fileOperations

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
  Maybe.map (\original ->
    Update.bijection
      (case of "true" -> True; "" -> True; _ -> False)
      (case of True -> if original == "true" || original == "" then original else "true"; _ -> "false") original) |>
  Maybe.withDefaultReplace (
    listDict.get name defaultOptions |> Maybe.withDefault resDefault |> freeze)

varadmin = boolVar "admin" False
varedit = boolVar "edit" True
varproduction = listDict.get "production" defaultOptions |> Maybe.withDefault (freeze False)
iscloseable = listDict.get "closeable" defaultOptions |> Maybe.withDefault (freeze False)

userpermissions = {pageowner= True, admin= varadmin}
permissionToCreate = userpermissions.admin
permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication
-- List.contains ("sub", "102014571179481340426") user -- That's my Google user ID.

canEditPage = userpermissions.pageowner && varedit

{freezeWhen} = Update

serverOwned what = freezeWhen (not permissionToEditServer) (\od -> """You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>

If you really intended to modify this, add ?admin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.<br><br>

For debugging purposes, below is the new value that was pushed:
<pre>@(Regex.replace "<" (always "&lt;") """@od""")</pre>
""")

canEvaluate = listDict.get "evaluate" vars |> Maybe.withDefaultReplace (serverOwned "default value of evaluate" "true")

{--------------------------------------------------------
 Rewrite path to either a folder or a default file under
---------------------------------------------------------}

path: String
path =
  if fs.isdir path then
   if listDict.get "ls" vars /= Just "true" then
     List.mapFirstSuccess (\test ->
       if fs.isfile <| path + test then Just (path + test) else Nothing)
       ["index.elm" , "/index.elm", "index.html", "/index.html", "README.md" , "/README.md" ]
     |> Maybe.withDefault path
   else path
  else path

{---------------------------------------------------------------------------
 Retrieves the string content of the path. For folders, creates a custom page
----------------------------------------------------------------------------}

(sourcecontent, folderView): (String, Boolean)
(sourcecontent, folderView) = Tuple.mapFirst String.newlines.toUnix <|
  if path == "server.elm" then
    ("""<html><head></head><body>The Elm server cannot display itself. This is a placeholder</body></html>""", False)
  else
    if fs.isdir path then
      flip (,) True <|
      let
        pathprefix = if path == "" then path else path + "/"
        maybeUp = case Regex.extract "^(.*)/.*$" path of
          Just [prev] -> """<li><a href="/@prev">..</li> :: """
          _ -> if path == "" then "" else """<li><a href="/" contenteditable="false">..</li> ::"""
      in
      """
      <html><head></head><body><h1><a href=''>/@path</a></h1>
      @@(["ul", [], @maybeUp (List.map (\name -> <li><a href=("/@pathprefix" + name)>@@name</li>) (fs.listdir path))])
      Hint: place a
      <a href=("/@(pathprefix)README.md?edit=true")  contenteditable="false">README.md</a>,
      <a href=("/@(pathprefix)index.html?edit=true") contenteditable="false">index.html</a> or
      <a href=("/@(pathprefix)index.elm?edit=true")  contenteditable="false">index.elm</a>
      file to display something else than this page.</body></html>"""
    else
      flip (,) False <|
      if fs.isfile path && Regex.matchIn """\.(png|jpg|ico|gif|jpeg)$""" path then -- Normally not called because server.js takes care of these cases.
        """<html><head><title>@path</title></head><body><img src="@path"></body></html>"""
      else
        fs.read path
      |> Maybe.withDefaultReplace (
        serverOwned "404 page" """<html><head></head><body>@(
            if permissionToCreate then """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>"""
          )</body></html>"""
      )

{---------------------------------------------------------------------------
 Evaluates the page according to the path extension.
 - Wraps html pages to parse them as raw html
 - Interprets markdown pages and evaluate them as raw html with CSS
 - Directly evaluate sources from elm/leo pages or folders
----------------------------------------------------------------------------}
evaluatedPage: Result String Html
evaluatedPage =
  if canEvaluate /= "true" then
    Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>
  else
  let isPhp = Regex.matchIn """\.php$""" path in
  let isHtml = Regex.matchIn """\.html$""" path in
  if isHtml || isPhp then
    let sourcecontent = if isHtml then sourcecontent else
      let phpToElm =
        let phpStringToElmString =
          (Regex.replace """(\")([^\"]*)(\")""" <| \m ->
            nth m.group 1 +
            (nth m.group 2
            |> Regex.replace """\$[0-9a-zA-Z_]*""" (\n ->
               freeze "\" + " + nth n.group 0 + freeze " + \"")) +
            nth m.group 3) >>
          (Regex.replace """\$_GET\[([^\]]*)\]""" <| \m ->
            freeze "listDict.get "+ nth m.group 1 + freeze " $_GET |> Maybe.withDefaultReplace ''"
          )
        in
        \string ->
        string |>
        Regex.replace """<\?php\s+echo\s+([^;]+?);\s+\?>"""
           (\m -> freeze "@(" + nth m.group 1 + freeze ")") |>
        Regex.replace """^\s*<\?php(\s+(?:(?!\?>)[\s\S])*)\?>([\s\S]*)$"""
          (\m ->
            nth m.group 1 
            |> Regex.replace """(\r?\n\s*)(\$[0-9a-zA-Z_]*\s*=\s*)((?:(?!;).)*)(;)"""
                 (\assign -> (nth assign.group 1) +
                   freeze "let " +
                   (nth assign.group 2) +
                   phpStringToElmString (nth assign.group 3) +
                   freeze " in")
            |> (\res -> res + freeze " " + String.q3 + nth m.group 2 + String.q3))
      in
      let elmSourceContent = phpToElm sourcecontent in
      __evaluate__ (("$_GET", vars)::("path", path)::("fs", fs)::preludeEnv) elmSourceContent |>
      case of
        Err msg -> serverOwned "error message" "<html><head></head><body><pre>Error elm-reinterpreted php: " + msg + "</pre></body></html>"
        Ok sourcecontent -> sourcecontent
    in
    let interpretableData =
          case Regex.extract """^\s*<!DOCTYPE(?:(?!>)[\s\S])*>([\s\S]*)$""" sourcecontent of
            Just [interpretableHtml] -> serverOwned "begin raw tag" "<raw>" + interpretableHtml + serverOwned "end raw tag" "</raw>"
            _ -> serverOwned "raw display of html - beginning" """<raw><html><head></head><body>""" + sourcecontent + serverOwned "raw display of html - end" """</body></html></raw>"""
    in
    __evaluate__ preludeEnv interpretableData
    |> Result.andThen (case of
      ["raw", _, nodes] ->
        case List.find (case of ["html", _, _] as n -> True; _ -> False) nodes of
          Just n -> Ok n
          Nothing -> Err """No top-level HTML node found""" 
      result -> Err """Html interpretation error: The interpretation of raw html did not work but produced @result"""
    )
  else if Regex.matchIn """\.md$""" path then
    let markdownized = String.markdown sourcecontent in
      case Html.parseViaEval markdownized of
        x -> 
          let markdownstyle = fs.read "markdown.css" |> Maybe.withDefaultReplace defaultMarkdowncss in
          Ok <html><head></head><body><style title="If you modify me, I'll create a custom markdwon.css that will override the default CSS for markdown rendering">@markdownstyle</style><div class="wrapper">@x</div></body></html>
  else if Regex.matchIn """\.(elm|leo)$""" path || fs.isdir path then
    __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::preludeEnv) sourcecontent
  else Err """Serving only .html, .md and .elm files. Got @path"""

{---------------------------------------------------------------------------
 Recovers from evaluation errors
----------------------------------------------------------------------------}
recoveredEvaluatedPage: Html
recoveredEvaluatedPage = case evaluatedPage of
  Err msg -> serverOwned "Error Report" <|
    <html><head></head><body style="color:#cc0000"><div style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">@msg</pre></div></body></html>
  Ok page -> page

{---------------------------------------------------------------------------
 Instruments the resulting HTML page
 - Removes whitespace that are siblings of <head> and <body>
 - !f the page is editable:
   * Adds the contenteditable attribute to body
   * Adds the edition menu and the source preview area
 - Else: Adds the "edit" box to switch to edit mode
 - Adds the initial scripts
 - Append the edition scripts so that we can modify the page even without edit mode (that's dangerous, should we avoid this?)
----------------------------------------------------------------------------}
main: Html
main = case recoveredEvaluatedPage of
  ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
    List.filter (case of [_, _] -> False; _ -> True) |>
    List.mapWithReverse identity (case of
      ["body", bodyattrs, bodychildren] ->
        ["body",
           (if canEditPage then
             [["contenteditable", "true"]] |> serverOwned "contenteditable attribute of the body due to edit=true" 
            else freeze []) ++
           bodyattrs,
          (if canEditPage then (serverOwned "edition menu" editionmenu ++ Update.sizeFreeze [(serverOwned "code preview box" codepreview) sourcecontent]) else
           if not varedit && not iscloseable && not varproduction then serverOwned "open edit box" [openEditBox] else
           serverOwned "edit prelude when not in edit mode" []) ++
           serverOwned "initial script" initialScript ++
           bodychildren ++
           (serverOwned "synchronization script and placeholder" [<script>@editionscript</script>, <div class="bottom-placeholder"> </div>])]
      x -> x -- head
    )]
  x-> <html><head></head><body>Not a valid html page: @("""@x""")</body></html>
  --|> Update.debug "main"

{---------------------------------------------------------------------------
 Definitions for the pipeline above
----------------------------------------------------------------------------}
  
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

-- Everything inside the modify menu is generated and is not visible to Editor
editionmenu = [
<div id="docslinkbubble" class="docs-bubble docs-linkbubble-bubble" list-ghost-attributes="style" help="Modify or delete a link" tabindex="0" contenteditable="false"><a rel="noreferrer" id="docslinkbubble-linkpreview" list-ghost-attributes="href contenteditable" children-are-ghosts="true"></a><span> – <button id="docslinkbubble-modify" class="docs-bubble-link" tabindex="0">Modify</button> | <button id="docslinkbubble-delete" class="docs-bubble-link" tabindex="0">Delete</button></span></div>,
<div id="modify-menu" list-ghost-attributes="style class" contenteditable="false">
<!--ul>
<li>Menu for mobile coming soon!</li>
<li>Selection, move, cut/copy/paste menu</li>
<li>Options menu (questions, autosave)</li>
<li>Source</li>
<li>Edit attribute menu / shortcut for links</li>
</ul-->
<div class="information" children-are-ghosts="true"></div>
</div>,
<div id="context-menu" children-are-ghosts="true" list-ghost-attributes="style class"></div>,
<menu id="themenu" ignore-modifications="true" class="edittoolbar" contenteditable="false">
@(if iscloseable then [] else closeEditBox)
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
  z-index: 100000;
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
}
menu input[type=checkbox]:checked + .label-checkbox {
  background: #bcbbff;
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
#editor_codepreview {
  width: 100%;
  height: 600px;
}
#editor_codepreview > textarea {
  width: 100%;
  height: 600px;
  -webkit-box-shadow: 10px 10px 5px 0px rgba(0,0,0,0.75);
  -moz-box-shadow: 10px 10px 5px 0px rgba(0,0,0,0.75);
  box-shadow: 10px 10px 5px 0px rgba(0,0,0,0.75);
}
@@media screen and (pointer: coarse) {
  body {
    font-size: 48px;
  }
  menu.edittoolbar {
    right: 10px;
  }
  div.editor-logo {
    display: none;
  }
  menuitem.filename {
    display: none;
  }
  /*menuitem {
    font-size: 2.5em;
  }*/
  button {
    font-size: 1em;
  }
  menuitem#question-menuitem {
    display: none;
  }
  menuitem#autosave-menuitem {
    display: none;
  }
  #menumargin {
    padding-top: 5em;
  }
  #editor_codepreview {
    width: 100%;
    height: 600px;
  }
  div.menu-separator {
    display: none;
  }
}
.summary {
  color: green;
}

div#modify-menu {
  -webkit-box-shadow: 0px 0px 34px 0px rgba(0,0,0,0.75);
  -moz-box-shadow: 0px 0px 34px 0px rgba(0,0,0,0.75);
  box-shadow: 0px 0px 34px 0px rgba(0,0,0,0.75);
  position: fixed;
  top: 0px;
  right: 0px;
  width: 400px;
  height: 100%;
  background-color: #DDD;
  font-size: 16px;
  transform: translate(100%, 0px);
  transition-property: transform;
  transition-duration: 0.5s;
}
div#modify-menu.visible {
  transform: translate(0%, 0%);
}
div#modify-menu h3 {
  margin-top: 2px;
  margin-bottom: 2px;
}
:root {
  --context-color: rgba(0, 128, 128, 0.8); 
  --context-button-color: rgba(0, 192, 192, 0.8);
  --context-button-color-hover: rgba(0, 212, 212, 0.8);
  --context-button-color-inert: rgba(128, 128, 128, 0.8);
  --context-button-color-inert-hover: rgba(150, 150, 150, 0.8);
  --context-menu-height: 30px;
  --context-menu-button-width: 40px;
}
[ghost-hovered=true] {
  outline: 2px solid var(--context-color);
}
[ghost-clicked=true] {
  outline: 2px solid var(--context-color);
}
div#context-menu {
  position: absolute;
  display: none;
  background-color: var(--context-color);
  color: white;
  font-weight: bold;
  z-index: 1000000;
}
div#context-menu.visible {
  display: block;
  height: var(--context-menu-height);
  width: 200px;
}
div#context-menu .context-menu-button {
  background: var(--context-button-color);
  display: inline-block;
  height: var(--context-menu-height);
  width: var(--context-menu-button-width);
  cursor: pointer;
}
div#context-menu .context-menu-button.inert {
  background: var(--context-button-color-inert)
}
svg.context-menu-icon > path {
  fill:none;
  stroke:#FFFFFF;
  stroke-width:2px;
  stroke-linecap:butt;
  -linejoin:miter;
  stroke-opacity:1;
}
svg.context-menu-icon.fill > path {
  fill:#FFFFFF;
  stroke-width:1px;
}
div#context-menu .context-menu-button:hover {
  background: var(--context-button-color-hover);
}
div#context-menu .context-menu-button.inert:hover {
  background: var(--context-button-color-inert-hover)
}
@@media (pointer: coarse) {
  div#modify-menu {
    font-size: 48px;
    bottom: 0px;
    left: 0px;
    top: initial;
    right: initial;
    height: 30%;
    width: 100%;
    transform: translate(0px, 100%);
  }
  div.bottom-placeholder {
    width: 100%;
    height: 30%;
  }
  
  :root {
    --context-menu-height: 48px;
    --context-menu-button-width: 48px;
  }
}

</style>
<div class="editor-logo">Editor <a href= "https://github.com/MikaelMayer/Editor/issues">(report issue)</a></div>
<div class="menu-separator"></div>
<menuitem class= "filename" title= "the path of the file you are currently viewing">@(if path == "" then serverOwned "empty path" "[root folder]" else path)</menuitem>
<div class="menu-separator"></div><menuitem>
<label title="Display the source code of this pagge below"><input id="input-showsource" type="checkbox" save-properties="checked"
  onchange="""
var cp = document.getElementById("editor_codepreview");
if(cp !== null) {
   cp.setAttribute("ghost-visible", this.checked ? "true": "false")
}"""><span class= "label-checkbox">Source</span></label>
</menuitem><div class="menu-separator"></div>
<menuitem id="question-menuitem">
<label title="If off, ambiguities are resolved automatically. Does not apply for HTML pages"><input id="input-question" type="checkbox" save-properties="checked" @(case listDict.get "question" vars of
                       Just questionattr -> boolToCheck questionattr
                       _ -> serverOwned "initial checked attribute (use &question=false in query parameters to modify it)" (
                              if boolVar "question" True then [["checked", ""]] else []))><span class= "label-checkbox">Ask questions</span></label>
</menuitem>
<div class="menu-separator"></div>
<menuitem id="autosave-menuitem">
<label title="If on, changes are automatically propagated 1 second after the last edit"><input id="input-autosave" type="checkbox" save-properties="checked" onchange="document.getElementById('manualsync-menuitem').setAttribute('ghost-visible', this.checked ? 'false' : 'true')" @(case listDict.get "autosave" vars of
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
var XHRequest = @(if listDict.get "browserSide" defaultOptions == Just True then "ProxiedServerRequest" else "XMLHttpRequest");

function el(tag, attributes, children, properties) {
  let x = document.createElement(tag);
  if(typeof attributes == "object")
    for(let k in attributes)
      x.setAttribute(k, attributes[k]);
  if(Array.isArray(children)) {
    for(let child of children) {
      if(typeof child !== "undefined")
        x.appendChild(child);
    }
  } else if(typeof children !== "undefined") {
    x.append(children);
  }
  if(typeof properties == "object") {
    for(let k in properties)
      x[k] = properties[k];
  }
  return x;
}

// TODO: Find a way to store a cookie containing credentials, and have this server refresh tokens.
// https://developers.google.com/identity/sign-in/web/server-side-flow
// https://stackoverflow.com/questions/32902734/how-to-make-google-sign-in-token-valid-for-longer-than-1-hour
// https://www.w3schools.com/js/js_cookies.asp
if(typeof googleAuthIdToken == "undefined") {
  var googleAuthIdToken = undefined;
}

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
  ((n && n.getAttribute && n.getAttribute("list-ghost-attributes")) || "").split(" ").concat(
    ((n && n.getAttribute && n.getAttribute("save-ghost-attributes")) || "").split(" ")).filter(a => a != "")
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
// For Google sign-in buttons and i-frames
(setGhostOnInserted || []).push(insertedNode =>
  (insertedNode.tagName == "DIV" &&
    insertedNode.classList.contains("abcRioButton")) ||
  (insertedNode.tagName == "IFRAME" &&
    insertedNode.getAttribute("id") == "ssIFrame_google")
);
// For anonymous styles inside HEAD (e.g. ace css themes and google sign-in)
(setGhostOnInserted || []).push(insertedNode => 
  insertedNode.tagName == "STYLE" && insertedNode.getAttribute("id") == null &&
  insertedNode.parentElement.tagName == "HEAD" && (insertedNode.setAttribute("save-ghost", "true") || true)
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
// For the grammarly extension
(setGhostOnInserted || []).push(insertedNode =>
  insertedNode.classList.contains("gr-top-z-index") || insertedNode.classList.contains("gr-top-zero")
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
    if(node.nextSibling != null) {
      var next = node.nextSibling;
      if(next.nodeType == 3 && next.nextSibling != null &&
         next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "TH" || node.tagName == "LI" || node.tagName == "TD")) {
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
    return cloned;
  }
}
function remove(node) {
  if(node.previousSibling != null) { // Remove whitespace as well
    var next = node.nextSibling;
    if(next.nodeType == 3 && next.nextSibling != null &&
       next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "TH" || node.tagName == "LI" || node.tagName == "TD")) {
      next.remove();
    }
  }
  node.remove();
}
</script>
]

codepreview sourcecontent = 
<div class="codepreview" id="editor_codepreview">
  <textarea id="editor_codepreview_textarea" save-properties="scrollTop"
     v=sourcecontent @(if boolVar "autosave" True then [] else [["onkeyup", "this.setAttribute('v', this.value)"]]) onchange="this.setAttribute('v', this.value)">@(Update.softFreeze (if Regex.matchIn "^\r?\n" sourcecontent then "\n" + sourcecontent else sourcecontent))</textarea>
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
          "ghost-visible", ghostModified[i].getAttribute("ghost-visible")]);
      }
    }
    var ghostAttributesModified = document.querySelectorAll("[save-ghost-attributes]");
    console.log(ghostAttributesModified);
    for(var i = 0; i < ghostAttributesModified.length; i++) {
      var elem = ghostAttributesModified[i];
      var id = elem.getAttribute("id");
      if(id != null) {
        var toSave = elem.getAttribute("save-ghost-attributes").split(" ");
        for(j in toSave) {
          var key = toSave[j];
          idGhostAttributes.push([id, key, elem.getAttribute(key)]);
        }
      }
    }
    
    var elemsWithAttributesToSave = document.querySelectorAll("[save-properties]");
    var idDynamicAttributes = [];
    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {
      var elem = elemsWithAttributesToSave[i];
      var id = elem.getAttribute("id");
      if(id !== null && typeof id !== "undefined") {
        var toSave = elem.getAttribute("save-properties").split(" ");
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
      var [id, key, attr] = idGhostAttributes[i];
      var elem = document.getElementById(id);
      if(elem != null) {
        elem.setAttribute(key, attr);
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
            var newMenu = el("menuitem", {isghost: "true"});
            var disambiguationMenu = `<span style="color:red" id="ambiguity-id" v="${ambiguityKey}">Ambiguity.</span> Solutions `;
            for(var i = 1; i <= n; i++) {
              var summary = summaries[i-1].replace(/"/g,'&quot;');
              if(i == selected) {
                disambiguationMenu += ` <span class="solution selected" title="${summary}">#${i}</span>`
              } else {
                disambiguationMenu += ` <span class="solution${i == n && ambiguityEnd != 'true' ? ' notfinal' : ''}" title="${summary}" onclick="this.classList.add('to-be-selected'); selectAmbiguity('${ambiguityKey}', ${i})">#${i}</span>`
              }
            }
            disambiguationMenu += ` <button id="saveambiguity" onclick='acceptAmbiguity("${ambiguityKey}", ${selected})'>Save</button>`;
            disambiguationMenu += ` <button id="cancelAmbiguity" onclick='cancelAmbiguity("${ambiguityKey}", ${selected})'>Cancel</button>`;
            newMenu.innerHTML = disambiguationMenu;
            if(document.getElementById("themenu"))
              document.getElementById("themenu").append(newMenu);
          } else {
            var opSummaryEncoded = xmlhttp.getResponseHeader("Operations-Summary");
            if(opSummaryEncoded) {
              var opSummary = decodeURI(opSummaryEncoded);
              var newMenu =
                el("menuitem", {id: "lastaction", isghost: "true"},
                  el("span", {"class": "summary"}, "Last action: " + opSummary));
              if(document.getElementById("themenu"))
                document.getElementById("themenu").append(newMenu);
                var newmenutimeout = setTimeout(function() { newMenu.remove(); }, 2000);
                newMenu.onclick = ((n) => () => clearTimeout(n))(newmenutimeout);
            }
          }
          if(newQueryStr !== null) {
            var newQuery = JSON.parse(newQueryStr);
            var strQuery = "";
            for(var i = 0; i < newQuery.length; i++) {
              var {_1: key, _2: value} = newQuery[i];
              strQuery = strQuery + (i == 0 ? "?" : "&") + key + (value === "" && key == "edit" ? "" : "=" + value)
            }
            window.history.replaceState({}, "Current page", strQuery);
          }
        }
    }
    
    notifyServer = callback => {
      var xmlhttp = new XHRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp, () => {
        if(document.getElementById("input-autosave") && !document.getElementById("input-autosave").checked) {
          document.getElementById("manualsync-menuitem").setAttribute("ghost-visible", "true") // Because it will be saved
        }
      });
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      if(googleAuthIdToken) {
        xmlhttp.setRequestHeader("id-token", googleAuthIdToken)
      }
      var result = callback(xmlhttp);
      xmlhttp.send(result || "{\"a\":1}");
    }
    
    function reloadPage() { // Use this only after a successful login
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("reload", "true");
      })
    }
    
    function selectAmbiguity(key, num) {
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("ambiguity-key", key);
        xmlhttp.setRequestHeader("select-ambiguity", JSON.stringify(num));
        xmlhttp.setRequestHeader("question", "true");
      });
    }
    
    function acceptAmbiguity(key, num) {
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("ambiguity-key", key);
        xmlhttp.setRequestHeader("accept-ambiguity", JSON.stringify(num));
      });
    }
    
    function cancelAmbiguity(key, num) {
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("ambiguity-key", key);
        xmlhttp.setRequestHeader("cancel-ambiguity", JSON.stringify(num));
      });
    }
    
    function sendModificationsToServer() {
      if(document.getElementById("notification-menu") != null) {
        //document.getElementById("notification-menu").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`
        return;
      }
      var newMenu = el("menuitem",
        {isghost: true, id: "notification-menu", class:"to-be-selected"},
        `Updating the source files...`);
      if(document.getElementById('lastaction')) {
        document.getElementById('lastaction').remove();
      }
      if(document.getElementById("themenu") && document.getElementById("manualsync-menuitem")) {
        document.getElementById("themenu").append(newMenu);
        document.getElementById("manualsync-menuitem").setAttribute("ghost-visible", "false");
      }
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("question", document.getElementById("input-question") && document.getElementById("input-question").checked ? "true" : "false");
        return JSON.stringify(domNodeToNativeValue(document.body.parentElement));
      })
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
          console.log("mutations other than attributes, childList and characterData are not ghosts", mutations);
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
              var div = document.createElement("div");
              div.innerHTML = html;
              var frag = document.createDocumentFragment(), node, lastNode;
              while ( (node = div.firstChild) ) {
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
        var xhr = new XMLHttpRequest();
        var tmp = location.pathname.split("/");
        tmp = tmp.slice(0, tmp.length - 1);
        var storageFolder = tmp.join("/");
        var storageLocation =  storageFolder + "/" + f.name;
        //if(f.size < 30000000)
        xhr.onreadystatechange = ((xhr, filetype, path, name) => () => {
          if (xhr.readyState == XMLHttpRequest.DONE) {
            if (xhr.status == 200) {@(if folderView then """
              reloadPage();"""
              else """
              if(filetype.indexOf("image") == 0) {
                pasteHtmlAtCaret(`<img src="${path}" alt="${name}">`);
              }""")
            } else {
              console.log("Error while uploading picture", xhr);
            }
          }
        })(xhr, f.type, insertRelative ? f.name : storageLocation, f.name);
        @(if listDict.get "browserSide" defaultOptions == Just True then """
        xhr.open("POST", "/editor.php?location=" + encodeURIComponent(storageLocation), true);
        """ else """
        xhr.open("POST", storageLocation, true);
        """);
        xhr.setRequestHeader("write-file", f.type);
        xhr.send(f);
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
    
      var lastclick = 0;
      // Shortcuts
      document.onkeydown = function(e) {
        var key = e.which || e.keyCode;
        if (e.which == 83 && (e.ctrlKey || e.metaKey)) { // CTRL+S or CMD+S: Save
          closeLinkWindow();
          if(document.getElementById("saveambiguity")) {
            eval(document.getElementById("saveambiguity").getAttribute("onclick") || "console.log('no onclick for saveambiguity')")
          } else {
            sendModificationsToServer();
          }
          e.preventDefault();
        }
        if(e.which == 75 && (e.ctrlKey || e.metaKey)) { // CTRL+K: Insert link
          if(new Date().valueOf() - lastclick > 100) {
            document.execCommand('createLink', false, 'http://');
            e.preventDefault();
            var s = getSelection();
            s = s ? s.anchorNode : s;
            s = s ? s.parentNode : s;
            lastclick = new Date().valueOf();
            onClickOnLink({target: s, modify: true});
          }
          // Open link.
        }
      };
      document.onkeyup = document.onkeydown
      
      var bodyeditable = document.querySelector("body[contenteditable=true]");
      var onKeypress = e => {
        if(e.keyCode==13 && !e.shiftKey){ // [Enter] key
            // If we are inside a paragraph, we split the paragraph.
            // If we are directly inside a div, we add a paragraph separator.
            // We delete everything between anchorNode and focusNode
            // TODO: Handle ul and li
            var caretSelection = document.getSelection();
            var x = caretSelection.anchorNode;
            if(x && x.nodeType == 3) { // text node
              if(x.parentNode && getComputedStyle(x.parentNode).display == "block") {
                e.preventDefault(); //Prevent default browser
                var range = caretSelection.getRangeAt(0);
                range.deleteContents();
                caretSelection = document.getSelection();
                x = caretSelection.anchorNode;
                if(x.parentNode.tagName == "p") { // Split the p
                  var newPar = document.createElement("p");
                  
                  var fo = caretSelection.anchorOffset;
                  if(fo < x.text.length) {
                    newPar.append(document.createTextNode(x.text.substring(fo)));
                    x.deleteData(fo,x.text.length - fo);
                  }
                  var y = x.nextSibling;
                  while(y) {
                    var yy = y;
                    y = y.nextSibling;
                    newPar.append(yy); // Moves yy
                  }
                  x.parentNode.insertAdjacentElement("afterend", newPar);
                } else { // insert br
                  range.insertNode(document.createElement("br"))
                }
              }
            }
        }
      }
      if(bodyeditable && !bodyeditable.configured) {
        bodyeditable.configured = true;
        bodyeditable.addEventListener("keypress", onKeypress, true);
      }
    }
    
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
    
    var currentlySelectedElement = undefined;
    
    onClickOnLink = function (event) {
      var clickedElem = event.target;
      var ancestors = [];
      var tmp = clickedElem;
      var aElement;
      var ancestorIsModifyBox = false;
      var ancestorIsContextMenu = false;
      while(tmp) {
        ancestors.push(tmp);
        if(tmp.getAttribute && tmp.getAttribute("id") == "modify-menu") {
          ancestorIsModifyBox = true;
        }
        if(tmp.getAttribute && tmp.getAttribute("id") == "context-menu") {
          ancestorIsContextMenu = true;
        }
        if(!aElement && clickedElem.tagName === "A") { // First link.
          aElement = clickedElem;
        }
        tmp = tmp.parentElement;
      }
      if(ancestorIsModifyBox || ancestorIsContextMenu || ancestors[ancestors.length - 1].tagName != "HTML") return;
      //console.log("not modify box", ancestors)
      document.querySelector("#context-menu").classList.remove("visible");
      document.querySelectorAll("[ghost-clicked=true]").forEach((elem) => elem.removeAttribute("ghost-clicked"));
      currentlySelectedElement = undefined;
      if(clickedElem.setAttribute) {
        currentlySelectedElement = clickedElem;
      } else if(clickedElem.parentNode.setAttribute) {
        currentlySelectedElement = clickedElem.parentNode;
      }
      if(currentlySelectedElement) {
        currentlySelectedElement.setAttribute("ghost-clicked", "true");
      }
      if(!window.matchMedia("(pointer: coarse)").matches) {// Only for desktop we display the link editor.
        if(aElement && aElement.tagName == "A" && aElement.getAttribute("id") != "docslinkbubble-linkpreview") {
          var href = aElement.getAttribute("href");
          if(href) {
            var rect = aElement.getBoundingClientRect();
            var bottomX = window.scrollX + rect.left;
            var bottomY = window.scrollY + rect.top + aElement.offsetHeight;
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
              aElement.outerHTML = aElement.innerHTML;
              d.setAttribute("ghost-visible", "false");
            }
            
            var modifyButton = document.getElementById("docslinkbubble-modify");
            var clickStart = 0;
            var onclick = function() {
              if (observeTargetA != null) {
                observeTargetA.disconnect();
                observeTargetA = null;
              }
              if(targetA.getAttribute("contenteditable") == "true") {
                targetA.setAttribute("contenteditable", "false");
                console.log("disconnect observeTargetA", targetA);
              } else {
                targetA.setAttribute("contenteditable", "true");
                targetA.focus();
                console.log("reconnect observeTargetA", targetA);
                observeTargetA = new MutationObserver(function(mutations) {
                  console.log("targetA modified");
                  aElement.setAttribute("href", targetA.innerText);
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
            modifyButton.onclick = onclick;
            if(event.modify) {
              onclick();
            }
          }
        } else if(clickedElem && clickedElem.getAttribute &&
             (clickedElem.getAttribute("id") == "docslinkbubble" ||
              (clickedElem.parentNode && clickedElem.parentNode.getAttribute && clickedElem.parentNode.getAttribute("id") == "docslinkbubble") ||
              (clickedElem.parentNode && clickedElem.parentNode.parentNode &&
               clickedElem.parentNode.parentNode.getAttribute && clickedElem.parentNode.parentNode.getAttribute("id") == "docslinkbubble")
              )) {
          // Let's not close the edit link window if the user clicked something inside it.
        } else {
          // Anything else will quit the link window.
          closeLinkWindow();
        }
      } // End if on Desktop.
      updateInteractionDiv(clickedElem);
      // Check if the event.target matches some selector, and do things...
    }
    
    function updateInteractionDiv(clickedElem) {
      var contextMenu = document.querySelector("#context-menu");
      var interactionDiv = document.querySelector("#modify-menu > .information");
      if(clickedElem && interactionDiv && contextMenu) {
        /*function summary(element) {
          var summary = element.tagName.toLowerCase();
          if(element.getAttribute("id")) {
            summary += "#" + element.getAttribute("id");
          }
          if(element.getAttribute("class") && element.getAttribute("class").trim().length) {
            summary += "." + element.getAttribute("class").split(".");
          }
          return summary;
        }*/
        interactionDiv.innerHTML = "";
        interactionDiv.append(el("h3", {"class":"elementName"}, clickedElem.tagName.toLowerCase()));
        for(let i = 0; i < clickedElem.attributes.length; i++) {
          let name = clickedElem.attributes[i].name;
          if(name === "ghost-clicked") continue;
          let value = clickedElem.attributes[i].value;
          let nv = el("div", {"class": "keyvalue"});
          if(false /*name == "style"*/) {
            // Do something special for styles.
          } else {
            let isHref = name === "href" && clickedElem.tagName === "A";
            interactionDiv.append(
              el("div", {"class": "keyvalue"}, [
                el("span", {}, name + ": "),
                el("input", {"type": "text", value: value},
                  [], {
                    onkeyup: ((name, isHref) => function () {
                        clickedElem.setAttribute(name, this.value);
                        if(isHref) {
                          this.nextSibling.setAttribute("href", this.value);
                        }
                      })(name, isHref)
                  }
                ),
                isHref ? el("a", {href: value}, "Open"): undefined
                ]
              ));
          }
        }
        interactionDiv.append(el("hr"));
        interactionDiv.append(el("div", {}, "Add an attribute:"));
        let highlightsubmit = function() {
          let attrName = this.parentElement.querySelector("[name=name]").value;
          this.parentElement.querySelector("[type=submit]").disabled =
            attrName === "" || attrName.trim() !== attrName
        }
        interactionDiv.append(el("div", {"class": "keyvalue"},
          [el("form", {}, [
             el("input", {"type": "text", placeholder: "name", value: "", name:"name"}, [], {onkeyup: highlightsubmit}),
             el("input", {"type": "text", placeholder: "value", value: "", name:"value"}, [], {onkeyup: highlightsubmit}),
             el("input", {"type": "submit", value: "Add"}, [], {disabled: true})],
               {onsubmit: function() {
                 clickedElem.setAttribute(
                   this.querySelector("[name=name]").value,
                   this.querySelector("[name=value]").value);
                 updateInteractionDiv(clickedElem);
               }})]));
        if(clickedElem.tagName === "SCRIPT" || clickedElem.tagName === "STYLE") {
          interactionDiv.append(el("hr"));
          interactionDiv.append(el("textarea", {style: "width:100%; height:50%"},
                  clickedElem.innerText, {
                    onkeyup: function () { clickedElem.innerText = this.value; }
                  }));
        }
        
        // What to put in context menu?
        contextMenu.innerHTML = "";
        let numButtons = 0;
        let addButton = function(innerHTML, attributes, properties) {
          let button = document.createElement("div");
          for(let k in attributes) {
            button.setAttribute(k, attributes[k]);
          }
          button.classList.add("context-menu-button");
          for(let k in properties) {
            button[k] = properties[k];
          }
          button.innerHTML = innerHTML;
          contextMenu.append(button);
          numButtons++;
        }
        let reorderCompatible = (node1, node2) => {
          let topLevelOrderableTags = {TABLE:1, P:1, LI:1, UL:1, OL:1, H1:1, H2:1, H3:1, H4:1, H5:1, H6:1, DIV:1};
          return node1.tagName === node2.tagName && node1.tagName !== "TD" && node1.tagName !== "TH" ||
            topLevelOrderableTags[node1.tagName] && topLevelOrderableTags[node2.tagName]
            ; 
        }
        if(clickedElem.parentElement) {
          addButton(`<svg class="context-menu-icon" width="40" height="30">
              <path d="M 8,19 8,22 11,22 M 12,18 8,22 M 8,10 8,7 11,7 M 12,10 8,7 M 27,7 30,7 30,10 M 26,10 30,7 M 31,19 31,22 28,22 M 26,18 31,22 M 12,12 12,10 M 12,16 12,14 M 14,18 12,18 M 18,18 16,18 M 22,18 20,18 M 26,18 24,18 M 26,14 26,16 M 26,10 26,12 M 22,10 24,10 M 18,10 20,10 M 14,10 16,10 M 5,5 35,5 35,25 5,25 Z"/></svg>`,
                {title: "Select parent", "class": "inert"},
                {onclick: (c => event => {
                  let parent = c.parentElement;
                  if(parent.tagName === "TBODY" && parent.parentElement && parent.parentElement.tagName === "TABLE") parent = parent.parentElement;
                  c.removeAttribute("ghost-clicked");
                  parent.setAttribute("ghost-clicked", "true");
                  updateInteractionDiv(parent)})(clickedElem) }
              );
        }
        if(clickedElem.children.length > 0) {
          addButton(`<svg class="context-menu-icon" width="40" height="30">
              <path d="M 28,22 27,19 30,19 M 33,23 27,19 M 8,20 11,19 11,22 M 7,24 11,19 M 10,6 11,9 8,10 M 28,6 27,9 30,10 M 33,6 27,9 M 6,6 11,9 M 5,15 5,10 M 5,25 5,20 M 15,25 10,25 M 25,25 20,25 M 35,25 30,25 M 35,15 35,20 M 35,5 35,10 M 25,5 30,5 M 15,5 20,5 M 5,5 10,5 M 12,10 26,10 26,18 12,18 Z"/></svg>`,
                {title: "Select first child", "class": "inert"},
                {onclick: (c => event => {
                  let firstChild = c.children[0];
                  if(firstChild.tagName === "TBODY" && firstChild.children && firstChild.children.length > 0) firstChild = firstChild.children[0];
                  c.removeAttribute("ghost-clicked");
                  firstChild.setAttribute("ghost-clicked", "true");
                  updateInteractionDiv(firstChild)})(clickedElem) }
              );
        }
        if(clickedElem.previousElementSibling) {
          addButton(`<svg class="context-menu-icon fill" width="40" height="30">
            <path d="m 10,14 3,3 4,-4 0,14 6,0 0,-14 4,4 3,-3 L 20,4 Z"/></svg>`,
          {title: "Select previous sibling", class: "inert"},
          {onclick: ((c, contextMenu) => (event) => {
              let prev = c.previousElementSibling;
              c.removeAttribute("ghost-clicked");
              prev.setAttribute("ghost-clicked", "true");
              updateInteractionDiv(prev);
            })(clickedElem, contextMenu)
          });
        }
        if(clickedElem.nextElementSibling) {
          addButton(`<svg class="context-menu-icon fill" width="40" height="30">
            <path d="m 10,17 3,-3 4,4 0,-14 6,0 0,14 4,-4 3,3 -10,10 z"/></svg>`,
          {title: "Select next sibling", class: "inert"},
          {onclick: ((c, contextMenu) => (event) => {
              let next = c.nextElementSibling;
              c.removeAttribute("ghost-clicked");
              next.setAttribute("ghost-clicked", "true");
              updateInteractionDiv(next);
            })(clickedElem, contextMenu)
          });
        }
        addButton(`<svg class="context-menu-icon fill" width="40" height="30">
            <path d="M 17.88,2.979 14.84,3.938 15.28,7.588 13.52,9.063 10,8 8.529,10.83 11.42,13.1 11.22,15.38 7.979,17.12 8.938,20.16 12.59,19.72 14.06,21.48 13,25 15.83,26.47 18.1,23.58 20.38,23.78 22.12,27.02 25.16,26.06 24.72,22.41 26.48,20.94 30,22 31.47,19.17 28.58,16.9 28.78,14.62 32.02,12.88 31.06,9.84 27.41,10.28 25.94,8.52 27,5 24.17,3.529 21.9,6.42 19.62,6.219 17.88,2.979 Z M 20,11 A 4,4 0 0 1 24,15 4,4 0 0 1 20,19 4,4 0 0 1 16,15 4,4 0 0 1 20,11 Z" /></svg>`,
            {title: "Open/close settings tab", "class": "inert"},
            {onclick: ((c, contextMenu) => event => {
                document.querySelector("#modify-menu").classList.toggle("visible");
                // TODO: Open meaningful context menu.
              })(clickedElem, contextMenu)
            });
        if(clickedElem.previousElementSibling && reorderCompatible(clickedElem.previousElementSibling, clickedElem)) {
          addButton(`<svg class="context-menu-icon fill" width="40" height="30">
            <path d="m 10,14 3,3 4,-4 0,14 6,0 0,-14 4,4 3,-3 L 20,4 Z"/></svg>`,
          {title: "Move selected element up"},
          {onclick: ((c, contextMenu) => (event) => {
              let wsTxtNode = c.previousSibling && c.previousSibling.nodeType == 3 &&
                 c.previousSibling.textContent.trim() === "" ? c.previousSibling : undefined;
              // There is whitespace before this element, we try to reinsert
              c.parentElement.insertBefore(c, c.previousElementSibling);
              if(wsTxtNode) { // We move the whitespace as well.
                c.parentElement.insertBefore(wsTxtNode, c.previousElementSibling);
              }
              updateInteractionDiv(c);
            })(clickedElem, contextMenu)
          });
        }
        if(clickedElem.nextElementSibling && reorderCompatible(clickedElem, clickedElem.nextElementSibling)) {
          addButton(`<svg class="context-menu-icon fill" width="40" height="30">
            <path d="m 10,17 3,-3 4,4 0,-14 6,0 0,14 4,-4 3,3 -10,10 z"/></svg>`,
          {title: "Move selected element down"},
          {onclick: ((c, contextMenu) => (event) => {
              let wsTxtNode = c.nextSibling && c.nextSibling.nodeType == 3 &&
                c.nextSibling.textContent.trim() === "" ? c.nextSibling : undefined;
              let nodeToInsertAfter = c.nextElementSibling;
              nodeToInsertAfter.insertAdjacentElement("afterend", c);
              if(wsTxtNode) { // We move the whitespace as well
                nodeToInsertAfter.parentElement.insertBefore(wsTxtNode, nodeToInsertAfter.nextSibling);
              }
              updateInteractionDiv(c);
            })(clickedElem, contextMenu)
          });
        }
        addButton(`<svg class="context-menu-icon" width="40" height="30">
            <path d="m 11,4 12,0 0,4 -4,0 0,14 -8,0 z" />
            <path d="m 19,8 12,0 0,18 -12,0 z" /></svg>`,
          {title: "Clone selected element"},
          {onclick: ((c, contextMenu) => (event) => {
              c.removeAttribute("ghost-clicked");
              let cloned = duplicate(c);
              if(cloned) updateInteractionDiv(cloned);
              else contextMenu.classList.remove("visible");
            })(clickedElem, contextMenu)
          });
        addButton(`<svg class="context-menu-icon" width="40" height="30">
            <path d="m 9,7.5 22,0" />
            <path d="m 12,7.5 0,14 c 0,3 1,4 3,4 l 10,-0 c 2,0 3,-1 3,-3.5 l 0,-14" />
            <path d="m 17,7 c -0,-4.5 6,-4.5 6,0"/>
            <path d="M 16,11.5 16,22.5" />
            <path d="m 20,11.5 0,11" />
            <path d="m 24,11.5 0,11" /></svg>`,
          {title: "Delete selected element"},
          {onclick: ((c, contextMenu) => (event) => {
              c.remove();
              contextMenu.classList.remove("visible");
            })(clickedElem, contextMenu)
          });
        
        let baseElem = clickedElem;
        while(baseElem.tagName == "SCRIPT" || baseElem.tagName == "STYLE") {
          baseElem = baseElem.nextElementSibling;
        }
        baseElem = baseElem || clickedElem;
        // Find out where to place context menu.
        let clickedElemLeft = window.scrollX + baseElem.getBoundingClientRect().left;
        let clickedElemTop = window.scrollY + baseElem.getBoundingClientRect().top;
        let clickedElemBottom = window.scrollY + baseElem.getBoundingClientRect().bottom;
        let clickedElemRight = window.scrollX + baseElem.getBoundingClientRect().right;
        let buttonHeight = window.matchMedia("(pointer: coarse)").matches ? 48 : 30;
        let buttonWidth  = window.matchMedia("(pointer: coarse)").matches ? 48 : 40;
        let desiredWidth = numButtons * buttonWidth;
        let desiredLeft = (clickedElemLeft + clickedElemRight) / 2 - desiredWidth;
        if(desiredLeft < clickedElemLeft) desiredLeft = clickedElemLeft;
        let desiredTop = clickedElemTop - buttonHeight; 
        if(desiredTop - window.scrollY < 9) {
          desiredTop = clickedElemBottom;
          if(desiredTop + buttonHeight > window.innerHeight) {
            desiredTop = window.innerHeight - buttonHeight; 
          }
        }
        if(desiredLeft < 0) desiredLeft = 0;
        if(desiredTop < 0) desiredTop = 0;
        contextMenu.style.left = desiredLeft + "px";
        contextMenu.style.top = desiredTop + "px";
        contextMenu.style.width = desiredWidth + "px";
        contextMenu.classList.add("visible");
        return true;
      }
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
@(if iscloseable then """
window.onbeforeunload = function (e) {
    e = e || window.event;

    var askConfirmation = document.getElementById("manualsync-menuitem") &&
         document.getElementById("manualsync-menuitem").getAttribute("ghost-disabled") == "false";
    const confirmation = 'You have unsaved modifications. Do you still want to exit?';

    // For IE and Firefox prior to version 4
    if (e) {
      if(askConfirmation) {
        e.returnValue = confirmation;
      }
    }
    if(askConfirmation) {
      // For Safari
      return confirmation;
    } else {
      var xmlhttp = new XHRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search, false); // Async
      xmlhttp.setRequestHeader("close", "true");
      xmlhttp.send("{\"a\":1}");
    }
};
""" else "")
"""

googlesigninbutton = serverOwned "the google sign-in button" [
<style>
a.closeGoogleSignIn {
  margin-left: 2px;
  padding-left: 4px;
  padding-right: 5px;
}
a.closeGoogleSignIn:hover {
  background: #AAA;
  color: white;
  border-radius: 10px;
}
</style>,
<div class="g-signin2" data-onsuccess="onGoogleSignIn" list-ghost-attributes="data-gapiscan data-onload" children-are-ghosts="true"></div>,
<script>
function onGoogleSignIn(googleUser) {
  var profile = googleUser.getBasicProfile();
  
  var wasSignedIn = googleAuthIdToken ? true : false;
  // When set, will be used throughout 
  googleAuthIdToken = googleUser.getAuthResponse().id_token;
  var addSignout = (name) => {
    var signin = document.querySelector(".abcRioButtonContents").children[1];
    signin.setAttribute("title", "Signed in as " + name);
    var signout = document.createElement("a");
    signout.classList.add("closeGoogleSignIn");
    signout.setAttribute("title", "Sign out");
    signout.innerText = "x";
    signout.onclick = () => {
      var auth2 = gapi.auth2.getAuthInstance();
      auth2.signOut().then(() => {
        auth2.disconnect();
        googleAuthIdToken = undefined;
        console.log('User signed out.');
      });
    }
    signin.append(signout);
  }
  addSignout(profile.getName());
  if(!wasSignedIn) { // Necessary to ensure that we don't reload the page the second time it is loaded.
    reloadPage();
  }
}
</script>,
<script id="googlesigninscript" src="https://apis.google.com/js/platform.js" async defer save-ghost-attributes="gapi_processed"></script>
]

defaultMarkdowncss = """img {
  max-width: 100%;
}
pre {
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
}"""

main