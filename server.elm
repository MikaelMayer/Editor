-- input: path            The file to serve.
-- input: vars:           URL query vars.
-- input: fileOperations  The current set of delayed file disk operations.
--    Note that Elm pages are given in context the path, the vars, and the file system (fs) to read other files
-- output: The page, either raw or augmented with the toolbar and edit scripts.
preludeEnv = __CurrentEnv__

fs = nodejs.delayed fileOperations

editdelay = 1000

userpermissions = {pageowner= True, admin= (case vars of {admin} -> admin == "true"; _ -> False)}

permissionToCreate = userpermissions.admin

permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication

canEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == "true"; _ -> False)

freezeWhen notPermission lazyMessage = Update.lens {
  apply x = x
  update {outputNew, diffs} =
    if notPermission then
      Err (lazyMessage ())
    else
      Ok (InputsWithDiffs [(outputNew, Just diffs)])
}

serverOwned what = freezeWhen (not permissionToEditServer) (\_ -> """You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>

If you really intended to modify this, add ?admin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.""")

path = if fs.isdir path then
       if fs.isfile <| path + "index.html" then path + "index.html"
  else if fs.isfile <| path + "/index.html" then path + "/index.html"
  else if fs.isfile <| path + "index.elm" then path + "index.elm"
  else if fs.isfile <| path + "/index.elm" then path + "/index.elm"
  else if fs.isfile <| path + "README.md" then path + "README.md"
  else if fs.isfile <| path + "/README.md" then path + "/README.md"
  else path
  else path

sourcecontent = String.newlines.toUnix <|
  if path == "server.elm" then
    """<html><head></head><body>Sample server Elm</body></html>"""
  else
    if fs.isdir path then
      """<html><head></head><body><h1><a href=''>/@path</a></h1>
      <ul>@@(case Regex.extract "^(.*)/.*$" path of
        Just [prev] -> [<li><a href=("/" + prev)>..</li>]
        _ -> [<li><a href="/">..</li>])@@(List.map (\name -> <li><a href=("/" + path + "/" + name)>@@name</li>) (fs.listdir path))</ul>
      Hint: place an <a href=(path + "/index.html")>index.html</a> or <a href=(path + "/index.elm")>index.elm</a> file to display something else than this page.</body></html>"""
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

canEvaluate = vars |> case of {evaluate} -> evaluate; _ -> "true" 
  
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
              (if canEditPage then (serverOwned "edition menu" editionmenu ++ Update.sizeFreeze [(serverOwned "code preview box" codepreview) sourcecontent]) else freeze []) ++ serverOwned "initial script" initialScript ++ bodychildren ++ Update.sizeFreeze (serverOwned "synchronization script" [<script>@editionscript</script>])]
          x -> x
        )]
      x-> <html><head></head><body>Not a valid html page: @("""@x""")</body></html>
  --|> Update.debug "main"

initialCheckedAttribute = [["checked", ""]]
  
editionmenu = [
<menu id="themenu" ignore-modifications="true" class="edittoolbar" contenteditable="false">
<style>
#menumargin {
  padding-top: 2em;
}
menu {
  position: fixed;
  margin-top: 0px;
  z-index: 10000;
}
menu.edittoolbar {
  display: block;
  background: black;
  color: white;
  padding: 2px;
}
menuitem.disabled {
  color: #BBB;
}
menuitem > .solution {
  
}
menuitem > .solution.selected {
  outline: white 2px solid;
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
  outline: #CCC 2px solid;
  cursor: pointer;
}
menuitem > .solution.notfinal {
  color: #CCC;
}
#editor_codepreview, #manualsync-menuitem {
  display: none;
  z-index: 9999;
}
[ghost-visible=true] {
  display: block;
}
#editor_codepreview[ghost-visible=true] {
  display: block;
}
#manualsync-menuitem[ghost-visible=true], #manualsync-menuitem[force-visible=true] {
  display: inline-block;
}
[ghost-visible=false] {
  display: none;
}
</style>
<menuitem>@path</menuitem>
<menuitem>
<label title="Display the source code of this pagge below"><input id="input-showsource" type="checkbox" save-attributes="checked"
  onchange="""
var cp = document.getElementById("editor_codepreview");
if(cp !== null) {
   cp.setAttribute("ghost-visible", this.checked ? "true": "false")
}""">Show source</label>
</menuitem>
<menuitem>
<label title="If on, changes are automatically propagated after 1s after the last edit"><input id="input-autosync" type="checkbox" save-attributes="checked" onchange="document.getElementById('manualsync-menuitem').setAttribute('ghost-visible', this.checked ? 'false' : 'true')" @(case vars of {autosync=autosyncattr} ->
                       Update.bijection (case of "true" -> [["checked", ""]]; _ -> []) (case of [["checked", ""]] -> "true"; _ -> "false") autosyncattr; _ -> serverOwned "initial checked attribute (use &autosync=true/false in query parameters to modify it)" [["checked", ""]])>Auto-sync</label>
</menuitem>
<menuitem id="manualsync-menuitem" @(case vars of {autosync="false"} -> [["force-visible", "true"]]; _ -> [])>
<button onclick="sendModificationsToServer()">Send changes to server</button>
</menuitem>
</menu>,
<div id="menumargin"></div>]

initialScript = [
<script>
function isGhostNode(elem) {
  return elem.nodeType == 1 &&
    (elem.tagName == "GHOST" || elem.getAttribute("isghost") == "true");
}
function hasGhostAncestor(htmlElem) {
  if(htmlElem == null) return false;
  if(isGhostNode(htmlElem)) return true;
  return hasGhostAncestor(htmlElem.parentNode);
}
function isGhostAttributeKey(name) {
  return name.startsWith("ghost-");
}

setGhostOnInserted = [];

setGhostOnInserted.push(insertedNode =>
  insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
     insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1
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
 ( document.body
 , { attributes: false
   , childList: true
   , characterData: false
   , attributeOldValue: false
   , characterDataOldValue: false
   , subtree: true
   }
 )
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
    return [idGhostAttributes, idDynamicAttributes];
  }
  function applyGhostAttributes(attrs) {
    var [idGhostAttributes, idDynamicAttributes] = attrs;
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
      if(elem !== null && typeof elem !== "undefined") {
        elem[key] = value;
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
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          var value = n.attributes[i].value;
          if(!isGhostAttributeKey(key)) {
            if(key == "style") {
              value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2);
            }
            attributes.push([key, value]);
          }
        }
        var children = [];
        for(i = 0; i < n.childNodes.length; i++) {
          if(!isGhostNode(n.childNodes[i])) {
            children.push(domNodeToNativeValue(n.childNodes[i]));
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
            newMenu.setAttribute("isghost", "true")
            document.getElementById("themenu").append(newMenu);
          }
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
    }
    
    function selectAmbiguity(key, num) {
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("ambiguity-key", key);
      xmlhttp.setRequestHeader("select-ambiguity", JSON.stringify(num));
      xmlhttp.setRequestHeader("Content-Type", "application/json");
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
        document.getElementById("notification-menu").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`
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
        if(document.getElementById("input-autosync") && !document.getElementById("input-autosync").checked) {
          document.getElementById("manualsync-menuitem").setAttribute("ghost-visible", "true") // Because it will be saved
        }
      });
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("Content-Type", "application/json");
      xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));
    }
    
    function handleMutations(mutations) {
      console.log("mutations", mutations);
      if(document.getElementById("input-autosync") && !document.getElementById("input-autosync").checked) return;
      var onlyGhosts = true;
      for(var i = 0; i < mutations.length && onlyGhosts; i++) {
        // A mutation is a ghost if either
        // -- The attribute starts with 'ghost-'
        // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
        // -- It is the modification of a node or an attribute inside a ghost node.
        var mutation = mutations[i];
        if(hasGhostAncestor(mutation.target)) continue;
        if(mutation.type == "attributes") {
          if(isGhostAttributeKey(mutation.attributeName)) {
          } else {
            onlyGhosts = false;
          }
        } else if(mutation.type == "childList") {
          for(var j = 0; j < mutation.addedNodes.length && onlyGhosts; j++) {
            if(!hasGhostAncestor(mutation.addedNodes[j])) {
              onlyGhosts = false;
            }
          }
          if(mutation.removedNodes.length > 0) {
            onlyGhosts = false;
          }
        } else {
          onlyGhosts = false;
        }
      }
      if(onlyGhosts) {
        console.log("mutations are only ghosts, skipping");
        return;
      } // Send in post the new HTML along with the URL
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
     }, 10)"""

main