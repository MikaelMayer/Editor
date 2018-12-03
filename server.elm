-- input: path  The file to serve.
-- input: vars:     URL query vars.
-- output: the page, either raw or augmented with the toolbar and edit scripts.
preludeEnv = __CurrentEnv__

editdelay = 1000

userpermissions = {pageowner= True, admin= True}

permissionToCreate = userpermissions.admin

permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication

canEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == "true"; _ -> False)

serverOwned = Update.conditionalFreeze (not permissionToEditServer)

path = if nodejs.isdir path then
       if nodejs.isfile <| path + "index.html" then path + "index.html"
  else if nodejs.isfile <| path + "/index.html" then path + "/index.html"
  else if nodejs.isfile <| path + "index.elm" then path + "index.elm"
  else if nodejs.isfile <| path + "/index.elm" then path + "/index.elm"
  else if nodejs.isfile <| path + "README.md" then path + "README.md"
  else if nodejs.isfile <| path + "/README.md" then path + "/README.md"
  else path
  else path

sourcecontent = if path == "server.elm" then
    """<html><head></head><body>Sample server Elm</body></html>"""
  else
    if nodejs.isdir path then
      """<html><head></head><body><h1><a href=''>/@path</a></h1>
      <ul>@@(List.map (\name -> <li><a href=(path + "/" + name)>@@name</li>) (nodejs.listdir path))</ul>
      Hint: place an <a href=(path + "/index.html")>index.html</a> or <a href=(path + "/index.elm")>index.elm</a> file to display something else than this page.</body></html>"""
    else
      if nodejs.isfile path && Regex.matchIn """\.(png|jpg|ico|gif|jpeg)$""" path then
        """<html><head><title>@path</title></head><body><img src="@path"></body></html>"""
      else
        nodejs.fileread path
      |> Maybe.withDefaultReplace (
        serverOwned """<html><head></head><body>@(
            if permissionToCreate then """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist</span>"""
          )</body></html>"""
      )

canEvaluate = vars |> case of {evaluate} -> evaluate; _ -> "true" 
  
main = (if canEvaluate == "true" then
      if Regex.matchIn """\.html$""" path then
        case Regex.extract """^(?:(?!<html)[\s\S])*((?=<html)[\s\S]*</html>)\s*$""" sourcecontent of
          Just [interpretableHtml] ->
            __evaluate__ preludeEnv """<raw>@interpretableHtml</raw>"""
            |> Result.andThen (case of
              ["raw", _, [htmlNode]] -> Ok htmlNode
              result -> Err """Html interpretation error: The interpretation of raw html did not work but produced @result"""
            )
          x ->  Err """@path is not a valid html file. Interpreation got: @x"""
      else if Regex.matchIn """\.md""" path then
        let markdownized = String.markdown sourcecontent in
          case Html.parseViaEval markdownized of
            x -> Ok <html><head></head><body>@x</body></html>
      else if Regex.matchIn """\.elm$""" path || nodejs.isdir path then
        __evaluate__ (("vars", vars)::("path", path)::preludeEnv) sourcecontent
      else
        Err """Serving only .html, .md and .elm files. Got @path"""
    else
      Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>
  ) |> (case of
  Err msg -> serverOwned <|
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
              (if canEditPage then serverOwned [["contenteditable", "true"]] else freeze []) ++
                bodyattrs,
              (if canEditPage then serverOwned [editionmenu, codepreview sourcecontent] else freeze []) ++ bodychildren ++ Update.sizeFreeze (serverOwned [<script>@editionscript</script>])]
          x -> x
        )]
      x-> <html><head></head><body>Not a valid html page: @("""@x""")</body></html>
  --|> Update.debug "main"

editionmenu = <menu id="themenu" ignore-modifications="true" class="edittoolbar" contenteditable="false">
<style>
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
menuitem > .solution:not(.selected):hover {
  outline: #CCC 2px solid;
  cursor: pointer;
}
#editor_codepreview {
  display: none;
}
#editor_codepreview[ghost-visible=true] {
  display: block;
}
</style>
<menuitem>@path</menuitem>
<menuitem class="disabled"><button onclick="""
var cp = document.getElementById("editor_codepreview");
if(cp !== null) {
   cp.setAttribute("ghost-visible", cp.getAttribute("ghost-visible") == "true" ? "false": "true")
}""">Display/hide source</button></menuitem>
</menu>

codepreview sourcecontent = 
<div class="codepreview" id="editor_codepreview">
  <textarea id="editor_codepreview_textarea" save-attributes="scrollTop"
    style="width:100%;height:200px" v=sourcecontent onchange="this.setAttribute('v', this.value)">@(Update.softFreeze (if Regex.matchIn "^\r?\n" sourcecontent then "\n" + sourcecontent else sourcecontent))</textarea>
</div>
    
editionscript = """function initSigninV2() {
    console.log("platform.js loaded");
  }

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
        for(i in toSave) {
          var key = toSave[i];
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
    
    handleServerPOSTResponse = xmlhttp => function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          //console.log("Received new content. Replacing the page.");
          var saved = saveGhostAttributes();
          replaceContent(xmlhttp.responseText);
          applyGhostAttributes(saved);
          
          var newQueryStr = xmlhttp.getResponseHeader("New-Query");
          var ambiguityKey = xmlhttp.getResponseHeader("Ambiguity-Key");
          var ambiguityNumber = xmlhttp.getResponseHeader("Ambiguity-Number");
          var ambiguitySelected = xmlhttp.getResponseHeader("Ambiguity-Selected");
          if(ambiguityKey !== null && typeof ambiguityKey != "undefined" &&
             ambiguityNumber !== null && typeof ambiguityNumber != "undefined" &&
             ambiguitySelected !== null && typeof ambiguitySelected != "undefined") {
            var n = JSON.parse(ambiguityNumber);
            var selected = JSON.parse(ambiguitySelected);
            var newMenu = document.createElement("menuitem");
            var disambiguationMenu = `<span style="color:red" id="ambiguity-id" v="${ambiguityKey}">Ambiguity.</span> Solutions `;
            for(var i = 1; i <= n; i++) {
              if(i == selected) {
                disambiguationMenu += ` <span class="solution selected">#${i}</span>`
              } else {
                disambiguationMenu += ` <a class="solution" onclick="selectAmbiguity('${ambiguityKey}', ${i})">#${i}</a>`
              }
            }
            disambiguationMenu += ` <button onclick='acceptAmbiguity("${ambiguityKey}", ${selected})'>Accept current</button>`;
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
    
    function handleMutations(mutations) {
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
        } else {
          onlyGhosts = false;
        }
      }
      if(onlyGhosts) {
        console.log("mutations are only ghosts, skipping", mutations);
        return;
      } // Send in post the new HTML along with the URL
      console.log("mutations", mutations);
      if(typeof t !== "undefined") {
        clearTimeout(t);
      }
      t = setTimeout(function() {
        t = undefined;
        console.log("sending post request");
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
        console.log("sending modifications");
        xmlhttp.open("POST", location.pathname + location.search);
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        xmlhttp.send(JSON.stringify(domNodeToNativeValue(document.body.parentElement)));
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