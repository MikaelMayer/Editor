-- input: pagename  The file to serve.
-- input: vars:     URL query vars.
-- output: the page, either raw or augmented with the toolbar and edit scripts.

preludeEnv = __CurrentEnv__

editdelay = 1000

userpermissions = {pageowner= True, admin= True}

permissionToCreate = userpermissions.admin

permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication

canEditPage = userpermissions.pageowner && (vars |> case of {edit} -> edit == "true"; _ -> False)

serverOwned = Update.conditionalFreeze (not permissionToEditServer)

sourcecontent = if pagename == "server.elm" then
    """<html><head></head><body>Sample server Elm</body></html>"""
  else
    nodejs.fileread pagename
  |> Maybe.withDefaultReplace (
    serverOwned """<html><body>@(
        if permissionToCreate then <span>@pagename does not exist yet. Modify this page to create it!</span> else <span>Error 404, @pagename does not exist</span>
      )</body></html>"""
  )

canEvaluate = vars |> case of {evaluate} -> evaluate; _ -> "true" 
  
main = (if canEvaluate == "true" then
    __evaluate__ (("vars", vars)::("pagename", pagename)::preludeEnv) sourcecontent 
    else
    Ok <html><head></head><body>URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>
  ) |> (case of
  Err msg -> serverOwned <|
    <html><body style="color:#cc0000"><div style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">@msg</pre></div></body></html>
  Ok page -> page)
  |> case of
      ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
        List.mapWithReverse identity (case of
          ["body", bodyattrs, bodychildren] ->
            ["body",
              (if canEditPage then serverOwned [["contenteditable", "true"]] else freeze []) ++
                bodyattrs,
              (if canEditPage then serverOwned [editionmenu, codepreview sourcecontent] else freeze []) ++ bodychildren ++ Update.sizeFreeze (serverOwned [<script>@editionscript</script>])]
          x -> x
        )]

editionmenu = <menu ignore-modifications="true" class="edittoolbar" contenteditable="false">
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
</style>
<menuitem>@pagename</menuitem>
<menuitem class="disabled"><button>Display source code</button></menuitem>
<menuitem class="disabled">Update feedback</menuitem>
</menu>

codepreview sourcecontent = 
<div class="codepreview">
  <textarea style="width:50%;height:200px" v=sourcecontent onchange="this.setAttribute('v', this.value)">@sourcecontent</textarea>
</div>
    
editionscript = """function initSigninV2() {
    console.log("platform.js loaded");
  }
  
  function domNodeToNativeValue(n) {
      if(n.nodeType == "3") {
        return ["TEXT", n.textContent];
      } else {
        var attributes = [];
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          var value = n.attributes[i].value;
          if(key == "style") {
            value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2);
          }
          attributes.push([key, value]);
        }
        var children = [];
        for(i = 0; i < n.childNodes.length; i++) {
          children.push(domNodeToNativeValue(n.childNodes[i]));
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
  
    function handleMutations(mutations) {
      // Send in post the new HTML along with the URL
      console.log("mutations", mutations);
      if(typeof t !== "undefined") {
        clearTimeout(t);
      }
      t = setTimeout(function() {
        t = undefined;
        console.log("sending post request");
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.onreadystatechange = function () {
            if (xmlhttp.readyState == XMLHttpRequest.DONE) {
              //console.log("going to replace with");
              //console.log(xmlhttp.responseText);
              replaceContent(xmlhttp.responseText);
              var newQueryStr = xmlhttp.getResponseHeader("New-Query");
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
        };
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