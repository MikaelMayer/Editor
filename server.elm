-- input: path            The file to serve.
-- input: vars:           URL query vars.
-- input: urlParams:      The URL params plainly
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

{--
updatecheckpoint name x = {
  apply x = x
  update {input, outputNew, diffs} =
    let _ = Debug.log """Checkpoint @name""" () in  
    Ok (InputsWithDiffs [(outputNew, Just diffs)])
}.apply x
--}

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

hydefilecache = listDict.get "hydefilecache" defaultOptions

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

browserSide = listDict.get "browserSide" defaultOptions == Just True

varadmin = boolVar "admin" False
varedit = boolVar "edit" False
varls = boolVar "ls" False
varraw = boolVar "raw" False
defaultVarEdit = listDict.get "edit" defaultOptions |> Maybe.withDefault False
varproduction = listDict.get "production" defaultOptions |> Maybe.withDefault (freeze False)
iscloseable = listDict.get "closeable" defaultOptions |> Maybe.withDefault (freeze False)

userpermissions = {pageowner= True, admin= varadmin}
permissionToCreate = userpermissions.admin
permissionToEditServer = boolVar "superadmin" False -- should be possibly get from user authentication
-- List.contains ("sub", "102014571179481340426") user -- That's my Google user ID.

canEditPage = userpermissions.pageowner && varedit && not varls

{freezeWhen} = Update

serverOwned what obj = freezeWhen (not permissionToEditServer) (\od -> """You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>

If you really intended to modify this, add ?superadmin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.<br><br>

For debugging purposes, below is the new value that was pushed:
<pre>@(Regex.replace "<" (always "&lt;") """@od""")</pre>
Here is the old value that was computed
<pre>@(Regex.replace "<" (always "&lt;") """@obj""")</pre>
""") obj

canEvaluate = listDict.get "evaluate" vars |> Maybe.withDefaultReplace (serverOwned "default value of evaluate" "true")

{--------------------------------------------------------
 Rewrite path to either a folder or a default file under
---------------------------------------------------------}

path: String
path =
  if fs.isdir path then
   if not varls then
     List.mapFirstSuccess (\test ->
       if fs.isfile <| path + test then Just (path + test) else Nothing)
       ["index.elm" , "/index.elm", "index.html", "/index.html", "README.md" , "/README.md" ]
     |> Maybe.withDefault path
   else path
  else path

{---------------------------------------------------------------------------
 Retrieves the string content of the path. For folders, creates a custom page
----------------------------------------------------------------------------}

applyDotEditor source = 
  let prefix = Regex.extract "^(.*/)[^/]*$" path |> Maybe.map (\[prefix] -> prefix) |> Maybe.withDefault "" in
  let dotEditor = prefix +  ".editor" in
  case fs.read dotEditor of
    Nothing -> source
    Just modifier ->
      case __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::("content", source)::preludeEnv) modifier of
        Err msg -> let _ = Debug.log ("Error while executing " + dotEditor + " : " + msg) () in
          source
        Ok newSource -> newSource

isTextFile path =
  Regex.matchIn """\.(?:txt|css|js|sass|scss)$""" path

(sourcecontent, folderView): (String, Boolean)
(sourcecontent, folderView) = --updatecheckpoint "sourcecontent" <|
  Tuple.mapFirst String.newlines.toUnix <| --updatecheckpoint "newlines restored" <|
  if path == "server.elm" then
    ("""<html><head></head><body>The Elm server cannot display itself. This is a placeholder</body></html>""", False)
  else
    if fs.isdir path then
      ("", True)
    else
      flip (,) False <|
      if fs.isfile path && Regex.matchIn """\.(png|jpg|ico|gif|jpeg)$""" path then -- Normally not called because server.js takes care of these cases.
        """<html><head><title>@path</title></head><body><img src="@path"></body></html>"""
      else
        (if hydefilecache == Nothing then fs.read path else
          case hydefilecache of
            Just {file=hydefile} ->
              let withoutPipeline = 
                    case hydefilecache of
                      Just {cacheContent} ->
                        case evaluate cacheContent of
                          {inputFiles, outputFiles} ->
                            List.find (\e -> e == path || e == "/" + path) inputFiles == Nothing &&
                            List.find (\e -> e == path || e == "/" + path) outputFiles == Nothing
                          _ -> False
                      _ -> False -- Need to recompute the cache anyway
              in
              if withoutPipeline then fs.read path else
              let source = fs.read hydefile |>
                      Maybe.withDefaultLazy (\_ -> """all = [Error "hydefile '@hydefile' not found?!"]""")
                  source = source + Update.freeze "\n\nlet t = " + (listDict.get "task" vars |> Maybe.withDefault "all") + "\n    t = if typeof t == 'function' then t () else t\n    t = if typeof t == 'list' then t else [t]\nin t"
                  fileDirectory = Regex.replace "/[^/]*$" "" hydefile
                  inDirectory name = if fileDirectory == "" then name else
                    fileDirectory  + "/" + name
                  fsReadRecord = 
                      { directReadFileSystem |
                        read name =
                          let name = inDirectory name in
                          let _ = recordFileRead name in
                          fs.read name,
                        listdir name =
                          let name = inDirectory name in
                          let _ = recordFolderList name in
                          fs.listdir name
                      }
                  fsHyde = nodejs.delayedFS fsReadRecord <|
                    Update.lens {
                      apply = identity
                      update {outputNew, diffs} = -- input and outputOld were empty, diffs is valid
                      -- We just need to change the paths
                        outputNew |>
                        List.map (case of
                          (name, Rename newName) -> (inDirectory name, Rename (inDirectory newName))
                          (name, x) -> (inDirectory name, x))|>
                        flip (,) (Just diffs) |>
                        List.singleton |> InputsWithDiffs |> Ok
                    } fileOperations
              in
              let (generatedFilesDict, errors) =
                    __evaluate__ (("fs", fsHyde)::preludeEnv) source
                    |> Result.map (\writtenContent ->
                        let (written, errors) = List.partition (case of Write -> True; _ -> False) writtenContent in
                        let tuplesToWrite =
                              List.map (case of Write name content -> (inDirectory name, content)) written
                            joinedErrors = 
                              List.map (case of Error msg -> msg) errors |> String.join "\n"
                        in
                        let _ = recordOutputFiles tuplesToWrite in -- Writes on disk and caches the names of written files
                        (tuplesToWrite, joinedErrors))
                    |> Result.withDefaultMapError (\msg -> ([], msg))
              in
              let _ = cacheResult () in
              --let _ = Debug.log "generatedFilesDict" generatedFilesDict in
              case listDict.get ("/" + path) generatedFilesDict of
                Nothing ->
                  case listDict.get path generatedFilesDict of
                    Nothing ->
                      if errors == "" then
                        let _ = Debug.log """Unable to read (/)@path from output of hydefile""" () in
                        fs.read path
                      else
                        Just <|
                        serverOwned "error recovery of hyde build tool" <|
                        """<html><head></head><body><h1>Error while resolving the generated version of @path</h1><pre>@errors</pre></body></html>"""
                    x -> x
                x -> x
            _ -> fs.read path
        )
      |> Maybe.withDefaultReplace (
        serverOwned "404 page" (if isTextFile path then
               if permissionToCreate then freeze """@path does not exist yet. Modify this page to create it!""" else """Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)"""
            else """<html><head></head><body>@(
              if permissionToCreate then freeze """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>"""
                )</body></html>""")
      )
{---------------------------------------------------------------------------
Utility functions to be inherited by the main body of any
view of editor (edit / file listing / word processor / etc)
LUCA stands for "Last Universal Common Ancestor"
----------------------------------------------------------------------------}

luca = 
  [<script id="thaditor-luca" class="editor-interface">
    function writeDocument(NC) {
      document.open();
      document.write(NC);
      document.close();
    }
    var XHRequest = @(if browserSide then "ProxiedServerRequest" else "XMLHttpRequest");
    var apache_server = @(if browserSide then "true" else "false");
    function doReadServer(action, name) {
      if (typeof readServer != "undefined") {
        console.log("reading server");
        return readServer(action, name);
      } else {
        var request = new XMLHttpRequest();
        var url = "/";
        request.open('GET', url, false);  // `false` makes the request synchronous
        request.setRequestHeader("action", action);
        request.setRequestHeader("name", name);
        request.send(null);
        if(request.status == 200) {
          return request.responseText || "";
        } else {
          console.log("error while reading " + url, request);
          return "";
        }
      }
    }
    function doWriteServer(action, name, content) {
      if (typeof writeServer != "undefined") {
        console.log("about to write to server");
        return writeServer(action, name, content);
      } else {
        var request = new XMLHttpRequest();
        request.open('POST', url, false);  // `false` makes the request synchronous
        request.setRequestHeader("action", action);
        request.setRequestHeader("name", name);
        request.send(content);
        if(request.status == 200) {
          return "";
        } else if(request.status == 500) {
          return request.responseText;
        } else {
          console.log("error while writing " + url, request);
          return "";
        }
      }
    }
    // Page reloading without trying to recover the editor's state.
    function doReloadPage(url, replaceState) {
      var xmlhttp = new XHRequest();
      xmlhttp.onreadystatechange = ((xmlhttp, replaceState) => () => {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          //source of the editing menu disappearing after reloading
          writeDocument(xmlhttp.responseText);
          var newLocalURL = xmlhttp.getResponseHeader("New-Local-URL");
          if(newLocalURL) {
            window.history[xmlhttp.replaceState ? "replaceState" : "pushState"]({localURL: newLocalURL}, "Nav. to " + newLocalURL, newLocalURL);
          }
        }
      })(xmlhttp, replaceState);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("reload", "true");
      xmlhttp.setRequestHeader("url", url);
      if(googleAuthIdToken) {
        xmlhttp.setRequestHeader("id-token", googleAuthIdToken)
      }
      console.log("setting url to ", url);
      xmlhttp.send("{\"a\":1}");
    }
    window.onpopstate = function(e){
        console.log("onpopstate", e);
        if(e.state && e.state.localURL) {
          doReloadPage(location, true);
        } else {
          doReloadPage(location.pathname + location.search, true);
        }
    };
    //document.body.appendChild(el("progress", {id:"progress-bar", max:100, value:0, visible:false}, [], {}));

    function sendNotification(msg, timeout) {
      /*
        Pushes the notification msg to the log & displays it for 3 seconds directly left of the moidfymenu.
        css for notification box is textarea .notif
      */

      let modifyMenuDiv = document.querySelector("#modify-menu");
      if (!modifyMenuDiv) {
        console.log("Notifications havent been set up for use outside of editor, like in the filesystem");
        console.log (msg);
        return;
      }
      let notifBox = document.getElementById("notif-box");
      if (!notifBox) { //function el(tag, attributes, children, properties) 
        notifBox = el("textarea", {id:"notif-box", class:"textarea notifs", visibility:true, readonly:true, isghost:true}, [], {value:msg});
        modifyMenuDiv.append(notifBox);
      }
      notifBox.value = msg;
      notifBox.style.display = "block";
      notifBox.classList.toggle("visible", true);
      notifBox.style.zIndex = 100;
      notifBox.style.visibility = true;
      editor_model.editor_log.push(msg);
      const issaving = editor_model.isSaving;
      let log = document.getElementById("fullLog");
      if (log) {
        let elog = editor_model.editor_log;
        let logtxt = "";
        for (let i = 0; i < editor_model.editor_log.length; i++) {
          logtxt = logtxt + editor_model.editor_log[i] + "\n";
        }
        logtxt == "" ? log.value = "(no log)" : log.value = logtxt;
        log.style.display = 'block';
      }
      setTimeout(hideNotification, timeout ? timeout : 3000);
    }

    function hideNotification() {
      let notifBox = document.getElementById("notif-box");
      if (notifBox) {
        notifBox.classList.toggle("visible", false);
      }
    }

    var uploadProgress = [];

    function initializeProgress(numFiles) {
      var progressBar = document.getElementById("progress-bar");
      if (!progressBar) {
        console.err ("Warning! Add the progress bar yourself before calling this. We'll add it for you this time.");
        document.body.appendChild(el("progress", {id:"progress-bar", max:100, value:0, visible:false}, [], {}));
      }
      progressBar.value = 0;
      progressBar.visible = true;
      uploadProgress = [];

      for(let i = numFiles; i > 0; i--) {
        uploadProgress.push(0);
      }
    }

    function updateProgress(fileNumber, percent) {
      uploadProgress[fileNumber] = percent;
      let total = uploadProgress.reduce((tot, curr) => tot + curr, 0) / uploadProgress.length;
      console.log ("prog updated");
      documentgetElementById("progress-bar").value = total;
    }

    // Editor's API should be stored in the variable editor.

    editor = typeof editor === "object" ? editor : {};
    editor.uploadFile = function(targetPathName, file, onOk, onError, updateProgFunction) {
      var xhr = new XMLHttpRequest();
      xhr.onprogress = (e) => {
        updateProgFunction(i, (e.loaded * 100.0 / e.total) || 100)
      }
      xhr.onreadystatechange = ((xhr, file) => () => {
        if (xhr.readyState == XMLHttpRequest.DONE) {
          if (xhr.status == 200 || xhr.status == 201) {
            onOk ? onOk(targetPathName, file) : 0;
          } else {
            console.log("Error while uploading picture or file", xhr);
            onError ? onError(targetPathName, file) : 0;
          }
        }
        var progbar = document.getElementById("progress-bar");
      })(xhr, file);
      @(if listDict.get "browserSide" defaultOptions == Just True then """
      xhr.open("POST", "/Thaditor/editor.php?action=write&name=" + encodeURIComponent(targetPathName), false);
      """ else """
      xhr.open("POST", targetPathName, false);
      xhr.setRequestHeader("write-file", file.type);
      """);
      xhr.send(file);
    }
    // Returns the storage folder that will prefix a file name on upload (final and initial slash excluded)
    editor.getStorageFolder = function(file) {
      var storageOptions = document.querySelectorAll("meta[editor-storagefolder]");
      for(let s of storageOptions) {
        if(file.type.startsWith(s.getAttribute("file-type") || "")) {
          let sf = storageOptions.getAttribute("editor-storagefolder") || "";
          if(!sf.endsWith("/")) sf = sf + "/";
          return sf;
        }
      }
      let extension = "";
      if(file && file.type.startsWith("image")) {
        var otherImages = document.querySelectorAll("img[src]");
        for(let i of otherImages) {
           extension = (i.getAttribute("src") || "").replace(/[^\/]*$/, "");
           break;
        }
        if(extension[0] == "/") { // Absolute URL
          return extension;
        }
      }
      if(extension != "" && extension[extension.length-1] != "/") {
        extension = extension + "/";
      }
      // extension ends with a / or is empty
      var tmp = location.pathname.split("/");
      tmp = tmp.slice(0, tmp.length - 1);
      storageFolder = tmp.join("/") + (extension != "" ?  "/" + extension : "/");
      return storageFolder;
    }
	  editor.fs = { listdir: 
		    function(dirname) {
          return JSON.parse(doReadServer("listdir", dirname) || "[]");
		    }
	  };
    editor.toTreasureMap = function(oldNode) {
      if(!oldNode) return undefined;
      if(oldNode.nodeType == 1 && oldNode.getAttribute("id") && document.getElementById(oldNode.getAttribute("id"))) {
        return {id: oldNode.getAttribute("id")};
      }
      let tentativeSelector = [];
      let t = oldNode;
      let isText = false, textIndex = 0;
      while(t && t.parentNode) {
        let index = Array.prototype.slice.call( t.parentNode.children ).indexOf(t);
        if(t.nodeType === 1) {
          tentativeSelector.unshift(t.tagName + ":nth-child(" + (index + 1) + ")" );
        } else {
          isText = true;
          textIndex = Array.prototype.slice.call( t.parentNode.childNodes ).indexOf(t);
        }
        t = t.parentNode;
      }
      return {tentativeSelector: tentativeSelector, isText: isText, textIndex: textIndex};
    }
    // Returns the new node that matches the old node the closest.
    // For text nodes, try to recover the text node, if not, returns the parent node;
    editor.fromTreasureMap = function(data) {
      if(!data) return undefined;
      if(typeof data === "object" && data.id) {
        return document.getElementById(data.id);
      }
      if(typeof data == "object" && Array.isArray(data.tentativeSelector)) {
        let tentativeSelector = data.tentativeSelector;
        while(tentativeSelector.length >= 1) {
          let newNode = document.querySelector(tentativeSelector.join(" "));
          if(newNode) {
            return data.isText && newNode.childNodes && newNode.childNodes[data.textIndex] || newNode;
          }
          tentativeSelector.shift();
        }
        return undefined;
      }
    }
        
    // Helper to create an element with attributes, children and properties
    function el(tag, attributes, children, properties) {
      let tagClassIds = tag.split(/(?=#|\.)/g);
      let x;
      for(let attr of tagClassIds) {
        if(x && attr.startsWith(".")) {
          x.classList.toggle(attr.substring(1), true);
        } else if(x && attr.startsWith("#")) {
          x.setAttribute("id", attr.substring(1));
        } else if(!x) {
          x = document.createElement(attr);
        }
      }
      if(typeof attributes == "object") {
        for(let k in attributes) {
          let v = attributes[k];
          if(typeof v != "undefined") {
            x.setAttribute(k, v);
          }
        }
      }
      if(Array.isArray(children)) {
        for(let child of children) {
          if(typeof child === "string") {
            x.append(child)
          } else if(typeof child !== "undefined")
            x.appendChild(child);
        }
      } else if(typeof children !== "undefined") {
        x.append(children);
      }
      if(typeof properties == "object") {
        for(let k in properties) {
          x[k] = properties[k];
        }
      }
      return x;
    }    
   </script>]


-- Conversion of php script to elm script
phpToElmFinal path string =
  let includingFolder = Regex.replace """(/)[^/]*$""" (\{submatches=[slash]} -> slash) path in
  let phpToElm string =
        let echoRaw content = "\nob = ob + " + content in
        let wrapStr content = freeze String.q3 + Regex.replace "@" (\{match=m} -> m + m) content + freeze String.q3 in
        let echo content = echoRaw (wrapStr content) in
        let phpStringToElmString =
                (Regex.replace """(\")([^\"]*)(\")""" <| \m ->
                  nth m.group 1 +
                  (nth m.group 2
                  |> Regex.replace """\$[0-9a-zA-Z_]*""" (\n ->
                     freeze "\" + " + nth n.group 0 + freeze " + \"")) +
                  nth m.group 3) >>
                (Regex.replace """\$_GET\[([^\]]*)\]""" <| \m ->
                  freeze "listDict.get "+ nth m.group 1 + freeze " $_GET |> Maybe.withDefaultReplace ''"
                ) >>
                (Regex.replace """\$_SERVER\[([^\]]*)\]""" <| \m ->
                  freeze "listDict.get "+ nth m.group 1 + freeze " $_SERVER |> Maybe.withDefaultReplace ''"
                )
        in
        if not (Regex.matchIn "<?php" string) then
          echo string
        else
        Regex.replace """^((?:(?!<\?php)[\s\S])+?)(?=(<\?php))|(\?>)([\s\S]*?)(?=<\?php)|(\?>)([\s\S]*?)$|(^)(<\?php)([\s\S]*?)(?=\?>)|(<\?php)\s*if\s*\(([\s\S]*?)\s*\)\s*\{\s*\?>((?:(?!<\?php)[\s\S])+?)<\?php\s*\}\s*(?=\?>)|(<\?php)([\s\S]*?)(?=\?>)""" (
             \{submatches=[content1, isRaw1, isRaw2, content2, isRaw3, content3, beginning1, isPhp1, code1, isPhpIf, condIf, codeIf, isPhp2, code2]} ->
          if isPhp1 /= "" || isPhp2 /= "" || isPhpIf /= "" then
            let prefix = if isPhp1 /= "" then echo beginning1 else freeze "" in
            prefix +
            if isPhpIf /= "" then
              echoRaw <| "(if "+condIf+" then " + wrapStr codeIf + " else \"\")"
            else
            let code = if isPhp1 /= "" then code1 else code2 in
            case Regex.extract """^\s*include\("([^"]*)"\)""" code of
              Just [included] ->
                phpToElm (fs.read (includingFolder + included) |> Maybe.withDefaultReplace ("\n[code to read " + included + " in " + includingFolder +"]"))
              _ ->
            case Regex.extract """^\s*switch\s*\(([^\)]*)\)\s*\{((?:\s*(?:case\s*[^:]*?\s*|default):((?:\s*\$[\w_]+\s*=\s*(?:(?!;\r?\n)[\s\S])*;)*)(?:\s*break\s*;)?)*)\s*\}\s*""" code of
              Just [input, assignments, lastAssignment] ->
                let vars = "(" + (Regex.find """(\$[\w_]+)\s*=""" lastAssignment |> List.map (\[_, name] -> name) |> String.join ", ") + ")" in
                let results = assignments |> Regex.find """\s*(case\s*([^:]*?)\s*|default):((?:\s*\$[\w_]+\s*=\s*(?:(?!;\r?\n)[\s\S])*;)*)(?:\s*break\s*;)?""" |>
                      List.map (\[whole, caseOrDefault, pattern, values] ->
                        let tuple =
                              Regex.find """\s*\$[\w_]+\s*=\s*((?:(?!;\r?\n)[\s\S])*?)\s*;""" values |>
                              List.map (\[whole2, value2] -> phpStringToElmString value2) |> String.join ", "
                        in
                        let finalPattern = if caseOrDefault == "default" then "_" else pattern in
                        "\n  " + finalPattern + " -> (" + tuple + ")"
                      ) |> String.join ""
                in
                "\n" + vars + " = case " + phpStringToElmString input + " of"  + results
              _ ->
            case Regex.extract """\s*(?:echo|print)\s+([^;]+?);\s*""" code of
              Just [content] -> echoRaw content
              res ->
                "\n[convert" + code + "]\n" + toString res
          else
          let content = if isRaw1 /= "" then
                content1
              else if isRaw2 /= "" then
                content2
              else -- if isRaw3 /= "" then
                content3
          in
          echo content
        ) string
  in
  flip (+) "\nob" <|
  (+) "date _ = '2019'\nob = freeze ''" <| phpToElm string

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
  else if isTextFile path || varraw then
    Ok <html>
        <head>
        <title>@path</title>
        <style type="text/css" media="screen">
            #aceeditor { 
                  height: 100%;
                  width: 100%;
                  border: 1px solid #DDD;
                  border-radius: 4px;
                  border-bottom-right-radius: 0px;
                  margin-top: 5px;
            }
        </style>
        <script>
          function loadAceEditor() {
            console.log("executing script");
            var aceeditor = ace.edit("aceeditor");
            var mode = path.match(/\.js$/) ? "ace/mode/javascript" :
                       path.match(/\.html?$/) ? "ace/mode/html" :
                       path.match(/\.css$/) ? "ace/mode/css" :
                       path.match(/\.json$/) ? "ace/mode/json" :
                       path.match(/\.leo$/) ? "ace/mode/elm" :
                       path.match(/\.elm$/) ? "ace/mode/elm" :
                       path.match(/\.php$/) ? "ace/mode/php" :
                       "ace/mode/plain_text";
            aceeditor.session.setMode({path: mode, v: Date.now()});
            aceeditor.setOptions({
              fontSize: "20pt"
            });
            aceeditor.setValue(document.getElementById("aceeditor").getAttribute("initdata"));
            aceeditor.session.on('change', function(e) {
              document.getElementById("aceeditor").setAttribute("initdata", aceeditor.getValue());
            });
            var callbackSelection = function() {
              var anchor = aceeditor.selection.getSelectionAnchor();
              var lead = aceeditor.selection.getSelectionLead();
              var div = document.querySelector("#aceeditor");
              div.setAttribute("ghost-anchor-row", anchor.row)
              div.setAttribute("ghost-anchor-column", anchor.column)
              div.setAttribute("ghost-lead-row", lead.row)
              div.setAttribute("ghost-lead-column", lead.column)
            }
            aceeditor.selection.on("changeSelection", callbackSelection);
            aceeditor.selection.on("changeCursor", callbackSelection);
            var div = document.querySelector("#aceeditor");
            aceeditor.selection.moveTo(div.getAttribute("ghost-anchor-row") || 0, div.getAttribute("ghost-anchor-column") || 0)
            aceeditor.focus();
          }
        </script>
        </head>
        <body>
        <div id="aceeditor" list-ghost-attributes="class draggable style" children-are-ghosts="true"
          save-ghost-attributes="style ghost-anchor-column ghost-anchor-row ghost-lead-column ghost-lead-row" initdata=@sourcecontent></div>
        <script>
        editor.ghostNodes.push(node =>
          node.tagName === "SCRIPT" && node.getAttribute("src") && node.getAttribute("src").match(/mode-(.*)\.js|libs\/ace\/.*\/ext-searchbox.js/)
        );
        
        var script = document.createElement('script');
        script.src = 'https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.2/ace.js';
        script.async = false;
        script.setAttribute("isghost", "true");
        ace = undefined;
        document.head.appendChild(script);
        var path = @(jsCode.stringOf path);
        onAceLoaded = (delay) => () => {
          if(typeof ace != "undefined") {
            console.log("ace loaded.")
            loadAceEditor();
          } else {
            console.log("ace not loaded. Retrying in " + (delay * 2) + "ms");
            setTimeout(onAceLoaded(delay * 2), 100);
          }
        }
        onAceLoaded(1)();
        </script>
        </body>
        </html>
  else 
  let isPhp = Regex.matchIn """\.php$""" path in
  let isHtml = Regex.matchIn """\.html?$""" path in
  if isHtml || isPhp then
    let sourcecontent = if isHtml then applyDotEditor sourcecontent else
      let elmSourceContent = phpToElmFinal path sourcecontent in
      __evaluate__ (("$_GET", vars)::("$_SERVER", [("SCRIPT_NAME", "/" + path)])::("path", path)::("fs", fs)::preludeEnv) elmSourceContent |>
      case of
        Err msg -> serverOwned "error message" "<html><head></head><body><pre>Error elm-reinterpreted php: " + Regex.replace "<" "&lt;" msg + "</pre>Original computed source <pre>" +
          Regex.replace "<" "&lt;" elmSourceContent +
          "</pre></body></html>"
        Ok sourcecontent -> applyDotEditor sourcecontent
    in
    let interpretableData =
          case Regex.extract """^\s*<!DOCTYPE(?:(?!>)[\s\S])*>([\s\S]*)$""" sourcecontent of
            Just [interpretableHtml] -> serverOwned "begin raw tag" "<raw>" + interpretableHtml + serverOwned "end raw tag" "</raw>"
            _ ->
          case Regex.extract """^[\s\S]*?(<html\b[\s\S]*)$""" sourcecontent of
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
  else if Regex.matchIn """\.(elm|leo)$""" path then
    __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::preludeEnv) sourcecontent
  else if fs.isdir path then
    let
      pathprefix = if path == "" then path else path + "/"
    in
    Ok <html><head>
      
      <script>
        var ispressed = false;
        var whichOne = "";
        //declare bool variable to be false
        document.onkeydown = function(e) {
          if (e.ctrlKey){
              ispressed = true;
          }
        };
        document.onkeyup = function(e) {
          if (e.keyCode == 17){ //releasing ctrl key. doesn't set e.ctrlKey properly or would use that.
            ispressed = false;
          }
        }
        var handleFileSelect = e => {
          e.preventDefault();
        }
        document.addEventListener('drop', handleFileSelect, false);
      </script>
      <style>
        #menu_bar {
          overflow: hidden;
          background-color: #ffffff;
          opacity:1;
        }

        #menu_bar a {
          float: left;
          display: block;
          color: #f2f2f2;
          text-align: center;
          padding: 14px 16px;
          text-decoration: none;
          font-size: 17px;
        }

        #menu_bar a:hover {
          background-color: #ddd;
          color: black;
        }

        #menu_bar a.active {
          background-color: #4CAF50;
          color: white;
        }
        .dropdown {
          float: left;
          overflow: hidden;
        }

        .dropdown .dropbtn {
          font-size: 16px;  
          border: none;
          outline: none;
          color: white;
          padding: 14px 16px;
          background-color: inherit;
          font-family: inherit;
          margin: 0;
        }
        .dropdown .dropbtn {
          font-size: 16px;  
          border: none;
          outline: none;
          color: white;
          padding: 14px 16px;
          background-color: inherit;
          font-family: inherit;
          margin: 0;
        }

        .menu_bar a:hover, .dropdown:hover .dropbtn {
          background-color: red;
        }

        .dropdown-content {
          display: none;
          position: absolute;
          background-color: #f9f9f9;
          min-width: 160px;
          box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
          z-index: 1;
        }

        .dropdown-content a {
          float: none;
          color: black;
          padding: 12px 16px;
          text-decoration: none;
          display: block;
          text-align: left;
        }

        .dropdown-content a:hover {
          background-color: #ddd;
        }

        .dropdown:hover .dropdown-content {
          display: block;
        }
        .content {
          padding: 16px;
        }

        .sticky {
          position: fixed;
          top: 0;
          width: 100%;
        }

        .sticky + .content {
          padding-top: 60px;
        }
        #fileListing div.file-item {
          display: block;
        }
        #fileListing div.file-item input {
          display: none;
        }
        #fileListing div.file-item {
          display: table-row;
        }
        #fileListing div.file-item label {
          display: table-cell;
          vertical-align: middle;
          padding: 0.3em;
        }
        #fileListing div.file-item label:hover {
          background: rgb(229,243,255);
        }
        #fileListing div.file-item input:checked + label {
          color: white;
          outline: 1px solid rgb(153,209,255);
          background: rgb(204,232,255);
        }
        #fileListing div.file-item label a {
          text-decoration: none;
          color: black;
          padding: 2px;
        }
        #fileListing div.file-item label a:hover {
          text-decoration: underline;
          color: blue;
        }
        #fileListing div.file-item label svg {
          vertical-align: middle;
          transform: scale(0.5);
        }
        #fileListing div.file-item label svg.file-extension-icon {
          opacity: 0.5;
        }
        #fileListing div.file-item label svg.file-extension-icon > path {
          stroke:black;
          stroke-width:2px;
          stroke-linecap:butt;
          fill:none;
          -linejoin:miter;
          stroke-opacity:1;
        }
        #fileListing div.file-item label svg.file-extension-icon > text {
          font-size: 2em;
        }

      </style>
      <div id="menu_bar">
        <button id="renamefs" onClick="renameFs()">Rename File(s)</button>
        <button id="duplicatefs" onClick="duplicateFs()">Make a Copy</button>
        <button id="movefs" onClick="moveFs()">Move File(s)</button>
        <button id="createFolder" onClick="createFolder()">Create a Folder</button>
        <button id="deletefs" onClick="deleteFs()">Delete File(s)</button>
        <div id="forprog"></div>
      </div>
      </head><body><h1><label value=path>@path</label></h1>
      <form id="fileListing"></form>
      <script>
      var fullListDir = (path) => JSON.parse(doReadServer("fullListDir", path));
      var thisListDir = fullListDir ("@path");
      var folders = thisListDir.filter((i) => i[1] == true);
      var getSelectedFiles = () => Array.from(document.querySelectorAll("input.filesBtn")).filter((btn) => btn.checked);
      var warnSelectFile = reason => window.alert (reason + ", please select some and click this button again");
      var warnDeselectFiles = reason => window.alert (reason + ", please deselect all files and folders and click this button again");
      var isDupInFolder = (folder, name) => folder.filter((i) => i[0] == name).length != 0;
      var isDuplicateHere = (name) => isDupInFolder(thisListDir, name);
      var isFolder = (name) => folders.filter((i) => i[0] == name).length != 0;

      window.onscroll = function() {stickyFun()};
      var menu_bar = document.getElementById("menu_bar");
      var sticky = menu_bar.offsetTop;

      function stickyFun() {
        if (window.pageYOffset >= sticky) {
          menu_bar.classList.add("sticky")
        } else {
          menu_bar.classList.remove("sticky");
        }
      }
      function getOneFile(reason) {
        var selected = getSelectedFiles();
        if (selected.length == 0) {
          warnSelectFile(reason);
          return 0;
        } else if (selected.length != 1) {
          window.alert ("Please select only one file to rename");
          return 0;
        }
        return selected[0];
      }
      function renameFs() {
        console.log ("in rename fs");
        var sel = getOneFile("To rename files or folders");
        if (! sel) return;
        if (sel.id == "..") {
          window.alert("Can't change the up dir");
          return;
        }
        var newname = window.prompt("Set new name for file: ", sel.id);
        if (newname == null) return;
        if (newname == "") {
          window.alert("Please specify a new name for the file.");
          return;
        }
        if (isDuplicateHere(newname)) {
          window.alert("Please choose a different name so as to not overwrite an existing file. (or change the name of that file).");
          return;
        }
        var x = doWriteServer("rename", "@path" + sel.id, "@path" + newname);
        if (x) {
          console.log ("rename failed");
          window.alert("rename failed");
          return;
        }
        console.log ("renamed", sel.id, newname);
        goodReload();
      }
      function deleteFs() {
        var selected = getSelectedFiles();
        if (selected.length == 0) {
          warnSelectFile("To delete a file or a folder"); 
          return;
        }
        if (selected.filter((i) => i.id == "..").length != 0) {
          window.alert("Can't delete the parent dir");
          return;
        }
        var warningMsg = "Are you sure you want to delete the following file(s)?"
        for (i = 0; i < selected.length; i++) {
          warningMsg = warningMsg + "\n" + selected[i].id;
        }
        var conf = window.confirm(warningMsg);
        if (conf) {
          for (i = 0; i < selected.length; i++) {
            var isfolder = folders.filter((j) => j[0] == selected[i].id); //optomizable
            console.log (isfolder);
            if (isfolder.length != 0) {
              doWriteServer("rmdir", "@path" + selected[i].id); //does this work on non-empty stuff? idts....
              continue;
            }
            doWriteServer("unlink", "@path" + selected[i].id);
          }
          goodReload();
          return;
        }
      }
      function duplicateFs() {
        var sel = getOneFile("To duplicate files or folders");
        if (! sel) return;
        if (sel.id == "..") {
          window.alert("Can't change the up dir");
          return;
        }
        var lastdot = sel.id.lastIndexOf(".");
        var nn;
        if (isFolder(sel.id)) {
          nn = sel.id + "_(Copy)";
        } else {
          nn = sel.id.substring(0, lastdot) + "_(Copy)" + sel.id.substring(lastdot);
        }
        var newname = window.prompt("Name for duplicate: ", nn);
        var contents = doReadServer("read", "@path" + sel.id);
        if (contents[0] != "1") {
          window.alert ("Couldn't read the file for some reason. aborting.");
          console.error ("couldn't read the file for some reason. aborting.");
          return;
        }
        contents = contents.substring(1, contents.length);
        var resp = doWriteServer("create", "@path" + newname, contents);
        if (resp) {
          console.error ("Duplicating file failed for some reason: ", resp);
        } 
        goodReload();
      }
      function createFolder() {
        var btns = getSelectedFiles();
        if (btns.length != 0) {
          warnDeselectFiles("To create a folder");
          return;
        }
        var newname = window.prompt("Name for new folder: ", "");
        console.log (newname);
        if (newname == null) return;
        if (newname == "") {
          window.alert("Please set a name for the new folder!");
          return;
        }
        var dups = isDuplicateHere(newname);
        if (dups) {
          window.alert("There is already a file / folder with that name. Please try again.");
          return;
        }
        doWriteServer("mkdir", newname, "");
        goodReload();
      }
      function moveFs() {
        var btn = getOneFile("To move files or folders");
        if (!btn) return;
        if (btn.id == "..") {
          window.alert("Can't change the up dir");
          return;
        }
        var newpath = window.prompt("New path to file (relative to root of server):", "@path");
        if (newpath == null) return;
        if (newpath[newpath.length -1] != "/") {
          newpath = newpath + "/";
        }
        try {
          var nldir = fullListDir(newpath);
          if (isDupInFolder(nldir, btn.id)) {
            window.alert("There already exists a file in that folder with this name. Move cancelled.");
            return;
          }
        } catch (e) {
          window.alert ("The path specified does not exist. Move cancelled.");
          return;
        }
        console.log ("move approved");
        var oldloc = ("@path" + btn.id);
        var newloc = newpath == "/" ? btn.id : (newpath + btn.id);
        console.log ("renamimg\n%s\n%s", ("@path" + btn.id), (newpath + btn.id));
        doWriteServer("rename", oldloc, newloc); 
        console.log ("rename successful");
        goodReload();
      }

      function radPressed(){
        var btns = document.querySelectorAll("input.filesBtn");
        if (!ispressed){
          for(var i = 0; i < btns.length; i++){
            if (btns[i].value == whichOne) continue;
            btns[i].checked = false;
          }
        }
      }
      var handleFiles = (files) => {
        var pgbr = document.getElementById("forprog");
        var progbar = document.getElementById("progress-bar");
        if (!progbar) {
          pgbr.append(el("progress", {id:"progress-bar", max:100, value:0, visible:true}, [], {}));
          progbar = document.getElementById("progress-bar");
        } else {
          progbar.visible = true;
        }
        initializeProgress(files.length);
        var didUp = false;
        ([...files]).forEach((fl) => {
          var isgud = true;
          thisListDir.forEach(([nm, isdir]) => {
            if (nm == fl.name) {
              window.alert("File named " + fl.name + " already exists in this folder. Overwrite cancelled.", fl.name);
              isgud = false;
            }
          });
          if (isgud) {
            editor.uploadFile("@path" + fl.name, fl, (ok) => console.log ("was ok\n" + ok), (err) => console.err (err), updateProgress);
            didUp = true;
          }
        });
        progbar.value = 100;
        progbar.visible = false;
        if (didUp) {
          goodReload();
          pgbr.innerHTML = "";
        }
      }
      function preventDefaults (e) {
        e.preventDefault()
        e.stopPropagation()
      }
      function handleDrop(e) {
        preventDefaults(e);
        let dt = e.dataTransfer;
        let files = dt.files;
        handleFiles(files);
      }
      function loadFileList() {
        let form = document.getElementById("fileListing");
        let path = @(jsCode.stringOf path);
        let files = thisListDir;
        function getRecordForCheckbox(file) {
          var rec = {type:"checkbox",
                      id:file,
                      class:"filesBtn",
                      name:"filesBtn",
                      value:file,
                      onClick:"whichOne=value",
                      onChange:"radPressed()"};
          return rec;
        }
        var dirIcon = () => {
          var d = el("div", {}, [], {innerHTML: 
          `<svg class="file-extension-icon" width="60" height="30">
            <path d="M 8,3 5,6 5,26 10,10 32,10 32,6 18,6 15,3 8,3 Z M 5,26 10,10 37,10 32,26 Z" />`});
          return d.childNodes[0];
        }
        var extensionIcon = name => {
          let extension = name.replace(/^(?:(?!\.(?=[^\.]*$)).)*\.?/, "");
          if("." + extension == name || extension === "") extension = "-";
          var d = el("div", {}, [], {innerHTML: 
          `<svg class="file-extension-icon" width="60" height="30">
            <text x="0" y="25">${extension}
           `});
          return d.childNodes[0];
        }

        var fileItemDisplay = function(link, name, isDir) {
           return el("div", {class:"file-item"}, [
              el("input", getRecordForCheckbox(name), ""),
              el("label", {for:name, value:name}, [
                isDir ? dirIcon() : extensionIcon(name),
                el("a", {href:link}, name, {onclick: function(event) {
                  event.preventDefault();
                  let link = this.getAttribute("href");
                  doReloadPage(link);
                }})])]);
        }
        //el(tag, attributes, children, properties)
        if (path != "") {
          var link = "../" + "?ls=true";
          form.append(fileItemDisplay(link, "..", true));
        }
        // directories before files, sorted case-insensitive
        files.sort(([name1, isDir1], [name2, isDir2]) =>
          isDir1 && !isDir2 ? -1 : isDir2 && !isDir1 ? 1 :
          name1.toLowerCase() < name2.toLowerCase() ? -1 : 0);
        for (i = 0; i < files.length; i++) {
          var [name, isDir] = files[i];
          var link = isDir ? name + "/?ls" : name + "/?edit";
          form.append(fileItemDisplay(link, name, isDir));
        }

        form.append(el("input", {type:"file", id:"fileElem", onchange:"handleFiles(this.files)"}, [], {}));
      }
      loadFileList();
      var goodReload = () => {
        document.getElementById("fileListing").innerHTML = "";
        thisListDir = fullListDir ("@path");
        loadFileList();
      }
    window.addEventListener('drop', handleDrop, false);
    window.addEventListener('dragover', (e) => e.preventDefault(), false);
    </script></body></html>
  else 
    Ok <html><head></head><body>
      <p>Editor cannot open file because it does not recognize the extension.</p>
      <p>As an alternative, you can open the file in raw mode by appending <code>?raw</code> to it.</p>
      <button onclick="""
        location.search = location.search + (location.search == "" ? "?raw" : "&raw");
      """>Open @path in raw mode</button>
    </body></html>  

{---------------------------------------------------------------------------
 Recovers from evaluation errors
----------------------------------------------------------------------------}
recoveredEvaluatedPage: Html
recoveredEvaluatedPage = --updatecheckpoint "recoveredEvaluatedPage" <|
  case evaluatedPage of
  Err msg -> serverOwned "Error Report" <|
    <html><head></head><body style="color:#cc0000"><div style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><pre style="white-space:pre-wrap">@msg</pre></div></body></html>
  Ok page -> page

jsEnabled = boolVar "js" True

removeJS node = case node of
  [text, content] -> node
  [tag, attrs, children] ->
    if tag == "script" then [tag, [], [["TEXT", "/*Script disabled by Thaditor*/"]]] else
    [tag, attrs, List.map removeJS children]
  _ -> []

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
main = 
  --updatecheckpoint "main" <|
  case recoveredEvaluatedPage of
  ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
    List.filter (case of [_, _] -> False; _ -> True) |>
    List.mapWithReverse identity (case of
      ["body", bodyattrs, bodyChildren] ->
        let bodyChildren = if jsEnabled then bodyChildren else List.map removeJS bodyChildren in
        ["body",
           (if canEditPage then
             [["contenteditable", "true"]] |> serverOwned "contenteditable attribute of the body due to edit=true" 
            else freeze []) ++
           bodyattrs, insertThereInstead identity True bodyChildren ++ luca ++
          if not varedit || varls then
            bodyChildren
          else 
             (if canEditPage then ((serverOwned "edition menu" editionmenu) sourcecontent) else
              if not varedit && not iscloseable && not varproduction then serverOwned "open edit box" [openEditBox] else
              serverOwned "edit prelude when not in edit mode" []) ++
             bodyChildren ++
             Update.sizeFreeze [["div", [["id", "editor-files-to-overwrite"]], insertThereInstead insertedElementsToWriteFile True fileOperations]] ++
             (serverOwned "synchronization script and placeholder" [<div class="bottom-placeholder editor-interface"> </div>, <script  id="thaditor-lastscript" class="editor-interface">@lastEditScript</script>] ++ insertThereInstead identity False bodyChildren -- All new nodes there are added back to bodyChildren.
             )]
      ["head", headattrs, headChildren] ->
        let headChildren = if jsEnabled then headChildren else List.map removeJS headChildren in
        ["head", headattrs,
           insertThereInstead identity True headChildren ++  -- All new nodes added to the beginning of the head are added back to headChildren.
           serverOwned "initial script" initialScript ++
           (serverOwned "stylesheet-of-server" <link rel="stylesheet" type="text/css" href="/server-elm-style.css" class="editor-interface"> :: headChildren)]
      x -> x -- head
    )]
  x-> <html><head></head><body>Not a valid html page: @("""@x""")</body></html>
  --|> Update.debug "main"

insertedElementsToWriteFile = List.map <| case of
   [_, [["class", "file-overwrite"], ["name", name], ["oldcontent", oldcontent], ["newcontent", newcontent]], _] ->
     (name, Write oldcontent newcontent (Update.diffs oldcontent newcontent |> Maybe.withDefault (VStringDiffs [])))
   thisInstead ->
     error """In #editor-files-to-overwrite, you should put attributes class="file-overwrite" name="[full path name]" oldcontent="[old file content]" newcontent="[new file content]"> in this order (even call the funciton addFileToSave(name, oldContent, newContent) fo simplicity. Got @thisInstead"""

-- Returns an empty list. If elements are inserted, inserts them in the given list instead.
insertThereInstead onInsert atBeginning list =
  Update.lens {apply _ = [],
    update {outputNew, input=list} =
      Ok (InputsWithDiffs [(if atBeginning then onInsert outputNew ++ list else list ++ onInsert outputNew, Just <|
        VListDiffs [(if atBeginning then 0 else List.length list, ListElemInsert (List.length outputNew))]
      )])
  } list

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
""" class="editor-interface">
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
editionmenu thesource = [
<div id="modify-menu" list-ghost-attributes="style class" sourcecontent=@thesource contenteditable="false" children-are-ghosts="true"></div>,
<div id="context-menu" children-are-ghosts="true" list-ghost-attributes="style class" contenteditable="false"></div>,
if iscloseable then <span class="editor-interface" dummy=""></span> else closeEditBox]

initialScript = serverOwned "initial script" [
<script class="editor-interface" type="text/javascript" src="https://cdn.jsdelivr.net/gh/MikaelMayer/lossless-css-parser@d4d64a4a87f64606794a47ab58428900556c56dc/losslesscss.js"></script>,
<script class="editor-interface">

// TODO: Find a way to store a cookie containing credentials, and have this server refresh tokens.
// https://developers.google.com/identity/sign-in/web/server-side-flow
// https://stackoverflow.com/questions/32902734/how-to-make-google-sign-in-token-valid-for-longer-than-1-hour
// https://www.w3schools.com/js/js_cookies.asp
if(typeof googleAuthIdToken == "undefined") {
  var googleAuthIdToken = undefined;
}

function isGhostNode(elem) {
  return elem && elem.isghost || (elem.nodeType == 1 &&
    (elem.tagName == "GHOST" || elem.getAttribute("isghost") == "true"));
}

function areChildrenGhosts(n) {
  return n && n.getAttribute && (
    n.getAttribute("children-are-ghosts") == "true" ||
    n.getAttribute("children-are-ghost") == "true"
  );
}
function hasGhostAncestor(htmlElem) {
  if(htmlElem == null) return false;
  if(isGhostNode(htmlElem)) return true;
  return areChildrenGhosts(htmlElem.parentNode) || (htmlElem.parentNode == null && htmlElem.nodeType !== 9 /*document*/) || hasGhostAncestor(htmlElem.parentNode);
}
function isGhostAttributeKey(name) {
  return name.startsWith("ghost-");
}

// Editor's API is stored in the variable editor.

editor = typeof editor === "object" ? editor : {};

editor.matches = function(elem, selector) {
  if(elem && elem.matches) {
    try {
      return elem.matches(selector);
    } catch(e) {
      return false;
    }
  }
  return false;
}
    
// An array of (node => {innerHTML, attributes, properties}) that can be defined by plug-ins.
editor.customContextMenuButtons = [];

// Creates an SVG icon from the given path. If fill is true, will have the path filled.
function svgFromPath(path, fill) {
  return `<svg class="context-menu-icon${fill ? " fill": ""}" width="40" height="30">
        <path d="${path}"></path></svg>`
}
editor.svgFromPath = svgFromPath;

// Array of functions on nodes returning an array of attributes that should be ghosts (i.e. removed on back-propagation)
editor.ghostAttrs = [];
editor.ghostAttrs.push(n =>
  ((n && n.getAttribute && n.getAttribute("list-ghost-attributes")) || "").split(" ").concat(
    ((n && n.getAttribute && n.getAttribute("save-ghost-attributes")) || "").split(" ")).filter(a => a != "")
);
editor.ghostAttrs.push(n =>
  n && n.tagName == "HTML" ? ["class"] : []
);
editor.ghostAttrs.push(n =>
  n && n.tagName == "BODY" ? ["data-gr-c-s-loaded"] : []
);
// attribute of some chrome extensions
editor.ghostAttrs.push(n => ["bis_skin_checked"]);

// Array of functions on nodes returning an array of attributes that should be ignored (i.e. old value returned on back-propagation)
editor.ignoredAttrs = [];
editor.ignoredAttrs.push(n =>
  ((n && n.getAttribute && n.getAttribute("list-ignored-attributes")) || "").split(" ").concat(
    ((n && n.getAttribute && n.getAttribute("save-ignored-attributes")) || "").split(" ")).filter(a => a != "")
)

// Returns a method that, for each key name, return true if it is a ghost attribute for the node
function isSpecificGhostAttributeKeyFromNode(n) {
  var additionalGhostAttributes = [];
  for(var k in editor.ghostAttrs) {
    additionalGhostAttributes = additionalGhostAttributes.concat(editor.ghostAttrs[k](n))
  }
  return (a => name => a.indexOf(name) != -1)(additionalGhostAttributes);
}

// Returns a method that, for each key name, return true if it is an ignored attribute for the node
function isIgnoredAttributeKeyFromNode(n) {
  var additionalIgnoredAttributes = [];
  for(var k in editor.ignoredAttrs) {
    additionalIgnoredAttributes = additionalIgnoredAttributes.concat(editor.ignoredAttrs[k](n))
  }
  return ((a, n) => (name, oldValue) => {
    let result = a.indexOf(name) != -1;
    if(result) { // let's store the previous attribute's value
      n.__editor__ = n.__editor__ || {};
      n.__editor__.ignoredAttrMap = n.__editor__.ignoredAttrMap || {};
      if(!(name in n.__editor__.ignoredAttrMap) && typeof oldValue !== "undefined") {
        n.__editor__.ignoredAttrMap[name] = oldValue;
      }
    }
    return result;
  })(additionalIgnoredAttributes, n);
}
function ignoredAttributeValue(n, name) {
  let result = n.__editor__.ignoredAttrMap[name];
  if(typeof result === "undefined") {
    return n.getAttribute(name);
  }
  return result;
}

// Array of predicates that, if they return true on a node, Editor will mark this node as ghost.
editor.ghostNodes = [];

// Analytics scripts
editor.ghostNodes.push(insertedNode =>
  editor.matches(insertedNode, "script[src]") &&
     (insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1 ||
      insertedNode.getAttribute("src").indexOf("google-analytics.com/gtm/js") != -1 ||
      insertedNode.getAttribute("src").indexOf("googletagmanager.com/gtm.js") != -1)
);

// For for ace styles in header
editor.ghostNodes.push(insertedNode => {
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
editor.ghostNodes.push(insertedNode =>
  editor.matches(insertedNode, "div.abcRioButton") ||
  editor.matches(insertedNode, "iframe#ssIFrame_google")
);
// For anonymous styles inside HEAD (e.g. ace css themes and google sign-in)
 editor.ghostNodes.push(insertedNode => 
   insertedNode.tagName == "STYLE" && insertedNode.getAttribute("id") == null && insertedNode.attributes.length == 0 &&
   insertedNode.parentElement.tagName == "HEAD" && typeof insertedNode.isghost === "undefined"&& insertedNode.textContent.match("error_widget\\.ace_warning")
   && (insertedNode.setAttribute("save-ghost", "true") || true)
 );
// For ace script for syntax highlight
editor.ghostNodes.push(insertedNode =>
  editor.matches(insertedNode, "script[src]") &&
     insertedNode.getAttribute("src").startsWith("https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.2/mode-j")
);
// For ace script for syntax highlight
editor.ghostNodes.push(insertedNode =>
  insertedNode.tagName == "ACE_OUTER"
);
// For the grammarly extension
editor.ghostNodes.push(insertedNode =>
  editor.matches(insertedNode, ".gr-top-z-index, .gr-top-zero")
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
        if(hasGhostAncestor(insertedNode)) {
          insertedNode.isghost = true;
        } else {
          if(typeof insertedNode.isghost === "undefined" && (insertedNode.nodeType == 1 && insertedNode.getAttribute("isghost") != "true" || insertedNode.nodeType == 3 && !insertedNode.isghost) && editor.ghostNodes.find(pred => pred(insertedNode, mutation))) {
           if(insertedNode.nodeType == 1) insertedNode.setAttribute("isghost", "true");
           insertedNode.isghost = true;
          } else { // Record ignored attributes
            if(insertedNode.nodeType == 1) {
              var isIgnoredAttributeKey = isIgnoredAttributeKeyFromNode(insertedNode);
              for(var k = 0; k < insertedNode.attributes.length; k++) {
                var attr = insertedNode.attributes[k];
                isIgnoredAttributeKey(attr.name, attr.value);
              }
            }
          }
        }
      }
    }
    
  }
}

if (typeof automaticGhostMarker !== "undefined") {
  // console.log("automaticGhostMarker.disconnect()");
  automaticGhostMarker.disconnect();
}

automaticGhostMarker = new MutationObserver(handleScriptInsertion);
automaticGhostMarker.observe
 ( document.head.parentElement
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
// Returns the closest ancestor of the selection having the given tagName
function getEnclosingCaret(tagName) {
  var w = getSelectionStart();
  while(w != null && w.tagName.toLowerCase() != tagName.toLowerCase()) {
    w = w.parentNode;
  }
  return w;
}

// Removes all the text from a node (not the text nodes themselves)
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
editor.emptyTextContent = emptyTextContent;

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
    var parentInsertion = options.target ? options.target.parentElement : node.parentElement;
    var insertBeforeNode = options.after ? options.target ? options.target.nextSibling : node.nextSibling :
                                           options.target ? options.target             : node;
    if(node.nextSibling != null && !options.target && !options.ignoreText) {
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
    insertBefore(parentInsertion, cloned, insertBeforeNode);
    return cloned;
  }
}
editor.duplicate = duplicate;

function remove(node, options) {
  if(typeof options == "undefined") options = {}
  if(node.previousSibling != null && !options.ignoreText) { // Remove whitespace as well
    var next = node.nextSibling;
    if(next.nodeType == 3 && next.nextSibling != null &&
       next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "TH" || node.tagName == "LI" || node.tagName == "TD")) {
      next.remove();
    }
  }
  node.remove();
}
editor.remove = remove;
</script>
]

-- Script added to the end of the page
lastEditScript = """ 
    console.log("lastEditScript running");
    var onMobile = () => window.matchMedia("(orientation: portrait)").matches;
    var buttonHeight = () => onMobile() ? 48 : 30;
    var buttonWidth  = () => onMobile() ? 48 : 40;
    
    // Before saving, call this function to that it eventually triggers a save action to any file.
    function addFileToSave(path, oldcontent, newcontent) {
      var placement = document.querySelector("#editor-files-to-overwrite");
      if(!placement) {
        console.log("could not save file " + name + "because #editor-files-to-overwrite not found.");
        return;
      }
      placement.append(el("div", {class: "file-overwrite", name:path, oldcontent: oldcontent, newcontent: newcontent}))   ;
    }
    function off_state_visible() {
      if (editor_model.state.includes("v")) {
        if (editor_model.state.includes("a")) {
          editor_model.state = editor_model.state.replace("a", "");
        }
        if (editor_model.state.includes("s")) {
          editor_model.state = editor_model.state.replace("s", "");
        }
        editor_model.state = editor_model.state.replace("v", "");
      } else {
        throw "State shouldn't be trying to be closed when it's already closed";
      }
    }

	  function set_state_visible() {
	    if (!editor_model.state.includes("v")) {
	      editor_model.state = editor_model.state + "v";
	    }
    }

	  function set_state_log() { //log needs visible to be on AND advanced to be on
	    if (!editor_model.state.includes("v")) throw "Shouldn't set state to log w/o v";
	    if (!editor_model.state.includes("a")) throw "Shouldn't set state to log w/o a";
	    if (editor_model.state.includes("a")) return;
	    editor_model.state = editor_model.state.replace("s", "");
	  }

	  function set_state_advanced() {
	    if (!editor_model.state.includes("v")) {
	      editor_model.state = editor_model.state + "v";
	      if (!editor_model.state.includes("a")) {
	        editor_model.state = editor_model.state + "a";
	      } else {
	        throw "Advanced state shouldn't be open when not visible";
	      }
	    } else { //is visible
	      if (editor_model.state.includes("a")) {
	        throw "Opening advanced state when it's already open";
	      } else {
	        if (editor_model.state.includes("d")) {
	          editor_model.state = editor_model.state.replace("d", "");
	        }
	        editor_model.state = editor_model.state + "a";
	      }
	    }
	  }

	  function set_state_linkselect() {
	    //linkselect state is definitely mututally exclusive with the menu being opened, at least.
	    //we can assume that those are turned off before turning linkselect on
	    if (editor_model.state.includes("v") || editor_model.state.includes("a")) {
	      throw "set_state_linkselect expects v + a to be off";
	    } else {
	      if (editor_model.state.includes("d")) throw "d was on without v what gives";
	      if (editor_model.state.includes("a")) throw "a was on without v what gives";
	      if (editor_model.state.includes("l")) {
	        //already in linkselectmode, do nothing.
	      } else {
	        editor_model.state = editor_model.state + "l";
	      }
	    }
	  }

	  function set_state_insert() {
	    if (editor_model.state.includes("i")) return;
	    editor_model.state = editor_model.state + "i";
	  }

	  function set_state_draftview() {
	    if (editor_model.state.includes("d")) return;
	    if (editor_model.state.includes("a")) {
	      editor_model.state = editor_model.state.replace("a", "");
	    }
	    if (!editor_model.state.includes("v")) {
	      editor_model.state = editor_model.state + v;
	    }
	    editor_model.state = editor_model.state + "d";
	  }

	  function off_state_draftview() {
	    if (!editor_model.state.includes("d")) sreturn;
	    editor_model.state = editor_model.state.replace("d", "");
	  }

	  function off_state_insert() {
	    if (!editor_model.state.includes("i")) return;
	    editor_model.state = editor_model.state.replace("i", "");
	  }

	  function off_state_advanced() {
	    if (!editor_model.state.includes("a")) return;
	    if (editor_model.state.includes("s")) {
	      editor_model.state = editor_model.state.replace("s", "");
	    }
	    editor_model.state = editor_model.state.replace("a", "");
	  }

	  function off_state_linkselect() {
	    if (!editor_model.state.includes("l")) return;
	    editor_model.state = editor_model.state.replace("l", "");
	  }

	  function off_state_visible() {
	    if (!editor_model.state.includes("v")) return;
	    if (editor_model.state.includes("a")) {
	      if (editor_model.state.includes("s")) {
	        editor_model.state = editor_model.state.replace("s", "");
	      }
	      editor_model.state = editor_model.state.replace("a", "");
	    }
	    if (editor_model.state.includes("d")) {
	      editor_model.state = editor_model.state.replace("d", "");
	    }
	    editor_model.state = editor_model.state.replace("v", "");
	  }

	  function off_state_log() {
	    if (!editor_model.state.includes("s")) return;
	    editor_model.state = editor_model.state.replace("s", "");
	  }

	  function toggle_visible_state() {
	    if (editor_model.state.includes("v")) { //open want to close
	      if (editor_model.state.includes("a")) { //close out of advanced
	        if (editor_model.state.includes("s")) {
	          editor_model.state = editor_model.state.replace("s", "");
	        }
	        editor_model.state = editor_model.state.replace("a", "");
	      }
	      if (editor_model.state.includes("d")) {
	        editor_model.state = editor_model.state.replace("d", "");
	      }
	      editor_model.state = editor_model.state.replace("v", "");
	    } else { //closed and we want to open
	      editor_model.state = editor_model.state + "v";
	    }
	  }
  
	  function toggle_advanced_state() { //can't be in advanced without visible
	    if (editor_model.state.includes("v")) {
	      //visible
	      if (editor_model.state.includes("a")) { //toggle it off
	        if (editor_model.state.includes("s")) {
	          editor_model.state = editor_model.state.replace("s", "");
	        }
	        editor_model.state = editor_model.state.replace("a", "");
	      } else { //toggle it on
	        if (editor_model.state.includes("d")) {
	          editor_model.state = editor_model.state.replace("d", "");
	        }
	        editor_model.state = editor_model.state + "a";
	      }
	    } else { //not visible
	      if (editor_model.state.includes("a")) { //can't be in advanced without visible
	        throw "model state shouldn't be in advanced when thing is not visible";
	      } //toggle advanced + visible
	      if (editor_model.state.includes("i") || editor_model.state.includes("l")) {
	        console.err("I really don't think we should be able to toggle a while i or l are set. maybe i'm wrong? if so delete this");
	      }
	      editor_model.state = editor_model.state + "va";
	    }
	  }

	  function toggle_draftview_state() { //can't be in draftview when advanced is on

	    if (!editor_model.state.includes("v")) { //not visible
	      if (editor_model.state.includes("l")) { //we're in linkselectmode , we should return here.
	        console.log ("trying to toggle_draftview_state when in linkselect mode. returning before doing anything.");
	        return;
	      }
	      if (editor_model.state.includes("d")) { //in not visible state, it has draft on
	        throw "State shouldn't be in draft mode when not visible";
	      }
	      if (editor_model.state.includes("a")) { //also assertion; not in visible state but advanced is wrong
	        throw "State shouldn't be in a without v";
	      }
	      editor_model.state = editor_model.state + "vd"; //else visible needs to be on for draft to be on.
	    } else {
	      //is visible; toggle d
	      if (editor_model.state.includes("d")) {
	        editor_model.state = editor_model.state.replace("d", "");
	      } else { //if we're in advanced we want to turn that off + turn on d
	        if (editor_model.state.includes("a")) {
	          editor_model.state = editor_model.state.replace("a", "");
	        }
	        editor_model.state = editor_model.state + "d";
	      }
	    }
	  }


	  // Before saving, call this function to that it eventually triggers a save action to any file.
	  function addFileToSave(path, oldcontent, newcontent) {
	    var placement = document.querySelector("#editor-files-to-overwrite");
	    if(!placement) {
	      console.log("could not save file " + name + "because #editor-files-to-overwrite not found.");
	      return;
	    }
      let existingDiv = placement.querySelector("div[name='"+path+"']");
      if(existingDiv) {
        existingDiv.setAttribute("newcontent", newcontent);
      } else {
	      placement.append(el("div", {class: "file-overwrite", name:path, oldcontent: oldcontent, newcontent: newcontent}));
      }
	  }
    

  
	  // Save/Load ghost attributes after a page is reloaded, only if elements have an id.
	  // Same for some attributes
	  function saveGhostAttributes() {
	    var ghostModified = document.querySelectorAll("[ghost-visible]");
	    var savedGhostAttributes = [];
	    for(var i = 0; i < ghostModified.length; i++) {
	      var elem = ghostModified[i];
	      savedGhostAttributes.push([editor.toTreasureMap(elem),
	          "ghost-visible", ghostModified[i].getAttribute("ghost-visible")]);
	    }


	    function saveAttributes(name) {
	       var ghostAttributesModified = document.querySelectorAll("["+name+"]");
	      for(var i = 0; i < ghostAttributesModified.length; i++) {
	        var elem = ghostAttributesModified[i];
	        var toSave = elem.getAttribute(name).split(" ");
	        for(j in toSave) {
	          var key = toSave[j];
	          savedGhostAttributes.push([editor.toTreasureMap(elem), key, elem.getAttribute(key)]);
	        }
	      }
	    }
      saveAttributes("save-ghost-attributes");
      saveAttributes("save-ignored-attributes");  
    
      var elemsWithAttributesToSave = document.querySelectorAll("[save-properties]");
	    var savedProperties = [];
	    for(var i = 0; i < elemsWithAttributesToSave.length; i++) {
	      var elem = elemsWithAttributesToSave[i];
	      var toSave = elem.getAttribute("save-properties").split(" ");
	      for(j in toSave) {
	        var key = toSave[j];
	        savedProperties.push([editor.toTreasureMap(elem), key, elem[key]])
        }
      }
      var parentsGhostNodes = [];
      var ghostElemsToReinsert = document.querySelectorAll("[save-ghost]");
      for(var i = 0; i < ghostElemsToReinsert.length; i++) {
        var elem = ghostElemsToReinsert[i];
        parentsGhostNodes.push({parent: editor.toTreasureMap(elem.parentNode), node: elem});
      }
      return [savedGhostAttributes, savedProperties, parentsGhostNodes];
    }
    function applyGhostAttributes(attrs) {
      var [savedGhostAttributes, savedProperties, parentsGhostNodes] = attrs;
      for(var i in savedGhostAttributes) {
        var [data, key, attr] = savedGhostAttributes[i];
        var elem = editor.fromTreasureMap(data);
        if(elem != null) {
          elem.setAttribute(key, attr);
        }
      }
      for(var i in savedProperties) {
        var [data, key, value] = savedProperties[i];
        var elem = editor.fromTreasureMap(id);
        if(elem != null) {
          elem[key] = value;
        }
      }
      for(var i in parentsGhostNodes) {
        var {parent: data, node: elem} = parentsGhostNodes[i];
        var parent = editor.fromTreasureMap(data);
        if(parent != null) {
          if(!elem.getAttribute("id") || !document.getElementById(elem.getAttribute("id"))) {
            parent.appendChild(elem);
          }
        }
      }
    }
   
    
    function domNodeToNativeValue(n) {
      if(n.nodeType == 3) {
        return ["TEXT", n.textContent];
      } else if(n.nodeType == 8) {
        return ["COMMENT", n.textContent];
      } else {
        var attributes = [];
        var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(n);
        var isIgnoredAttributeKey =  isIgnoredAttributeKeyFromNode(n); // TODO recover ignored value
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          if(!isGhostAttributeKey(key) && !isSpecificGhostAttributeKey(key)) {
            var value = isIgnoredAttributeKey(key) ? ignoredAttributeValue(n, key) : n.attributes[i].value;
            if(key == "style") {
              value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2)
            }
            attributes.push([key, value]);
          }
        }
        var children = [];
        var childNodes = n.childNodes;
        if(n.tagName.toLowerCase() === "noscript" && n.childNodes.length === 1 && n.childNodes[0].nodeType === 3) {
          // We'll recover the associated HTML node
          childNodes = el("div", {}, [], {innerHTML: n.childNodes[0].textContent, parentNode: n}).childNodes;
        }
        if(!areChildrenGhosts(n)) {
          for(i = 0; i < childNodes.length; i++) {
            if(!isGhostNode(childNodes[i])) {
              children.push(domNodeToNativeValue(childNodes[i]));
            }
          }
        }
        return [n.tagName.toLowerCase(), attributes, children];
      }
    }

    //(outer lastEditScript)
    function saveDisplayProperties() {
      let singleChildNodeContent = document.querySelector("textarea#singleChildNodeContent");
      if(singleChildNodeContent) {
        editor_model.textareaScroll = singleChildNodeContent.scrollTop;
        editor_model.textareaSelectionStart = singleChildNodeContent.selectionStart;
        editor_model.textareaSelectionEnd = singleChildNodeContent.selectionEnd;
      }
    }
    
    function replaceContent(NC) {
      saveDisplayProperties();
      if(editor_model.caretPosition) {
        editor_model.caretPosition = dataToRecoverCaretPosition(editor_model.caretPosition);
      }
      if(editor_model.selectionRange) {
        editor_model.selectionRange = dataToRecoverSelectionRange(editor_model.selectionRange);
      }
      if(editor_model.clickedElem) {
        editor_model.clickedElem = editor.toTreasureMap(editor_model.clickedElem);
      }
      writeDocument(NC);
    }
    
    var t = undefined;
    
    onResponse = (xmlhttp) => function() {
      if(xmlhttp.readyState == XMLHttpRequest.DONE) {
        replaceContent(xmlhttp.responseText);
      }
    }
    
    handleServerPOSTResponse = (xmlhttp, onBeforeUpdate) => function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          editor_model.isSaving = false;
          if(typeof onBeforeUpdate !== "undefined") onBeforeUpdate();
          var saved = saveGhostAttributes();
          //return
          
          //source of the editing menu disappearing after reloading
          replaceContent(xmlhttp.responseText);
          
          applyGhostAttributes(saved);
          var newLocalURL = xmlhttp.getResponseHeader("New-Local-URL");
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
            console.log ("handleServerPOSTResponse ambiguity");
            var selected = JSON.parse(ambiguitySelected);
            var summaries = JSON.parse(ambiguitySummaries);
            
            var disambiguationMenuContent = [];
            disambiguationMenuContent.push(el("span#ambiguity-id", {v: ambiguityKey}, "Choose the update you prefer, and click the save button:"));
            // Find the common path of all files so that we don't need to repeat its name or path.
            var fileOf = x => x.replace(/^(.*): *\n[\s\S]*$/, "$1")
            var commonPrefix = fileOf(summaries[1]);
            for(var i = 1; i <= n; i++) {
              while(!fileOf(summaries[i - 1]).startsWith(commonPrefix)) {
                let n = commonPrefix.replace(/^(.*)(?:\/|\\).*$/, "$1");
                commonPrefix = n === commonPrefix ? "" : n;
              }
            }
            if(commonPrefix) {
              disambiguationMenuContent.push(el("span#ambiguity-prefix", {}, commonPrefix + ":"));
            }
            for(var i = 1; i <= n; i++) {
              var summary = summaries[i-1].substring(commonPrefix.length).
                    replace(/"/g,'&quot;').
                    replace(/</g, "&lt;").
                    replace(/---\)|\+\+\+\)/g, "</span>").
                    replace(/\(---/g, "<span class='remove'>").
                    replace(/\(\+\+\+/g, "<span class='add'>").
                    replace(/(\nL\d+C\d+:)(.*)/, "$1<span class='codepreview'>$2</span>");
              disambiguationMenuContent.push(el("span.solution" + (i == selected ? ".selected" : "") + (i == n && ambiguityEnd != 'true' ? '.notfinal' : ''), {
              title: i == selected ? "Currently displaying this solution" : "Select this solution" + (i == n && ambiguityEnd != 'true' ? " (compute further solutions after if any)" : ""), onclick: i == selected ? `` : `this.classList.add('to-be-selected'); selectAmbiguity('${ambiguityKey}', ${i})`}, "", {innerHTML: "#" + i + " " + summary}));
            }
            //disambiguationMenuContent.push(el("button.modifyMenuButton#cancelAmbiguity", {title: "Revert to the original version", onclick: `cancelAmbiguity("${ambiguityKey}", ${selected})`}, "Cancel"));
            editor_model.disambiguationMenu = el("div.disambiguationMenu", {}, disambiguationMenuContent);
            editor_model.disambiguationMenu.ambiguityKey = ambiguityKey;
            editor_model.disambiguationMenu.selected = selected;
            editor_model.clickedElem = undefined;
            editor_model.displayClickedElemAsMainElem = true;
            editor_model.notextselection = false;
            editor_model.caretPosition = undefined;
            editor_model.link = undefined;
            editor_model.advanced = true; // Opens advanced mode.
            editor_model.visible = true;
            set_state_advanced();
            //editor_model.displaySource: false, // Keep source opened or closed
            // TODO: Disable click or change in DOM until ambiguity is resolved.
          } else { //no ambiguity
            editor_model.disambiguationMenu = undefined;
            let opSummaryEncoded = xmlhttp.getResponseHeader("Operations-Summary");
            if(opSummaryEncoded) {
              var opSummary = decodeURI(opSummaryEncoded);
              let newMenu = el("menuitem#lastaction", {},
                  el("span.summary", {}, "Last action: " + opSummary)) 
              editor_model.feedback = newMenu;
              var newmenutimeout = setTimeout(function() { editor_model.feedback = undefined; newMenu.remove(); }, 2000);
              newMenu.onclick = ((n) => () => clearTimeout(n))(newmenutimeout);
            }
          } // /noambiguity
          var strQuery = "";
          if(newQueryStr != null) { //newQueryStr = undefined ==> (newQueryStr !== null) ==> false;
            var newQuery = JSON.parse(newQueryStr);
            for(var i = 0; i < newQuery.length; i++) {
              var {_1: key, _2: value} = newQuery[i];
              strQuery = strQuery + (i == 0 ? "?" : "&") + key + (value === "" && key == "edit" ? "" : "=" + value)
            } 
          }
          if(newLocalURL) { // Overrides query parameters
            window.history[xmlhttp.replaceState ? "replaceState" : "pushState"]({localURL: newLocalURL}, "Nav. to " + newLocalURL, newLocalURL);
          } else if(strQuery) {
            window.history.replaceState({}, "Current page", strQuery);
          }
          updateInteractionDiv(); 
        } //xhr.onreadystatechange == done
    } //handleServerPOSTResponse
    
    window.onpopstate = function(e){
        console.log("onpopstate", e);
        if(e.state && e.state.localURL) {
          navigateLocal(location, true);
        } else {
          navigateLocal(location.pathname + location.search, true);
        }
    };
    
    notifyServer = callback => {
      var xmlhttp = new XHRequest();
      xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp);
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
    function relativeToAbsolute(url) {
      if(isAbsolute(url) || url && url.length && url[0] == "/") return url;
      let u =  new URL(location.href);
      if(url[0] === "#") {
        return u.pathname + url; 
      }
      else {
        return u.pathname.replace(/[^\/]*$/, "") + url;
      }
    }
    function navigateLocal(url, replaceState) {
      notifyServer(xmlhttp => {
        xmlhttp.setRequestHeader("reload", "true");
        xmlhttp.setRequestHeader("url", url);
        console.log("setting url to ", url);
        xmlhttp.replaceState = replaceState;
      });
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
    function sendModificationsToServerNode() {
      if(document.getElementById("notification-menu") != null) {
        //document.getElementById("notification-menu").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`
        // TODO: Listen and gather subsequent modifications when it is loading
        return;
      }
      editor_model.isSaving = true;
      var newMenu = el("menuitem#notification-menu.to-be-selected", {isghost: true});
      if(document.getElementById('lastaction')) {
        document.getElementById('lastaction').remove();
      }
      if(document.getElementById("modify-menu")) {
        document.getElementById("modify-menu").append(newMenu);
      }
      saveDisplayProperties();
      updateInteractionDiv();
      setTimeout( () => {
        notifyServer(xmlhttp => {
          xmlhttp.setRequestHeader("question", editor_model.askQuestions ? "true" : "false");
          return JSON.stringify(domNodeToNativeValue(document.body.parentElement));
        })
      }, 0);
    }
    
    //var serverWorker = new Worker("/Thaditor/editor.js");

    function sendModificationsToServer() {
      
      if(document.getElementById("notification-menu") != null) {
        //document.getElementById("notification-menu").innerHTML = `cannot send the server more modifications until it resolves these ones. Refresh the page?`
        // TODO: Listen and gather subsequent modifications when it is loading
        return;
      }
      editor_model.isSaving = true;
      var newMenu = el("menuitem#notification-menu.to-be-selected", {isghost: true});
      if(document.getElementById('lastaction')) {
        document.getElementById('lastaction').remove();
      }
      if(document.getElementById("modify-menu")) {
        document.getElementById("modify-menu").append(newMenu);
      }
      editor_model.actionsDuringSave = [];
      updateInteractionDiv();
      sendNotification("Beginning save!");
      /*
        Spawn new worker thread to 
          (1) read SERVER_CONTENT. 
            set up xmlhttp request over in a worker thread and wait for the promise to be fullfilled.
          (2) save - also on worker thread
            on message over here will be notified when the save it complete and will be given the new 
            page content within the xmlhttp response. We need to rewrite the page with these data.
      */
      
      //let serverWorker = new Worker("/Thaditor/editor.js");
      const tosend = JSON.stringify(domNodeToNativeValue(document.body.parentElement));
      let data = {action:"sendMods", 
                  toSend:tosend,
                  gaidt:googleAuthIdToken,
                  aq:editor_model.askQuestions,
                  loc:location.pathname + location.search,
                  server_content:(typeof SERVER_CONTENT == "undefined" ? undefined : SERVER_CONTENT)};
      editor_model.serverWorker.onmessage = function(e) {
        //handle confirmDone
        if (e.data.action == "confirmDone") {
          let xmlhttp = new XHRequest();
          xmlhttp.response.setHeader("newLocalURL", e.data.newLocalURL);
          xmlhttp.response.setHeader("newQueryStr", e.data.newQueryStr);
          xmlhttp.response.setHeader("ambiguityKey", e.data.ambiguityKey);
          xmlhttp.response.setHeader("ambiguityNumber", e.data.ambiguityNumber);
          xmlhttp.response.setHeader("ambiguitySelected", e.data.ambiguitySelected);
          xmlhttp.response.setHeader("ambiguityEnd", e.data.ambiguityEnd);
          xmlhttp.response.setHeader("ambiguitySummaries", e.data.ambiguitySummaries);
          xmlhttp.response.setHeader("opSummaryEncoded", e.data.opSummaryEncoded);
          xmlhttp.response.text = e.data.text;
          /*
            We want to undo everything in the undo stack that has been done since the save began.
            In the process of vanilla undoing this (using mark's function), the items will be
            pushed onto the redoStack in the normal way, s.t. we can redo them in a moment.
            Once we're at the state we were at when we began to save, we re-write the page
            with the confirmed content that the worker gave us.
            Once the confirmed content has been rewritten, we have undo/redo stacks that point,
            as the undo/redo stacks are an array of array of MutationRecords, all of whose target
            has just been erased and replaced with a new object. 
            So we need to convert the old UR stacks to be pointing to the right objects.
            We solve this in the undo()/redo() functions, by checking to see if the object
            pointed to in the mutationrecord is still connected to the active DOM. if not,
            we use the inactive node to record the path up the tree, and search for the
            corresponding node in the newly active tree, replacing the MR.target with the active one.
            Once we have the UR stacks set up, we just need to vanilla undo/redo to get back to
            the state pre-update & post-save.
          */
          const ads = editor_model.actionsDuringSave;
          const adsLen = editor_model.actionsDuringSave.length;
          ads.forEach((action) => {
            if (action == "undo") {
              undo();
            } else if (action == "redo") {
              redo();
            } else {
              throw new Error("Unidentified action in restoring post-save state post-save");
            }
          });
          
          editor_model.outputObserver.disconnect();
          xmlhttp.onreadystatechange = handleServerPOSTResponse(xmlhttp, () => {});
          xmlhttp.readyState = XMLHttpRequest.DONE
          xmlhttp.onreadystatechange();
          const newAds = editor_model.actionsDuringSave;
          const newAdsLen = newAds.length;
          for (let i = 0; i < adsLen; i++) {
            if (newAds[i] == "undo") {
              undo();
            } else if (newAds[i] == "redo") {
              redo();
            } else {
              throw new Error("unidentified action in actionsduringsave");
            }
          }
          updateInteractionDiv();
          sendNotification("Save completed!");
        } else if(e.data.action == "message") {
          sendNotification(e.data.message)
        } else if(e.data.action == "reconnect") {
          thaditor_reconnect();
        }
      }
      editor_model.serverWorker.postMessage(data);
    } //sendModificationsToServer


    //other possible approaches
    //add writable property (for oldValue) to mutation object
    //create array with necessary properties/attributes
    /*
     * adds writiable properties to the MutationRecord objects so the undo/redo functions
     * will actually function later on
     */
     //sends info over to the undostack
    function sendToUndo(m, time) {
      //for childLists, add mutable next/previous sibling properties
      if(m.type === "childList") {
          /*Object.defineProperty(m, 'rePrevSib', {value: m.previousSibling /*&& !(m.previousSibling.nodeType == 1)) ? 
                                                m.previousSibling.previousElementSibling : m.previousSibling, 
                                                writable: true});
          Object.defineProperty(m, 'reNextSib', {value: m.nextSibling /*&& !(m.nextSibling.nodeType == 1)) ? 
                                                m.nextSibling.nextElementSibling : m.nextSibling,
                                                writable: true});*/
      }
        //for attributes/characterData, add alternative mutable oldValue
      else {
        Object.defineProperty(m, 'URValue', {value: m.oldValue, writable: true});
      }
      //Object.defineProperty(m, 'timestamp', {value: time})
      m.timestamp = time;
      //check if the last element on currently on the stack is operating on the same "information", i.e. oldValue or nodelists
      //and should be combined together when undoing/redoing
      
      let lastUndo = editor_model.undoStack[editor_model.undoStack.length-1];
      //makes single actions that are recorded as multiple mutations a single action
      //true here ==> mutation is separate action
      if(!lastUndo || (lastUndo[0].timestamp < (time - 10))) {  
        if (editor_model.isSaving) {
          editor_model.actionsDuringSave.unshift("undo");
        }
        editor_model.undoStack.push([m]);
        editor_model.redoStack = [];
      }
      //false here ==> mutation is same action as last mutation
      //makes no sense for somethign that is first added then removed for those actions to be grouped together 
      //i.e. if i add text then get rid of it, it makes no sense for undo to revert the removal and addition direclty in sequence
      else {
        lastUndo = editor_model.undoStack.pop();
        lastUndo.push(m);
        editor_model.undoStack.push(lastUndo);
      }     
    } //sendToUndo
    
    function handleMutations(mutations, observer) {
      var onlyGhosts = true;
      let cur_date = new Date();
      let cur_time = cur_date.getTime();
      for(var i = 0; i < mutations.length; i++) {
        // A mutation is a ghost if either
        // -- The attribute starts with 'ghost-'
        // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
        // -- It is the modification of a node or an attribute inside a ghost node.
        /*  
         * Add mutations to undo list if they are not ghosts and if they are really doing something.
         */
        let mutation = mutations[i];
        if(hasGhostAncestor(mutation.target)) {
          continue;
        }
        if(mutation.type == "attributes") {
          var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(mutation.target);
          var isIgnoredAttributeKey = isIgnoredAttributeKeyFromNode(mutation.target);
          if(isGhostAttributeKey(mutation.attributeName) || isSpecificGhostAttributeKey(mutation.attributeName) ||
             mutation.target.getAttribute(mutation.attributeName) === mutation.oldValue ||
             isIgnoredAttributeKey(mutation.attributeName)
              ) {
          } else {
            onlyGhosts = false;
            sendToUndo(mutation, cur_time);
            // Please do not comment out this line until we get proper clever save.
            console.log("Attribute is not ghost", mutation);
          }
        } else if(mutation.type == "childList") {
          if(!areChildrenGhosts(mutation.target)) {
            for(var j = 0; j < mutation.addedNodes.length; j++) {
              if(!hasGhostAncestor(mutation.addedNodes[j])) {
                onlyGhosts = false;
                sendToUndo(mutation, cur_time);
                // Please do not comment out this line until we get proper clever save.
                console.log(`Added node ${j} does not have a ghost ancestor`, mutation);
              }
            }
            for(var j = 0; j < mutation.removedNodes.length; j++) {
              if(!isGhostNode(mutation.removedNodes[j])) {
                onlyGhosts = false;
                sendToUndo(mutation, cur_time);
                // Please do not comment out this line until we get proper clever save.
                console.log(`Removed node ${j} was not a ghost`, mutation);
              }
            }
          }
        } else {
          onlyGhosts = false;
          sendToUndo(mutation, cur_time);
          // Please do not comment out this line until we get proper clever save.
          console.log("mutations other than attributes, childList and characterData are not ghosts", mutations);
        }
      }
      if(onlyGhosts) {
        return;
      } // Send in post the new HTML along with the URL
      
      if(!editor_model.autosave) {
        if(editor_model.undoStack.length)
        {
          editor_model.canSave = true;
        }
        var saveButtons = document.querySelectorAll(".saveButton");
        // TODO: Can we regenerate the whole interface for consistency?
        for(let sb of saveButtons) {
          sb.classList.toggle("disabled", false);
        }
        return;
      } 
      //autosave is on
      if(typeof t !== "undefined") {
        clearTimeout(t);
      }
      t = setTimeout(function() {
        t = undefined;
        if (apache_server) {
          sendModificationsToServer();
        } else {
          sendModificationsToServerNode();
        }
      }, @editdelay)
    } //handleMutations
  
    //debugging function for printing both teh undo and redo stacks.
    function printstacks() {
      console.log("-----------------------------");
      let i, j;
      console.log("UNDO STACK:");
      for(i = 0; i < editor_model.undoStack.length; i++) {
        console.log(i + ".");
        for(j = 0; j < editor_model.undoStack[i].length; j++) {
          console.log(editor_model.undoStack[i][j]);
        }
      }
      console.log("REDO STACK:");
      for(i = 0; i < editor_model.redoStack.length; i++) {
        console.log(i + "."); 
        for(j = 0; j < editor_model.redoStack[i].length; j++) {
          console.log(editor_model.redoStack[i][j]);
        }
      }
      console.log("-----------------------------");
    }

    //undo function: handles undo feature
    function undo() {
      let undoElem = editor_model.undoStack.pop();
      //need to check if undoStack is empty s.t. we can set the "savability" of the document accurately
      if(undoElem == undefined) {
        editor_model.canSave = false;
        return 0;
      }
      else if (!editor_model.undoStack.length) {
        editor_model.canSave = false;
      }
      //TODO prevent pressing the undo button while save underway while letting Editor use the undo function. (just not the user);
      //need to disconnect the MutationObserver such that our undo does not get recorded as a mutation
      editor_model.outputObserver.disconnect();
      const quicker = node => recoverElementFromData(dataToRecoverElement(node));
      let k;
      for(k = undoElem.length - 1; k >= 0; k--) {
        let mutType = undoElem[k].type; 
        let qk = quicker(undoElem[k].target);
        
        let target = (undoElem[k].target.isConnected ? 
                        undoElem[k].target :
                        (qk == undefined ? undoElem[k].target : qk));
        //in each case, we reverse the change, setting the URValue/oldValue as the current value
        //at the target, and replacing the URValue/oldValue with the current value present in target
        if(mutType == "attributes") {
          let cur_attr = target.getAttribute(undoElem[k].attributeName);
          if(undoElem[k].URValue === null) {
            target.removeAttribute(undoElem[k].attributeName); 
          }       
          else { 
            target.setAttribute(undoElem[k].attributeName, undoElem[k].URValue);
          }
          undoElem[k].URValue = cur_attr; 
        }
        else if(mutType == "characterData") {
          const cur_data = target.data;
          target.data = undoElem[k].URValue;
          undoElem[k].URValue = cur_data;
          //undoElem[k].isConnected ? undoElem[k].URValue : quicker(undoElem[k]).URValue = cur_data;
        }
        else {
          let uRemNodes = undoElem[k].removedNodes;
          let uAddNodes = undoElem[k].addedNodes;
          //readding the removed nodes
          // -in this case, we loop through the childNodes and add them in the appropriate spot 
          // or remove them 
          // NOTE: we only change the nextSib property of the undoElem, and alternate between adding/removing from the 
          //       addedNodes & removedNodes lists depending on whether we are undoing (in which case we will add)
          // NOTE: Since there is only one nextSibling/prevSibling property, and based off the fact that MutationObserver
          //       should take into account every mutation, we should only have elements in one of uRemNodes and uAddNodes
          //       at once.
          let kidNodes = target.childNodes;
          let i, j;
          if(uRemNodes.length) {
            if(kidNodes.length === 0) {            
              if(undoElem[k].nextSibling == null && undoElem[k].previousSibling == null) {
                for(i = 0; i < uRemNodes.length; i++) { 
                  if(hasGhostAncestor(uRemNodes.item(i))) {
                    continue;
                  }
                  target.appendChild(uRemNodes.item(i)); 
                }
              }
            }
            for(j = 0; j < kidNodes.length; j++) {  
              let ns = undoElem[k].nextSibling && undoElem[k].nextSibling.isConnected ? undoElem[k].nextSibling : quicker(undoElem[k].nextSibling);
              let ps = undoElem[k].previousSibling && undoElem[k].previousSibling.isConnected ? undoElem[k].previousSibling : quicker(undoElem[k].previousSibling);

              let knode = kidNodes.item(j);
              let knode_may = quicker(knode);
              //if(kidNodes.item(j) === undoElem[k].nextSibling && kidNodes.item(j).previousSibling === undoElem[k].previousSibling) {
              if ((knode == ns || knode_may == ns || ns == undefined) &&
                  (knode.previousSibling == ps || knode_may.previousSibling == ps || ps == undefined)){
                for(i = 0; i < uRemNodes.length; i++) { 
                  if(hasGhostAncestor(uRemNodes.item(i))) {
                    continue;
                  }
                  let uremnode = uRemNodes.item(i);
                  let urn = quicker(uremnode);
                  target.insertBefore(urn == undefined ? uremnode : urn, knode_may == undefined ? knode : knode_may); 
                }
              }
            }
          }
          for(i = 0; i < uAddNodes.length; i++) {
            if(hasGhostAncestor(uAddNodes.item(i))) {
              continue;
            }
            else if(!target.contains(uAddNodes.item(i))) {
              console.log("The item you are trying to undo doesn't exist in the parent node.");
            }
            else {
              target.removeChild(uAddNodes.item(i));
              
            }
          }
        }
      } //mutation looper
      editor_model.redoStack.push(undoElem);
      if (editor_model.isSaving) {
        editor_model.actionsDuringSave.unshift("redo");
      }
      //TODO make sure save button access is accurate (i.e. we should ony be able to save if there are thigns to undo)
      //turn MutationObserver back on
      editor_model.outputObserver.observe
       ( document.body.parentElement
       , { attributes: true
         , childList: true
         , characterData: true
         , attributeOldValue: true
         , characterDataOldValue: true
         , subtree: true
         }
       );
      updateInteractionDiv();
      return 1;
    } //undo

    

    function redo() {
      let redoElem = editor_model.redoStack.pop();
      if(redoElem === undefined) {
        return 0;
      }
      editor_model.outputObserver.disconnect();
      const quicker = node => recoverElementFromData(dataToRecoverElement(node));
      let k;
      for(k = 0; k < redoElem.length; k++) {
        let mutType = redoElem[k].type;
        let qk = quicker(redoElem[k].target);
        let target = (redoElem[k].target.isConnected ? 
                        redoElem[k].target : 
                        (qk == undefined ? redoElem[k].target : qk));
        if(mutType == "attributes") {
          let cur_attr = target.getAttribute(redoElem[k].attributeName);
          if (redoElem[k].URValue === null) {
            target.removeAttribute(redoElem[k].attributeName); 
          } else { 
            target.setAttribute(redoElem[k].attributeName, redoElem[k].URValue);
          }
          redoElem[k].URValue = cur_attr;
        } else if(mutType == "characterData") {
          let cur_data = target.data;
          target.data = redoElem[k].URValue;  
          redoElem[k].URValue = cur_data;
          //redoElem[k].isConnected ? redoElem[k].URValue : quicker(redoElem[k]).URValue = cur_data;
        } else {
          let rRemNodes = redoElem[k].removedNodes;
          let rAddNodes = redoElem[k].addedNodes;
          let i, j;
          let kidNodes = target.childNodes;
          if(rAddNodes.length) {
            for(j = 0; j < kidNodes.length; j++) {
              let knode = kidNodes.item(j);
              let raddnode = rAddNodes.item(i);
              let ran = quicker(raddnode);
              let knode_may = quicker(knode);
              //if(kidNodes.item(j) === redoElem[k].nextSibling && kidNodes.item(j).previousSibling === redoElem[k].previousSibling)
              let ns = redoElem[k].nextSibling && redoElem[k].nextSibling.isConnected ? redoElem[k].nextSibling : quicker(redoElem[k].nextSibling);
              let ps = redoElem[k].previousSibling && redoElem[k].previousSibling.isConnected ? redoElem[k].previousSibling : quicker(redoElem[k].previousSibling);
              
              if ((knode == ns || knode_may == ns || ns == undefined) &&
                  (knode.previousSibling == ps || knode_may.previousSibling == ps || ps == undefined)) {
                for(i = 0; i < rAddNodes.length; i++) {
                  if(hasGhostAncestor(rAddNodes.item(i))) {
                    continue;
                  }
                  target.insertBefore(ran == undefined ? rAddNodes.item(i) : ran, knode_may == undefined ? knode : knode_may);
                }
              }
            }
          }
          for(i = 0; i < rRemNodes.length; i++) {
            if(hasGhostAncestor(rRemNodes.item(i))) {
              continue;
            } else if(!target.parentElement.contains(quicker(rRemNodes.item(i)))) { //bc the node in rRemNodes isn't necessarily connected, we need to rewrite this.
              console.log("The item you are trying to redo doesn't exist in the parent node.");
            } else {
              target.removeChild(quicker(rRemNodes.item(i)));
            }
          }
        }
      } //mut looper
      editor_model.undoStack.push(redoElem);
      if (editor_model.isSaving) {
        editor_model.actionsDuringSave.unshift("undo");
      }
      editor_model.canSave = true;
      editor_model.outputObserver.observe
       ( document.body.parentElement
       , { attributes: true
         , childList: true
         , characterData: true
         , attributeOldValue: true
         , characterDataOldValue: true
         , subtree: true
         }
       );
       updateInteractionDiv();
       return 1;
    } //end of redo

    
    
    function pasteHtmlAtCaret(html) {
      var sel, range;
      if (window.getSelection) {
          // IE9 and non-IE
          sel = window.getSelection();
          // do not paste html into modify menu
          if (sel.anchorNode.offsetParent && sel.anchorNode.offsetParent.id === "modify-menu") {
            return;
          }
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
      var files = evt.dataTransfer.files; // FileList object
      uploadFilesAtCursor(files);
    }
    
    function uploadFilesAtCursor(files) { 
      // files is a FileList of File objects. List some properties.
      for (var i = 0, file; file = files[i]; i++) {
        var targetPathName =  editor.getStorageFolder(file) + file.name;
        // if(file.size < 30000000)
        editor.uploadFile(targetPathName, file, (targetPathName, file) => {
          @(    
            if folderView then 
            """
            reloadPage();
            """
            else
            """
            if(file.type.indexOf("image") == 0) {
              pasteHtmlAtCaret(`<img src="${targetPathName}" alt="${file.name}">`);
            } else {
              pasteHtmlAtCaret(`<a href="${path}">${path}</a>`); 
            }
            """
          )
        });
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
          if(document.getElementById("savebutton") && document.getElementById("savebutton").onclick) {
            document.getElementById("savebutton").onclick();
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
            onClickGlobal({target: s, modify: true});
          }
          // Open link.
        }
        if(e.which == 90 && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          if(!undo()) sendNotification("Nothing to undo!");
        }
        if(e.which == 89 && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          if(!redo()) sendNotification("Nothing to redo!");
        }
        //in link select mode, escape on the keyboard can be
        //used to exit the link select mode (same as escape button)
        if (editor_model.state.includes("l")) {
        //if(editor_model.linkSelectMode) {
          if(e.which == 27) {
            escapeLinkMode();
          }
        }
      };
      
      var bodyeditable = document.querySelector("body[contenteditable=true]");
      var onKeypress = e => {
        if(e.keyCode==13 && !e.shiftKey){ // [Enter] key
            // If we are inside a paragraph, we split the paragraph.
            // If we are directly inside a div, we add a paragraph separator.
            // We delete everything between anchorNode and focusNode
            // TODO: Handle ul and li
            var caretSelection = document.getSelection();
            var x = caretSelection.anchorNode;
            if(x && x.nodeType == 3 && caretSelection.rangeCount) { // text node
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
    
    var observeTargetA = null;
    
    var addEditEqualToUrl = function(href, what) {
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
    
    // Prevent mouse down on modify-menu that end outside modify-menu to trigger onclick
    var onMouseDownGlobal = function(event) {
      var tmp = event.target;
      while(tmp) {
        if(tmp.getAttribute && tmp.getAttribute("id") == "modify-menu") {
          editor_model.dismissNextClick = true;
          return;
        }
        tmp = tmp.parentElement;
      }
    }    
    var onClickGlobal = function (event) {
      if(editor_model.dismissNextClick) {
	      editor_model.dismissNextClick = false;
	      return;
      }
      var clickedElem = event.target;
      console.log(typeof event.target);
      var editorSelectOptions = document.querySelectorAll("meta[editor-noselect],meta[editor-doselect]");
      var matchOptions = function(clickedElem) {
        var result = true;
        for(let i = 0; i < editorSelectOptions.length; i++) {
          let negativeSelector = editorSelectOptions[i].getAttribute("editor-noselect"),
              positiveSelector = editorSelectOptions[i].getAttribute("editor-doselect");
          if(result && negativeSelector) {
            result = !editor.matches(clickedElem, negativeSelector);
          }
          if(!result && positiveSelector) {
            result = editor.matches(clickedElem, positiveSelector);
          }
        }
        return result;
      }
      while(clickedElem && editorSelectOptions && !matchOptions(clickedElem)) {
        clickedElem = clickedElem.parentElement;
      }                           
      var ancestors = [];
      var tmp = clickedElem;
      var aElement;
      var ancestorIsModifyBox = false;
      var ancestorIsContextMenu = false;
      var link = undefined;
      while(tmp) {
        ancestors.push(tmp);
        if(tmp.getAttribute && tmp.getAttribute("id") == "modify-menu") {
          ancestorIsModifyBox = true;
        }
        if(tmp.getAttribute && tmp.getAttribute("id") == "context-menu") {
          ancestorIsContextMenu = true;
        }
        if(!aElement && tmp.tagName === "A") { // First link.
          aElement = tmp;
          link = aElement.getAttribute("href");
        }
        tmp = tmp.parentElement;
      }
      document.querySelectorAll("[ghost-hovered=true]").forEach(e => e.removeAttribute("ghost-hovered"));
      if(ancestorIsModifyBox || ancestorIsContextMenu || ancestors[ancestors.length - 1].tagName != "HTML") return;
      //console.log("not modify box", ancestors)
      document.querySelector("#context-menu").classList.remove("visible");
      
      editor_model.clickedElem = clickedElem;
      editor_model.link = link;
      editor_model.link_href_source = aElement; // So that we can modify it
      editor_model.insertElement = false;
      off_state_insert();
      editor_model.advanced = false;
      off_state_advanced();
      editor_model.notextselection = false;
      updateInteractionDiv();
      // Check if the event.target matches some selector, and do things...
    }
    var arrowDown = svgFromPath("M 10,17 13,14 17,18 17,4 23,4 23,18 27,14 30,17 20,27 Z", true);
    var arrowRight = svgFromPath("M 21,25 18,22 22,18 8,18 8,12 22,12 18,8 21,5 31,15 Z", true);
    var arrowUp = svgFromPath("M 10,14 13,17 17,13 17,27 23,27 23,13 27,17 30,14 20,4 Z", true);
    var arrowLeft = svgFromPath("M 19,25 22,22 18,18 32,18 32,12 18,12 22,8 19,5 9,15 Z", true);
    var cloneSVG = svgFromPath("M 19,8 31,8 31,26 19,26 Z M 11,4 23,4 23,8 19,8 19,22 11,22 Z");
    var saveSVG = svgFromPath("M 10,5 10,25 30,25 30,9 26,5 13,5 Z M 13,6 25,6 25,12 13,12 Z M 22,7 22,11 24,11 24,7 Z M 13,15 27,15 27,24 13,24 Z M 11,23 12,23 12,24 11,24 Z M 28,23 29,23 29,24 28,24 Z", true);
    var openLeftSVG = svgFromPath("M 27.5,4 22.5,4 12.5,15 22.5,25 27.5,25 17.5,15 Z", true);
    var closeRightSVG = svgFromPath("M 12.5,4 17.5,4 27.5,15 17.5,25 12.5,25 22.5,15 Z", true);
    var openTopSVG = svgFromPath("M 9.5,22 9.5,17 20.5,7 30.5,17 30.5,22 20.5,12 Z", true);
    var closeBottomSVG = svgFromPath("M 9.5,7 9.5,12 20.5,22 30.5,12 30.5,7 20.5,17 Z", true);
    var wasteBasketSVG = svgFromPath("m 24,11.5 0,11 m -4,-11 0,11 m -4,-11 0,11 M 17,7 c 0,-4.5 6,-4.5 6,0 m -11,0.5 0,14 c 0,3 1,4 3,4 l 10,0 c 2,0 3,-1 3,-3.5 L 28,8 M 9,7.5 l 22,0");
    var plusSVG = svgFromPath("M 18,5 22,5 22,13 30,13 30,17 22,17 22,25 18,25 18,17 10,17 10,13 18,13 Z", true);
    var liveLinkSVG = link => `<a class="livelink" href="javascript:navigateLocal(relativeToAbsolute('${link}'))">${svgFromPath("M 23,10 21,12 10,12 10,23 25,23 25,18 27,16 27,24 26,25 9,25 8,24 8,11 9,10 Z M 21,5 33,5 33,17 31,19 31,9 21,19 19,17 29,7 19,7 Z", true)}</a>`;
    var gearSVG = svgFromPath("M 17.88,2.979 14.84,3.938 15.28,7.588 13.52,9.063 10,8 8.529,10.83 11.42,13.1 11.22,15.38 7.979,17.12 8.938,20.16 12.59,19.72 14.06,21.48 13,25 15.83,26.47 18.1,23.58 20.38,23.78 22.12,27.02 25.16,26.06 24.72,22.41 26.48,20.94 30,22 31.47,19.17 28.58,16.9 28.78,14.62 32.02,12.88 31.06,9.84 27.41,10.28 25.94,8.52 27,5 24.17,3.529 21.9,6.42 19.62,6.219 17.88,2.979 Z M 20,11 A 4,4 0 0 1 24,15 4,4 0 0 1 20,19 4,4 0 0 1 16,15 4,4 0 0 1 20,11 Z", true);
    var folderSVG = svgFromPath("M 8,3 5,6 5,26 10,10 32,10 32,6 18,6 15,3 8,3 Z M 5,26 10,10 37,10 32,26 Z");
    var reloadSVG = svgFromPath("M 32.5,8.625 30.25,15.25 24.75,11.125 M 6.75,20 9.875,14.5 15.125,19 M 29.5,18 C 28.25,22.125 24.375,25 20,25 14.5,25 10,20.5 10,15 M 10.5,12 C 11.75,7.875 15.625,5 20,5 25.5,5 30,9.5 30,15");
    var logSVG = svgFromPath("M 17.24,16 A 1.24,2 0 0 1 16,18 1.24,2 0 0 1 14.76,16 1.24,2 0 0 1 16,14 1.24,2 0 0 1 17.24,16 Z M 20,16 21.24,16 21.24,16 A 1.24,2 0 0 1 20,18 1.24,2 0 0 1 18.76,16 1.24,2 0 0 1 20,14 1.33,2.16 0 0 1 21,15 M 12,14 12,18 14,18 M 10,12 23,12 23,20 10,20 Z M 23,6 23,11 28,11 M 14,6 14,12 10,12 10,20 14,20 14,25 28,25 28,11 23,6 14,6 Z");
    var sourceSVG = svgFromPath("M 22.215125,2 25,3 18.01572,27 15,26 Z M 12,19 12,25 2,14 12,4 12,9 7,14 Z M 28,9 28,4 38,15 28,25 28,20 33,15 Z", true);
    var isAbsolute = url => url.match(/^https?:\/\/|^www\.|^\/\//);
    var linkToEdit = @(if defaultVarEdit then "link => link" else 
     """link => link && !isAbsolute(link) ? link.match(/\?/) ? link + "&edit" : link + "?edit" : link;""");
    var undoSVG = svgFromPath("M 9.5,12.625 11.75,19.25 17.25,15.125 M 31.5,16 C 30.25,11.875 26.375,9 22,9 16.5,9 12,13.5 12,19");
    var redoSVG = svgFromPath("M 31.5,12.625 29.25,19.25 23.75,15.125 M 9.5,16 C 10.75,11.875 14.625,9 19,9 24.5,9 29,13.5 29,19");

    var isDraftSVG = svgFromPath("M 2,7 2,25 38,25 38,7 M 36,6 C 32,6 29.1,3.9 26.1,3.9 23.1,3.9 22,5 20,6 L 20,23 C 22,22 23.1,20.9 26.1,20.9 29.1,20.9 32,22.9 36,22.9 Z M 4,6 C 8,6 10.9,3.9 13.9,3.9 16.9,3.9 18,5 20,6 L 20,23 C 18,22 16.9,20.9 13.9,20.9 10.9,20.9 8,22.9 4,22.9 Z");
    var escapeSVG = svgFromPath("M 7.5 4 L 17.5 15 L 7.5 25 L 12.5 25 L 20 17.5 L 27.5 25 L 32.5 25 L 22.5 15 L 32.5 4 L 27.5 4 L 20 12.25 L 12.5 4 L 7.5 4 z", true);
    var linkModeSVG = svgFromPath("M 14,3 14,23 19,19 22,27 25,26 22,18 28,18 Z");
    var checkSVG = svgFromPath("M 10,13 13,13 18,21 30,3 33,3 18,26 Z", true);
    var ifAlreadyRunning = typeof editor_model === "object";
    if (!ifAlreadyRunning) {
      var the_path;
      var thaditor_files = [
        "Thaditor", "Makefile", "ThaditorPackager.py", "ThaditorInstaller.py", "ThaditorInstaller.php",
        "ThaditorInstaller.htaccess", "composer.json", "credentials.json", "cacert.pem", "versions",
        "vendor", "ssg",
      ];
    }
    the_path = @(path |> jsCode.stringOf);
    if (isLive == undefined) {
      var isLive = () => !(the_path.includes("Thaditor/versions/"));
    }

    var verz = "Live";
    if (!isLive()) {
      verz = the_path.slice(the_path.lastIndexOf("versions/")+9, the_path.lastIndexOf("/"));
    }
    //hover mode functions for linkSelectMode
    function escapeLinkMode() {
      document.body.removeEventListener('mouseover', linkModeHover1, false);
      document.body.removeEventListener('mouseout', linkModeHover2, false);
      //removing the hovered element (which is retained if the escape key is hit)
      document.querySelectorAll("[ghost-hovered=true]").forEach(e => e.removeAttribute("ghost-hovered"));
      //editor_model.clickedElem = editor_model.linkFrom;
      off_state_visible();
      editor_model.visible = false;
      off_state_linkselect();
      editor_model.linkSelectMode = false;
      editor_model.linkSelectCallback = undefined;
      editor_model.linkSelectOtherMenus = undefined;
      updateInteractionDiv();
    }
    function noGhostHover (node) {
      curClass = node.getAttribute("class")
      if(curClass === "modify-menu-icon-label-link" ||
        curClass === "context-menu-icon" ||
        curClass === "context-menu-icon fill") {
          return false;
        }
      else if(node.tagName === "path" || node.tagName === "PATH") {
        return false;
      }
      return true;
    }
    function linkModeHover1(event) {
      //console.log(event.target);
      //console.log(event.target.tagName);
      //console.log(event.target.getAttribute("class"));
      if(noGhostHover(event.target)) { 
        event.target.setAttribute("ghost-hovered", true);
        updateInteractionDiv();
        //console.log("hey!");
      }
    }
    function linkModeHover2(event) {
      if(noGhostHover(event.target)) {
        event.target.removeAttribute("ghost-hovered");
        updateInteractionDiv();
      }
    }

    function dataToRecoverElement(oldNode) {
      if(!oldNode) return undefined;
      if(oldNode.nodeType == 1 && oldNode.getAttribute("id") && document.getElementById(oldNode.getAttribute("id"))) {
        return {id: oldNode.getAttribute("id")};
      }
      let tentativeSelector = [];
      let t = oldNode;
      let isText = false, textIndex = 0;
      while(t && t.parentNode) {
        let index = Array.prototype.slice.call( t.parentNode.children ).indexOf(t);
        if(t.nodeType === 1) {
          tentativeSelector.unshift(t.tagName + ":nth-child(" + (index + 1) + ")" );
        } else {
          isText = true;
          textIndex = Array.prototype.slice.call( t.parentNode.childNodes ).indexOf(t);
        }
        t = t.parentNode;
      }
      return {tentativeSelector: tentativeSelector, isText: isText, textIndex: textIndex};
    }
    
    // Returns the new node that matches the old node the closest.
    // For text nodes, try to recover the text node, if not, returns the parent node;
    function recoverElementFromData(data) {
      if(!data) return undefined;
      if(typeof data === "object" && data.id) {
        return document.getElementById(data.id);
      }
      if(typeof data == "object" && Array.isArray(data.tentativeSelector)) {
        let tentativeSelector = data.tentativeSelector;
        while(tentativeSelector.length >= 1) {
          let newNode = document.querySelector(tentativeSelector.join(" "));
          if(newNode) {
            return data.isText && newNode.childNodes && newNode.childNodes[data.textIndex] || newNode;
          }
          tentativeSelector.shift();
        }
        return undefined;
      }
    }
    function setCaretPositionIn(node, position) {
      position = Math.min(position, node.textContent.length);
      if (node.nodeType == 3) {
        let sel  = window.getSelection()
        setTimeout( () => sel.collapse(node, position), 0);
      } else {
        let p = position
        let n = node.firstChild
        while(n != null && p > n.textContent.length) {
          p = p - n.textContent.length
          n = n.nextSibling
        }
        if(n != null) {
          setCaretPositionIn(n, p)
        } else {
          console.log("Could not find position. Reached node and position ", [n, p])
        }
      }
    }
    function dataToRecoverCaretPosition(caretPosition) {
      if(!caretPosition) return undefined;
      return {target: editor.toTreasureMap(caretPosition.startContainer), startOffset: caretPosition.startOffset};
    }
    function recoverCaretPositionFromData(data) {
      if(!data) return;
      let newTextNodeOrParent = editor.fromTreasureMap(data.target);
      if(newTextNodeOrParent) setCaretPositionIn(newTextNodeOrParent, data.startOffset)
    }
    function dataToRecoverSelectionRange(selectionRange) { // TODO
      if(!selectionRange) return undefined;
      return undefined;
    }
    function recoverSelectionRangeFromData(data) { // TODO
      if(!data) return;
      return undefined;
    }
    //(outer lastEditScript)
    /*
    State is currently (8/12) being kept track of in many variables. I'm setting out to condense that
    into one variable, "state", a string describing the current state of the system. 
    For the sake of clarity, I will list the current variables within the editor_model that are used to keep track of state

      visible :v
      advanced :a
      show_log :s
      insertElement :i //(not defined in our inital editor_model object)
      linkSelectMode :l
      isDraftSwitcherVisible :d
    */
    var editor_model = { // Change this and call updateInteractionDiv() to get something consistent.
      //makes visibility of editor model consistent throughout reloads
      state: ifAlreadyRunning ? editor_model.state : "",
      visible: ifAlreadyRunning ? editor_model.visible : false, //here
      clickedElem: ifAlreadyRunning ? editor.fromTreasureMap(editor_model.clickedElem) : undefined,
      displayClickedElemAsMainElem: true, // Dom selector status switch signal
      previousVisitedElem: [], // stack<DOM node> which helps showing previous selected child in the dom selector
      notextselection: false, // When using the relative DOM selector, set to true to avoid considering the caret (e.g. for insertions and deletions)
      selectionRange: ifAlreadyRunning ? recoverSelectionRangeFromData(editor_model.selectionRange) : undefined,
      caretPosition: ifAlreadyRunning ? recoverCaretPositionFromData(editor_model.caretPosition) : undefined,
      link: undefined,
      advanced: ifAlreadyRunning ? editor_model.advanced : false, //here
      displaySource: ifAlreadyRunning ? editor_model.displaySource : false,
      disambiguationMenu: undefined, //here
      isSaving: false,
      //data structures to represent undo/redo "stack"
      undoStack: ifAlreadyRunning ? editor_model.undoStack : [],
      redoStack: ifAlreadyRunning ? editor_model.redoStack : [],
      actionsDuringSave: ifAlreadyRunning ? editor_model.actionsDuringSave : [],
      isDraftSwitcherVisible : ifAlreadyRunning ? editor_model.isDraftSwitcherVisible : false,
      //observer to listen for muts
      outputObserver: ifAlreadyRunning ? editor_model.outputObserver : undefined,
      //worker for interface with the server
      serverWorker: ifAlreadyRunning ? editor_model.serverWorker : new Worker("/Thaditor/editor.js"),
      //editor log
      editor_log: ifAlreadyRunning ? editor_model.editor_log : [],
      show_log: ifAlreadyRunning ? editor_model.show_log : false, //here
      linkSelectMode: false, //here
      linkSelectCallback: undefined, // Callback that is going to be called with the selected node.
      idNum: ifAlreadyRunning ? editor_model.idNum : 1,
      //new attribute to keep menu state after reload
      curScrollPos: ifAlreadyRunning ? editor_model.curScrollPos : 0,
      textareaScroll: ifAlreadyRunning ? editor_model.textareaScroll : 0,
      textareaSelectionStart: ifAlreadyRunning ? editor_model.textareaSelectionStart : 0,
      textareaSelectionEnd: ifAlreadyRunning ? editor_model.textareaSelectionEnd: 0,
      askQuestions: ifAlreadyRunning ? editor_model.askQuestions :
                    @(case listDict.get "question" vars of
                       Just questionattr -> "true"
                       _ -> if boolVar "question" True then "true" else 'false'),
      autosave: ifAlreadyRunning ? editor_model.autosave :
                    @(case listDict.get "autosave" vars of
                      Just autosaveattr -> "true"
                      _ -> if boolVar "autosave" True then "true" else "false"),
      path: @(path |> jsCode.stringOf),
      version : verz,
      interfaces: ifAlreadyRunning ? editor_model.interfaces : []
    }
    
    function init_interfaces() {
      let createButton = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("modify-menu-button");
        button.innerHTML = innerHTML;
        return button;
      } //you can append a createbutton to the element returning in render
      let add_btn_to_div = (div, innerHTML, attributes, properties) => {
        div.append(createButton(innerHTML, attributes, properties));
      };
      editor_model.interfaces.push({
        title: "Advanced",
        minimized: true,
        priority(editor_model) {
          return undefined;
        },
        enabled(editor_model) {
          false;
        },
        render: function render(editor_model, innerBox) {
          let retDiv = el("div", {"class":"modify-menu-icons"});
          //We need 3 btns: refresh, filesystem + help.
          add_btn_to_div(retDiv, reloadSVG,
            {"class": "tagName", title: "Reload the current page"},
              {onclick: function(event) { editor_model.curScrollPos = (editor_model.displaySource ? document.getElementById("sourcecontentmodifier").scrollTop : 0);
                reloadPage(); } }
            );
          add_btn_to_div(retDiv, folderSVG,
            {"class": "tagName", title: "List files in current directory"},
              {onclick: function(event) {
                let u =  new URL(location.href);
                u.pathname = u.pathname.replace(/[^\/]*$/, "");
                u.searchParams.set("ls", "true");
                navigateLocal(u.href);
              }
            }
          );
          
          return retDiv;
        }
      });
      
      editor_model.interfaces.push({
        title: "Log",
        minimized: true,
        priority(editor_model) {
          return this.enabled(editor_model) ? 1 : undefined;
        },
        enabled(editor_model) {
          return false;
        },
        render: function render(editor_model, innerBox) {
          let retDiv = el("div", {"class":"modify-menu-icons"});
          let log = document.getElementById("fullLog");
          if (!log) {
            log = el("textarea", {id:"fullLog", class:"textarea logger", readonly:true, isghost:true, value:"(no log)"}, [], {});
          }
          let logtxt = "";
          const elog = editor_model.editor_log;
          for (let i = 0; i < elog.length; i++) {
            const l = elog[i];
            logtxt = logtxt + l + "\n";
          }
          logtxt == "" ? log.value = "(no log)" : log.value = logtxt;
          retDiv.append(log);
          return retDiv;
        }
      });
      editor_model.interfaces.push({
        title: "Source",
        minimized: true,
        priority(editor_model) {
          return this.enabled(editor_model) ? 1 : undefined;
        },
        enabled(editor_model) {
          return false;
        },
        render: function render(editor_model, innerBox) {
          return "pass";
        }
      });
    }
    
    // First time: We add the interface containers.
    if(!ifAlreadyRunning) {
      init_interfaces();
    }
    

    function reorderCompatible(node1, node2){
      let topLevelOrderableTags = {TABLE:1, P:1, LI:1, UL:1, OL:1, H1:1, H2:1, H3:1, H4:1, H5:1, H6:1, DIV:1};
      let metaOrderableTags = {META:1, TITLE:1, SCRIPT: 1, LINK: 1, STYLE: 1};
      return node1.tagName === node2.tagName && node1.tagName !== "TD" && node1.tagName !== "TH" ||
        topLevelOrderableTags[node1.tagName] && topLevelOrderableTags[node2.tagName] ||
        metaOrderableTags[node1.tagName] && metaOrderableTags[node2.tagName];
    }
    function preventTextDeselection(e){
      e = e || window.event;
      e.preventDefault();
    }
    function restoreCaretPosition() {
      if(typeof editor_model.caretPosition != "undefined") {
        var sel = window.getSelection();
        sel.removeAllRanges();
        var range = document.createRange();
        range.setStart(editor_model.caretPosition.startContainer, editor_model.caretPosition.startOffset);
        range.setEnd(editor_model.caretPosition.endContainer, editor_model.caretPosition.endOffset);
        sel.addRange(range);
      }
    }
    // This function activates the node selection mode, in which one DOM node can be selected,
    // After clicking on confirm, the callback is called with the selected node.
    // callbackUI is invoked to render other buttons along with the confirmation button.
    function activateNodeSelectionMode(msg, callback, callbackUI) {
      editor_model.visible = false;
      off_state_visible();
      
      editor_model.linkSelectMode = true;
      set_state_linkselect();
      editor_model.clickedElem = document.body; //"center" clicked element on document body
      //removes all context menu stuff 
      document.querySelector("#context-menu").classList.remove("visible");
      editor_model.linkSelectCallback = callback;
      editor_model.linkSelectMsg = "Confirm " + msg;
      editor_model.linkSelectOtherMenus = callbackUI;
      updateInteractionDiv();
      sendNotification(editor_model.linkSelectMsg);
      document.body.addEventListener('mouseover', linkModeHover1, false);
      document.body.addEventListener('mouseout', linkModeHover2, false);
    }

    

    function copy_website(source, dest) {
      let website_files = JSON.parse(doReadServer("fullListDir", source));
      let is_dest_valid = doReadServer("isdir", dest)
      if (!website_files) throw "copy_website(): invalid source";
      if (!is_dest_valid) throw "copy_website(): invalid dest";
      
      //filter out Thaditor files
      website_files = website_files.filter(val => !thaditor_files.includes(val[0]));
      website_files = website_files.filter(val => val[0][0] != ".");
      //cpy website_files to to dest
      website_files.forEach(val => {
        let [nm, isdir] = val;
        const s = (source + nm);
        const d = (dest + nm);
        if (isdir) {
          doWriteServer("fullCopy", s, d);
        } else {
          doWriteServer("move", d, s);
        }
      });
      let dh = doReadServer("read", source + "/.thaditor_meta");
      dh = dh.slice(1, dh.length);
      let draft_history = (dh == "" ? undefined : JSON.parse(dh));
      const get_date_meta = () => (new Date).toString();
      if (draft_history == undefined) {
        draft_history = ["live:" + get_date_meta()];
      } else {
        draft_history.push(editor_model.version + ":" + get_date_meta());
      }
      doWriteServer("write", dest + "/.thaditor_meta", JSON.stringify(draft_history));
      return 1;
    }
    function deleteCurrentDraft() {
      if (editor_model.version == "Live") throw "Shouldn't be able to call deleteCurrentDraft when in Live";
      const ans = window.confirm("Are you sure you want to permanently delete " + editor_model.version + "?");
      if (!ans) return;
      //the path of the folder we want to delete is and always will be Thaditor/versions/<editor_model.version>/
      const pth_to_delete = "Thaditor/versions/" + editor_model.version + "/";
      doWriteServer("deletermrf", pth_to_delete);
      navigateLocal("/?edit");
      sendNotification("Permanently deleted draft named: " + editor_model.version);
    }
    function publishToLive() {
      //Find which version we're at by examining editor_model.version and/or the path
      //copy all of the files in the draft/ folder out to the public facing site.
      //simple as thaditor_files.includes

      const conf = window.confirm("Are you sure you want to publish " + editor_model.version + " to live?");
      if (!conf) {
        return;
      }
      if (isLive()) {
        throw "Can't publish live to live";
      }
      let t_src = editor_model.path.slice(0, editor_model.path.lastIndexOf("/")+1);
      copy_website(t_src, "");
      const oldver = editor_model.version;
      editor_model.version = "Live";
      navigateLocal("/?edit", true);
      updateInteractionDiv();
      sendNotification("Successfully published " + oldver + " to live.");
      setTimeout (() => sendNotification("Switched to live."), 1500)
    }
    
    

    updateInteractionDiv(); //outer lastEditScript

    

    function updateInteractionDiv() {
      
      // Set up
      let model = editor_model;
      var clickedElem = model.clickedElem;
      var CSSparser = new losslesscssjs();
      var contextMenu = document.querySelector("#context-menu");
      var modifyMenuDiv = document.querySelector("#modify-menu");
      if(!modifyMenuDiv || !contextMenu) return;
      modifyMenuDiv.classList.toggle("editor-interface", true);

      // Display the interface or not
      modifyMenuDiv.classList.toggle("visible", editor_model.visible); //Mikael what does this do? -B

      // Make sure at most one element is marked as ghost-clicked.
      document.querySelectorAll("[ghost-clicked=true]").forEach(e => e.removeAttribute("ghost-clicked"));
      if(clickedElem && clickedElem.nodeType === 1) {
        clickedElem.setAttribute("ghost-clicked", "true");
      }
      
      // Recover selection if it exists
      model.selectionRange = model.notextselection ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0); 
        if(!f || !f.getBoundingClientRect ||
            f.startOffset === f.endOffset && f.startContainer === f.endContainer) return;
        return f;
      })();
      
      // Recover caret position if it exists
      model.caretPosition = model.notextselection || clickedElem && clickedElem.tagName === "HEAD" ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0);
        if(!f || f.startOffset !== f.endOffset && f.startContainer !== f.endContainer) return;
        return f;
      })();
      
      // Helpers: Text preview and summary
      function textPreview(element, maxLength) {
        let x = element.textContent;
        let result = "'" + x + "'";;
        if(x == "") {
          if(element.tagName === "META") {
            result = element.getAttribute("charset") ? "charset:" + element.getAttribute("charset")  :
                    (element.getAttribute("name") || element.getAttribute("http-equiv") || "(name?)") + ": " + (element.getAttribute("content") || "(content?)");
          } else if(element.tagName === "SCRIPT" || element.tagName === "IMG") {
            result = typeof element.getAttribute("src") === "string" ? (element.getAttribute("src") || "(src?)").replace(/(https?:\/\/)?(www\.)?/, "") : "empty script";
          } else if(element.tagName === "LINK") {
            result = typeof element.getAttribute("href") === "string" ? (element.getAttribute("href") || "(src?)").replace(/(https?:\/\/)?(www\.)?/, "") : "empty script";
          }
        }
        if(typeof maxLength !== "undefined" && result.length > maxLength) {
          return result.substring(0, maxLength) + "...'";
        }
        return result;
      }
      function summary(element, idAndClasses, maxLength) {
        var summary = element.tagName.toLowerCase();
        if(idAndClasses && element.getAttribute("id")) {
          summary += "#" + element.getAttribute("id");
        }
        var elemClass = element.getAttribute("class");
        if(idAndClasses && elemClass && elemClass.trim().length) {
          summary += "." + elemClass.split(/\s+/g).join(".");
        }
        summary += " " + textPreview(element);
        maxLength = maxLength || 80;
        summary = summary.substring(0, maxLength || 80) + (summary.length > 80 ? "..." : "");
        return summary;
      }
      
      // We render the content of modifyMenuDiv from scratch
      modifyMenuDiv.innerHTML = "";
      let modifyMenuPinnedIconsDiv = el("div", {"class":"modify-menu-icons pinned"}); // Icons always visible
      let modifyMenuIconsDiv = el("div", {"class":"modify-menu-icons"}); // Top-level icons on the top bar
      let domSelector = el("div", {"class": "dom-selector noselect"}); // create dom selector interface
      let interactionDiv = el("div", {"class": "information"}); // Everything else for now
      modifyMenuDiv.append(modifyMenuPinnedIconsDiv); // Keep this one as it.
      modifyMenuDiv.append(modifyMenuIconsDiv);       // TODO: Move to editor_model.interfaces
      modifyMenuDiv.append(domSelector);              // TODO: Move to editor_model.interfaces
      
      /*
        Render interfaces / containers
      */

      for(let i = 1; i < editor_model.interfaces.length; i++) {
        let x = editor_model.interfaces[i];
        let priority = x.priority(editor_model);
        if(i > 0 && typeof priority === "number") {
          let previous = editor_model.interfaces[i-1]
          let beforePriority = previous.priority(editor_model);
          if(typeof beforePriority === "undefined" && (!previous.enabled(editor_model) || previous.minimized)) {
            var tmp = editor_model.interfaces[i];
            editor_model.interfaces[i] = editor_model.interfaces[i-1];
            editor_model.interfaces[i-1] = tmp;
            i--; // Bubble up
          }
        }
      }
      for(let i = 0; i < editor_model.interfaces.length; i++) {
        let x = editor_model.interfaces[i];
        let priority = x.priority(editor_model);
        let initMinimized = typeof priority == "number" ? false :
                            x.enabled(editor_model) ? x.minimized : true;
        let renderedContent = x.render(editor_model);
        let menu = el("div", {
          class:"editor-container" + (x.enabled(editor_model) ? "" : " disabled") + (initMinimized ? " minimized" : "")}, [
          el("div.editor-container-title", {
            title: typeof renderedContent === "string" ? renderedContent : undefined
         }, [el("span", {title: "Expand menu"}, x.title),
             el("div.editor-container-icon#displayarrow", {}, [], {innerHTML: openTopSVG}),
             i >= editor_model.interfaces.length - 1 ? undefined :
             el("div.editor-container-icon", {title: "Move menu down"}, [], {innerHTML: arrowDown,
               onclick: (i => event => {
                 var tmp = editor_model.interfaces[i];
                 editor_model.interfaces[i] = editor_model.interfaces[i+1];
                 editor_model.interfaces[i+1] = tmp;
                 updateInteractionDiv();
               })(i)}),
             i == 0 ? undefined :
             el("div.editor-container-icon", {title: "Move menu up"}, [], {innerHTML: arrowUp,
               onclick: (i => event => {
                 var tmp = editor_model.interfaces[i];
                 editor_model.interfaces[i] = editor_model.interfaces[i-1];
                 editor_model.interfaces[i-1] = tmp;
                 updateInteractionDiv();
               })(i)})
            ], {
            onclick: ((x, initMinimized) => event => {
              let target = event.target;
              while(!target.matches(".editor-container")) target = target.parentNode;
              //console.log("onclick", event.target);
              x.minimized = target.classList.contains("minimized");
              x.minimized = !x.minimized;
              target.classList.toggle("minimized", x.minimized);
            })(x, initMinimized)
          }),
          el("div.editor-container-content", {}, renderedContent),
        ]);
        modifyMenuDiv.append(menu);
      }
      // TODO: Migrate the content of interfactionDiv to editor_model.interfaces
      
      modifyMenuDiv.append(interactionDiv);
      let createButton = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("modify-menu-button");
        button.innerHTML = innerHTML;
        return button;
      }
      let addModifyMenuIcon = function(innerHTML, attributes, properties) {
        modifyMenuIconsDiv.append(createButton(innerHTML, attributes, properties));
      }
      let addPinnedModifyMenuIcon = function(innerHTML, attributes, properties) {
        modifyMenuPinnedIconsDiv.append(createButton(innerHTML, attributes, properties));
      }
      var panelOpenCloseIcon = function() {
        return document.querySelector("#modify-menu").classList.contains("visible") ?
            onMobile() ? closeBottomSVG : closeRightSVG + "<span class='modify-menu-icon-label'>Close</span>"
          : onMobile() ? openTopSVG : openLeftSVG + "<span class='modify-menu-icon-label'>Open</span>";
      }
      var alwaysVisibleButtonIndex = 0;
      function nextVisibleBarButtonPosStyle() {
        let result = "position: absolute;" +
          (onMobile() ? "top:-"+buttonHeight()+"px;left:"+alwaysVisibleButtonIndex*buttonWidth()+"px" :
                        "left:-"+buttonWidth()+"px;top:"+alwaysVisibleButtonIndex*buttonHeight()+"px")
        alwaysVisibleButtonIndex++;
        return result;
      }
      if (!editor_model.state.includes("l")) {
      //if(!editor_model.linkSelectMode) {
        addPinnedModifyMenuIcon(
          panelOpenCloseIcon(),
          {title: "Open/close settings tab", "class": "inert" },
          {onclick: function(event) {
              document.querySelector("#modify-menu").classList.toggle("visible");
              toggle_visible_state();
              editor_model.visible = !editor_model.visible;
              setTimeout(maybeRepositionContextMenu, 500);
              this.innerHTML = panelOpenCloseIcon();
            }
        });
        addPinnedModifyMenuIcon(
          gearSVG + "<span class='modify-menu-icon-label'>Misc.</span>",
          {title: "Advanced", "class": "inert" + (editor_model.advanced ? " active": "")
          },
          {onclick: (c => function(event) {
            //defaults to turning on advanced menu if the editor model is already visible, otherwise toggles advanced menu.
            toggle_advanced_state();
            if(editor_model.visible) {
              editor_model.advanced = !editor_model.advanced;
            }
            else {
              editor_model.advanced = true;
            }
            editor_model.visible = true;
            updateInteractionDiv();
          })(clickedElem)}
        )
        addPinnedModifyMenuIcon(undoSVG + "<span class='modify-menu-icon-label'>Undo</span>", 
          {"class": "inert", title: "Undo most recent change",
            id: "undobutton"
          },
          {onclick: function(event) {
            if(!undo()) sendNotification("Nothing to undo!");
            }
          }   
        );
        addPinnedModifyMenuIcon(redoSVG + "<span class='modify-menu-icon-label'>Redo</span>",
          {"class": "inert", title: "Redo most recent undo",
            id: "redobutton"
          },
        	{onclick: function(event) {
        	 if(!redo()) sendNotification("Nothing to redo!");
            }
          }
        );
        if (apache_server) {
          addPinnedModifyMenuIcon(isDraftSVG + "<span class='modify-menu-icon-label'>" + editor_model.version + "</span>",
            {title: editor_model.version == "Live" ? "Saved edits are live" : "Edits saved to draft named '"+editor_model.version+"'",
             "class": "inert"},
            {onclick: function(event) {
              
              editor_model.isDraftSwitcherVisible = !editor_model.isDraftSwitcherVisible;
              set_state_visible();
              toggle_draftview_state();
              if (!document.querySelector("#modify-menu").classList.contains("visible")) {
                document.querySelector("#modify-menu").classList.toggle("visible");
                editor_model.isDraftSwitcherVisible = true;
                set_state_draftview();
              }
              editor_model.visible = true;
              updateInteractionDiv();
            }});
        }
        addPinnedModifyMenuIcon(saveSVG + "<span class='modify-menu-icon-label'>Save</span>",
        {title: editor_model.disambiguationMenu ? "Accept proposed solution" : "Save", "class": "saveButton" + (editor_model.canSave || editor_model.disambiguationMenu ? "" : " disabled") + (editor_model.isSaving ? " to-be-selected" : ""),
          id: "savebutton"  
        },
          {onclick: editor_model.disambiguationMenu ? 
            ((ambiguityKey, selected) => () => acceptAmbiguity(ambiguityKey, selected))(
              editor_model.disambiguationMenu.ambiguityKey, editor_model.disambiguationMenu.selected)
            : function(event) {
              if (editor_model.isSaving) {
                sendNotification("Can't save while save is being undertaken");
              }else {
                if(!this.classList.contains("disabled")) {
                  if (apache_server) {
                    sendModificationsToServer();
                  } else {
                    sendModificationsToServerNode();
                  }
                }
              }
            }
          }
        )
      }
      else {
        addPinnedModifyMenuIcon(escapeSVG + "<span class='modify-menu-icon-label-link'>Cancel</span>", 
          {"class": "link-select-button", title: "Go back to original screen",
            id: "escapebutton"
          },
          {onclick: function(event) {
              escapeLinkMode();
            }
          }
        );
        addPinnedModifyMenuIcon(checkSVG + "<span class='modify-menu-icon-label-link'>Select</span>", 
          {"class": "link-select-button", title: editor_model.linkSelectMsg || "Select target",
            id: "selectbutton"
          },
          {onclick: function(event) {
              editor_model.linkSelectCallback(editor_model.clickedElem);
              escapeLinkMode();
            }
          }
        );
        if(editor_model.linkSelectOtherMenus) {
          editor_model.linkSelectOtherMenus(addPinnedModifyMenuIcon)
        }
      }
      //if(model.advanced || model.disambiguationMenu) { //change here
      if(editor_model.state.includes("a") && editor_model.state.includes("v")) {
        modifyMenuDiv.append(
          el("a", { class:"troubleshooter", href: "https://github.com/MikaelMayer/Editor/issues"}, "Help"));
        modifyMenuIconsDiv.append(
          el("span", { class:'filename', title:"the path of the file you are currently viewing"}, 
            editor_model.path ? editor_model.path : "[root folder]"));
        addModifyMenuIcon(sourceSVG,
          {"class": "tagName" + (model.displaySource ? " selected" : ""), title: model.displaySource ? "Hide source" : "Show Source"},
            {onclick: function(event) { editor_model.displaySource = !editor_model.displaySource; updateInteractionDiv(); } }
        );
        //when we click reload, it will save the current scroll position as the one it was at the beginning of the run
        addModifyMenuIcon(reloadSVG,
          {"class": "tagName", title: "Reload the current page"},
            {onclick: function(event) { editor_model.curScrollPos = (editor_model.displaySource ? document.getElementById("sourcecontentmodifier").scrollTop : 0);
              reloadPage(); } }
        );
        addModifyMenuIcon(folderSVG,
          {"class": "tagName", title: "List files in current directory"},
            {onclick: function(event) {
              let u =  new URL(location.href);
              u.pathname = u.pathname.replace(/[^\/]*$/, "");
              u.searchParams.set("ls", "true");
              navigateLocal(u.href);
            }
          }
        );
        const flog = () => el("textarea", {id:"fullLog", class:"textarea logger", visibility:false, readonly:true, isghost:true}, [], {});
        function toggleEditorLog() {
          //function el(tag, attributes, children, properties) 
          let log = document.getElementById("fullLog");
          if (!log) {
            log = flog();
            log.style.zIndex = 100;
            log.style.display = 'none';
            modifyMenuDiv.append(log);
          }
          let logtxt = "";
          const elog = editor_model.editor_log;
          for (let i = 0; i < elog.length; i++) {
            const l = elog[i];
            logtxt = logtxt + l + "\n";
          }
          logtxt == "" ? log.value = "(no log)" : log.value = logtxt;
          if (log.style.display == 'block') {
            editor_model.show_log = false;
            off_state_log();
            log.style.visibility = false;
            log.style.display = 'none';
          } else {
            editor_model.show_log = true;
            set_state_log();
            log.style.visibility = true;
            log.style.display = 'block';
          }
        }
        addModifyMenuIcon(logSVG,
          {"class": "tagName", title: "Display the full log"},
            {onclick: function(e) {
              toggleEditorLog();
            }});
        
        //if (editor_model.show_log) {
        if (editor_model.state.includes("s")) {
          let log = document.getElementById("fullLog");
          if (!log) {
            log = flog();
            modifyMenuDiv.append(log);
          }
          let logtxt = "";
          const elog = editor_model.editor_log;
          for (let i = 0; i < elog.length; i++) {
            const l = elog[i];
            logtxt = logtxt + l + "\n";
          }
          logtxt == "" ? log.value = "(no log)" : log.value = logtxt;
          editor_model.show_log = true;
          set_state_log();
          log.style.visibility = true;
          log.style.display = 'block';
        }
  
        if(editor_model.disambiguationMenu) {
          interactionDiv.append(editor_model.disambiguationMenu);
        	interactionDiv.append(el("button.modifyMenuButton#cancelAmbiguity", 
        		{title: "Revert to the original version"}, "Cancel",
        		{onclick: function(event) {cancelAmbiguity(editor_model.disambiguationMenu.ambiguityKey, editor_model.disambiguationMenu.selected)}}));
        }
        if(editor_model.feedback) {
          interactionDiv.append(editor_model.feedback);
        }
        if(model.displaySource) {
          let source = document.querySelector("#modify-menu").getAttribute("sourcecontent");
          interactionDiv.append(el("div", {"class": "tagName"},
             [el("textarea",
                  {style: "width:100%; height: 100%",
                   id: "sourcecontentmodifier", placeholder: "Source of the page, before evaluation", "class": "templateengine"}, [], {
                onkeyup: function() {
                  if(document.querySelector("#modify-menu").getAttribute('sourcecontent') !== this.value)
                    document.querySelector("#modify-menu").setAttribute('sourcecontent', this.value);
                  },
                value: source
               })])); 
          let sourceEdit = document.getElementById("sourcecontentmodifier");
          sourceEdit.scrollTop = editor_model.curScrollPos;
        }
        modifyMenuDiv.append(
          el("label", {class:"switch", title: "If off, ambiguities are resolved automatically. Does not apply for HTML pages"},
            [el("input", {class: "global-setting", id: "input-question", type: "checkbox"}, [], {
              onchange: function() { editor_model.askQuestions = this.checked; },
              checked: editor_model.askQuestions}),
             el("span", {class:"slider round"})]));
        modifyMenuDiv.append(
          el("label", {"for": "input-question", class: "label-checkbox"}, "Ask questions"));
        modifyMenuDiv.append(el("br"));
        modifyMenuDiv.append(
          el("label", {class:"switch", title: "If on, changes are automatically propagated 1 second after the last edit"}, [
            el("input", {class: "global-setting", id: "input-autosave", type:"checkbox"}, [], {
              onchange: function() { editor_model.autosave = this.checked; },
            checked: editor_model.autosave}),
             el("span", {class:"slider round"})])
        );
        modifyMenuDiv.append(
          el("label", {"for": "input-autosave", class: "label-checkbox"}, "Auto-save"));
        if(apache_server) {
          modifyMenuDiv.append(
            el("a", {href:"javascript:0", id:"thaditor-sign-out-button", style:"display:block"}, "Sign out of Google", {
              onclick() {
                let onOk = () => thaditor_sign_out(() => {
                  modifyMenuDiv.append(
                    el("a", {href:"javascript:0", id:"thaditor-google-log-in-button", style:"display:block"}, "Sign in with Google",
                    {onclick: thaditor_sign_in()}));
                  document.querySelector("#thaditor-sign-out-button").remove();
                });
                if(!gapi.auth2) {
                  thaditor_gapi_onload(onOk);
                } else {
                  onOk();
                }
              }})
          );
        }
      //} else if(model.insertElement)  {
      } else if (editor_model.state.includes("i")) {
        //insert?
        interactionDiv.classList.add("insert-information-style");
        interactionDiv.classList.add("information-style");
        interactionDiv.append(el("h1", {}, "Insert"));
        let insertOption = function(value, msg, checked, title) {
          return el("span", {class: "insertOption"}, [
            el("input", {type: "radio", id: "radioInsert" + value, name: "insertionPlace", value: value}, [], {checked: checked || false}),
            el("label", {"for": "radioInsert" + value, title: title}, msg)], {onclick: restoreCaretPosition});
        }
        let t = clickedElem.tagName;
        let isHTML = t === "HTML";
        let isTop = isHTML || t === "BODY" || t === "HEAD";
        let caretBlinks = model.caretPosition;
        interactionDiv.append(el("div", {id: "insertionPlace"}, [
          isTop ? undefined : insertOption("before", "Before node"),
          isHTML ? undefined : insertOption("first-child", "As first child"),
          isHTML || !caretBlinks ? undefined : insertOption("caret", "At caret", !isTop && caretBlinks),
          isHTML ? undefined : insertOption("last-child", "As last child", isTop || !caretBlinks),
          isTop ? undefined : insertOption("after", "After node"),
          isTop ? undefined : insertOption("wrap", "Wrap node", false, "Put the selected node inside the newly inserted node")
        ]));
        let getInsertionPlace = () => {
          let radios = document.querySelectorAll('#insertionPlace input[name=insertionPlace]');
          let value = "after";
          for (let i = 0, length = radios.length; i < length; i++) {
            if (radios[i].checked) return radios[i].getAttribute("value");
            value = radios[i].getAttribute("value");
          }
          return value;
        };
        let insertTag = function(event, newElement, insertionStyle) {
          newElement = newElement || (() => {
            let parent = this;
            while(parent && !parent.classList.contains("tagName")) parent = parent.parentElement;
            let m = parent.querySelector(".templateengine");
            if(typeof m.innerHTMLCreate === "string") return m.innerHTMLCreate;
            return el(m.createParams.tag, m.createParams.attrs, m.createParams.children, m.createParams.props);
          })();
          if(insertionStyle === "after") {
            if(typeof newElement === "string") {
              clickedElem.insertAdjacentHTML("afterend", newElement);
              newElement = clickedElem.nextElementSibling;
            } else {
              clickedElem.parentElement.insertBefore(newElement, clickedElem.nextSibling);
            }
          } else if(insertionStyle === "before") {
            if(typeof newElement === "string") {
              clickedElem.insertAdjacentHTML("beforebegin", newElement);
              newElement = clickedElem.previousElementSibling;
            } else {
              clickedElem.parentElement.insertBefore(newElement, clickedElem);
            }
          } else if(insertionStyle === "wrap") {
            if(typeof newElement === "string") {
              clickedElem.insertAdjacentHTML("beforebegin", newElement);
              newElement = clickedElem.previousElementSibling;
            } else {
              clickedElem.parentElement.insertBefore(newElement, clickedElem);
            }
            newElement.appendChild(clickedElem);
            console.log("newElement's parent HTML", newElement.parentElement.outerHTML);
          } else if(insertionStyle === "caret") {
            let s = model.caretPosition;
            let txt = s.startContainer;
            if(txt.textContent.length > s.startOffset && s.startOffset > 0) { // split
              // Need to split the text node.
              txt.parentElement.insertBefore(document.createTextNode(txt.textContent.substring(s.startOffset)), txt.nextSibling);
              txt.textContent = txt.textContent.substring(0, s.startOffset);
            }
            if(typeof newElement === "string") {
              let tmpSpan = el("span");
              clickedElem.insertBefore(tmpSpan, txt.nextSibling)
              tmpSpan.insertAdjacentHTML("afterend", newElement);
              newElement = tmpSpan.nextElementSibling;
              tmpSpan.remove();
            } else {
              clickedElem.insertBefore(newElement, txt.nextSibling)
            }
          } else if(insertionStyle === "last-child") { // Insert at the end of the selected element, inside.
            if(typeof newElement === "string") {
              let tmpSpan = el("span");
              clickedElem.insertBefore(tmpSpan, null);
              tmpSpan.insertAdjacentHTML("afterend", newElement); // afterend or beforeend same, tmpSpan to be removed.
              newElement = tmpSpan.nextElementSibling;
              tmpSpan.remove();
            } else {
              console.log("insert at the end");
              // Insert at the end.
              clickedElem.insertBefore(newElement, null);
            }
          } else if(insertionStyle === "first-child") { // Insert at the end of the selected element, inside.
            if(typeof newElement === "string") {
              let tmpSpan = el("span");
              clickedElem.insertBefore(tmpSpan, clickedElem.children[0]);
              tmpSpan.insertAdjacentHTML("afterend", newElement);// afterend or beforeend same, tmpSpan to be removed.
              newElement = tmpSpan.nextElementSibling;
              tmpSpan.remove();
            } else {
              console.log("insert at the beginning");
              // Insert at the beginning.
              clickedElem.prepend(newElement);
            }
          }
          editor_model.insertElement = false;
          off_state_insert();
          set_state_visible();
          editor_model.visible = true;
          editor_model.clickedElem  = typeof newElement !== "string" && typeof newElement !== "undefined" ?
            newElement : clickedElem;
          updateInteractionDiv();
        }
        let addElem = function(name, createParams) {
          interactionDiv.append(
            el("div", {"class": "tagName", title: createParams.title},
              el("span", { "class": "templateengine"}, name, {createParams: createParams}), {
                  onclick: function(event) {
                    let insertionStyle = getInsertionPlace();
                    insertTag.call(this, event, undefined, insertionStyle);
                }}
            )
          );
        }
        if(clickedElem.tagName === "HEAD") {
          addElem("Title", {tag:"title", children: "Page_title", title: "Insert <title>"});
          addElem("Meta", {tag:"meta", attrs:{name:"", content: ""}, props: {}, title: "Insert <meta>"});
          addElem("Link", {tag:"link", attrs:{rel:"", href: ""}, props: {}, title: "Insert <link>"});
        }
        if(clickedElem.tagName !== "HEAD") {
          interactionDiv.append(
            el("div", {"class":"modify-menu-icon", id: "selectExistingNodeToMove", title: "Select an existing node to move"}, [], {
                innerHTML: linkModeSVG,
                onclick: function(event) {
                  editor_model.insertElement = false;
                  off_state_insert();
                  let insertionStyle = getInsertionPlace();
                  activateNodeSelectionMode(
                    "to move",
                    node => insertTag.call(this, event, node, insertionStyle),
                    addPinnedModifyMenuIcon => {
                      addPinnedModifyMenuIcon(cloneSVG + "<span class='modify-menu-icon-label-link'>Clone</span>", 
                        {"class": "link-select-button", title: "Confirm to clone",
                          id: "selectbutton"
                        },
                        {onclick: function(event) {
                          let node = editor_model.clickedElem;
                          let clonedNode = editor.duplicate(node, {ignoreText: true});
                          insertTag.call(this, event, clonedNode, insertionStyle);
                          escapeLinkMode();
                          editor_model.clickedElem = clonedNode;
                          }
                        }
                      );
                    }
                  )
                }
              })
          )
          interactionDiv.append(el("input", {"type": "file", multiple: "", value: "Images or files..."}, [], {
            onchange: function(evt) { uploadFilesAtCursor(evt.target.files); }})
          );
          // TODO: Filter and sort which one we can add, also depending on where to insert.
          console.log("got here!");
          addElem("List item", {tag:"li", props: { innerHTML: "<br>" }, title: "Insert <li>"});
          addElem("Bulleted list", {tag:"ul", props: { innerHTML: "<ul>\n<li><br></li>\n</ul>" }, title: "Insert <ul>"});
          addElem("Numbered list", {tag:"ol", props: { innerHTML: "<ol>\n<li><br></li>\n</ol>" }, title: "Insert <ol>"});
          addElem("Button", {tag: "button", props: {innerHTML: "Name_your_button" }, title: "Insert <button>"});
          // something is wrong with creating link and paragraph using childCreate
          // addElem("Link", {tag:"a", childCreate: "Name_your_link"});
          // addElem("Paragraph", {tag:"p", childCreate: "Inserted paragraph"});
          addElem("Link", {tag: "a", props: { innerHTML: "Name_your_link", href: "" }, title: "Insert <a href=''>"});
          addElem("Paragraph", {tag: "p", props: { innerHTML: "Insert_paragraph" }, title: "Insert <p>"});
          addElem("Division content", {tag: "div", title: "Insert <div>"});
          addElem("Preformatted text", {tag: "pre", title: "Insert <pre>"});
          for(let i = 1; i <= 6; i++) {
            addElem("Header " + i, {tag:"h" + i, props: { innerHTML: "Title" + i }, title: "Insert <h"+i+">"});
          }
          addElem("Newline", {tag: "br", title: "Insert <br>"});
        }
        addElem("Stylesheet", {tag:"style", children: "/*Your CSS there*/", title: "Insert <style>"});
        addElem("JavaScript", {tag:"script", children: "/*Your CSS below*/", title: "Insert <script>"});

        
        interactionDiv.append(
          el("div", {"class": "tagName", id: "customHTML"}, [
            el("textarea", {id: "customHTMLToInsert", placeholder: "Custom HTML here...", "class": "templateengine", onkeyup: "this.innerHTMLCreate = this.value"}),
            el("div", {"class":"modify-menu-icon", title: "Insert HTML", style: "display: inline-block"}, [], {
                innerHTML: plusSVG, 
                onclick: insertTag
              }
            )
          ])
        );
        document.querySelector("#modify-menu").classList.toggle("visible", true);
      //} else if (editor_model.isDraftSwitcherVisible) {
      } else if (editor_model.state.includes("v") && editor_model.state.includes("d")) {
        //Now we want to open some sort of draft-picker/creater UI inside the modify-menu
        if (!apache_server) {
          throw "Should not have been able to enter into drafting mode when apache_server == false.";
        }
        const createNewDraft = () => {
          return el("div", {"class": "childrenSelector"},
                    [
                      el("div", {"class": "childrenSelectorName"}, "Create new draft off of " + editor_model.version, {}),
                    ], 
                    {
                      onclick: (event) => {
                        /*
                          Alright, for now, I'm going to launch the creation of versioning here.
                          First, check to see if the versions folder exists. If it does, exit. 
                          We don't want to overwrite anything.
                        */
                        const draft_name = window.prompt ("Please provide the name for the new draft. Leave blank to cancel");
                        if (!draft_name) {
                          return;
                        }
                        const verzExist = JSON.parse(doReadServer("isdir", "Thaditor/versions"));
                        let fail = false;
                        if (!verzExist) {
                          console.log ("making versions folder");
                          doWriteServer("mkdir", "Thaditor/versions");
                          console.log ("made versions folder?");
                        } else 
                        {
                          //we need to make sure we're not overwriting anothe draft
                          let versionsList = JSON.parse(doReadServer("fullListDir", "Thaditor/versions/"));
                          versionsList.forEach(val => {
                            let [nm, isdir] = val;
                            if (isdir) {
                              if (nm == draft_name) {
                                window.alert("Can't overwrite an existing draft!");
                                fail = true;
                              }
                            }
                          });
                        }
                        if (fail) return;

                        doWriteServer("mkdir", "Thaditor/versions/" + draft_name);
                        const t_pth = editor_model.path.slice(0, editor_model.path.lastIndexOf("/"));
                        const f_pth = (isLive() ? "" : editor_model.path.slice(0, editor_model.path.lastIndexOf("/")+1));
                        const success = copy_website(f_pth, "Thaditor/versions/" + draft_name + "/");
                        //change our URL to the versions/draft/
                        editor_model.version = draft_name;
                        navigateLocal("/Thaditor/versions/" + draft_name + "/?edit");
                        setTimeout( () => {
                          sendNotification("Successfully created + switched to draft: " + draft_name);
                        }, 1500);
                      }
                    }
                  );
        };
        const btnGetter = (name) => {
          return el("div", {"class": "childrenSelector"},
                    [
                      el("div", {"class": "childrenSelectorName"}, name, {}),
                    ], 
                    {
                      onclick: (event) => {
                        navigateLocal("/Thaditor/versions/" + name + "/?edit");
                        updateInteractionDiv();
                        setTimeout( () => {
                          sendNotification("Successfully switched to draft: " + name);
                        }, 1500);
                        
                      }
                    }
                  );
        };

        const liveBtn = () => {
          return el("div", {"class": "childrenSelector"},
                    [
                      el("div", {"class": "childrenSelectorName"}, "Live", {}),
                    ], 
                    {
                      onclick: (event) => {
                        navigateLocal("/?edit");
                        updateInteractionDiv();
                        setTimeout( () => {
                          sendNotification("Switched to live");
                        }, 1500);
                      }
                    });
        };
        
        const publishToLiveBtn = () => {
          return el("div", {"class": "childrenSelector"},
                    [
                      el("div", {"class": "childrenSelectorName"}, "Publish " + editor_model.version + " to live!", {}),
                    ], 
                    {
                      onclick: (event) => {
                        publishToLive();
                      }
                    });
        };

        const deleteCurrentDraftBtn = () => {
          return el("div", {"class": "childrenSelector"},
                    [
                      el("div", {"class": "childrenSelectorName"}, "Irreversibly delete " + editor_model.version + "!", {}),
                    ], 
                    {
                      onclick: (event) => {
                        deleteCurrentDraft();
                      }
                    });
        };

        let draftListDiv = el("div", {"class":".childrenElem"}, [], {});
        if (JSON.parse(doReadServer("isdir", "Thaditor/versions/"))) {
          const vers = JSON.parse(doReadServer("listdir", "Thaditor/versions/"));
          vers.forEach(ver => {
            draftListDiv.append(btnGetter(ver));
          });
        }
        if (!isLive()) draftListDiv.append(liveBtn());
        if (!isLive()) draftListDiv.append(publishToLiveBtn());
        draftListDiv.append(createNewDraft());
        if (!isLive()) draftListDiv.append(deleteCurrentDraftBtn());
        modifyMenuDiv.append(draftListDiv);
      } else {
      if(clickedElem) {
        interactionDiv.classList.add("information-style");
        /*
          Build the DOM node selector:
          Two parts:
          |-----------------------|
          |      main element     |
          |-----------------------|
          |  children / siblings  |
          |-----------------------|
          Two status:
          editor_model.displayClickedElemAsMainElem = true
          Status 1 (default). Show current clicked element as main element on the top:
          |-----------------------|
          |    clicked element*   |
          |-----------------------|
          |   children elements   |
          |-----------------------|
          Status 2. Show current clicked element's parent element as main element on the top:
          |-----------------------------------------------------|
          |                  parent element                     |
          |-----------------------------------------------------|  
          | previous sibling | clicked element* | next sibling  |
          |-----------------------------------------------------|
          All the HTML elements (except empty parts) in the selector can be clicked.
          Check parent: When the clicked element in status 1 is clicked in selector, the selector switches to status 2 (display its parent as main element in first level).
          Check parent: When the parent element in status 2 is clicked in selector, the selector remains status 2 while the parent element becomes current clicked element.
          
          Check children: When the children element in status 1 is clicked in selector, it will become clicked element in status 1.
          Check children: When the clicked elements in status 2 is clicked in selector, the selector switches to status 1 (only if it has children).
          
          Check siblings: When the siblings in status 2 is clicked in selector, the sibling will become 'clicked element' but the selector won't switch status 1. 
          The siblings will be in the middle of second level of status 2. This is because we want user switching siblings in second level easily.
          When other elements in selector are clicked, change 'clicked element' to it. And it also follow rules above.
          Bonus feature (Memoization):
            When user select DOM nodes along DOM tree from bottom to top continuously, the selector will remember its traverse path.
            When user tries to traverse back through DOM tree from top to bottom, the selector will show previous visited children element with its parent element.
            img -> div -> body
            body -> div -> img
            The main point is to decide which child should be displayed in the middle of second part (children element part) of selector in status 1.
            Implementation:
              editor_model.previousVisitedElem = [], as a stack storing DOM node path
              
              When user tries to visit siblings, clear the stack because previous path is broken, should restart memorizing
              When user tries to visit parent element:
                if the stack is empty, store current element and the parent element into stack;
                if the stack contains sth.: if the top element of stack is the child of the parent element, then stores the parent element, otherwise clear the stack 
              
              When user tries to visit children element (traverse back):
              if the stack is empty, show first 3 children of clicked element
                if current clicked element is the top element of stack, pop the top element then show the top element of stack as middle children
                otherwise clear the stack, then follow first rule
        */
        domSelector.classList.add("dom-selector-style");
        domSelector.append(
          el("div", {"class": "mainElem"}, []),
          el("div", {"class": "childrenElem"}, [])
        );
        let displayMainElem = function(elem) {
          let mainElemDiv = document.querySelector(".dom-selector > .mainElem");
          mainElemDiv.append(
            el("div", {"class":"mainElemName", "type":"text", value: elem.tagName.toLowerCase()}, "<" + elem.tagName.toLowerCase() + ">", {
              onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(elem),
              onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(elem)
            }),
            el("div", {"class": "mainElemInfo"}, textPreview(elem, 50))
          );
        }
        let displayChildrenElem = function(elem) {
          let childrenElemDiv = document.querySelector(".dom-selector > .childrenElem");
          childrenElemDiv.append(
            el("div", {
                 "class": "childrenSelector" + (elem.matches(".editor-interface") ? " editor-interface-dom-selector" : "") +
                   (isGhostNode(elem) ? " editor-recorded-ghost-node" : ""),
                 title: elem.matches(".editor-interface") ? "This is part of Editor" : (isGhostNode(elem) ? "(temporary) " : "") + textPreview(elem, 20)
                 },
              [
                el("div", {"class": "childrenSelectorName"}, "<" + elem.tagName.toLowerCase() + ">", {}),
                // el("div", {"class": "childrenSelectorInfo"}, textPreview(elem, 20))
              ], 
              {
                onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(elem),
                onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(elem)
              }
            )
          );
        }
        // show attributes of element on the dom selector
        let displayElemAttr = function(targetDiv, elem) {
          for (let i = 0; elem && elem.attributes && i < elem.attributes.length; i++) {
            let name = elem.attributes[i].name;
            let value = elem.attributes[i].value;
            if (name === "ghost-clicked" || name === "ghost-hovered") continue;
            targetDiv.append(
              el("div", { "class": "elementAttr" },
                [
                  el("span", { title: "This element has attribute name '" + name + "'" }, name + ": "),
                  el("span", { title: "This element has attribute value '" + value + "'" }, value)
                ]
              )
            );
          }
        }
        // display children and siblings in the second part of selector
        let displayChildrenSiblings = function(middleChild, selectMiddleChild) {
          // display clicked element's previous sibling, clicked element, clicked element's next sibling
          let cnt = 0;
          // display previous sibling
          if (middleChild.previousElementSibling && 
              (middleChild.previousElementSibling.id !== "context-menu" || middleChild.previousElementSibling.id !== "modify-menu" || middleChild.previousElementSibling.id !== "editbox")) {
            displayChildrenElem(middleChild.previousElementSibling);
            document.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
              let c = middleChild.previousElementSibling;
              if ((c.tagName && c.tagName === "HTML") || !c.tagName) {
                return;
              }
              // still in status 2, but clicked element change to previous sibling
              editor_model.displayClickedElemAsMainElem = false;
              editor_model.previousVisitedElem = []; // clear the stack
              editor_model.clickedElem.removeAttribute("ghost-hovered");
              editor_model.clickedElem = c;
              editor_model.notextselection = true;
              updateInteractionDiv();
            }
          } else {
            let childrenElemDiv = document.querySelector(".dom-selector > .childrenElem");
            childrenElemDiv.append(
              el("div", {"class": "childrenSelector no-sibling"}, "no sibling")
            );
          }
          cnt++;
          // display certain child in the middle
          displayChildrenElem(middleChild);
          document.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
            let c = middleChild;
            if (!c.tagName) {
              return;
            }

            if (!c.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
               // still in status 2
              editor_model.displayClickedElemAsMainElem = false;
            } else {
              // switch to status 1
              editor_model.displayClickedElemAsMainElem = true;
            }
            editor_model.clickedElem.removeAttribute("ghost-hovered");
            editor_model.clickedElem = c;
            editor_model.notextselection = true;
            updateInteractionDiv();
          }
          if (selectMiddleChild) {
            document.querySelectorAll(".childrenElem > .childrenSelector")[cnt].classList.add("selectedDom");
          }
          cnt++;
          // display next sibling
          if (middleChild.nextElementSibling && 
             (middleChild.nextElementSibling.id !== "context-menu" || middleChild.nextElementSibling.id !== "modify-menu" || middleChild.nextElementSibling.id !== "editbox")) {
            displayChildrenElem(middleChild.nextElementSibling);
            document.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
              let c = middleChild.nextElementSibling;
              if ((c.tagName && c.tagName === "HTML") || !c.tagName) {
                return;
              }
              // still in status 2, but clicked element change to next sibling
              editor_model.displayClickedElemAsMainElem = false;
              editor_model.previousVisitedElem = []; // clear the stack
              editor_model.clickedElem.removeAttribute("ghost-hovered");
              editor_model.clickedElem = c;
              editor_model.notextselection = true;
              updateInteractionDiv();
            }
          } else {
            let childrenElemDiv = document.querySelector(".dom-selector > .childrenElem");
            childrenElemDiv.append(
              el("div", {"class": "childrenSelector no-sibling"}, "no sibling")
            );
          }
        }
        // editor itself should be invisible
        if (clickedElem.id !== "context-menu" || clickedElem.id !== "modify-menu" || clickedElem.id !== "editbox") {
          if (!clickedElem.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
            editor_model.displayClickedElemAsMainElem = false;
          }
          // status 1. display clicked element in main part
          if (editor_model.displayClickedElemAsMainElem) {
            let mainElemDiv = document.querySelector(".dom-selector > .mainElem");
            displayMainElem(clickedElem);
            domSelector.classList.add("selectedDom");
            mainElemDiv.onclick = function () {
              if (!clickedElem.tagName) {
                return;
              }
              // When the main element in selector is clicked, selector switch to status 2 so that user can see its parent element
              editor_model.displayClickedElemAsMainElem = false;
              editor_model.clickedElem.removeAttribute("ghost-hovered");
              editor_model.clickedElem = clickedElem;
              editor_model.notextselection = true;
              updateInteractionDiv();
            }
            displayElemAttr(mainElemDiv, clickedElem);
            // display children, if no previous selected child, display first 3 children elements in second part of selector
            if (editor_model.previousVisitedElem.length < 2 ||
                (editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1] != clickedElem)) {
              if (editor_model.previousVisitedElem.length !== 0) {
                editor_model.previousVisitedElem = [];
              }
              if (clickedElem.children.length > 0) {
                // only display first 3 children elements
                let childrenElem = clickedElem.children;
                for (let i = 0, cnt = 0; i < childrenElem.length && cnt < 3; ++i) {
                  // prevent displaying editor itself
                  if (cnt === 0 && (childrenElem[i].matches(".editor-interface") || isGhostNode(childrenElem[i]))) {
                    continue;
                  }
                  displayChildrenElem(childrenElem[i]);
                  document.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
                    let c = childrenElem[i];
                    if (!c.tagName) {
                      return;
                    }

                    if (!c.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
                      editor_model.displayClickedElemAsMainElem = false;
                    } else {
                      // still in status 1
                      editor_model.displayClickedElemAsMainElem = true;
                    }
                    editor_model.clickedElem.removeAttribute("ghost-hovered");
                    editor_model.clickedElem = c;
                    editor_model.notextselection = true;
                    updateInteractionDiv();
                  }
                  cnt++;
                }
              } else {
                // document.querySelector(".childrenElem").append(
                //     el("div", {"class": "no-children"}, "No Children")
                // );
              }
            } else {
              editor_model.previousVisitedElem.pop();
              let middleChild = editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1];
              displayChildrenSiblings(middleChild, false);
            }
          } else {
            // status 2. display clicked element's parent element in main part
            let mainElemDiv = document.querySelector(".dom-selector > .mainElem");
            // <html> has no parent element
            if(clickedElem.parentElement) {
              displayMainElem(clickedElem.parentElement);
              mainElemDiv.onclick = function () {
                if (!clickedElem.parentElement.tagName) {
                  return;
                }
                // still in status 2 while current clicked element's parent element becomes clicked element so that user can see grandparent element
                editor_model.displayClickedElemAsMainElem = false;
                // memoization. when user click parent element:
                if (editor_model.previousVisitedElem.length === 0) {
                  editor_model.previousVisitedElem.push(clickedElem);
                  editor_model.previousVisitedElem.push(clickedElem.parentElement);
                } else {
                  if (editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1] == clickedElem) {
                    editor_model.previousVisitedElem.push(clickedElem.parentElement);   // continuous storing path
                  } else {
                    editor_model.previousVisitedElem = []; // clear the stack
                  }
                }
                editor_model.clickedElem.removeAttribute("ghost-hovered");
                editor_model.clickedElem = clickedElem.parentElement;
                editor_model.notextselection = true;
                updateInteractionDiv();
              }
              displayElemAttr(mainElemDiv, clickedElem.parentElement);
            } else {
              document.querySelector(".mainElem").append(
                  el("div", {"class": "no-parent"}, "No Parent")
              );
            } 
            displayChildrenSiblings(clickedElem, true);
          }

         
          
        }
      }
      
        // _______ .__   __.  _______  
        // |   ____||  \ |  | |       \ 
        // |  |__   |   \|  | |  .--.  |
        // |   __|  |  . `  | |  |  |  |  of DOM SELECTOR
        // |  |____ |  |\   | |  '--'  |
        // |_______||__| \__| |_______/ 

      var linkSelect = function() {
        activateNodeSelectionMode("to link to",
          (linkFrom => linkTo => {
            let targetID = linkTo.getAttribute("id");
            if(!targetID) {
              targetID = "ID" + editor_model.idNum
              linkTo.setAttribute("id", targetID);
              editor_model.idNum += 1;
            }
            else if(targetID.length > 100) {
              targetID = targetID.trim();
              linkTo.setAttribute("id", targetID);
            }
            linkFrom.setAttribute("href", "#" + targetID);
          })(editor_model.clickedElem)
        );
      } 
      /*
        Attribute box
      */
      let keyvalues = el("div", {"class":"keyvalues"});
      if (clickedElem) {
        // modify tagname
          keyvalues.append(
            el("div", {"class": "keyvalue"}, [
              el("span", {title: "This element has tag name '" + clickedElem.tagName.toLowerCase() + "'"}, "tag: "),
              el("span", {class:"attribute-key-value"}, [
                el("input", {"type": "text", value: clickedElem.tagName.toLowerCase(), "id": "newTagName"}, 
                  [], {
                    onkeyup() {
                       document.querySelector("#applyNewTagName").classList.toggle("visible", this.value !== this.getAttribute("value") && this.value.match(/^\w+$/));
                    }
                  }),
                  el("input", {"type": "button", id: "applyNewTagName", value: "Set", title: "Apply new tag name"}, [], {onclick() {
                        let newel = el(document.querySelector("#newTagName").value);
                        let elements = clickedElem.childNodes;
                        while(elements.length) {
                          newel.append(elements[0]);
                        }
                        for(let i = 0; i < clickedElem.attributes.length; i++) {
                          newel.setAttribute(clickedElem.attributes[i].name, clickedElem.attributes[i].value);
                        }
                        clickedElem.parentElement.insertBefore(newel, clickedElem);
                        clickedElem.remove();
                        editor_model.clickedElem = newel;
                        updateInteractionDiv();
                      }
                    }
                  ),
                  el("div", {id:"newtagname-align-placeholder"}, " ")
                ]
              )
            ])
          );
        let isGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(clickedElem);
        let isIgnoredAttributeKey = isIgnoredAttributeKeyFromNode(clickedElem);

        for(let i = 0; clickedElem.attributes && i < clickedElem.attributes.length; i++) {
          let name = clickedElem.attributes[i].name;
          if(name === "ghost-clicked" || name === "ghost-hovered") continue;
          let value = clickedElem.attributes[i].value;
          // Inline styles incoporated into CSS display editor
          if(name !== "style") {
            let isGhost = isGhostAttributeKey(name);
            let isIgnored = isIgnoredAttributeKey(name);
            let isHref = name === "href" && clickedElem.tagName === "A";
            keyvalues.append(
              el("div", {"class": "keyvalue" + (isGhost ? " editor-recorded-ghost-attribute" : "")
                                             + (isIgnored ? " editor-ignored-attribute" : ""),
                         "title": isGhost ? "Key/value generated by a script" : isIgnored ? "key/value ignored after being modified by a script" : undefined
              }, [
                el("span", {title: "Element attribute name"}, name + ": "),
                el("span", {class: "attribute-key-value", title: "Element attribute value of " + name}, [
                  el("input", {"type": "text", value: value, "id": ("dom-attr-" + name)}, [], {
                      onkeyup: ((name, isHref) => function () {
                          clickedElem.setAttribute(name, this.value);
                          if(isHref) {
                            let livelinks = document.querySelectorAll(".livelink");
                            for(let livelink of livelinks) {
                              let finalLink = livelink.matches("#context-menu *") ?
                                `javascript:navigateLocal(relativeToAbsolute('${linkToEdit(this.value)}'))` : this.value;
                              livelink.setAttribute("href", finalLink);
                              livelink.setAttribute("title", "Go to " + this.value);
                            }
                          }
                      })(name, isHref)
                    }),
                  isHref ? el("div", {title: "Go to " + model.link, "class": "modify-menu-icon inert"}, [], {
                    innerHTML: liveLinkSVG(model.link)
                  }) : undefined,
                  isHref ? el("div", {title: "Select a node on the page to refer to", "class": "modify-menu-icon inert"}, [], { 
                    innerHTML: linkModeSVG,
                    onclick: linkSelect
                  }) : undefined,
                  el("div", {"class":"modify-menu-icon", title: "Delete attribute '" + name + "'"}, [], {
                    innerHTML: wasteBasketSVG,
                    onclick: ((name) => function() {
                      clickedElem.removeAttribute(name);
                      editor_model.clickedElem = clickedElem;
                      updateInteractionDiv();
                      })(name)
                    })
                  ]
                )
              ]
            ));
          }
        }
        let highlightsubmit = function() {
          let attrName = this.parentElement.parentElement.querySelector("[name=name]").value;
          this.parentElement.parentElement.querySelector("div.modify-menu-icon").disabled =
            attrName === "" || attrName.trim() !== attrName
        }

        if(clickedElem.nodeType === 1) {
          keyvalues.append(
            el("div", {"class": "keyvalue keyvalueadder"}, [
              el("span", {class: "attribute-key"}, el("input", {"type": "text", placeholder: "key", value: "", name: "name"}, [], {onkeyup: highlightsubmit})),
              el("span", {class: "attribute-key-value"}, [
                el("span", {}, el("input", {"type": "text", placeholder: "value", value: "", name: "value"}, [], {
                  onfocus: function() {
                    let keyInput = document.querySelector("div.keyvalueadder input[name=name]");
                    if(keyInput && keyInput.value != "") {
                      let name = document.querySelector("div.keyvalueadder input[name=name]").value;
                      clickedElem.setAttribute(
                        name,
                        document.querySelector("div.keyvalueadder input[name=value]").value
                      );
                      updateInteractionDiv();
                      let d=  document.querySelector("div.keyvalue input#dom-attr-" + name);
                      if(d) d.focus();
                    }
                  },
                  onkeyup: highlightsubmit})),
                el("div", {"class":"modify-menu-icon", title: "Add this name/value attribute"}, [], {innerHTML: plusSVG,
                  disabled: true,
                  onclick() {
                    clickedElem.setAttribute(
                      this.parentElement.querySelector("[name=name]").value,
                      this.parentElement.querySelector("[name=value]").value
                    );
                    updateInteractionDiv();
                  },
                  onkeyup: highlightsubmit })])
            ])
          );
        }
      }

      interactionDiv.append(keyvalues);

            ///////////////////////////////////////////////////////////////////////
      /*
        /\_/\
       ( o.o )
        > ^ <
      */
      //CSS parser


      function findText(parsed, startIndex, endIndex) {
        //console.log("Start index is:", startIndex);
        //console.log("End index is:", endIndex);
        var textSegment = "";
        //console.log("got to findtext");
        //console.log("startIndex is:" + startIndex + " endIndex is:" + endIndex);
        for(let i = startIndex; i < endIndex; i++) {
          //console.log(parsed[0].directive);
          //console.log(CSSparser.unparseRules([parsed[i]]));
          textSegment += parsed ? parsed[0].selector ? CSSparser.unparseCSS([parsed[i]]) :
            (parsed[0].directive ? CSSparser.unparseRules([parsed[i]]) : "") : "";
          //console.log(textSegment);
        }
        return textSegment;
      }      
      if(clickedElem && clickedElem.id !== "context-menu" && clickedElem.id !== "modify-menu" && clickedElem.id !== "editbox" &&
        !model.insertElement) {
        console.log("here for now!");
        //console.log("All style tags:", document.querySelectorAll("style"));
        //parse relevant CSS, recording prior and post CSS text as well 
        function fullParseCSS() {
          var fullCSS = [], keyframes = [], rawCSS = [];
          console.log("All style tags:", document.querySelectorAll("style"));
          document.querySelectorAll("link").forEach((e) => {
            if(e.getAttribute("type") === "text/css" && e.getAttribute("href")) {
              let CSSFilePath = relativeToAbsolute(e.getAttribute("href"));
              let CSSvalue = doReadServer("read", CSSFilePath);
              console.log(CSSFilePath.match(/server-elm-style/g));
              if(!(CSSFilePath.match(/server-elm-style/g)) && CSSvalue) {
                console.log("Found string is:" + CSSvalue);
                CSSvalue = CSSvalue.slice(1);
                rawCSS.push({text: CSSvalue, tag: e});
              }
            }
          });
          document.querySelectorAll("style").forEach((e) => {
            rawCSS.push({text: e.textContent, tag: e});
          });
          for(let z in rawCSS) {
            var parsedCSS = CSSparser.parseCSS(rawCSS[z].text);
            //console.log("The parsed CSS is:", parsedCSS);
            for(let i in parsedCSS) {
              if(parsedCSS[i].kind === 'cssBlock' && editor.matches(clickedElem, parsedCSS[i].selector)) {
                let content = CSSparser.unparseCSS([parsedCSS[i]]);
                let wsBefore = content.replace(/^(\s*)[\s\S]*$/g, (m, ws) => ws);
                let contentTrimmed = content.replace(/^\s*/,"");
                //calculating before and after text
                fullCSS.push({type: 'cssBlock', content: contentTrimmed, 
                  before: findText(parsedCSS, 0, i) + wsBefore, after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag});
              }
              else if(parsedCSS[i].kind === '@media' && window.matchMedia(parsedCSS[i].selector).matches) {
                //saving selector information 
                let curMedia = parsedCSS[i];
                for(let j in curMedia.content) {
                  if(editor.matches(clickedElem, curMedia.content[j].selector)) {
                    var insertMedia = {type: '@media', content: curMedia, 
                      innerBefore: findText(curMedia.content, 0, j), innerAfter: findText(curMedia.content, Number(j) + 1, curMedia.content.length),
                      before: findText(parsedCSS, 0, i), after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag};
                    curMedia.content = CSSparser.unparseCSS([curMedia.content[j]]);
                    insertMedia.content = curMedia.content;
                    fullCSS.push(insertMedia);
                  }
                }
              }
              else if(parsedCSS[i].kind === '@keyframes') {
                console.log(parsedCSS[i]);
                keyframes.push({type: 'keyframes', content: CSSparser.unparseCSS([parsedCSS[i]]), 
                  before: findText(parsedCSS, 0, i), after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag});
              }
              else if(parsedCSS[i].kind === 'whitespace') { 
                continue;
              }
              if(i === parsedCSS.length - 1 && !curCSS.length) {
                console.log("Nothing relevant in style tag: ", rawCSS[z].tag);
              }
            }
            //console.log("The parsed text looks like:", curCSS);
          }
          console.log(keyframes);
          for(i in keyframes) {
            let animationName = CSSparser.parseCSS(keyframes[i].content).atNameValue;
            for(j in fullCSS) {
              let parsedSection = CSSparser.parseCSS(fullCSS[j].content);
              for(k in parsedSection.content) {
                for(l in parsedSection.content[k].rules) {
                  if(Number(parsedSection.content[k].rules[l].search(animationName)) >= 0) {
                    fullCSS.push(keyframes[i]);
                  }
                }
              }
              for(k in parsedSection.rules) {
                if(Number(parsedSection.rules[k].search(animationName)) >= 0) {
                  fullCSS.push(keyframes[i]);
                }
              }
            }
          }
          console.log(fullCSS);
          return fullCSS;
        }
        
        function fullUnparseCSS(curCSS) {
          //console.log("Before unparse update:")
          //document.querySelectorAll("style").forEach((e) => { 
            //console.log(CSSparser.parseCSS(e.textContent));
          //});
          let curTag = curCSS.orgTag;
          let CSSString = "";
          if(curCSS.type === 'cssBlock') {
            console.log(curCSS.content);
            CSSString = curCSS.before + curCSS.content + curCSS.after;
            console.log(CSSString);
          }
          else if(curCSS.type === '@media') { 
            let curMedia = CSSparser.parseCSS(curCSS.content);
            curMedia.content = curCSS.beforeInner + curCSS.content.content + curCSS.afterInner;
            CSSString = CSSparser.unparseCSS([curMedia]);        
          }
          console.log("Text is:" + CSSString);
          curTag.textContent = CSSString;
          //debugger
          //consolw.log("After");
          document.querySelectorAll("style").forEach((e) => { 
            //console.log(CSSparser.parseCSS(e.textContent));
          });
        }

        var CSSarea = el("div", {id: "CSS-modification", value: ""}, [], {}); 
        var curCSSWindow = undefined;
        //CSSState = fullParseCSS();
        function setCSSAreas() {
          while(CSSarea.firstChild) {
            //console.log("Removed child:", CSSarea.firstChild);
            CSSarea.removeChild(CSSarea.firstChild);
          }

          //if there is linked CSS text
          console.log("The type attribute of the clickedElem is:" + clickedElem.getAttribute("type"));
          if(clickedElem.tagName === "LINK" && clickedElem.getAttribute("type") === "text/css" && clickedElem.getAttribute("hreff")) {
            let CSSFilePath = relativeToAbsolute(clickedElem.getAttribute("href"));
            let CSSvalue = doReadServer("read", CSSFilePath).slice(1);
            console.log(CSSvalue);
            CSSarea.append(
              el("div", {"class": "CSS-modify-unit"}, [
                el("textarea", {"class": "linked-CSS"}, [], {
                  defaultValue: CSSvalue,
                  onfocusout() {
                    setCSSAreas();
                  },
                  oninput() {
                    let nextSibGhostCSS = clickedElem.nextSibling;
                    if(nextSibGhostCSS && (nextSibGhostCSS.getAttribute("class") === "ghost-CSS")) {
                      nextSibGhostCSS.innerHTML = this.value;
                    }
                    else {
                      clickedElem.parentElement.insertBefore(el("style", {"isghost": true, "class": "ghost-CSS"}, [], {
                          innerHTML: this.value
                        }), 
                        nextSibGhostCSS);
                    }
                  }
                }),
                el("div", {"class": "delete-CSS", "title": "Update the CSS"}, [], {
                  innerHTML: plusSVG,
                  onclick() {
                    let linked_CSS = document.querySelectorAll(".linked-CSS");
                    addFileToSave(CSSFilePath, CSSvalue, linked_CSS.value);
                    //setCSSAreas();
                  }
                })
              ])
            );
          }

          //inline styles 
          var inline = clickedElem.getAttribute("style"); //? CSSparser.parseCSS(clickedElement.getAttribute("style")) : undefined;
          if(inline) {
            //debugger;
            let inlineCSS = el("div", {"class": "CSS-modify-unit"}, [
              el("textarea", {"class": "inline-CSS"}, [], {
                defaultValue: inline,
                onfocusout() {
                  setCSSAreas();
                },
                oninput() {
                  clickedElem.setAttribute("style", this.value);
                }
              }),
              el("div", {"class": "delete-CSS"}, [], {
                innerHTML: wasteBasketSVG,
                onclick() {
                  let inline_CSS = document.querySelectorAll(".inline-CSS");
                  inline_CSS.value = "";
                  clickedElem.setAttribute("style", inline_CSS.value);
                  setCSSAreas();
                }
              })
            ]);
            CSSarea.append(el("span", {}, [], {innerHTML: "Inline styles:"}));
            CSSarea.append(inlineCSS);
            //debugger;
          }
          //rest of CSS
          editor_model.CSSState = fullParseCSS();
          console.log("CSS state is:", editor_model.CSSState);
          const count = (str) => {
            const re = /\n/g
            return ((str || '').match(re) || []).length
          }
          for(let i in editor_model.CSSState) {
            let cssState = editor_model.CSSState[i];
            let orgTag = cssState.orgTag;
            console.log("cssState", cssState);
            let headerStr = orgTag.tagName.toLowerCase() + (orgTag.tagName === "LINK" ? " (" + orgTag.getAttribute("href")+":" + (count(cssState.before) + 1) + ")" : "");
            for(let curElem = orgTag.parentElement; curElem; curElem = curElem.parentElement) {
              headerStr =  curElem.tagName.toLowerCase() + " > " + headerStr; 
            }
            CSSarea.append(el("span", {}, [], {innerHTML: headerStr}));
            let eachCSS = el("div", {"class": "CSS-modify-unit"}, [
              el("textarea", {"class": "CSS-selectors" }, [], {
                defaultValue: cssState.content,
                onfocusout() {
                  setCSSAreas();
                },
                oninput() {
                  if(this.orgTag.tagName != "LINK") {
                    let throwError = false;
                    curCSSState = CSSparser.parseCSS(this.value);
                    //console.log(curCSSState);
                    //check to make sure CSS is still relevant to clicked element.
                    for(let i in curCSSState) {
                      if(curCSSState[i].kind === 'cssBlock' || curCSSState[i].kind === '@media') {
                        if(!(curCSSState[i].kind === 'cssBlock' ? editor.matches(clickedElem, curCSSState[i].selector) : 
                        (window.matchMedia(curCSSState[i].selector).matches ? editor.matches(clickedElem, curCSSState[i].content.selector) : false))) {
                          throwError = true;
                        }
                      }
                    }
                    if(throwError) {
                      sendNotification("CSS selector does not match");
                      this.setAttribute("wrong-selector", true);
                      this.setAttribute("title", "The current CSS selector doesn't apply to the selected element!");
                    }
                    else {
                      this.setAttribute("wrong-selector", false);
                      this.removeAttribute("title");
                    }
                    //when a change is made, write first to stored 
                    //"semi-parsed" CSS (CSS that contains location information)
                    //then write to original style tag
                    this.storedCSS.content = this.value;
                    //console.log("Other selectors under the same style tag is:", editor_model.CSSState[i]);
                    fullUnparseCSS(this.storedCSS);
                    console.log("The updated CSS is now:", editor_model.CSSState);
                    //setCSSAreas();
                  }
                  else {
                    this.storedCSS.content = this.value;
                    let nextSibGhostCSS = this.storedCSS.orgTag.nextSibling;
                    if(nextSibGhostCSS && (nextSibGhostCSS.getAttribute("class") === "ghost-CSS")) {
                      nextSibGhostCSS.innerHTML = this.value;
                    }
                    else {
                      this.storedCSS.orgTag.parentElement.insertBefore(el("style", {"isghost": true, "class": "ghost-CSS"}, [], {
                          innerHTML: this.value
                        }), 
                        nextSibGhostCSS);
                    }
                  }
                },
                storedCSS: cssState
              }),
              orgTag.tagName === "LINK" ?
                el("div", {"class": "delete-CSS", "title": "Update the CSS once save is clicked"}, [], {
                  innerHTML: plusSVG,
                  onclick() {
                    let linked_CSS = document.querySelectorAll(".linked-CSS");
                    let CSSFilePath = relativeToAbsolute(this.parentElement.childNodes[0].storedCSS.orgTag.getAttribute("href"));
                    let CSSvalue = doReadServer("read", CSSFilePath);
                    addFileToSave(CSSFilePath, CSSvalue, linked_CSS.value);
                  }
                }) :
                el("div", {"class": "delete-CSS"}, [], {
                  innerHTML: wasteBasketSVG,
                  onclick() {
                    //console.log(this.parentElements.childNodes);
                    this.parentElement.childNodes[0].value = "";
                    this.parentElement.childNodes[0].storedCSS.content = this.parentElement.childNodes[0].value;
                    fullUnparseCSS(editor_model.CSSState);
                    setCSSAreas();
                  }
                })
            ]);
            CSSarea.append(eachCSS);
          }
        }
        setCSSAreas();   
        interactionDiv.append(CSSarea);
        /*interactionDiv.append(el("button", {"class": "CSSbutton", title: "Create new selector"}, [], {
          innerHTML: "New CSS",
          onclick() { 
            CSSarea.append(el("div", {"class": "CSS-modify-unit"}, [
              el("textarea", {"class": "CSS-selectors", }, [], {
                onkeyup() {
                  //not sure if this is necessary: once storedCSS is set, we will reset the whole operation.
                  if(this.storedCSS) {
                    this.storedCSS.content = this.value;
                    //console.log(editor_model.CSSState[i]);
                    fullUnparseCSS(editor_model.CSSState);
                    //setCSSAreas();
                  }
                  else {
                    let parsedNewCSS = CSSparser.parseCSS(this.value);
                    for(let i in parsedNewCSS) {
                      if(parsedNewCSS[i] && parsedNewCSS[i].kind != 'whitespace') {
                        if(parsedNewCSS[i].kind == 'cssBlock' ? editor.matches(clickedElem, parsedNewCSS[i].selector) :
                        (window.matchMedia(parsedNewCSS[i].selector).matches ? editor.matches(clickedElem, parsedNewCSS[i].content.selector) : false))
                        {
                          let addedStyles = document.getElementById("new-Style");
                          if(!addedStyles) { 
                            document.head.appendChild(el("style", {id: "new-Style"}, [], {innerHTML: this.value}));
                            //console.log("Added new style tag", addedStyles);
                          } 
                          else {
                            addedStyles.innerHTML += "\n" + this.value;
                            //console.log("Style tag exists:", addedStyles);
                          }
                          //console.log("Added new style tag", addedStyles);
                          //reset to normal case (i.e. reconstruct all text areas)
                          setCSSAreas();
                          return;
                          }
                      }
                      popupMessage("Unparsable atm!");
                    }
                  }
                },
                storedCSS: undefined
              })
            ]));
          }
        }));*/
      }


      //extract url and extraneous text from specified CSS value (which is originally part of a rule)
      function findURLS(styleStr) {
        var urls = [];
        var diffPics = styleStr.split(",");
        for(let k in diffPics) {
          //extracts only url(...)
          var matches = diffPics[k].match(/url\((.*?)\)/g);
          console.log("the matches are:", matches);
          //deepcopy string
          var remainStr = diffPics[k].slice(0); 
          for(let j in matches) {
            //from current understanding, there should only be one url(...) per split of ,
            console.log("the current match is:", matches[j]);
            if(j == 1) {
              console.log(`Odd syntax, ${matches[j]} also matched!`);
            }
            let sIndex = diffPics[k].indexOf(matches[j]);
            //extracting the rest of the string 
            afterStr = remainStr.slice(sIndex + matches[j].length);
            beforeStr = remainStr.slice(0, sIndex);
            urls.push({remainderBefore: beforeStr, url: matches[j], remainderAfter: afterStr});  
          }
        }
        return urls;
      }

      //checks the inline CSS of the clicked node/element to see if background or background-image is a rule, and if 
      //a link to an image is provided as part of the value for this rule;
      //TODO: expand the set of CSS being checked to any style tags as well.
      function checkForBackgroundImg() {
        //console.log("clicked element is:", clickedElem);
        //clickedElem ? console.log(clickedElem.getAttribute("style")) : console.log("nothing clicked");
        var clickedStyle = clickedElem ? CSSparser.parseRules(clickedElem.getAttribute("style")) : []; 
        //console.log(clickedStyle);
        //inefficient way of doing things, but since background takes precedence over background-image, we need to process the 
        //former first, if it contains a url. for now, I am looping through the CSS rules twice.
        //console.log("^parsed rules ");
        for(let i in clickedStyle) {
          for(let j in clickedStyle[i]) {
            if(clickedStyle[i][j].directive === "background") {
              clickedStyle[i][j].value = findURLS(clickedStyle[i][j].value);  
              if(clickedStyle[i][j].value.length) {
                //console.log(clickedStyle[i][j]);
                return {beforeCSS: findText(clickedStyle[i], 0, Number(j)), relCSS: clickedStyle[i][j], 
                  imageSelection: 0, afterCSS: findText(clickedStyle[i], Number(j) + 1, clickedStyle[i].length)};
              }
            }
          }
        }
        for(let i in clickedStyle) {
          for(let j in clickedStyle[i]) {
            if(clickedStyle[i][j].directive === "background-image") {
              //console.log("hello?");
              //console.log(clickedStyle[i][j].value);
              clickedStyle[i][j].value = findURLS(clickedStyle[i][j].value);  
              if(clickedStyle[i][j].value.length) {
                return {beforeCSS: findText(clickedStyle[i], 0, Number(j)), relCSS: clickedStyle[i][j], 
                  imageSelection: 0, afterCSS: findText(clickedStyle[i], Number(j) + 1, clickedStyle[i].length)};
              }
            }
          }
        } 
        //console.log("unsuccessful");
        return undefined;
      }

      //unparse the background/background-image object
      function unparseBackgroundImg(backImgObj) {
        var textSegment = "";
        let valueText = "";
        for(let i in backImgObj.relCSS.value) {
          valueText += (Number(i) !== 0 ? ", " : "") + backImgObj.relCSS.value[i].remainderBefore + backImgObj.relCSS.value[i].url + backImgObj.relCSS.value[i].remainderAfter;
          //console.log(valueText);
        }
        backImgObj.relCSS.value = valueText;
        //console.log("Object about to be unparsed:");
        //console.log(backImgObj);
        return backImgObj.beforeCSS + findText([backImgObj.relCSS], 0, 1) + backImgObj.afterCSS;
      }

      function uploadImagesAtCursor(files, srcName, backImgObj) {
        for (var i = 0, file; file = files[i]; i++) {
          var targetPathName =  editor.getStorageFolder(file) + file.name;
          editor.uploadFile(targetPathName, file, (targetPathName, file) => {
            if(backImgObj) {
              backImgObj.imageSelection = (() => {
                let radios = document.querySelectorAll(".background-img-radio");
                let defaultValue = 0;
                for (let i in radios) {
                  //hopefully there aren't more than 10 images!
                  if (radios[i].checked) defaultValue = Number(radios[i].getAttribute("value").match(/[0-9]/g));
                }
                return defaultValue;
              })();
              backImgObj.relCSS.value[backImgObj.imageSelection].url = 'url("'+ targetPathName +'")';
              clickedElem.setAttribute("style", unparseBackgroundImg(backImgObj));
            }
            else {
              document.getElementById("dom-attr-src").setAttribute("value", file.name);
              clickedElem.setAttribute("src", targetPathName);
            }
            // adapt to HTML5 new attribute 'srcset'
            // IF website use 'srcset', we force to set this attribute to null then replace image using 'src'
            if (clickedElem.getAttribute("srcset") != undefined) {
              clickedElem.setAttribute("srcset", "");
            }
          });
        }
        // refresh images list
        showListsImages(targetPathName);  // targetPathName is the last file of files array, but it seems that user can only upload one file once
        // automatically select upload image
        let selectedImage = document.querySelectorAll(".imgFolder > img");
        for (let i = 0; i < selectedImage.length; ++i) {
          let imgName = selectedImage[i].getAttribute("src").split("/").pop();
          if (imgName === files[files.length - 1].name) {
            selectedImage[i].parentElement.classList.add("highlight-select-image");
          } else {
            selectedImage[i].parentElement.classList.remove("highlight-select-image");
          }
        }
      }

      function showListsImages(srcName, backImgObj) {
        console.log("hello!");
        console.log("Source name is:", srcName);
        srcName = relativeToAbsolute(srcName)
        let dir = "";
        for(let i = 0, arr = srcName.split(/\\|\//); i < arr.length - 1; ++i) {
          dir += (arr[i] + "/");
        }
        files = editor.fs.listdir(dir);
        
        let images = [];
        let currentSelectedImage;
        files.forEach(file => {
          let ext = file.split('.').pop().toLowerCase();
          if (ext == 'jpeg' || ext == 'jpg' || ext == 'png' || ext == 'gif' || ext == 'svg' || ext == 'bmp') {
            if (file.split('/').pop() === srcName.split("/").pop().split("?")[0]) {   // note that srcName maybe "/1.jpg?raw=true"
              currentSelectedImage = file;
            } else {
              images.push(file);
            }
          }
        });
        // sometimes website use 'srcset' as the url of image, we cannot find currentSelectedImage precisely
        if (currentSelectedImage != null) {
          images.unshift(currentSelectedImage);   // currentSelectedImage should be placed as the first one
        }

        // init: clear image list
        let selectedImage = document.querySelectorAll(".imgFolder");
        selectedImage.forEach(e => e.remove());

        let imgDiv = el("div", { "id": "imgGallery" });
        if (!document.getElementById("imgGallery")) {
          interactionDiv.append(imgDiv);
        } else {
          imgDiv = document.getElementById("imgGallery");
        }

        for (let i = 0; i < images.length; ++i) {
          imgDiv.append(
            el("div", { "class": "imgFolder" }, el("img", { "src": dir + images[i], "title": images[i], "alt": images[i] },  [], {}), {
              onclick() {
                //console.log("At the beginning:");
                //console.log(JSON.stringify(backImgObj));
                // highlight the selected image
                let otherImages = document.querySelectorAll(".imgFolder");
                for (let i = 0; i < otherImages.length; ++i) {
                  otherImages[i].classList.remove("highlight-select-image");
                }
                // replace image
                if(backImgObj) {
                  backImgObj.imageSelection = (() => {
                    let radios = document.querySelectorAll(".background-img-radio");
                    let defaultValue = 0;
                    for (let i in radios) {
                      //hopefully there aren't more than 10 images!
                      if (radios[i].checked) defaultValue = Number(radios[i].getAttribute("value").match(/[0-9]/g));
                    }
                    return defaultValue;
                  })();
                  //console.log("Here?");
                  //console.log(JSON.stringify(backImgObj));
                  if(!(typeof backImgObj.relCSS.value === 'string')){
                    //console.log("Here?");
                    //console.log(JSON.stringify(backImgObj));
                    //console.log(backImgObj.relCSS.value.length);
                    backImgObj.relCSS.value[backImgObj.imageSelection].url = 'url('+ this.children[0].getAttribute("src") +')';
                  }
                  else {
                    console.log("Second time around:");
                    backImgObj = checkForBackgroundImg();
                    backImgObj.relCSS.value[backImgObj.imageSelection].url = 'url('+ this.children[0].getAttribute("src") +')';
                  }
                  //console.log("current link", this.children[0].getAttribute("src"));
                  //console.log("current section number is:", backImgObj.imageSelection);
                  //console.log("current selection is:", backImgObj.relCSS.value[backImgObj.imageSelection].url); 
                  clickedElem.setAttribute("style", unparseBackgroundImg(backImgObj));
                  //console.log("new style attribute is:", clickedElem.getAttribute("style"));

                  console.log(JSON.stringify(backImgObj));

                }
                // adapt to HTML5 new attribute 'srcset'
                // IF website use 'srcset', we force to set this attribute to null then make image replacemenet
                else if (clickedElem.getAttribute("srcset") != undefined) {
                  clickedElem.setAttribute("srcset", "");
                }
                else {
                  clickedElem.setAttribute("src", this.children[0].getAttribute("src"));
                  document.getElementById("dom-attr-src").setAttribute("value", this.children[0].getAttribute("src"));
                }
                // this.style.outline = "2px solid white";
                this.classList.add("highlight-select-image");
              }
            })
          );
        }
        if (currentSelectedImage != null) {
          document.querySelectorAll(".imgFolder")[0].classList.add("highlight-select-image");
        }
      }

      
      let backgroundImgSrc = checkForBackgroundImg();
      if (clickedElem && (clickedElem.tagName === "IMG" || backgroundImgSrc)) {
        //image replacement
        //console.log("got here!");
        //console.log(backgroundImgSrc.relCSS.value[0].url.match(/\((.*?)\)/g));
        //console.log(backgroundImgSrc.relCSS);
        var remParentheses = /\((.*?)\)/g;
        let srcName = backgroundImgSrc ? remParentheses.exec(backgroundImgSrc.relCSS.value[0].url)[1] : clickedElem.attributes[0].value;

        //console.log(srcName);
        //console.log(backgroundImgSrc.relCSS.value[0].url);
        clickedElem.ondragover = function (e) {
          e.preventDefault();
        }
        clickedElem.ondrop = function (e) {
          // upload and replace the image 
          e.stopPropagation();
          e.preventDefault();
          var files = e.dataTransfer.files; // FileList object
          if (files && files[0]) {
            uploadImagesAtCursor(files);
          }
        }

        // radio buttons for cases when there are two background images
        if(backgroundImgSrc && backgroundImgSrc.relCSS.value.length > 1) {
          for(let i in backgroundImgSrc.relCSS.value) {
            interactionDiv.append(el("span", {class: "insertOption"}, [
              el("input", {type: "radio", class: "background-img-radio", id: `radio${i}`, name: "", value: `Image {i}`}, [], {checked: Number(i) === 0}),
              el("label", {"for": "radio${i}"}, `Image {i}`)]),);
          }         
        }
        // upload image button
        interactionDiv.append(
          el("a", 
            { "id": "upload-image-btn-a" }, 
            el(
              "input", {"id": "upload-image-btn-input", "type": "file", value: "Please upload images..."}, 
              [], 
              { onchange: function(evt) { uploadImagesAtCursor(evt.target.files, srcName, backgroundImgSrc); }}
            ), 
            {}
          )
        );
        
        // show lists of images in selected image's folder
        showListsImages(srcName, backgroundImgSrc);
      }

      let voidTags = {AREA: true, BASE: true, BR: true, COL: true, COMMANd: true, EMBED: true, HR: true, IMG: true, INPUT: true, KEYGEN: true, LINK: true, META: true, PARAM: true, SOURCE: true, TRACK: true, WBR: true};
      //interactionDiv.append(el("hr"));
      // Nodes only with 1 text child
      if(clickedElem && clickedElem.children.length === 0 && !voidTags[clickedElem.tagname]) {
        //Text editing when one text child node
        // interactionDiv.append(el("hr"));
        let txt = el("textarea", {id:"singleChildNodeContent"},
          [], {
            value: clickedElem.innerText,
            onkeyup: function () { clickedElem.textContent = this.value; },
            onscroll: function() { editor_model.textareaScroll = this.scrollTop },
          });
        interactionDiv.append(txt);
        setTimeout((txt => () => {
          txt.scrollTop = editor_model.textareaScroll;
          txt.selectionEnd = editor_model.textareaSelectionEnd;
          txt.selectionStart = editor_model.textareaSelectionStart;
        })(txt), 0);
      }
      // let user modify button content
      if(clickedElem && (clickedElem.tagName === "BUTTON")) {
        //not working yet
        // provide the onclick attribute to users so user can modify it in modify-menu
        if (!clickedElem.hasAttribute("onclick")) {
          clickedElem.setAttribute("onclick", "");
          updateInteractionDiv();
        }
        if (!clickedElem.hasAttribute("type")) {
          clickedElem.setAttribute("type", "");
          updateInteractionDiv();
        }
      }
    }
      if (!editor_model.state.includes("l")) {
      //if(!editor_model.linkSelectMode) {
        contextMenu.innerHTML = "";
        var whereToAddContextButtons = contextMenu;
        var noContextMenu = false;
        // What to put in context menu?
        if(onMobile() || (editor_model.clickedElem && editor_model.clickedElem.matches("html, head, head *, body"))) {
          whereToAddContextButtons = modifyMenuIconsDiv;
          noContextMenu = true;
        }
        let numButtons = 0;
        let addContextMenuButton = function(innerHTML, attributes, properties) {
          let button = el("div", attributes, [], properties);
          button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
          button.classList.add("context-menu-button");
          button.innerHTML = innerHTML;
          whereToAddContextButtons.append(button);
          numButtons++;
        }
        if(model.link) {
          addContextMenuButton(liveLinkSVG(linkToEdit(model.link)),
            {title: "Go to " + model.link, "class": "inert"});
        }
        var computedStyle = clickedElem && window.getComputedStyle(clickedElem);
        var isDisplayInline = computedStyle && computedStyle.display.startsWith("inline");
        if(!model.selectionRange && clickedElem && clickedElem.matches && !clickedElem.matches(".editor-interface") && clickedElem.previousElementSibling && !clickedElem.previousElementSibling.matches(".editor-interface") && reorderCompatible(clickedElem.previousElementSibling, clickedElem)) {
          addContextMenuButton(isDisplayInline ? arrowLeft : arrowUp,
          {title: "Move selected element " + (isDisplayInline ? "to the left" : "up")},
          {onclick: (c => event => {
              let wsTxtNode = c.previousSibling && c.previousSibling.nodeType == 3 &&
                c.previousSibling.textContent.trim() === "" ? c.previousSibling : undefined;
              // There is whitespace before this element, we try to reinsert
              c.parentElement.insertBefore(c, c.previousElementSibling);
              if(wsTxtNode) { // We move the whitespace as well.
                c.parentElement.insertBefore(wsTxtNode, c.previousElementSibling);
              }
              editor_model.clickedElem = c;
              updateInteractionDiv();
            })(clickedElem)
          });
        }
        if(!model.selectionRange && clickedElem && clickedElem.matches && !clickedElem.matches(".editor-interface") && clickedElem.nextElementSibling && !clickedElem.nextElementSibling.matches(".editor-interface") && reorderCompatible(clickedElem, clickedElem.nextElementSibling)) {
          addContextMenuButton(isDisplayInline ? arrowRight : arrowDown,
          {title: "Move selected element " + (isDisplayInline ? "to the right" : "down")},
          {onclick: (c => (event) => {
              let wsTxtNode = c.nextSibling && c.nextSibling.nodeType == 3 && 
                c.nextSibling.textContent.trim() === "" ? c.nextSibling : undefined;
              let nodeToInsertAfter = c.nextElementSibling;
              nodeToInsertAfter.insertAdjacentElement("afterend", c);
              if(wsTxtNode) { // We move the whitespace as well
                nodeToInsertAfter.parentElement.insertBefore(wsTxtNode, nodeToInsertAfter.nextSibling);
              }
              editor_model.clickedElem = c;
              updateInteractionDiv();
            })(clickedElem)
          });
        }
        if(!model.selectionRange && clickedElem && clickedElem.tagName !== "HTML" && clickedElem.tagName !== "BODY" && clickedElem.tagName !== "HEAD") {
          addContextMenuButton(cloneSVG,
            {title: "Clone selected element"},
            {onclick: ((c, contextMenu) => event => {
                c.removeAttribute("ghost-clicked");
                let cloned = duplicate(c);
                if(cloned) {
                  editor_model.clickedElem = cloned;
                  updateInteractionDiv();
                } else contextMenu.classList.remove("visible");
              })(clickedElem, contextMenu)
            });
          addContextMenuButton(wasteBasketSVG,
            {title: "Delete selected element"},
            {onclick: (c => event => {
                if(editor_model.clickedElem.nextElementSibling) editor_model.clickedElem = editor_model.clickedElem.nextElementSibling;
                else editor_model.clickedElem = editor_model.clickedElem.previousElementSibling;
                c.remove();
                updateInteractionDiv();
              })(clickedElem)
            });
        }
        if(model.selectionRange && (model.selectionRange.startContainer === model.selectionRange.endContainer || model.selectionRange.startContainer.parentElement === model.selectionRange.commonAncestorContainer && model.selectionRange.endContainer.parentElement === model.selectionRange.commonAncestorContainer)) {
          addContextMenuButton(plusSVG,
              {title: "Wrap selection"},
              {onclick: (s => event => {
                let elements = [];
                let tmp = s.startContainer;
                let nodeToInsertAfter = s.startContainer;
                let parent = nodeToInsertAfter.parentElement;
                while(tmp && tmp !== s.endContainer.nextSibling) {
                  if(tmp.nodeType === 3) {
                    elements.push(tmp === s.startContainer ? tmp === s.endContainer ? tmp.textContent.substring(s.startOffset, s.endOffset) : tmp.textContent.substring(s.startOffset) :
                      tmp === s.endContainer ? tmp.textContent.substring(0, s.endOffset) :
                      tmp.textContent);
                    if(tmp === s.startContainer) {
                      if(tmp === s.endContainer && tmp.textContent.length > s.endOffset) {
                        // Need to split the text node.
                        tmp.parentElement.insertBefore(document.createTextNode(tmp.textContent.substring(s.endOffset)), tmp.nextSibling);
                      }
                      if(s.startOffset === 0) {
                        nodeToInsertAfter = nodeToInsertAfter.previousSibling;
                        tmp.remove();
                      } else {
                        tmp.textContent = tmp.textContent.substring(0, s.startOffset);
                      }
                    } else if(tmp === s.endContainer) {
                      if(s.endOffset === s.endContainer.textContent.length) {
                        tmp.remove();
                      } else {
                        tmp.textContent = tmp.textContent.substring(s.endOffset);
                      }
                    } else {
                      tmp.remove();
                    }
                  } else {
                    elements.push(tmp);
                    tmp.remove();
                  }
                  tmp = tmp.nextSibling;
                }
                let insertedNode = el("span", {"ghost-clicked": "true"});
                for(let k of elements) {
                  insertedNode.append(k);
                }
                let nodeToInsertBefore = nodeToInsertAfter ? nodeToInsertAfter.nextSibling : parent.childNodes[0];
                parent.insertBefore(insertedNode, nodeToInsertBefore);
                document.querySelector("#modify-menu").classList.toggle("visible", true);
                set_state_visible();
                editor_model.visible = true;
                editor_model.clickedElem = insertedNode;
                updateInteractionDiv();
              })(model.selectionRange)}
              )
        }
        if(!model.selectionRange) {
          addContextMenuButton(plusSVG,
              {title: "Insert element", contenteditable: false},
              {onclick: event => {
                editor_model.clickedElem = clickedElem;
                editor_model.displayClickedElemAsMainElem = true;
                editor_model.insertElement = true;
                set_state_insert();
                updateInteractionDiv();
                restoreCaretPosition();
              }});
        }
        if(model.clickedElem) {
          // Thaditor-defined custom context menu buttons
          if(typeof thaditor === "object") {
            for(let button of thaditor.customContextMenuButtons(model.clickedElem)) {
              addContextMenuButton(button.innerHTML, button.attributes, button.properties)
            }
          }
          // Page-defined custom context menu buttons
          for(let custom of editor.customContextMenuButtons) {
            for(let button of custom(model.clickedElem)) {
              addContextMenuButton(button.innerHTML, button.attributes, button.properties)
            }
          }
        }

        let baseElem = clickedElem;
        while(baseElem && (baseElem.tagName == "SCRIPT" || baseElem.tagName == "STYLE")) {
          baseElem = baseElem.nextElementSibling;
        }
        baseElem = model.selectionRange || baseElem || clickedElem;
      
        if(baseElem && !noContextMenu) {
          let clientRect = baseElem.getBoundingClientRect();
          // Find out where to place context menu.
          let clickedElemLeft = window.scrollX + clientRect.left;
          let clickedElemTop = window.scrollY + clientRect.top;
          let clickedElemBottom = window.scrollY + clientRect.bottom;
          let clickedElemRight = window.scrollX + clientRect.right;
          let desiredWidth = numButtons * buttonWidth();
          let desiredLeft = (clickedElemLeft + clickedElemRight) / 2 - desiredWidth;
          if(desiredLeft < clickedElemLeft) desiredLeft = clickedElemLeft;
          let desiredTop = clickedElemTop - buttonHeight(); 
          if(desiredTop - window.scrollY < 9) {
            desiredTop = clickedElemBottom;
            if(desiredTop + buttonHeight() > window.innerHeight) {
              desiredTop = window.innerHeight - buttonHeight(); 
            }
          }
          if(desiredLeft < 0) desiredLeft = 0;
          if(desiredTop < 0) desiredTop = 0;
          contextMenu.style.left = desiredLeft + "px";
          contextMenu.style.top = desiredTop + "px";
          contextMenu.style.width = desiredWidth + "px";
          contextMenu.classList.add("visible");
          setTimeout(maybeRepositionContextMenu, 0);
        }
        if(noContextMenu) {
          contextMenu.classList.remove("visible");
        }
      }
      return true;

    } //end of updateInteractionDiv

    function maybeRepositionContextMenu() {
      //move the context menu if overlaps with modify-menu
       let contextMenu = document.querySelector("#context-menu");
       let modifyMenuDiv = document.querySelector("#modify-menu");
       let pinnedIcons = document.querySelector(".modify-menu-icons.pinned")
       let pcr = pinnedIcons.getBoundingClientRect();
       let ccr = contextMenu.getBoundingClientRect();
       let mcr = modifyMenuDiv.getBoundingClientRect();
       if(onMobile()) {
         if(ccr.bottom > pcr.top) {
           contextMenu.style.top = (ccr.y - (ccr.bottom - pcr.top)) + "px"
         } else if(ccr.bottom > mcr.top) {
           contextMenu.style.top = (ccr.y - (ccr.bottom - mcr.top)) + "px"
         }
       } else {
         if(ccr.right > pcr.left && ccr.top < pcr.bottom) { // Overlap with icons.
           contextMenu.style.left = (ccr.x - (ccr.right - pcr.left)) + "px"
         } else if(ccr.right > mcr.left) {
           contextMenu.style.left = (ccr.x - (ccr.right - mcr.left)) + "px"
         }
       }
    }

    
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
      """document.addEventListener('click', onClickGlobal, false);
         document.addEventListener('mousedown', onMouseDownGlobal, false);
      """
    else "")

      window.onbeforeunload = function (e) {
        e = e || window.event;
        var askConfirmation = editor_model.canSave || editor_model.isSaving || editor_model.disambiguationMenu;
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
    }; //end of window.onbeforeload
    if (typeof editor_model === "object" && typeof editor_model.outputObserver !== "undefined") {
      editor_model.outputObserver.disconnect();
    }

    editor_model.outputObserver = new MutationObserver(handleMutations);
    editor_model.outputObserver.observe
      ( document.body.parentElement
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: true
        }
      );
"""--end of lastEditionScript

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