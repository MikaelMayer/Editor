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

varadmin = boolVar "admin" False
varedit = boolVar "edit" False
varls = boolVar "ls" False
defaultVarEdit = listDict.get "edit" defaultOptions |> Maybe.withDefault False
varproduction = listDict.get "production" defaultOptions |> Maybe.withDefault (freeze False)
iscloseable = listDict.get "closeable" defaultOptions |> Maybe.withDefault (freeze False)



userpermissions = {pageowner= True, admin= varadmin}
permissionToCreate = userpermissions.admin
permissionToEditServer = userpermissions.admin -- should be possibly get from user authentication
-- List.contains ("sub", "102014571179481340426") user -- That's my Google user ID.

canEditPage = userpermissions.pageowner && varedit && not varls

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
              let source = fs.read hydefile |>
                      Maybe.withDefaultLazy (\_ -> """all = [Error "hydefile '@hydefile' not found?!"]""")
                  source = source + Update.freeze "\n\nlet t = " + (listDict.get "task" vars |> Maybe.withDefault "all") + "\n    t = if typeof t == 'function' then t () else t\n    t = if typeof t == 'list' then t else [t]\nin t"
                  fileDirectory = Regex.replace "^/|/[^/]*$" "" hydefile
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
              let _ = Debug.log "generatedFilesDict" generatedFilesDict in
              case listDict.get path generatedFilesDict of
                Nothing -> if errors == "" then fs.read path else
                  Just <|
                  serverOwned "error recovery of hyde build tool" <|
                  """<html><head></head><body><h1>Error while resolving the generated version of @path</h1><pre>@errors</pre></body></html>"""
                  
                x -> x
            _ -> fs.read path
        )
      |> Maybe.map applyDotEditor
      |> Maybe.withDefaultReplace (
        serverOwned "404 page" """<html><head></head><body>@(
            if permissionToCreate then freeze """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>"""
          )</body></html>"""
      )
{---------------------------------------------------------------------------
Utility functions to be inherited by the main body of any
view of editor (edit / file listing / word processor / etc)
LUCA stands for "Last Universal Common Ancestor"
----------------------------------------------------------------------------}

luca = 
  [<script>
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
    function doReloadPage() {
      document.location.reload();
      //TODO not full reload bump: Mikael
    }

    //document.body.appendChild(el("progress", {id:"progress-bar", max:100, value:0, visible:false}, [], {}));
    let uploadProgress = [];

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
    editor.uploadFile = function(targetPathName, file, onOk, onError) {
      
      var xhr = new XMLHttpRequest();
      xhr.onprogress = (e) => {
        updateProgress(i, (e.loaded * 100.0 / e.total) || 100)
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
      xhr.open("POST", "/TharzenEditor/editor.php?action=write&name=" + encodeURIComponent(targetPathName), false);
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
      // extension ends with a / or is empty
      var tmp = location.pathname.split("/");
      tmp = tmp.slice(0, tmp.length - 1);
      storageFolder = tmp.join("/") + (extension != "" ?  "/" + extension : "");
      return storageFolder;
    }
	  editor.fs = { listdir: 
		  @(if browserSide then """
		    defaultOptions.nodefs.listdir
		  """ else """
		    function(dirname) {
		      var xhr = new XMLHttpRequest();
		      xhr.onreadystatechange = ((xhr, file) => () => {})
		      xhr.open("GET", dirname, false);
		      xhr.setRequestHeader("action", "listdir");
		      xhr.setRequestHeader("name", dirname);
		      xhr.send();
		      if(xhr.status === 200) {
		        return JSON.parse(xhr.responseText);
		      } else {
		        return [];
		      }
		    }
		  """)
	  };
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
      if(typeof attributes == "object")
        for(let k in attributes)
          x.setAttribute(k, attributes[k]);
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
        for(let k in properties)
          x[k] = properties[k];
      }
      return x;
    }    
   </script>]


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
          window.alert("Can't change the up dir");
          return;
        }
        var warningMsg = "Are you sure you want to delete the following file(s)?"
        for (i = 0; i < selected.length; i++) {
          warningMsg = warningMsg + "\n" + selected[i].id;
        }
        var conf = window.confirm(warningMsg);
        if (conf) {
          for (i = 0; i < selected.length; i++) {
            var isfolder = folders.filter((j) => j[0] == selected[i].id);
            console.log (isfolder);
            if (isfolder.length != 0) {
              doWriteServer("rmdir", "@path" + selected[i].id);
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
            editor.uploadFile("@path" + fl.name, fl, (ok) => console.log ("was ok\n" + ok), (err) => console.err (err));
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
                el("a", {href:link}, name)])]);
        }
        //el(tag, attributes, children, properties)
        if (path != "") {
          var link = "../" + "?ls=true&amp;edit";
          form.append(fileItemDisplay(link, "..", true));
        }
        // directories before files, sorted case-insensitive
        files.sort(([name1, isDir1], [name2, isDir2]) =>
          isDir1 && !isDir2 ? -1 : isDir2 && !isDir1 ? 1 :
          name1.toLowerCase() < name2.toLowerCase() ? -1 : 0);
        for (i = 0; i < files.length; i++) {
          var [name, isDir] = files[i];
          var link = isDir ? name + "/?ls&" + "edit" : name;
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
  else if Regex.matchIn """\.txt$""" path then
    Ok <html><head></head><body>
      <textarea id="thetext" style="width:100%;height:100%" initdata=@sourcecontent
        onkeyup="if(this.getAttribute('initdata') !== this.value) this.setAttribute('initdata', this.value)"></textarea>
      <script>
        document.getElementById('thetext').value = document.getElementById('thetext').getAttribute('initdata')
      </script>
    </body></html>
  else Err """Serving only .html, .md and .elm files. Got @path"""

{---------------------------------------------------------------------------
 Recovers from evaluation errors
----------------------------------------------------------------------------}
recoveredEvaluatedPage: Html
recoveredEvaluatedPage = --updatecheckpoint "recoveredEvaluatedPage" <|
  case evaluatedPage of
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
main = 
  --updatecheckpoint "main" <|
  case recoveredEvaluatedPage of
  ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
    List.filter (case of [_, _] -> False; _ -> True) |>
    List.mapWithReverse identity (case of
      ["body", bodyattrs, bodychildren] ->
        ["body",
           (if canEditPage then
             [["contenteditable", "true"]] |> serverOwned "contenteditable attribute of the body due to edit=true" 
            else freeze []) ++
           bodyattrs, luca ++
          if not varedit || varls then
            bodychildren
          else 
          (if canEditPage then ((serverOwned "edition menu" editionmenu) sourcecontent) else
           if not varedit && not iscloseable && not varproduction then serverOwned "open edit box" [openEditBox] else
           serverOwned "edit prelude when not in edit mode" []) ++
           serverOwned "initial script" initialScript ++
           bodychildren ++
           (serverOwned "synchronization script and placeholder" [<script>@editionscript</script>, <div class="bottom-placeholder"> </div>])]
      ["head", headattrs, headChildren] ->
        ["head", headattrs, <meta name="viewport" content="width=device-width"> :: headChildren]
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
  
</style>@msg
</div>

openEditBox = switchEditBox True
closeEditBox = switchEditBox False

boolToCheck = Update.bijection (case of "true" -> [["checked", ""]]; _ -> []) (case of [["checked", ""]] -> "true"; _ -> "false")

-- Everything inside the modify menu is generated and is not visible to Editor
editionmenu thesource = [
<div id="modify-menu" list-ghost-attributes="style class" sourcecontent=@thesource contenteditable="false" children-are-ghosts="true"></div>,
<div id="context-menu" children-are-ghosts="true" list-ghost-attributes="style class" contenteditable="false"></div>,
if iscloseable then <span dummy=""></span> else closeEditBox,
<style id="booleanswitch">
  /* The switch - the box around the slider */
.switch {
  position: relative;
  display: inline-block;
  width: 30px;
  height: 17px;
  vertical-align: middle;
}

/* Hide default HTML checkbox */
.switch input {display:none;}

/* The slider */
.slider {
  position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #ccc;
  -webkit-transition: .4s;
  transition: .4s;
}

.slider:before {
  position: absolute;
  content: "";
  height: 13px;
  width: 13px;
  left: 2px;
  bottom: 2px;
  background-color: white;
  -webkit-transition: .4s;
  transition: .4s;
}

input:checked + .slider {
  background-color: #2196F3;
}

input:focus + .slider {
  box-shadow: 0 0 1px #2196F3;
}

input:checked + .slider:before {
  -webkit-transform: translateX(13px);
  -ms-transform: translateX(13px);
  transform: translateX(13px);
}

/* Rounded sliders */
.slider.round {
  border-radius: 17px;
}

.slider.round:before {
  border-radius: 50%;
}

.switch + label {
  cursor: pointer;
  padding-top: 5px;
  padding-bottom: 5px;
}
</style>,
<style>
.filename {
  color: #FFF;
  padding-left: 3px;
  vertical-align: top;
  display: block;
}
div.imgFolder {
  display: inline-block;
  margin: 0.2em;
}
div.imgFolder > img {
  max-width: 100%;
  min-width:2em;
}
a.troubleshooter {
  position: absolute;
  top: calc(100% - 3em);
  display: inline-block; 
}
.editor-menu {
  display: initial !important;
}
.disabled {
  color: #BBB;
}
input.global-setting[type=checkbox] {
  display: none;
}
input.global-setting[type=checkbox]:checked + span.label-checkbox {
  background: #bcbbff;
}
span.label-checkbox {
  padding: 2px;
  border-radius: 10px;
}
span.label-checkbox:hover {
  background-color: rgba(0,0,0,0.06);
  cursor: pointer;
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

span#ambiguity-id {
  color: #9dff00;
  display: block;
}
.solution {
  display: block;
  padding: 3px;
}
.solution.selected {
  background: rgba(255,255,255,0.2);
}
.to-be-selected {
  outline: #FCC 2px solid;
  animation:spin 1s linear infinite;
}
.solution:not(.selected):hover {
  background: rgba(255,255,255,0.1);
  cursor: pointer;
}
.solution.notfinal {
  color: #f6f6aa;
}
.add {
  background: lightgreen
}
.remove {
  background: #f8a7a7;
  text-decoration: line-through
}
div.disambiguationMenu {
	height: 40%;
	overflow-y: auto;
  padding-left: 0.3em;
  padding-top: 0.3em;
  padding-bottom: 0.3em;
  border: 2px solid #f6f6aa;
}
.codepreview {
  font-family: monospace;
  padding: 3px;
  background: white;
  color: black;
  margin-left: 0.1em;
}
.solution:not(.selected) .codepreview {
  background: rgba(255,255,255,0.8);
}
#modify-menu button.modifyMenuButton {
  background-color: var(--context-button-color);
  min-height: var(--context-button-width);
  color: white;
  border-style: none;
  margin-top: 5px;
  padding: 0.4em;
  font-weight: bold;
  cursor: pointer;
}
#modify-menu button.modifyMenuButton:hover {
  background-color: var(--context-button-color-hover);
}
#editor_codepreview, #manualsync-menuitem {
  display: none;
  z-index: 999;
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
  background-color: var(--context-color);
  color: white;
  padding: 5px;
  font-size: 16px;
  transform: translate(100%, 0px);
  transition-property: transform;
  transition-duration: 0.5s;
  z-index: 100001;
}
.modify-menu-icon {
  vertical-align: middle;
  cursor: pointer;
  width: var(--context-menu-button-width);
}
.modify-menu-icon:hover {
  background-color: var(--context-button-color-hover);
}
div#modify-menu > div.modify-menu-icons {
  width: 100%;
  overflow-x: auto;
}
div#modify-menu > div.information {
  overflow-y: auto;
  max-height: calc(100% - var(--context-menu-height));
}
div#modify-menu.visible {
  transform: translate(0%, 0%);
}
div#modify-menu h3 {
  margin-top: 2px;
  margin-bottom: 2px;
}
div#modify-menu div.keyvalues {
  display: table;
  width: 100%;
  table-layout: fixed;
}
div#modify-menu div.keyvalues > div.keyvalue {
  display: table-row;
}
div#modify-menu div.keyvalues > div.keyvalue > * {
  display: table-cell;
  padding: 4px;
  vertical-align: middle;
}
div#modify-menu div.keyvalues > div.keyvalueadder {
  opacity: 0.5;
}
div#modify-menu div.keyvalues > div.keyvalueadder:hover {
  opacity: 1;
}
div#modify-menu input {
  padding: 4px;
}
div#modify-menu #newTagName {
  width: 50%;
  font-size: 1em;  
}
div#modify-menu .tagname-info {
  color: #d4d4d4;
  display: inline-block;
  width: 50%;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
div#modify-menu input[type=radio] {
  width: initial;
  font-size: 1em;
}
.inline-input {
  background: transparent;
  color: white;
  border: none;
}

#upload-image-btn-a {
  margin: 10%; 
  padding: 4px 10px; 
  height: 20px; 
  position: relative; 
  color: #888; 
  background: #fafafa; 
  border-radius: 4px; 
  display: inline-block; 
  height: 20px; 
  width: 280px;
}

#upload-image-btn-input {
  position: absolute; 
  top: 0; 
  left: 0; 
  width: 100%;
}

/* highlight selected image */
div.highlight-select-image {
  box-shadow: 2px 2px 3px black;
}

:root {
  --context-color: rgba(0, 128, 128, 0.8);
  --context-color-next: rgba(0, 158, 158, 0.8); 
  --context-button-color: rgba(0, 192, 192, 0.8);
  --context-button-color-hover: rgba(0, 232, 232, 0.8);
  --context-button-color-inert: rgba(128, 128, 128, 0.8);
  --context-button-color-inert-hover: rgba(150, 150, 150, 0.8);
  --context-button-color-inert-active: rgba(182, 182, 182, 0.8);
  --context-menu-height: 30px;
  --context-menu-button-width: 40px;
  --context-menu-padding-top: 0px;
  --context-menu-padding-left: 0px;
  --context-button-selected: rgba(0, 212, 212, 0.8);
}
div.tagName {
  padding: 4px;
  cursor: pointer;
  background: var(--context-button-color);
  vertical-align: text-bottom;
}
div.tagName:hover {
  background: var(--context-button-color-hover);
}

[ghost-hovered=true] {
  outline: 2px dashed var(--context-color-next);
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
div#context-menu .context-menu-button, div#modify-menu .modify-menu-button {
  background: var(--context-button-color);
  display: inline-block;
  height: var(--context-menu-height);
  width: var(--context-menu-button-width);
  cursor: pointer;
}
div#context-menu .context-menu-button.selected, div#modify-menu .modify-menu-button.selected {
  background: var(--context-button-selected);
}

div#modify-menu .modify-menu-button.inert.active {
  background: var(--context-button-color-inert-active)
}
div#modify-menu .modify-menu-button.disabled {
  opacity: 0.5;
  cursor: not-allowed;
}
div#context-menu .context-menu-button > svg, div#modify-menu .modify-menu-button > svg {
  padding-left: var(--context-menu-padding-left);
  padding-right: var(--context-menu-padding-left);
  padding-top: var(--context-menu-padding-top);
  padding-bottom: var(--context-menu-padding-top);
}
div#context-menu .context-menu-button.inert, div#modify-menu .modify-menu-button.inert {
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
  fill-rule:evenodd;
  stroke-width:1px;
}
div#context-menu .context-menu-button:hover, div#modify-menu .modify-menu-button:hover:not(.disabled) {
  background: var(--context-button-color-hover);
}
div#context-menu .context-menu-button.inert:hover, div#modify-menu .modify-menu-button.inert:hover:not(.disabled) {
  background: var(--context-button-color-inert-hover)
}
#applyNewTagName {
  display: none;
}
#applyNewTagName.visible {
  display: inline-block;
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
    min-height: 350px;
    transform: translate(0px, 100%);
  }
  div.bottom-placeholder {
    width: 100%;
    height: 30%;
  }
  
  :root {
    --context-menu-height: 48px;
    --context-menu-button-width: 48px;
    --context-menu-padding-top: 9px;
    --context-menu-padding-left: 4px;
  }
}
</style>]

browserSide = listDict.get "browserSide" defaultOptions == Just True

initialScript = [
<script>
var XHRequest = @(if browserSide then "ProxiedServerRequest" else "XMLHttpRequest");

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

// Editor's API is stored in the variable editor.

editor = typeof editor === "object" ? editor : {};

// Array of functions on nodes returning an array of attributes that should be ghosts.
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


function isSpecificGhostAttributeKeyFromNode(n) {
  var additionalGhostAttributes = [];
  for(var k in editor.ghostAttrs) {
    additionalGhostAttributes = additionalGhostAttributes.concat(editor.ghostAttrs[k](n))
  }
  return (a => name => a.indexOf(name) != -1)(additionalGhostAttributes);
}

// Array of predicates that, if they return true on a node, Editor will mark this node as ghost.
editor.ghostNodes = [];

// Analytics scripts
editor.ghostNodes.push(insertedNode =>
  insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
     (insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1 ||
      insertedNode.getAttribute("src").indexOf("google-analytics.com/gtm/js") != -1)
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
  (insertedNode.tagName == "DIV" &&
    insertedNode.classList.contains("abcRioButton")) ||
  (insertedNode.tagName == "IFRAME" &&
    insertedNode.getAttribute("id") == "ssIFrame_google")
);
// For anonymous styles inside HEAD (e.g. ace css themes and google sign-in)
editor.ghostNodes.push(insertedNode => 
  insertedNode.tagName == "STYLE" && insertedNode.getAttribute("id") == null &&
  insertedNode.parentElement.tagName == "HEAD" && (insertedNode.setAttribute("save-ghost", "true") || true)
);
// For ace script for syntax highlight
editor.ghostNodes.push(insertedNode =>
  insertedNode.tagName == "SCRIPT" && typeof insertedNode.getAttribute("src") == "string" &&
     insertedNode.getAttribute("src").startsWith("https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.2/mode-j")
);
// For ace script for syntax highlight
editor.ghostNodes.push(insertedNode =>
  insertedNode.tagName == "ACE_OUTER"
);
// For the grammarly extension
editor.ghostNodes.push(insertedNode =>
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
        if(!hasGhostAncestor(insertedNode) && (insertedNode.nodeType == 1 && insertedNode.getAttribute("isghost") != "true" || insertedNode.noteType == 3 && !insertedNode.isghost) && editor.ghostNodes.find(pred => pred(insertedNode))) {
         if(insertedNode.nodeType == 1) insertedNode.setAttribute("isghost", "true");
         insertedNode.isghost = true;
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
    if(node.nextSibling != null && !options.target) {
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

-- Script added to the end of the page
editionscript = """
  var onMobile = () => window.matchMedia("(pointer: coarse)").matches;
  var buttonHeight = () => onMobile() ? 48 : 30;
  var buttonWidth  = () => onMobile() ? 48 : 40;
  console.log("editionscript running");
  
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
              value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2)
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
      if(editor_model.caretPosition) {
        editor_model.caretPosition = dataToRecoverCaretPosition(editor_model.caretPosition);
      }
      if(editor_model.selectionRange) {
        editor_model.selectionRange = dataToRecoverSelectionRange(editor_model.selectionRange);
      }
      if(editor_model.clickedElem) {
        editor_model.clickedElem = dataToRecoverElement(editor_model.clickedElem);
      }
      document.open();
      document.write(NC);
      document.close();
    }
    
    var t = undefined;
    
    handleServerPOSTResponse = (xmlhttp, onBeforeUpdate) => function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          editor_model.isSaving = false;
          //console.log("Received new content. Replacing the page.");
          if(typeof onBeforeUpdate !== "undefined") onBeforeUpdate();
          var saved = saveGhostAttributes();
          
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
            editor_model.notextselection = false;
            editor_model.caretPosition = undefined;
            editor_model.link = undefined;
            editor_model.advanced = true; // Opens advanced mode.
            editor_model.visible = true;
            //editor_model.displaySource: false, // Keep source opened or closed
            // TODO: Disable click or change in DOM until ambiguity is resolved.
          } else {
            editor_model.disambiguationMenu = undefined;
            var opSummaryEncoded = xmlhttp.getResponseHeader("Operations-Summary");
            if(opSummaryEncoded) {
              var opSummary = decodeURI(opSummaryEncoded);
              let newMenu = el("menuitem#lastaction", {},
                  el("span.summary", {}, "Last action: " + opSummary)) 
              editor_model.feedback = newMenu;
              var newmenutimeout = setTimeout(function() { editor_model.feedback = undefined; newMenu.remove(); }, 2000);
              newMenu.onclick = ((n) => () => clearTimeout(n))(newmenutimeout);
            }
          }
          
          var strQuery = "";
          if(newQueryStr !== null) {
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
        }
    }
    
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
      return u.pathname.replace(/[^\/]*$/, "") + url;
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
      updateInteractionDiv();
      setTimeout( () => {
        notifyServer(xmlhttp => {
          xmlhttp.setRequestHeader("question", editor_model.askQuestions ? "true" : "false");
          return JSON.stringify(domNodeToNativeValue(document.body.parentElement));
        })
      }, 0);
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
      if(!editor_model.autosave) {
        editor_model.canSave = true;
        var saveButtons = document.querySelectorAll(".saveButton");
        // TODO: Can we regenerate the whole interface for consistency?
        for(let sb of saveButtons) {
          sb.classList.toggle("disabled", false);
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
          // do not paste html into modify menu
          if (sel.anchorNode.offsetParent.id === "modify-menu") {
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
    
    onClickGlobal = function (event) {
      var clickedElem = event.target;
      
      var editorSelectOptions = document.querySelectorAll("meta[editor-noselect],meta[editor-doselect]");
      var matchOptions = function(clickedElem) {
        var result = true;
        for(let i = 0; i < editorSelectOptions.length; i++) {
          let negativeSelector = editorSelectOptions[i].getAttribute("editor-noselect"),
              positiveSelector = editorSelectOptions[i].getAttribute("editor-doselect");
          if(result && negativeSelector) {
            result = !clickedElem.matches(negativeSelector);
          }
          if(!result && positiveSelector) {
            result = clickedElem.matches(positiveSelector);
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
      editor_model.advanced = false;
      updateInteractionDiv();
      // Check if the event.target matches some selector, and do things...
    }
    function mkSvg(path, fill) {
      return `<svg class="context-menu-icon${fill ? " fill": ""}" width="40" height="30">
            <path d="${path}" /></svg>`
    }
    
    var saveSVG = mkSvg("M 10,5 10,25 30,25 30,9 26,5 13,5 Z M 13,6 25,6 25,12 13,12 Z M 22,7 22,11 24,11 24,7 Z M 13,15 27,15 27,24 13,24 Z M 11,23 12,23 12,24 11,24 Z M 28,23 29,23 29,24 28,24 Z", true);
    var openLeftSVG = mkSvg("M 27.5,4 22.5,4 12.5,15 22.5,25 27.5,25 17.5,15 Z", true);
    var closeRightSVG = mkSvg("M 12.5,4 17.5,4 27.5,15 17.5,25 12.5,25 22.5,15 Z", true);
    var openTopSVG = mkSvg("M 9.5,22 9.5,17 20.5,7 30.5,17 30.5,22 20.5,12 Z", true);
    var closeBottomSVG = mkSvg("M 9.5,7 9.5,12 20.5,22 30.5,12 30.5,7 20.5,17 Z", true);
    var wasteBasketSVG = mkSvg("m 24,11.5 0,11 m -4,-11 0,11 m -4,-11 0,11 M 17,7 c 0,-4.5 6,-4.5 6,0 m -11,0.5 0,14 c 0,3 1,4 3,4 l 10,0 c 2,0 3,-1 3,-3.5 L 28,8 M 9,7.5 l 22,0");
    var plusSVG = mkSvg("M 18,5 22,5 22,13 30,13 30,17 22,17 22,25 18,25 18,17 10,17 10,13 18,13 Z", true);
    var liveLinkSVG = link => `<a class="livelink" href="javascript:navigateLocal(relativeToAbsolute('${link}'))">${mkSvg("M 23,10 21,12 10,12 10,23 25,23 25,18 27,16 27,24 26,25 9,25 8,24 8,11 9,10 Z M 21,5 33,5 33,17 31,19 31,9 21,19 19,17 29,7 19,7 Z", true)}</a>`;
    var gearSVG = mkSvg("M 17.88,2.979 14.84,3.938 15.28,7.588 13.52,9.063 10,8 8.529,10.83 11.42,13.1 11.22,15.38 7.979,17.12 8.938,20.16 12.59,19.72 14.06,21.48 13,25 15.83,26.47 18.1,23.58 20.38,23.78 22.12,27.02 25.16,26.06 24.72,22.41 26.48,20.94 30,22 31.47,19.17 28.58,16.9 28.78,14.62 32.02,12.88 31.06,9.84 27.41,10.28 25.94,8.52 27,5 24.17,3.529 21.9,6.42 19.62,6.219 17.88,2.979 Z M 20,11 A 4,4 0 0 1 24,15 4,4 0 0 1 20,19 4,4 0 0 1 16,15 4,4 0 0 1 20,11 Z", true);
    var folderSVG = mkSvg("M 8,3 5,6 5,26 10,10 32,10 32,6 18,6 15,3 8,3 Z M 5,26 10,10 37,10 32,26 Z");
    var reloadSVG = mkSvg("M 32.5,8.625 30.25,15.25 24.75,11.125 M 6.75,20 9.875,14.5 15.125,19 M 29.5,18 C 28.25,22.125 24.375,25 20,25 14.5,25 10,20.5 10,15 M 10.5,12 C 11.75,7.875 15.625,5 20,5 25.5,5 30,9.5 30,15");
    var sourceSVG = mkSvg("M 22.215125,2 25,3 18.01572,27 15,26 Z M 12,19 12,25 2,14 12,4 12,9 7,14 Z M 28,9 28,4 38,15 28,25 28,20 33,15 Z", true);
    var isAbsolute = url => url.match(/^https?:\/\/|^www\.|^\/\//);
    var linkToEdit = @(if defaultVarEdit then "link => link" else 
     """link => link && !isAbsolute(link) ? link.match(/\?/) ? link + "&edit" : link + "?edit" : link;""");
    
    var ifAlreadyRunning = typeof editor_model === "object";
    
    
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
      return {target: dataToRecoverElement(caretPosition.startContainer), startOffset: caretPosition.startOffset};
    }
    function recoverCaretPositionFromData(data) {
      if(!data) return;
      let newTextNodeOrParent = recoverElementFromData(data.target);
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
    
    var editor_model = { // Change this and call updateInteractionDiv() to get something consistent.
      //makes visibility of editor model consistent throughout reloads
      visible: ifAlreadyRunning ? editor_model.visible : false,
      clickedElem: ifAlreadyRunning ? recoverElementFromData(editor_model.clickedElem) : undefined,
      notextselection: false,
      selectionRange: ifAlreadyRunning ? recoverSelectionRangeFromData(editor_model.selectionRange) : undefined,
      caretPosition: ifAlreadyRunning ? recoverCaretPositionFromData(editor_model.caretPosition) : undefined,
      link: undefined,
      advanced: ifAlreadyRunning ? editor_model.advanced : false,
      displaySource: ifAlreadyRunning ? editor_model.displaySource : false,
      disambiguationMenu: undefined,
      isSaving: false,
      //new attribute to keep menu state after reload
      curScrollPos: ifAlreadyRunning ? editor_model.curScrollPos : 0,
      askQuestions: ifAlreadyRunning ? editor_model.askQuestions :
                    @(case listDict.get "question" vars of
                       Just questionattr -> "true"
                       _ -> if boolVar "question" True then "true" else 'false'),
      autosave: ifAlreadyRunning ? editor_model.autosave :
                    @(case listDict.get "autosave" vars of
                      Just autosaveattr -> "true"
                      _ -> if boolVar "autosave" True then "true" else "false"),
      path: ifAlreadyRunning ? editor_model.path : @(path |> jsCode.stringOf)
    }

    function reorderCompatible (node1, node2){
      let topLevelOrderableTags = {TABLE:1, P:1, LI:1, UL:1, OL:1, H1:1, H2:1, H3:1, H4:1, H5:1, H6:1, DIV:1};
      return node1.tagName === node2.tagName && node1.tagName !== "TD" && node1.tagName !== "TH" ||
        topLevelOrderableTags[node1.tagName] && topLevelOrderableTags[node2.tagName]
        ; 
    }
    function preventTextDeselection(e){
      e = e || window.event;
      e.preventDefault();
    }

    updateInteractionDiv();
    
    function updateInteractionDiv() {
      let model = editor_model;
      var clickedElem = model.clickedElem;
      var contextMenu = document.querySelector("#context-menu");
      var modifyMenuDiv = document.querySelector("#modify-menu");
      //if both are closed, just return 
      if(!modifyMenuDiv || !contextMenu) return;
      modifyMenuDiv.classList.toggle("visible", editor_model.visible);
      document.querySelectorAll("[ghost-clicked=true]").forEach(e => e.removeAttribute("ghost-clicked"));
      if(clickedElem && clickedElem.nodeType === 1) {
        clickedElem.setAttribute("ghost-clicked", "true");
      }
      model.selectionRange = model.notextselection ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0); 
        if(!f || !f.getBoundingClientRect ||
            f.startOffset === f.endOffset && f.startContainer === f.endContainer) return;
        return f;
      })();
      model.caretPosition = model.notextselection || clickedElem && clickedElem.tagName === "HEAD" ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0);
        if(!f || f.startOffset !== f.endOffset && f.startContainer !== f.endContainer) return;
        return f;
      })();
      function textPreview(element, maxLength) {
        let x = element.innerText;
        let result = "'" + x + "'";
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
      modifyMenuDiv.innerHTML = "";
      let modifyMenuIconsDiv = el("div", {"class":"modify-menu-icons"});
      let interactionDiv = el("div", {"class": "information"});
      modifyMenuDiv.append(modifyMenuIconsDiv);
      modifyMenuDiv.append(interactionDiv);
      let addModifyMenuIcon = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("modify-menu-button");
        button.innerHTML = innerHTML;
        modifyMenuIconsDiv.append(button);
      }
      var panelOpenCloseIcon = function() {
        return document.querySelector("#modify-menu").classList.contains("visible") ?
            onMobile() ? closeBottomSVG : closeRightSVG
          : onMobile() ? openTopSVG : openLeftSVG;
      }
      var alwaysVisibleButtonIndex = 0;
      function nextVisibleBarButtonPosStyle() {
        let result = "position: absolute;" +
          (onMobile() ? "top:-"+buttonHeight()+"px;left:"+alwaysVisibleButtonIndex*buttonWidth()+"px" :
                        "left:-"+buttonWidth()+"px;top:"+alwaysVisibleButtonIndex*buttonHeight()+"px")
        alwaysVisibleButtonIndex++;
        return result;
      }
      addModifyMenuIcon(
        panelOpenCloseIcon(),
        {title: "Open/close settings tab", "class": "inert", style: nextVisibleBarButtonPosStyle() },
        {onclick: function(event) {
            document.querySelector("#modify-menu").classList.toggle("visible");
            editor_model.visible = !editor_model.visible;
            this.innerHTML = panelOpenCloseIcon();
          }
        });
      addModifyMenuIcon(
        gearSVG,
        {title: "Advanced", "class": "inert" + (editor_model.advanced ? " active": ""),
         style: nextVisibleBarButtonPosStyle()
        },
        {onclick: (c => function(event) {
          //defaults to turning on advanced menu if the editor model is already visible, otherwise toggles advanced menu.
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
      addModifyMenuIcon(saveSVG,
      {title: editor_model.disambiguationMenu ? "Accept proposed solution" : "Save", "class": "saveButton" + (editor_model.canSave || editor_model.disambiguationMenu ? "" : " disabled") + (editor_model.isSaving ? " to-be-selected" : ""),
          style: nextVisibleBarButtonPosStyle(),
          id: "savebutton"
      },
        {onclick: editor_model.disambiguationMenu ? 
          ((ambiguityKey, selected) => () => acceptAmbiguity(ambiguityKey, selected))(
            editor_model.disambiguationMenu.ambiguityKey, editor_model.disambiguationMenu.selected)
          : function(event) {
            if(!this.classList.contains("disabled")) {
              sendModificationsToServer();
            }
          }
        }
      )
      if(model.advanced || model.disambiguationMenu) {
        modifyMenuDiv.append(
          el("a", { class:"troubleshooter", href:  "https://github.com/MikaelMayer/Editor/issues"}, "Help"));
        modifyMenuIconsDiv.append(
          el("span", { class:'filename', title:"the path of the file you are currently viewing"}, 
            editor_model.path ? editor_model.path : "[root folder]"));
        
        // TODO: Ambiguity interaction (should be stored in the model)
        // TODO: Current URL (can be changed) + reload button (double circular arrow) + list files button (folder icon)
        // TODO: Stage/create draft (clone and save icon)
        // TODO: Source code (expandable - can use Ace Editor)
        // TODO: Options: Ask questions, Autosave.
        // TODO: Report issue. About.
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
            } }
        )
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
          let sourceEdit = document.getElementById("sourcecontentmodifier")
          //Added by Mark Nie on 6/18-6/19
          //console.log("Scroll position before is:" + sourceEdit.scrollTop);
          //console.log("Stored scroll position is:" + editor_model.curPos);
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
        )
        modifyMenuDiv.append(
          el("label", {"for": "input-autosave", class: "label-checkbox"}, "Auto-save"));
      } else if(model.insertElement) {
        interactionDiv.append(el("h1", {}, "Insert"));
        interactionDiv.append(el("div", {id: "insertionPlace"}, [
          clickedElem.tagName === "BODY" || clickedElem.tagName === "HTML" || clickedElem.tagName === "HEAD" ? undefined :
          el("span", {}, [
            el("input", {type: "radio", id: "radioInsertBeforeNode", name: "insertionPlace", value: "before"}),
            el("label", {"for": "radioInsertBeforeNode"}, "Before node")]),
          clickedElem.tagName === "HTML" ? undefined :
          el("span", {}, [
            el("input", {type: "radio", id: "radioInsertAtCaret", name: "insertionPlace", value: "caret"}),
            el("label", {"for": "radioInsertAtCaret"}, model.caretPosition ? "At caret" : "As child")]),
          clickedElem.tagName === "BODY" || clickedElem.tagName === "HTML" || clickedElem.tagName === "HEAD" ? undefined :
          el("span", {}, [
            el("input", {type: "radio", id: "radioInsertAfterNode", name: "insertionPlace", value: "after"}, [], {checked: true}),
            el("label", {"for": "radioInsertAfterNode"}, "After node")]),
        ]));
        let insertTag = function() {
          let newElement = (() => {
            let parent = this;
            while(parent && !parent.classList.contains("tagName")) parent = parent.parentElement;
            let m = parent.querySelector(".templateengine");
            if(typeof m.innerHTMLCreate === "string") return m.innerHTMLCreate;
            return el(m.tag, m.attrs, m.children, m.props);
          })();
          let insertionStyle = (() => {
            let radios = document.querySelectorAll('#insertionPlace input[name=insertionPlace]');
            let defaultValue = "after";
            for (let i = 0, length = radios.length; i < length; i++) {
              if (radios[i].checked) return radios[i].getAttribute("value");
              defaultValue = radios[i].getAttribute("value")
            }
            return defaultValue;
          })();
          if(insertionStyle === "after") {
            if(typeof newElement === "string") {
              clickedElem.insertAdjacentHTML("afterend", newElement);
            } else {
              clickedElem.parentElement.insertBefore(newElement, clickedElem.nextSibling);
            }
          } else if(insertionStyle === "before") {
            if(typeof newElement === "string") {
              clickedElem.insertAdjacentHTML("beforebegin", newElement);
            } else {
              clickedElem.parentElement.insertBefore(newElement, clickedElem);
            }
          } else if(typeof model.caretPosition !== "undefined") {
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
              tmpSpan.remove();
            } else {
              clickedElem.insertBefore(newElement, txt.nextSibling)
            }
          } else { // Insert at the end of the selected element, inside.
            if(typeof newElement === "string") {
              // TODO: append at the element.
              let tmpSpan = el("span");
              clickedElem.insertBefore(tmpSpan, null);
              tmpSpan.insertAdjacentHTML("afterend", newElement);
              tmpSpan.remove();
            } else {
              // Insert at the end.
              clickedElem.insertBefore(newElement, null);
            }
          }
          if(typeof newElement !== "string") {
            editor_model.clickedElem = newElement;
            updateInteractionDiv();
          } else {
            editor_model.clickedElem = clickedElem;
            updateInteractionDiv();
          }
        }
        let addElem = function(name, createParams) {
          interactionDiv.append(el("div", {"class": "tagName"},
            el("span", { "class": "templateengine"}, name, createParams), {onclick: insertTag}));
        }
        if(clickedElem.tagName === "HEAD") {
          addElem("Title", {tag:"title", children: "Page_title"});
          addElem("Style", {tag:"style", children: "/*Your CSS there*/"});
          addElem("Script", {tag:"script", children: "/*Your CSS below*/"});
        } else {
          interactionDiv.append(el("input", {"type": "file", multiple: "", value: "Images or files..."}, [], {
            onchange: function(evt) { uploadFilesAtCursor(evt.target.files); }}));
          // TODO: Filter and sort which one we can add
          addElem("List item", {tag:"li", props: { innerHTML: "<br>"}});
          addElem("New bulleted list", {tag:"ul", props: { innerHTML: "<ul>\n  <li><br></li>\n</ul>"}});
          addElem("New numbered list", {tag:"ol", props: { innerHTML: "<ol>\n  <li><br></li>\n</ol>"}});
          addElem("Link", {tag:"a", childCreate: "Name_your_link"});
          addElem("Paragraph", {tag:"p", childCreate: "Inserted paragraph"});
          for(let i = 1; i <= 6; i++) {
            addElem("Header " + i, {tag:"h" + i, props: { innerHTML: "Title" + i}});
          }
        }
        interactionDiv.append(el("div", {"class": "tagName"},
           [el("textarea", {id: "customHTMLToInsert", placeholder: "Custom HTML here...", "class": "templateengine", onkeyup: "this.innerHTMLCreate = this.value"}),
           el("div", {"class":"modify-menu-icon", title: "Insert HTML", style: "display: inline-block"}, [], {
              innerHTML: plusSVG,
              onclick: insertTag
            })]));
        document.querySelector("#modify-menu").classList.toggle("visible", true);
      } else {
      if(clickedElem && clickedElem.parentElement) {
        let parent = model.selectionRange ? clickedElem : clickedElem.parentElement;
        if(parent.tagName === "TBODY" && parent.parentElement && parent.parentElement.tagName === "TABLE") parent = parent.parentElement;
        addModifyMenuIcon(`<svg class="context-menu-icon" width="40" height="30">
            <path d="M 8,19 8,22 11,22 M 12,18 8,22 M 8,10 8,7 11,7 M 12,10 8,7 M 27,7 30,7 30,10 M 26,10 30,7 M 31,19 31,22 28,22 M 26,18 31,22 M 12,12 12,10 M 12,16 12,14 M 14,18 12,18 M 18,18 16,18 M 22,18 20,18 M 26,18 24,18 M 26,14 26,16 M 26,10 26,12 M 22,10 24,10 M 18,10 20,10 M 14,10 16,10 M 5,5 35,5 35,25 5,25 Z"/></svg>`,
              {title: "Select parent (" + summary(parent) + ")", "class": "inert"},
              {onclick: ((c, parent) => event => {
                editor_model.clickedElem = parent;
                editor_model.notextselection = true;
                updateInteractionDiv()
                })(clickedElem, parent),
               onmouseenter: (p => () => { p.setAttribute("ghost-hovered", "true") })(parent),
               onmouseleave: (p => () => { p.removeAttribute("ghost-hovered") })(parent)
              }
            );
      }
      if(!model.selectionRange && clickedElem && clickedElem.previousElementSibling) {
        addModifyMenuIcon(`<svg class="context-menu-icon fill" width="40" height="30">
          <path d="m 10,14 3,3 4,-4 0,14 6,0 0,-14 4,4 3,-3 L 20,4 Z"/></svg>`,
        {title: "Select previous sibling (" + summary(clickedElem.previousElementSibling) + ")", class: "inert"},
        {onclick: (c => (event) => {
            editor_model.clickedElem = c;
            editor_model.notextselection = true;
            updateInteractionDiv();
          })(clickedElem.previousElementSibling),
         onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(clickedElem.previousElementSibling),
         onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(clickedElem.previousElementSibling)
        });
      }
      if(!model.selectionRange && clickedElem && clickedElem.nextElementSibling) {
        addModifyMenuIcon(`<svg class="context-menu-icon fill" width="40" height="30">
          <path d="m 10,17 3,-3 4,4 0,-14 6,0 0,14 4,-4 3,3 -10,10 z"/></svg>`,
        {title: "Select next sibling (" + summary(clickedElem.nextElementSibling) + ")", class: "inert"},
        {onclick: (c => event => {
            editor_model.clickedElem = c;
            editor_model.notextselection = true;
            updateInteractionDiv();
          })(clickedElem.nextElementSibling),
         onmouseenter: (c => () =>  { c.setAttribute("ghost-hovered", "true") })(clickedElem.nextElementSibling),
         onmouseleave: (c => () =>  { c.removeAttribute("ghost-hovered") })(clickedElem.nextElementSibling)
        });
      }
      if(!model.selectionRange && clickedElem && clickedElem.children && clickedElem.children.length > 0) {
        addModifyMenuIcon(`<svg class="context-menu-icon" width="40" height="30">
            <path d="M 28,22 27,19 30,19 M 33,23 27,19 M 8,20 11,19 11,22 M 7,24 11,19 M 10,6 11,9 8,10 M 28,6 27,9 30,10 M 33,6 27,9 M 6,6 11,9 M 5,15 5,10 M 5,25 5,20 M 15,25 10,25 M 25,25 20,25 M 35,25 30,25 M 35,15 35,20 M 35,5 35,10 M 25,5 30,5 M 15,5 20,5 M 5,5 10,5 M 12,10 26,10 26,18 12,18 Z"/></svg>`,
              {title: "Select first cFhild (" + summary(clickedElem.children[0]) + ")", "class": "inert"},
              {onclick: (c => event => {
                let firstChild = c;
                if(firstChild.tagName === "TBODY" && firstChild.children && firstChild.children.length > 0) firstChild = firstChild.children[0];
                editor_model.clickedElem = firstChild;
                editor_model.notextselection = true;
                updateInteractionDiv()})(clickedElem.children[0]),
                onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(clickedElem.children[0]),
                onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(clickedElem.children[0])
               }
            );
      }

      interactionDiv.append(el("br"));

      if(clickedElem) {
        interactionDiv.append(el("div", {"class": "tagname-summary"}, [
          el("input", {"id":"newTagName", "class": "inline-input", "type":"text", value: clickedElem.tagName.toLowerCase(), title:"This element's tag name"}, [], { onkeyup() {
            document.querySelector("#applyNewTagName").classList.toggle("visible", this.value !== this.getAttribute("value") && this.value.match(/^\w+$/));
          }}),
          el("span", {"class": "tagname-info"}, textPreview(clickedElem, 50))
          ]));
      }

      interactionDiv.append(el("input", {"type": "button", id: "applyNewTagName", value: "Apply new tag name"}, [], {onclick() {
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
      }}));
        
      let keyvalues = el("div", {"class":"keyvalues"});
      for(let i = 0; clickedElem && clickedElem.attributes && i < clickedElem.attributes.length; i++) {
        let name = clickedElem.attributes[i].name;
        if(name === "ghost-clicked" || name === "ghost-hovered") continue;
        let value = clickedElem.attributes[i].value;
        if(false /*name == "style"*/) {
          // Do something special for styles.
        } else {
          let isHref = name === "href" && clickedElem.tagName === "A";
          keyvalues.append(
            el("div", {"class": "keyvalue"}, [
              el("span", {title: "This element has attribute name '" + name + "'"}, name + ": "),
              el("span", {},
                el("input", {"type": "text", value: value, "id": "image-src-input"},
                  [], {
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
                  })
              ),
              isHref ? el("span", {title: "Go to " + model.link, "class": "modify-menu-icon inert"}, [],
                        {innerHTML: liveLinkSVG(model.link)}): undefined,
              el("div", {"class":"modify-menu-icon", title: "Delete attribute '" + name + "'"}, [], {
                innerHTML: wasteBasketSVG,
                onclick: ((name) => function() {
                  clickedElem.removeAttribute(name);
                  editor_model.clickedElem = clickedElem;
                  updateInteractionDiv();
                })(name)
              })
              ]
            ));
        }
      }

      let highlightsubmit = function() {
        let attrName = this.parentElement.parentElement.querySelector("[name=name]").value;
        this.parentElement.parentElement.querySelector("button").disabled =
          attrName === "" || attrName.trim() !== attrName
      }

      if(clickedElem && clickedElem.nodeType === 1) {
        // interactionDiv.append(el("div", {}, "Add an attribute:"));
        keyvalues.append(
          el("div", {"class": "keyvalue keyvalueadder"}, [
            el("span", {}, el("input", {"type": "text", placeholder: "key", value: "", name: "name"}, [], {onkeyup: highlightsubmit})),
            el("span", {}, el("input", {"type": "text", placeholder: "value", value: "", name: "value"}, [], {onkeyup: highlightsubmit})),
            el("div", {"class":"modify-menu-icon", title: "Add this name/value attribute"}, [], {innerHTML: plusSVG,
              disabled: true,
              onclick() {
                clickedElem.setAttribute(
                  this.parentElement.querySelector("[name=name]").value,
                  this.parentElement.querySelector("[name=value]").value
                );
                editor_model.clickedElem = clickedElem;
                updateInteractionDiv();
              }
            })
          ])
        );
      }

      function uploadImagesAtCursor(files, srcName) {
        for (var i = 0, file; file = files[i]; i++) {
          var targetPathName =  editor.getStorageFolder(file) + file.name;
          editor.uploadFile(targetPathName, file, (targetPathName, file) => {
            document.getElementById("image-src-input").setAttribute("value", file.name);
            clickedElem.setAttribute("src", targetPathName);
          });
        }

        // refresh images list
        showListsImages(srcName);

        // automatically select upload image
        let selectedImage = document.querySelectorAll(".imgFolder");
        for (let i = 0; i < selectedImage.length; ++i) {
          if (selectedImage[i].getAttribute("src") === files[files.length - 1]) {
            // selectedImage[i].style.outline = "2px solid white";
            selectedImage[i].classList.add("highlight-select-image");
          } else {
            // selectedImage[i].style.outline = "none";
            selectedImage[i].classList.remove("highlight-select-image");
          }
        }
      }
      
      function showListsImages(srcName) {
        srcName = relativeToAbsolute(srcName)
        let dir = "";
        for(let i = 0, arr = srcName.split(/\\|\//); i < arr.length - 1; ++i) {
          dir += (arr[i] + "/");
        }
        files = editor.fs.listdir(dir);
        
        images = [];
        files.forEach(file => {
          let ext = file.split('.').pop().toLowerCase();
          if (ext == 'jpeg' || ext == 'jpg' || ext == 'png' || ext == 'gif') {
            images.push(file);
          }
        });
        // init: clear image list
        let selectedImage = document.querySelectorAll(".imgFolder");
        selectedImage.forEach(e => e.remove());

        for (let i = 0; i < images.length; ++i) {
          interactionDiv.append(
            el("div", { class: "imgFolder" }, el("img", { "src": dir + images[i], "title": images[i], "alt": images[i] },  [], {}), {
              onclick() {
                // highlight the selected image
                let otherImages = document.querySelectorAll(".imgFolder");
                for (let i = 0; i < otherImages.length; ++i) {
                  otherImages[i].classList.remove("highlight-select-image");
                  // otherImages[i].style.outline = "none";
                }
                // replace image
                clickedElem.setAttribute("src", this.children[0].getAttribute("src"));
                document.getElementById("image-src-input").setAttribute("value", this.children[0].getAttribute("src"));
                // this.style.outline = "2px solid white";
                this.classList.add("highlight-select-image");
              }
            })
          );
        }
      }
      
      interactionDiv.append(keyvalues);

      if (clickedElem && clickedElem.tagName === "IMG") {
        let srcName = clickedElem.attributes[0].value;

        clickedElem.ondragover = function (e) {
          e.preventDefault();
        }

        clickedElem.ondrop = function (e) {
          // upload and replace the image
          e.stopPropagation();
          e.preventDefault();
          var files = e.dataTransfer.files; // FileList object
          uploadImagesAtCursor(files, srcName);
        }

        // upload image button
        interactionDiv.append(
          el("a", 
            { "id": "upload-image-btn-a" }, 
            el(
              "input", {"id": "upload-image-btn-input", "type": "file", value: "Please upload images..."}, 
              [], 
              { onchange: function(evt) { uploadImagesAtCursor(evt.target.files, srcName); }}
            ), 
            {}
          )
        );
        
        // show lists of images in selected image's folder
        showListsImages(srcName);
      }

      //interactionDiv.append(el("hr"));

      if(clickedElem && (clickedElem.tagName === "SCRIPT" || clickedElem.tagName === "STYLE" || clickedElem.tagName === "TITLE")) {
        interactionDiv.append(el("hr"));
        interactionDiv.append(el("textarea", {style: "width:100%; height:50%"},
          [], {
            value: clickedElem.childNodes[0].textContent,
            onkeyup: function () { clickedElem.childNodes[0].textContent = this.value; }
          }));
      }
      }
      
      
      // What to put in context menu?
      contextMenu.innerHTML = "";
      let numButtons = 0;
      let addContextMenuButton = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("context-menu-button");
        button.innerHTML = innerHTML;
        contextMenu.append(button);
        numButtons++;
      }
      if(model.link) {
        addContextMenuButton(liveLinkSVG(linkToEdit(model.link)),
          {title: "Go to " + model.link, "class": "inert"});
      }
      if(!model.selectionRange && clickedElem && clickedElem.previousElementSibling && reorderCompatible(clickedElem.previousElementSibling, clickedElem)) {
        addContextMenuButton(`<svg class="context-menu-icon fill" width="40" height="30">
          <path d="m 10,14 3,3 4,-4 0,14 6,0 0,-14 4,4 3,-3 L 20,4 Z"/></svg>`,
        {title: "Move selected element up"},
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
      if(!model.selectionRange && clickedElem && clickedElem.nextElementSibling && reorderCompatible(clickedElem, clickedElem.nextElementSibling)) {
        addContextMenuButton(`<svg class="context-menu-icon fill" width="40" height="30">
          <path d="m 10,17 3,-3 4,4 0,-14 6,0 0,14 4,-4 3,3 -10,10 z"/></svg>`,
        {title: "Move selected element down"},
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
        addContextMenuButton(`<svg class="context-menu-icon" width="40" height="30">
            <path d="m 11,4 12,0 0,4 -4,0 0,14 -8,0 z" />
            <path d="m 19,8 12,0 0,18 -12,0 z" /></svg>`,
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
              c.remove();
              editor_model.clickedElem = undefined;
              updateInteractionDiv();
            })(clickedElem)
          });
      }
      if(model.selectionRange && (model.selectionRange.startContainer === model.selectionRange.endContainer || model.selectionRange.startContainer.parentElement === model.selectionRange.commonAncestorContainer && model.selectionRange.endContainer.parentElement === model.selectionRange.commonAncestorContainer)) {
        // There should be different ways to wrap the selection:
        // a href, span, div.
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
              editor_model.insertElement = true;
              updateInteractionDiv();
              var sel = window.getSelection();
              sel.removeAllRanges();
              var range = document.createRange();
              range.setStart(model.caretPosition.startContainer, model.caretPosition.startOffset);
              range.setEnd(model.caretPosition.endContainer, model.caretPosition.endOffset);
              sel.addRange(range);
            }});
      }

      let baseElem = clickedElem;
      while(baseElem && (baseElem.tagName == "SCRIPT" || baseElem.tagName == "STYLE")) {
        baseElem = baseElem.nextElementSibling;
      }
      baseElem = model.selectionRange || baseElem || clickedElem;
      
      if(baseElem) {
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
      }
      return true;

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
      """document.addEventListener('click', onClickGlobal, false);"""
    else "")
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