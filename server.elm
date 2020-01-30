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

{--}
updatecheckpoint name x = {
  apply x = x
  update {input, outputNew, diffs} =
    let _ = Debug.log """Checkpoint @name""" () in  
    Ok (InputsWithDiffs [(outputNew, Just diffs)])
}.apply x

debugcheckpoint name x = let _ = Debug.log name () in x
--}

serverOwned what obj = freezeWhen (not permissionToEditServer) (\od -> """You tried to modify @what, which is part of the server. We prevented you from doing so.<br><br>

If you really intended to modify this, add ?superadmin=true to the URL and redo this operation. This is likely going to create or modify the existing <code>server.elm</code> at the location where you launched Editor.<br><br>

For debugging purposes, below is the new value that was pushed:
<pre>@(Regex.replace "<" (always "&lt;") """@od""")</pre>
Here is the old value that was computed
<pre>@(Regex.replace "<" (always "&lt;") """@obj""")</pre>
""") obj


preludeEnv =  __CurrentEnv__

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
varraw = boolVar "raw" False
varedit = boolVar "edit" False || varraw
varls = boolVar "ls" False
varclearhydecache = boolVar "clearhydecache" False
varhydeNotDisabled = boolVar "hyde" True
defaultVarEdit = listDict.get "edit" defaultOptions |> Maybe.withDefault False
varproduction = listDict.get "production" defaultOptions |> Maybe.withDefault (freeze False)
iscloseable = listDict.get "closeable" defaultOptions |> Maybe.withDefault (freeze False)
varFast = boolVar "fast" False

userpermissions = {pageowner= True, admin= varadmin}
permissionToCreate = userpermissions.admin
permissionToEditServer = boolVar "superadmin" False -- should be possibly get from user authentication
-- List.contains ("sub", "102014571179481340426") user -- That's my Google user ID.

canEditPage = userpermissions.pageowner && varedit && not varls

freezeWhen = Update.freezeWhen

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

mbDotEditorFile = 
  let prefix = Regex.extract "^(.*/)[^/]*$" path |> Maybe.map (\[prefix] -> prefix) |> Maybe.withDefault "" in
  let dotEditor = prefix +  ".editor" in
  fs.read dotEditor

applyDotEditor source = 
  let prefix = Regex.extract "^(.*/)[^/]*$" path |> Maybe.map (\[prefix] -> prefix) |> Maybe.withDefault "" in
  let dotEditor = prefix +  ".editor" in
  case mbDotEditorFile of
    Nothing -> source
    Just modifier ->
      case __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::("content", source)::preludeEnv) modifier of
        Err msg -> let _ = Debug.log ("Error while executing " + dotEditor + " : " + msg) () in
          source
        Ok newSource -> newSource

isTextFile path =
  Regex.matchIn """\.(?:txt|css|js|sass|scss)$""" path

---------------------
-- Hyde file support.
-- Everything unrolled on the main let definitions to benefit from caching
---------------------

(withoutPipeline, hydefilepath, hydefileSource) = case hydefilecache of
  Just {file=hydefile} ->
    case if varclearhydecache then Nothing else hydefilecache of
      Just {cacheContent} ->
        case evaluate cacheContent of
          {inputFiles, outputFiles} ->
            (not (List.isEmpty outputFiles) &&
             List.find (\e -> e == path || e == "/" + path) inputFiles == Nothing &&
             List.find (\e -> e == path || e == "/" + path) outputFiles == Nothing,
               hydefile, fs.read hydefile)
          _ -> (False, hydefile, fs.read hydefile)
      _ -> (False, hydefile, fs.read hydefile) -- Need to recompute the cache anyway
  _ -> (True, "", Nothing)

(folderView, mbSourcecontentAny1, hydeNotNeeded): (Boolean, Maybe String, Boolean)
(folderView, mbSourcecontentAny1, hydeNotNeeded) =
  if path == "server.elm" then
    (False, Just """<html><head></head><body class="editor-error">The Elm server cannot display itself. This is a placeholder</body></html>""", True)
  else if fs.isdir path then
    (True, Just "", True)
  else if fs.isfile path && Regex.matchIn """\.(png|jpg|ico|gif|jpeg)$""" path then -- Normally not called because server.js takes care of these cases.
    (False, Just """<html><head><meta name="viewport" content="width=device-width, minimum-scale=0.1"><title>@path</title></head><body style="margin: 0px; background: #0e0e0e;"><img style="-webkit-user-select: none;margin: auto;" src="@path"></body></html>""", True)
  else if hydefilecache == Nothing || withoutPipeline || not varhydeNotDisabled then
    (False, fs.read path, True)
  else
    (False, Nothing, False)

hyde_sourceRaw = if hydeNotNeeded then "" else
  hydefileSource |> Maybe.withDefaultLazy (\_ -> """all = [Error "hyde file '@hydefilepath' not found?!"]""")

hyde_source = if hydeNotNeeded then "" else
  hyde_sourceRaw + Update.freeze "\n\nlet t = " + (listDict.get "task" vars |> Maybe.withDefault "all") + "\n    t = if typeof t == 'function' then t () else t\n    t = if typeof t == 'list' then t else [t]\nin t"

hyde_fileDirectory = if hydeNotNeeded then "" else
  Regex.replace "/[^/]*$" "" hydefilepath

hyde_inDirectory name =
  if hyde_fileDirectory == "" then name else
    hyde_fileDirectory  + "/" + name

hyde_fs = 
  if hydeNotNeeded then {} else
  let hyde_fsReadRecord = 
        { directReadFileSystem |
          read name =
            let name = hyde_inDirectory name in
            let _ = recordFileRead name in
            fs.read name,
          listdir name =
            let name = hyde_inDirectory name in
            let _ = recordFolderList name in
            fs.listdir name
        } in
  nodejs.delayedFS hyde_fsReadRecord <|
  Update.lens {
    apply = identity
    update {outputNew, diffs} = -- input and outputOld were empty, diffs is valid
    -- We just need to change the paths
      outputNew |>
      List.map (case of
        (name, Rename newName) -> (hyde_inDirectory name, Rename (hyde_inDirectory newName))
        (name, x) -> (hyde_inDirectory name, x))|>
      flip (,) (Just diffs) |>
      List.singleton |> InputsWithDiffs |> Ok
  } fileOperations

-- A Hyde plug-is a function that takes two arguments
--   options (as a list, object or tuple)
--   a list of Write
-- Returns a list of Write
plugin name =
  fs.read (hyde_inDirectory ("hydefile-plugin-" + name + ".leo")) |>
  Maybe.andThen (\plugin_content ->
    case __evaluate__ (("fs", hyde_fs)::("plugin", plugin)::preludeEnv) plugin_content of
      Ok x -> Just x
      Err msg -> 
        let _ = Debug.log ("loading of plugin " + name + " failed:" + msg) () in
        Nothing
  ) |> Maybe.withDefaultLazy (\_ ->
   let _ = Debug.log ("plugin " + name + " not found") () in
   \options -> identity)

hyde_resEvaluatedHydeFile =
  if hydeNotNeeded then {} else
  __evaluateWithCache__ (("fs", hyde_fs)::("plugin", plugin)::preludeEnv) hyde_source

(hyde_generatedFilesDict, hyde_errors) =
  if hydeNotNeeded then (False, False) else
  case hyde_resEvaluatedHydeFile of
    Err msg -> ([], msg)
    Ok (writtenContent, _) ->
      let (written, errors) = List.partition (case of Write -> True; _ -> False) writtenContent in
      let tuplesToWrite =
            List.map (case of Write name content -> (hyde_inDirectory name, content)) written
          joinedErrors = 
            List.map (case of Error msg -> msg) errors |> String.join "\n"
      in
      (tuplesToWrite, joinedErrors)

hyde_record_output_files =
  if hydeNotNeeded then False else
  let hyde_dummy = recordOutputFiles hyde_generatedFilesDict in  -- Writes on disk 
  False

hyde_dummy = if hydeNotNeeded then (False, False) else
  let x = hyde_generatedFilesDict in -- to make sure dependency is executed
  if hyde_errors == "" then
    cacheResult () -- Caches the names of written files
  else 
    ()

mbSourcecontentAny =
  if hydeNotNeeded then mbSourcecontentAny1 else
  case listDict.get ("/" + path) hyde_generatedFilesDict of
    Nothing ->
      case listDict.get path hyde_generatedFilesDict of
        Nothing ->
          if hyde_errors == "" then
            let _ = Debug.log """Unable to read (/)@path from output of hydefile""" () in
            fs.read path
          else
            Just <|
            serverOwned "error recovery of hyde build tool" <|
            """<html><head></head><body class="editor-error"><h1>Error while resolving the generated version of @path</h1><pre>@hyde_errors</pre></body></html>"""
        x -> x
    x -> x

---------------------------------
-- Hyde file support ends here --
---------------------------------

sourcecontentAny = Maybe.withDefaultReplace (
    serverOwned "404 page" (if isTextFile path then
           if permissionToCreate then freeze """@path does not exist yet. Modify this page to create it!""" else """Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)"""
        else """<html><head></head><body class="editor-error">@(
          if permissionToCreate then freeze """<span>@path does not exist yet. Modify this page to create it!</span>""" else """<span>Error 404, @path does not exist or you don't have admin rights to modify it (?admin=true)</span>"""
            )</body></html>""")
  ) mbSourcecontentAny

sourcecontent = String.newlines.toUnix sourcecontentAny

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
              _ ->
            case Regex.extract """\s*([\$\w_]+\s*=\s*)([\s\S]+?)\s*;(?=\r?\n)""" code of
              Just [varNameEqual,toAssign] -> "\n" + varNameEqual + phpStringToElmString toAssign
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
evaluatedPage: Result String (List HtmlNode)
evaluatedPage = 
  if canEvaluate /= "true" then
    Ok [<html><head></head><body class="editor-error">URL parameter evaluate=@(canEvaluate) requested the page not to be evaluated</body></html>]
  else if isTextFile path || varraw then
    Ok [<html style="height:100%;">
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
            var mode = editor.config.path.match(/\.js$/) ? "ace/mode/javascript" :
                       editor.config.path.match(/\.html?$/) ? "ace/mode/html" :
                       editor.config.path.match(/\.css$/) ? "ace/mode/css" :
                       editor.config.path.match(/\.json$/) ? "ace/mode/json" :
                       editor.config.path.match(/\.leo$/) ? "ace/mode/elm" :
                       editor.config.path.match(/\.elm$/) ? "ace/mode/elm" :
                       editor.config.path.match(/\.php$/) ? "ace/mode/php" :
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
        <body style="height:100%">
        <div id="aceeditor" list-ghost-attributes="class draggable style" children-are-ghosts="true"
          save-ghost-attributes="style ghost-anchor-column ghost-anchor-row ghost-lead-column ghost-lead-row" initdata=@sourcecontent></div>
        <script>
        editor.ghostNodes.push(node =>
          node.tagName === "SCRIPT" && node.getAttribute("src") && node.getAttribute("src").match(/mode-(.*)\.js|libs\/ace\/.*\/ext-searchbox.js/)
        );
        
        var script = document.createElement('script');
        script.src = 'https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.6/ace.js';
        script.async = false;
        script.setAttribute("isghost", "true");
        ace = undefined;
        document.head.appendChild(script);
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
        </html>]
  else 
  let isPhp = Regex.matchIn """\.php$""" path in
  let isHtml = Regex.matchIn """\.html?$""" path in
  if isHtml || isPhp then
    let sourcecontent = if isHtml then applyDotEditor sourcecontent else
      let elmSourceContent = phpToElmFinal path sourcecontent in
      __evaluate__ (("$_GET", vars)::("$_SERVER", [("SCRIPT_NAME", "/" + path)])::("path", path)::("fs", fs)::preludeEnv) elmSourceContent |>
      case of
        Err msg -> serverOwned "error message" "<html><head></head><body class="editor-error"><pre>Error elm-reinterpreted php: " + Regex.replace "<" "&lt;" msg + "</pre>Original computed source <pre>" +
          Regex.replace "<" "&lt;" elmSourceContent +
          "</pre></body></html>"
        Ok sourcecontent -> applyDotEditor sourcecontent
    in
    let interpretableData = serverOwned "begin raw tag" "<raw>" + sourcecontent + serverOwned "end raw tag" "</raw>" in
    __evaluate__ preludeEnv interpretableData |>
    Result.andThen (case of
      ["raw", _, nodes] -> Ok nodes
      result -> Err """Html interpretation error: The interpretation of raw html did not work but produced @result"""
    )
  else if Regex.matchIn """\.md$""" path then
    let markdownized = String.markdown sourcecontent in
      case Html.parseViaEval markdownized of
        x -> 
          let markdownstyle = fs.read "markdown.css" |> Maybe.withDefaultReplace defaultMarkdowncss in
          Ok [<html><head></head><body><style title="If you modify me, I'll create a custom markdwon.css that will override the default CSS for markdown rendering">@markdownstyle</style><div class="wrapper">@x</div></body></html>]
  else if Regex.matchIn """\.(elm|leo)$""" path then
    let res = __evaluate__ (("vars", vars)::("path", path)::("fs", fs)::preludeEnv) sourcecontent in
    case res of
      ["html", _, _] -> [res]
      _ -> res
  else if folderView then
    Ok [<html><head>
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
      el = editor.el;
      var fullListDir = (path) => JSON.parse(editor._internals.doReadServer("fullListDir", path));
      var thisListDir = fullListDir(editor.config.path);
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
          const doit = window.confirm("Are you sure you want to overwrite an existing file with the name " + newname + "?");
          if (!doit) return;
        }
        var x = editor._internals.doWriteServer("rename", editor.config.path + sel.id, editor.config.path + newname);
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
              editor._internals.doWriteServer("rmdir", editor.config.path + selected[i].id); //does this work on non-empty stuff? idts....
              continue;
            }
            editor._internals.doWriteServer("unlink", editor.config.path + selected[i].id);
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
        var contents = editor._internals.doReadServer("read", editor.config.path + sel.id);
        if (contents[0] != "1") {
          window.alert ("Couldn't read the file for some reason. aborting.");
          console.error ("couldn't read the file for some reason. aborting.");
          return;
        }
        contents = contents.substring(1, contents.length);
        var resp = editor._internals.doWriteServer("create", editor.config.path + newname, contents);
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
          const conf = window.confirm ("Are you sure you want to overwrite a folder with the name " + newname + " with an empty file? This would delete the folder.");
          if (!conf) return;
        }
        editor._internals.doWriteServer("mkdir", newname, "");
        goodReload();
      }
      function moveFs() {
        var btn = getOneFile("To move files or folders");
        if (!btn) return;
        if (btn.id == "..") {
          window.alert("Can't change the up dir");
          return;
        }
        var newpath = window.prompt("New path to file (relative to root of server):", "");
        if (newpath == null) return;
        if (newpath[newpath.length -1] != "/") {
          newpath = newpath + "/";
        }
        try {
          var nldir = fullListDir(newpath);
          if (isDupInFolder(nldir, btn.id)) {
            const conf = window.confirm("Are you sure you want to overwrite an existing file?");
            if (!conf) return;
          }
        } catch (e) {
          window.alert ("The path specified does not exist. Move cancelled.");
          return;
        }
        console.log ("move approved");
        var oldloc = (editor.config.path + btn.id);
        var newloc = newpath == "/" ? btn.id : (newpath + btn.id);
        console.log ("renamimg\n%s\n%s", (editor.config.path + btn.id), (newpath + btn.id));
        editor._internals.doWriteServer("rename", oldloc, newloc); 
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
      
      // Returns a progress bar or reuses the existing one.
      function initializeProgress() {
        var progressBar = document.getElementById("progress-bar");
        if (!progressBar) {
          progressBar = el("progress", {id:"progress-bar", max:"100", value:"0", visible:false}, [], {isghost: true});
        }
        progressBar.value = 0;
        progressBar.visible = true;
        return progressBar;
      }

      var handleFiles = (files) => {
        var pgbr = document.getElementById("forprog");
        var progbar = initializeProgress();
        pgbr.append(progbar);
        var uploadProgress = {};
        var didUp = false;
        ([...files]).forEach((fl) => {
          var fileName = editor.config.path + fl.name;
          uploadProgress[fileName] = 0;
        });
        var callbackUpload = function (fileName, file, percent) {
          uploadProgress[fileName] = typeof percent == "number" ? percent : 100;
          let total = 0;
          let count = 0;
          for(var i in uploadProgress) {
            total += uploadProgress[i]
            count++;
          }
          progbar.value = total / count;
          if(total == 100) {
            progbar.visible = false;
            if (didUp) {
              goodReload();
              pgbr.innerHTML = "";
            }
          }
        }
        ([...files]).forEach((fl) => {
          var fileName = editor.config.path + fl.name;
          editor.uploadFile(fileName, fl,
            callbackUpload,
            (err) => {
              pgbr.innerHTML = "";
              console.err(err);
            },
            callbackUpload);
          didUp = true;
        });
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
        form.innerHTML = "";
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

        var fileItemDisplay = function(name, isDir) {
           let newURL = name == ".." ?
                    editor.config.path.replace(/(\/|^)[^\/]+\/?$/, "")
                  : editor.config.path + "/" + name;
           var link = typeof isDir == "boolean" ? (isDir ? newURL + "/?ls" : newURL + "?edit") : name;
           if(link.length > 0 && link[0] != "/") link = "/" + link;
           return el("div", {class:"file-item"}, [
              el("input", getRecordForCheckbox(name), ""),
              el("label", {for:name, value:name}, [ 
                isDir ? dirIcon() : extensionIcon(name),
                el("a", {href:link}, name, {onclick: function(event) {
                  event.preventDefault();
                  if(isDir) {
                    window.history.pushState({localURL: location.href}, name, link);
                    editor.config.path = newURL;
                    goodReload();
                  } else {
                    editor._internals.doReloadPage(link);
                  }
                }})])]);
        }
        /*var otherItemDisplay = function(link, name) {
           return el("div", {class:"file-item"}, [
              el("input", getRecordForCheckbox(name), ""),
              el("label", {for:name, value:name}, [ 
                extensionIcon(name),
                el("a", {href:link}, name)
                ])
              ]);
        }*/
        //el(tag, attributes, children, properties)
        if (editor.config.path != "") {
          var link = "../" + "?ls";
          form.append(fileItemDisplay("..", true));
        }
        // directories before files, sorted case-insensitive
        files.sort(([name1, isDir1], [name2, isDir2]) =>
          isDir1 && !isDir2 ? -1 : isDir2 && !isDir1 ? 1 :
          name1.toLowerCase() < name2.toLowerCase() ? -1 : 0);
        for (i = 0; i < files.length; i++) {
          var [name, isDir] = files[i];
          let extension = name.replace(/^(?:(?!\.(?=[^\.]*$)).)*\.?/, "");
          if("." + extension == name || extension === "") extension = "-";
          const img_exts = ["jpeg", "jpg", "png", "svg", "tiff", "tif", "gif", "pdf"]
          const is_img = img_exts.includes(extension.toLowerCase());
          if (!is_img) {
            if (isDir) {
              form.append(fileItemDisplay(name, isDir))
            } else {
              form.append(fileItemDisplay(name, isDir));
            }
          } else {
            form.append(fileItemDisplay(name));
          }
        }

        form.append(el("input", {type:"file", id:"fileElem", onchange:"handleFiles(this.files)"}, [], {}));
      }
      loadFileList();
      var goodReload = () => {
        document.getElementById("fileListing").innerHTML = "";
        thisListDir = fullListDir (editor.config.path);
        loadFileList();
      }
    window.addEventListener('drop', handleDrop, false);
    window.addEventListener('dragover', (e) => e.preventDefault(), false);
    </script></body></html>]
  else 
    Ok [<html><head></head><body class="editor-error">
      <p>Editor cannot open file because it does not recognize the extension.</p>
      <p>As an alternative, you can open the file in raw mode by appending <code>?raw</code> to it.</p>
      <button onclick="""
        location.search = location.search + (location.search == "" ? "?raw" : "&raw");
      """>Open @path in raw mode</button>
    </body></html>]

{---------------------------------------------------------------------------
 Recovers from evaluation errors
 Recovers if page does not contain an html tag or a body tag
----------------------------------------------------------------------------}
recoveredEvaluatedPage: List HtmlNode
recoveredEvaluatedPage = --updatecheckpoint "recoveredEvaluatedPage" <|
  case evaluatedPage of
  Err msg -> serverOwned "Error Report" <|
    [<html><head></head><body style="color:#cc0000" class="editor-error"><div style="max-width:600px;margin-left:auto;margin-right:auto"><h1>Error report</h1><button onclick="editor.reload();" title="Reload the current page">Reload</button><pre style="white-space:pre-wrap">@msg</pre></div></body></html>]
  Ok nodes ->
    let hasChildTag theTag nodes = List.any (case of [tag, _, _] -> tag == theTag; _ -> False) nodes
        recoverHtmlChildren nodes =
          let hasHead = hasChildTag "head" nodes
              hasBody = hasChildTag "body" nodes
          in
          if hasHead && hasBody then nodes else
          let startBodyIndex = List.indexWhere (
                 case of
                   [tag, _, _] -> tag /= "title" && tag /= "link" && tag /= "meta" && tag /= "script" && tag /= "style" && tag /= "base" && tag /= "isindex" && tag /= "nextid" && tag /= "range" && tag /= "head"
                   ["TEXT", x] -> not (Regex.matchIn """^\s*$""" x)
                   ["COMMENT", _] -> False
               ) nodes in
          case List.split startBodyIndex nodes of
              ((["head", _, _] as head)::whitespace, bodyElems) -> head :: (whitespace ++ [["body", [], bodyElems]])
              (headElems, (["body", _, _] as body) :: whitespace) -> [["head", [], headElems], body]
              (headElems, bodyElems) -> [["head", [], headElems], ["body", [], bodyElems]]
        recoverHtml nodes =  nodes |> List.mapWithReverse identity (case of
          ["html", attrs, children] -> ["html", attrs, recoverHtmlChildren children]
          x -> x
        )
    in
    if hasChildTag "html" nodes then
      recoverHtml nodes
    else -- We need to wrap nodes with html, title, links, consecutive style and script and empty text nodes
      let aux nodes = case nodes of
        (["TEXT", x] as head) :: rest ->
          if Regex.matchIn """^\s*$""" x then
            head :: aux rest
          else recoverHtml [["html", [], nodes]]
        (["COMMENT", x] as head) :: rest ->
          head :: aux rest
        ([tag, _, _] as head) :: rest ->
           if tag == "!DOCTYPE" then
             head :: aux rest
           else recoverHtml [["html", [], nodes]]
      in aux nodes

jsEnabled = boolVar "js" True

removeJS node = case node of
  [text, content] -> node
  [tag, attrs, children] ->
    if tag == "script" then [tag, [], [["TEXT", "/*Script disabled by Thaditor*/"]]] else
    [tag, attrs, List.map removeJS children]
  _ -> []

{-
-- On reverse, all ghost elems will have disappeared. We reinsert them.
prependGhosts ghostElems = update.lens {
  apply elems = ghostElems ++ elems
  update _ = Ok (InputsWithDiffs [(elems, VListDiffs [(0, ListElemInsert)
} finalElems
-}

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
main: List HtmlNode
main =
  --updatecheckpoint "main" <|
  let filteredMainPage = List.filter (case of -- Remove text nodes from top-level document.
        ["TEXT", _] -> False
        _ -> True
      ) recoveredEvaluatedPage in
  List.mapWithReverse identity (case of
    ["html", htmlattrs, htmlchildren] -> ["html", htmlattrs, htmlchildren |>
      (let removeAfterBody l = case l of (["body", _, _] as head) :: _ -> [head]; a :: b -> a :: removeAfterBody b
           removeBeforeHead l = case l of ["head", _, _] :: _ -> l; a :: b -> removeBeforeHead b; _ -> l in
       removeBeforeHead >> removeAfterBody) |>
      List.mapWithReverse identity (case of
        ["head", headattrs, headChildren] ->
          let headChildren =  if jsEnabled then headChildren else List.map removeJS headChildren in
          ["head", headattrs, if varFast then headChildren else
             insertThereInstead identity True headChildren ++  -- All new nodes added to the beginning of the head are added back to headChildren.
             serverOwned "initial script and style " initialScript ++ headChildren]
        ["body", bodyattrs, bodyChildren] ->
          let bodyChildren = if jsEnabled then bodyChildren else List.map removeJS bodyChildren in
          ["body", bodyattrs, bodyChildren]
        x -> x -- anything else, i.e. comments or text nodes between end of head and start of body.
      )]
    x -> x
  ) filteredMainPage --|> Update.debug "main"

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

boolToCheck = Update.bijection (case of "true" -> [["checked", ""]]; _ -> []) (case of [["checked", ""]] -> "true"; _ -> "false")

initialScript = serverOwned "initial script" <| [
  <script id="thaditor-vars" class="editor-interface">
     editor = typeof editor == "undefined" ? {} : editor;
     editor.config = typeof editor.config == "undefined" ? {} : editor.config;
     editor.config.EDITOR_VERSION = typeof EDITOR_VERSION === "number" ? EDITOR_VERSION : 0;
     editor.config.path = @(jsCode.stringOf path);
     editor.config.varedit = @(if varedit then "true" else "false");
     editor.config.varls = @(if varls then "true" else "false");
     editor.config.askQuestions = @(case listDict.get "question" vars of
                       Just questionattr -> "true"
                       _ -> if boolVar "question" True then "true" else 'false');
     editor.config.autosave = @(case listDict.get "autosave" vars of
                      Just autosaveattr -> "true"
                      _ -> if boolVar "autosave" True then "true" else "false");
     editor.config.canEditPage = @(if canEditPage then "true" else "false");
     editor.config.editIsFalseButDefaultIsTrue = @(if varedit == False && (listDict.get "edit" defaultOptions |> Maybe.withDefault False) == True then "true" else "false");
     // Are we using Thaditor?
     editor.config.thaditor = typeof thaditor !== "undefined";
     // User name, if defined. Used to create personalized temporary CSS files.
     editor.config.userName = typeof userName === "string" ? userName : "anonymous";
   </script>,
-- The following is replaced by an inline <script> for when Editor runs as a file opener.
   <script id="thaditor-luca" class="editor-interface" src="/server-elm-script.js"></script>,
-- The following is replaced by an inline <style> for when Editor runs as a file opener.
-- And the path is modified when Editor runs as Thaditor.
<link rel="stylesheet" type="text/css" href="/server-elm-style.css" class="editor-interface" id="server-elm-style">
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

["#document", [], main]