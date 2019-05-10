url = listDict.get "url" vars |> Maybe.withDefaultReplace (freeze "http://nodejs.org/dist/index.json")

proxyurl = freeze "http://127.0.0.1:3000"

diffs = listDict.get "diffs" vars
newOutput = listDict.get "newOutput" vars

-- Up to three arguments for datatypes
toJSExp x =
  let aux i x = case x of
    { $d_ctor= x, args= {_1=a1,_2=a2, _3=a3}} ->
      jsCode.datatypeOf """@(x)@(i)""" x [
        aux (i*10+1) a1,
        aux (i*10+2) a2,
        aux (i*10+3) a3]
    { $d_ctor= x, args= {_1=a1,_2=a2}} ->
      jsCode.datatypeOf """@(x)@(i)""" x [
        aux (i*10+1) a1,
        aux (i*10+2) a2]
    { $d_ctor= x, args= {_1=a1}} ->
      jsCode.datatypeOf """@(x)@(i)""" x [
        aux (i*10+1) a1]
    { $d_ctor= x, args= {}} ->
      jsCode.datatypeOf """@(x)@(i)""" x []
    [] -> "[]"
    a :: b -> let auxList acc l = case l of
      [] -> acc + "]"
      head::tail ->
        if acc == "[" then auxList (acc + aux (10*i+1) head) tail
        else auxList (acc + "," + aux (10*i+1) head) tail
      in auxList "[" x
    True -> "true"
    False -> "false"
    x -> if typeof x == "string" then jsCode.stringOf x else """@x""" in
  aux 0 x

content = Update.lens {
  apply (diffs, newOutput) = 
    let oldOutput =
      fs.read url |> Maybe.withDefault """hu ? not found"""
    in
  case (newOutput) of
    Nothing -> oldOutput
    Just newOutput ->
      """<html><head><title>Tharzen Editor Proxy</title></head>
      <body><h1>This was just a test!</h1>
      <p>The Tharzen Editor Proxy was able to find out where to propagate the changes you saved in the HTML that it received from the  website @(url).<br>
      Here are the changes:</p>
      <pre>@(Regex.replace "\\(\\+\\+\\+" "<span class='add'>" <| 
             Regex.replace "\\+\\+\\+\\)" "</span>" <|
             Regex.replace "\\(---" "<span class='remove'>" <| 
             Regex.replace "---\\)" "</span>" <|
             Regex.replace "<" "&lt;" <| __jsEval__ <| """
      function makeSummary_(oldString, newString, stringDiffs) {
          if(stringDiffs["$d_ctor"] == "Nothing") return "[No change observed]";
          if(stringDiffs["$d_ctor"] == "Just") stringDiffs = stringDiffs.args._1;
          var listStringDiffs = stringDiffs.args._1; // It's a VStringDiffs
          var offset = 0;
          var summary = "";
          for(var i = 0; i < listStringDiffs.length; i++) {
            var {args: {_1: start, _2: end, _3: replaced}} = listStringDiffs[i];
            var removed = oldString.substring(start, end);
            var inserted = newString.substring(start + offset, start + offset + replaced);
            var beforeRemoved = oldString.substring(0, start);
            var afterRemoved = oldString.substring(end);
            var linesBeforeRemoved = beforeRemoved.split(/\r?\n/);
            var lineNumber = linesBeforeRemoved.length;
            var charNumber = linesBeforeRemoved[linesBeforeRemoved.length - 1].length + 1;
            summary += "\nLine " + lineNumber + ", Column " + charNumber + ":" +
              linesBeforeRemoved[linesBeforeRemoved.length - 1] +
                (removed === "" ? "" : "(---" + removed + "---)") +
                (inserted === "" ? "" : "(+++" + inserted + "+++)") + afterRemoved.split(/\r?\n/)[0];
            offset += replaced - (end - start);
          }
          return summary;
        }
        makeSummary_(
        @(toJSExp oldOutput),
        @(toJSExp newOutput),
        @(diffs |> Debug.log "diffs" |> Maybe.map evaluate |> Debug.log "diffs - after map evaluate" |> Maybe.withDefault Nothing|> Debug.log "diffs - before toJSExp" |> toJSExp |> Debug.log "diffs - after toJSExp" ))"""
       )</pre>
      </body>
      </html>"""

  update {input,outputNew,diffs} =
    let _ = Debug.log "outputNew" outputNew in
    let _ = Debug.log "diffs" diffs in
    Ok (Inputs [(Just (toString (Just diffs)), Just outputNew)])
} (diffs, newOutput)

urlencode comp = Regex.replace "[:/]" (\m -> case m.match of
  ":" -> freeze "%3A"
  "/" -> freeze "%2F") comp

simplify href = href |> Regex.replace "(https?://)(.*)" (\{submatches=[prefix, postfix]} ->
  let removerelativeups str = 
        let newStr = Regex.replace "/[^/]*?/\\.\\.(?=/|$)" "" str in
        if newStr /= str then removerelativeups newStr else newStr
  in
  postfix |>
  Regex.replace "/[^/]*?\\.html(?=/)" "" |>
  Regex.replace "/(?:\\.)?(?=/)" "" |>
  removerelativeups |>
  (\res -> prefix + res))

  
isabsolute url = Regex.matchIn "^https?://|^www\\.|^//" url

makeabsolute url relativeurl = 
  simplify (url + "/" + relativeurl)
  
mapLinks node = case node of
  [text, content] -> node
  [tag, attrs, children] ->
    if tag == "head" then
      [tag, attrs, <style>.add { background: lightgreen} .remove {background: #f8a7a7; text-decoration: line-through}</style>::
        List.map mapLinks children]
    else
    if tag == "script" then [tag, [], [["TEXT", "/*Script disabled for edition*/"]]] else
    [tag, List.map (\([name, value] as nv) ->
        if (name == "href" || name == "src" || name == "background") && tag /= "a" then
          if isabsolute value then nv
          else [name, makeabsolute url value]
        else if tag == "a" && name == "href" then
          [name, proxyurl + "/?url=" + urlencode (makeabsolute url value)]
        else nv
      ) attrs, List.map mapLinks children]
  _ -> []

  
case Regex.extract """^\s*<!(?:DOCTYPE|doctype)(?:(?!>)[\s\S])*>([\s\S]*)$""" content |> Maybe.orElse
     (Regex.extract """^[\s\S]*(<(?:html|HTML)[\s\S]*)$""" content) of
  Just [html] -> Html.parseViaEval html |>
    List.find (case of ["html", _, _] as n -> True; _ -> False) |> case of
      Just htmlnode -> mapLinks htmlnode
      _ -> <html><head></head><body>Empty html page</body></html>
  Nothing -> 
<html>
<head>
</head>
<body>
No html node found in @url
</body>
</html>