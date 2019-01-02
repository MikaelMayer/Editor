-- Make sure to install sync-request:
--    npm install -g sync-request
-- Also make sure that `echo $NODE_PATH` points to where npm installs its modules
-- (e.g. `export NODE_PATH=C:\\Users\\*\\AppData\\Roaming\\npm\\node_modules` on Windows)


url = listDict.get "url" vars |> Maybe.withDefaultReplace (freeze "http://nodejs.org/dist/index.json")

proxyurl = freeze "http://127.0.0.1:3000"

content = Update.lens {
  apply () = __jsEval__ """var request = require("sync-request");
var res = request('GET', @(jsCode.stringOf url));
res.getBody("utf8");"""
  update {outputNew} =
    Err <| "The website does not support PUSH to modify this file."
} ()

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

  
isabsolute url = Regex.matchIn "^https?://" url

makeabsolute url relativeurl = 
  simplify (url + "/" + relativeurl)
  
mapLinks node = case node of
  [text, content] -> node
  [tag, attrs, children] ->
    [tag, List.map (\([name, value] as nv) ->
        if (name == "href" || name == "src") && tag /= "a" then
          if isabsolute value then nv
          else [name, makeabsolute url value]
        else if tag == "a" && name == "href" then
          [name, proxyurl + "/?url=" + urlencode (makeabsolute url value)]
        else nv
      ) attrs, List.map mapLinks children]
  _ -> []

case Regex.extract "(<html[\\s\\S]*</html>)" content of
  Just [html] -> Html.parseViaEval html |> case of
    htmlnode :: _ -> mapLinks htmlnode
    _ -> <html><head></head><body>Empty html page</body></html>
  Nothing -> 
<html>
<head>
</head>
<body>
No html node found in @url
</body>
</html>