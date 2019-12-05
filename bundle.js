const fs = require("fs");
const sns = require("sketch-n-sketch");

var res = sns.string.evaluateWithoutCache(`
fs = nodejs.delayedFS nodejs.nodeFS nodejs.nodeFSWrite

elmserver = fs.read "server.elm" |> Maybe.withDefaultLazy (\\_ -> error "server.elm not found")

server_elm_style = fs.read "server-elm-style.css" |> Maybe.withDefaultLazy (\\_ -> error "server-elm-style.css not found") |> Regex.replace "@" (always "@@")

server_elm_script = fs.read "server-elm-script.js" |> Maybe.withDefaultLazy (\\_ -> error "server-elm-script.js not found") |> Regex.replace "@" (always "@@")

elmserver = Regex.replace """<link[^>]*href="/server-elm-style.css"[^>]*>""" (
  \\_ -> "<style>" + server_elm_style + "</style>"
) elmserver

elmserver = Regex.replace """(<script[^>]*)src="/server-elm-script.js"([^>]*>)\s*(</script>)""" (
  \\{submatches=pre::post::close::_} -> pre + post + server_elm_script + close
) elmserver

fs.read "bin/server.js"
|> Maybe.withDefaultLazy (\\_ -> error "bin/server.js not found")
|> Regex.replace "const defaultServerContent = .*;\\\\s*const useDefaultServerContent = false;" (\\_ ->
  """const defaultServerContent = @(jsCode.stringOf elmserver);
const useDefaultServerContent = true;""")
`);

if(res.ctor == "Err") {
  console.log("Error", res._0);
} else {
  var result = sns.valToNative(res._0)._0;
  fs.writeFileSync("bin/server-run.js", "#!/usr/bin/env node\n\n" + result, "utf8");
  fs.writeFileSync("bin/server-npm-package.js", result.replace(/\/\*REMOVE_FOR_NPM_INCLUDE[\s\S]*END_REMOVE\*\//g, ""), "utf8");
}