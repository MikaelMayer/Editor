const fs = require("fs");
const sns = require("sketch-n-sketch");

var res = sns.string.evaluate(`
fs = nodejs.delayed nodejs.directFileOperations

elmserver = fs.read "server.elm" |> Maybe.withDefaultLazy (\\_ -> error "server.elm not found")

fs.read "bin/server.js"
|> Maybe.withDefaultLazy (\\_ -> error "bin/server.js not found")
|> Regex.replace "const defaultServerContent = .*;" (\\_ ->
  """const defaultServerContent = @(jsCode.stringOf elmserver);""")
`);

if(res.ctor == "Err") {
  console.log("Error", res._0);
} else {
  var result = sns.valToNative(res._0)._0;
  fs.writeFileSync("bin/server-generated.js", "#!/usr/bin/env node\n\n" + result, "utf8");
}