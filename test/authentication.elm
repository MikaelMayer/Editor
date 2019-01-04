<html>
<head>
<meta name="google-signin-client_id" content=@googleClientId>
<title>Hello authenticated world</title>
</head>
<body>
@googlesigninbutton
<h1>
<img src=@(listDict.get "picture" user |> Maybe.withDefault "")> Hello @(listDict.get "given_name" user |> Maybe.withDefault "Anonymous")!</h1>
</body>
</html>