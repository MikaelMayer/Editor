-- The user database.
userdata: List (String, (String, Int))
userdata = [
  ("7989", ("Laurent", 1)),
  ("10000", ("Ravi", 3))
]

-- List of potential admins
potentialadmins =
  []
  |> List.map Just 

-- Ensures that only owners and admins can modify their own pizza
userdata = 
  let canModify (sub, x) =
    if sub == usersub || admin then
      Nothing
    else
      Just "cannot modify someone else's data if already answered"
  in
  {-
  -- TODO: Implement these checks
  List.update.errOnInsert canModify <|
  List.update.errOnUpdate canModify <|
  List.update.errOnDelete canModify <|
  -}
  userdata

-- Helpers
adminModifiable hint =
  Update.freezeWhen (not admin) (\_ -> """Cannot modify @hint, only admins can""")

potentialadminModifiable hint =
  Update.freezeWhen (not potentialadmin) (\_ -> """Cannot modify @hint, only potential admins can""")

-- If the current user has the right to act as admin
potentialadmin = 
  listDict.get "sub" user |>
  flip List.contains potentialadmins

-- If the current user activated the admin mode
admin =
  potentialadmin && (listDict.get "admin" vars == Just "true")
  
-- Default value of type a for 'Maybe a' that can only be modified by admins
mbReplaceAdmin hint =
  if admin then
    Maybe.withDefaultReplace
  else
    \default x ->
      Maybe.withDefaultReplace (Update.lens { apply = identity, update = always <| Err """Cannot change the @hint if you are not an admin"""} default) x

-- The user subject number. Admins can simulate users by adding &sub=... to the URL
usersub =
  let default _ = listDict.get "sub" user |> mbReplaceAdmin "default sub" "0" in
  if potentialadmin then
    listDict.get "sub" vars
    |> Maybe.withDefault (default ())
  else
    default ()

-- The username. We'll take it from the database if available, else from the credentials
username  =
  listDict.get usersub userdata
  |> Maybe.map Tuple.first
  |> Maybe.withDefaultLazy (\_ ->
    let default _ =   listDict.get "given_name" user |> mbReplaceAdmin "default name" "Anonymous" in
    if admin then
      listDict.get "given_name" vars
      |> Maybe.withDefault (default ())
    else
      default ()
  )

    
-- The list of pizzas (only modifiable by admin)
options = adminModifiable "list of pissas" ["Margharita", "Four Cheese", "Pepper"]

-- The interface for potential admins and admins
admininterface =
  if admin then 
    <span>- admin mode activated<br></span>
  else if potentialadmin then
    <button onclick="""location.href = location.pathname + '?admin=true'""">Activate admin mode</button>
  else []

-- Content displayed to the user
content =
  if usersub == "0" || (potentialadmin && listDict.get "admin" vars == Just "false") then
    potentialadminModifiable "text when not signed in" <span>Please sign in or wait a bit for the auto-sign</span>
  else
    let
      selection =
        Html.select [] (adminModifiable "text when no pizza selected" "Choose one..." :: options) (
        listDict.get usersub userdata
        |> mbReplaceAdmin "default pizza choice" (username, 0)
        |> Tuple.second)
    
      finalChoices =
        userdata
        |> List.map (\(sub, (name, id)) ->
          <span>@name choose @(List.findByAReturnB Tuple.first Tuple.second (id - freeze 1) (List.zipWithIndex options)
            |> Maybe.withDefault "a pizza that does not exist").<br></span>
        )
    in
      adminModifiable "content template" (\username selection finalChoices ->
        <span><br>Hi <span sub=@usersub>@username</span>!<br>
          Select the pizza you want
          @selection<br><br>
          Final choices made by everyone:<br>@finalChoices
        </span>) username selection finalChoices

main = adminModifiable "general html page" (\googleClientId googlesigninbutton admininterface content ->
<html>
<head>
<meta name="google-signin-client_id" content=@googleClientId>
</head>
<body>
@googlesigninbutton@(admininterface)
@content

<hr>
<h1>Fully authenticated webpage</h1>
This webpage only authorizes you to modify the parts you <i>own</i>.<br>
<br>
Let's how to get started to play around this file:
<ul>
<li>Sign in using your Google account, accept the permissions</li>
<li>After the page reloads, select a pizza. Wait for the page to save your changes.<br>The list "final choices" should display your choice</li>
<li>Go to the source file `authenticated-pizzas.elm`. In the variable <code>userdata</code>, you should see a string of digits close to your name, it's your <code>sub</code>.<br>Add your <code>sub</code> to the list of potential admins and reload the page</li>
<li>Now you should see the button <button>Activate admin mode</button>. Click on it. It will add <code>?admin=true</code> to the URL.</li>
<li>As an admin, you can modify the template. Modify "choose" to "would like a" for example.</li>
<li>As an admin, you can now simulate another's existing account
  <ul>
     <li>Append <code>&amp;sub=7989</code> to the URL.</li>
     <li>The website displays what Laurent would see if he was logged in.</li>
     <li>Remove <code>admin=true&amp;</code> from the URL.</li>
     <li>Change Laurent's name, and pizza. This modification is stored into the database</li>
     <li>Try to modify the template by modifying "would like a" to "choose". It fails because Laurent is not an admin</li>
  </ul>
  </li>
<li>
   You can also simulate a not yet existing account
   <ul>
     <li>Append <code>&amp;sub=12345&amp;given_name=faked</code</li>
     <li>Choose a pizza. The name and pizza are store with respect to the sub</li>
   </ul>
</li>
</ul>
</body>
</html>) googleClientId googlesigninbutton admininterface content