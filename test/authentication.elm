<html>
<head>
<meta name="google-signin-client_id" content=@googleClientId>
<title>Hello authenticated world</title>
<style>
a.closeSignIn {
  margin-left: 2px;
  padding-left: 4px;
  padding-right: 5px;
}
a.closeSignIn:hover {
  background: #AAA;
  color: white;
  border-radius: 10px;
}
</style>
</head>
<body>
<script>
(setGhostOnInserted || []).push(insertedNode =>
  insertedNode.tagName == "STYLE" && insertedNode.parentNode.tagName == "HEAD" &&
  insertedNode.attributes.length == 0 && 
  (insertedNode.setAttribute("save-ghost", "true") || true));
(setGhostOnInserted || []).push(insertedNode =>
  (insertedNode.tagName == "DIV" &&
    insertedNode.classList.contains("abcRioButton")) ||
  (insertedNode.tagName == "IFRAME" &&
    insertedNode.getAttribute("id") == "ssIFrame_google")
);
</script>
<script id="googlesigninscript" src="https://apis.google.com/js/platform.js" async defer save-ghost-attributes="gapi_processed"></script>
<h1>
<img src=@(listDict.get "picture" user |> Maybe.withDefault "")> Hello @(listDict.get "given_name" user |> Maybe.withDefault "Anonymous")!</h1>
<div class="g-signin2" data-onsuccess="onSignIn" list-ghost-attributes="data-gapiscan data-onload" children-are-ghost="true"></div>
<script>
addSignout = (name) => {
  var signin = document.querySelector(".abcRioButtonContents").children[1];
  signin.setAttribute("title", "Signed in as " + name);
  var signout = document.createElement("a");
  signout.classList.add("closeSignIn");
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

function onSignIn(googleUser) {
  var profile = googleUser.getBasicProfile();
  
  var wasSignedIn = googleAuthIdToken ? true : false;
  // When set, will be used throughout 
  googleAuthIdToken = googleUser.getAuthResponse().id_token;
  addSignout(profile.getName());
  if(!wasSignedIn) { // Necessary to ensure that we don't reload the page the second time it is loaded.
    reloadPage();
  } else {
  }
}
</script>

</body>
</html>