# Contributing

## Local server

If you want to start the webserver after cloning the github repo, enter the commands:

    npm install
    node bin/server.js

then point your browser to http://127.0.0.1:3000

You can test the server by pointing your browser to http://127.0.0.1:3000/test/pizzas.elm?user=Anonymous&hl=fr&edit=true  
Here you can witness the possibilities of a 60-line-of-code webpage in Editor. You can...
* Include and interpret data tiles (`nodejs.fileread "data/pizzas.txt"`).
* Modify the current username directly from the webpage (see how it's updated in the URL)
* Select a pizza choice and so that it is stored and added it to the corresponding summary below.
* Change the summary style (text, color, font...) directly using the DOM explorer.
* Change the translation language
* Edit the current translated sentences
* Add a new translation language (go to the DOM inspector, and duplicate `<option>English</option>`, and then rename it to German)
* Add a new pizza (same but duplicate a pizza option)
* Rename a pizza wherever it appears
* Add new translated sentences from the webpage (wrap "Final choices" so that it becomes "{:Final choices:}". After a roundtrip update, switch to French and translate it ("Choix finaux").
* Remove one's pizza choice by selecting the first "Choose your pizza" option.

### Publishing

Before doing `npm publish`, make sure to

1. `npm bundle.js` to bundle the server code into the executable.
2. `npm version patch` to patch Editor's version number

### Publishing locally for testing.

Inside the folder containing the Editor folder, run the following command.

     npm install ./Editor

This will install all executables at the location given in `npm bin` and make them available in your PATH.
This can be useful for development.
     