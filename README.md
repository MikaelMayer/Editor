# Editor

Editor is a bidirectional webserver that uses [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch) as the back-end for executing web pages and propagating back modifications to the source files.

To start the Editor web server, enter the following on a command line (you need node.js):

    npm install sketch-n-sketch
    node server.js

then point your browser to http://127.0.0.1:3000

# Example

Point your browser to http://127.0.0.1:3000/pizzas.elm?user=Anonymous&hl=fr&edit=true  
Here you can witness the possibilities of a 60-line-of-code webpage in Editor. You can...
* Include and interpret data tiles (`nodejs.fileread "data/pizzas.txt"`).
* Modify the current username directly from the webpage (see how it's updated in the URL)
* Select a pizza choice and so that it is stored and added it to the corresponding summary below.
* Change the summary style (text, color, font...) directly using the DOM explorer.
* Change the translation language
* Edit the current translated sentences
* Add a new translation language (go to the DOM inspector, and duplicate <option>English</option>, and then rename it to German)
* Add a new pizza (same but duplicate a pizza option)
* Rename a pizza wherever it appears
* Add new translated sentences from the webpage (wrap "Final choices" so that it becomes "{:Final choices:}". After a roundtrip update, switch to French and translate it ("Choix finaux").
* Remove one's pizza choice by selecting the first "Choose your pizza" option.

# Workflow and API

Editor is an experimental web server.
If someone points the browser to a file `127.0.0.1/A.elm`, Editor executes `A.elm` and interpret sthe resulting object as an HTML webpage, and sends it back to the client.
If the page includes `@clientscript` somewhere in the body, all modifications to this webpage are observed (via a MutationObserver) and the changes are back-propagated to the file `A.elm`.

# Limitations, future work and caution

## Need for authentication

Since there is no authentication yet, everybody that has access to the server can in theory modify all the files present.
Please do not use this server publicly until there is proper authentication.

## Need for concurrent editing

In case there are two conflicting edits, they will not be merged, only the second will take place. There is a work in progress for merging edit diffs.

## Need for better diffs

Currently, updating the program

    x = "user"
    <html><body>Hello @x</body></html>

with the new output value

    <html><body>Hello <b>user</b></body></html>

does not produce the expected

    x = "user"
    <html><body>Hello <b>@x</b></body></html>

but

    x = "user"
    <html><body>Hello <b>user</b></body></html>

A work in progress will overcome this issue.

## Need for templates

We did not push templates yet but we will soon, by importing them from [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch). Among the templates, we want
* Slides
* Bidirectional converters (e.g. Markdown, LaTeX)
* Self-modifying webpages.
* Recipe editor
* Worksheets
* Forms

## Need for a toolbar

We need a general-purpose HTML edition toolbar to edit all the web pages without relying only on the devtools.
Furthermore, we need the toolbar to expose udpate ambiguities and let the user choose from them.

## Need for a better WYSIWYG webpage editor.

Why not try [Slate?](https://www.slatejs.org) if it works for the entire HTML.

# License

This technology is offered without any warranty (for the moment).
Please refer [Sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)'s license to use this technology commercially.
For academic projects this should be fine.
