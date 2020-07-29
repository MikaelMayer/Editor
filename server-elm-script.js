editor = typeof editor == "undefined" ? {} : editor;

(function(editor) {
  // Default configuration.
  editor.config = typeof editor.config == "undefined" ? {} : editor.config;
  editor.config = {
    EDITOR_VERSION: 1,
    path: location.pathname,
    varedit: location.search.match(/(^\?|&)edit(?:=true)?(&|$)/),
    varls: location.search.match(/(^\?|&)ls(?:=true)?(&|$)/),
    askQuestions: location.search.match(/(^\?|&)question(?:=true)?(&|$)/),
    autosave: location.search.match(/(^\?|&)autosave(?:=true)?(&|$)/),
    canEditPage: false,
    editIsFalseButDefaultIsTrue : false,
    thaditor: false,
    userName: typeof userName === "string" ? userName : "anonymous",
    ...editor.config};
  var mbEditQ = editor.config.fast ? "" : "?edit&fast=false";
  var mbEditA = editor.config.fast ? "" : "&edit&fast=false";
  
  var _internals = {};
  editor._internals = _internals;
  
  // function(Time in ms, key, callback)
  // If the time elapses, calls the callback. Reset the timer every time the same callback is called, thanks to the key.
  editor._internals.ifIdleAfter = (() => {
    let waiting = {};
    return function(ms, key, callback) {
      if(key in waiting) {
        clearTimeout(waiting[key]);
      }
      waiting[key] = setTimeout(() => {
        delete waiting[key];
        callback();
      }, ms);
    }
  })();
  
  
  // Overwrite the entire document (head and body)
  // I'm not sure there is a better way.
  function writeDocument(NC) {
    let {scrollX, scrollY} = window;
    document.open();
    document.write(NC);
    document.close();
    setTimeout(() => {
      window.scrollTo(scrollX, scrollY);
    }, 0)
  }
  _internals.writeDocument = writeDocument;

  // Asynchronous if onOk is defined and readServer is defined. and asynchronous and returns result otherwise.
  
  // Could be overriden so that Editor could work with a local file system, Git, or anything else.
  _internals.doReadServer = function doReadServer(action, name, onOk, onErr) {
    if (typeof thaditor != "undefined") { // apache_server, everything goes through thaditor
      return thaditor._internals.readServer(action, name, onOk, onErr);
    } else {
      var request = new XMLHttpRequest();
      var url = "/";
      request.open('GET', url, false);  // `false` makes the request synchronous
      request.setRequestHeader("action", action);
      request.setRequestHeader("name", name);
      request.send(null);
      if(request.status == 200) {
        return request.responseText || "";
      } else {
        console.log("error while reading " + url, request);
        return undefined;
      }
    }
  };

  // Returns a promise after performing a direct GET action on the server.
  function getServer(action, name) {
    return new Promise(function(resolve, reject) {
      _internals.doReadServer(action, name, resolve, reject);
    });
  }
  editor.getServer = getServer;

  // Asynchronous if onOk is defined and writeServer is defined. and asynchronous and returns result otherwise.
  function doWriteServer(action, name, content, onOk, onErr) {
    if (typeof thaditor != "undefined") {
      return thaditor._internals.writeServer(action, name, content, onOk, onErr);
    } else {
      var request = new XMLHttpRequest();
      var url = "/";
      request.open('POST', url, false);  // `false` makes the request synchronous
      request.setRequestHeader("action", action);
      request.setRequestHeader("name", name);
      request.send(content);
      if(request.status == 200) {
        return request.responseText;
      } else if(request.status == 500) {
        return request.responseText;
      } else {
        console.log("error while writing " + url, request);
        return "";
      }
    }
  }
  _internals.doWriteServer = doWriteServer;

  // Returns a promise after performing a direct POST action on the server.
  function postServer(action, name, content) {
    return new Promise(function(resolve, reject) {
      _internals.doWriteServer(action, name, content, resolve, reject);
    });
  }
  editor.postServer = postServer;

  // Page reloading without trying to recover the editor's state.
  function doReloadPage(url, replaceState) {
    function finish(text, newLocalURL) {
      writeDocument(text);
      if(newLocalURL) {
        window.history[replaceState ? "replaceState" : "pushState"]({localURL: newLocalURL}, "Nav. to " + newLocalURL, newLocalURL);
      }
    }
    if(editor.config.thaditor) { // Ask the worker to recompute the page
      thaditor.do({action:"sendRequest",
        toSend: "{\"a\":1}",
        loc: location.pathname + location.search,
        requestHeaders: {reload: true, url: url},
        what: undefined}).then(data => {
          if (data.action == "confirmDone") {
            finish(data.text, data.newLocalURL);
          }
        });
    } else { // Ask Editor's web server to recompute the page.
      var xmlhttp = new XMLHttpRequest();
      xmlhttp.onreadystatechange = (xmlhttp => () => {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          finish(xmlhttp.responseText, xmlhttp.getResponseHeader("New-Local-URL"));
        }
      })(xmlhttp);
      xmlhttp.open("POST", location.pathname + location.search);
      xmlhttp.setRequestHeader("reload", "true");
      xmlhttp.setRequestHeader("url", url);
      console.log("setting url to ", url);
      xmlhttp.send("{\"a\":1}");
    }
  }
  editor._internals.doReloadPage = doReloadPage;

  // Uploads a file
  // targetPathName: The path where to upload the file
  // file: The file to upload (typically from a drop event or a input[type=file] change)
  // onOk: Callback when upload is complete, with targetPathName and file
  // onErr: callback if upload fails, with targetPathName and file
  // onProgress: callback every progress made, with targetPathName, file and percentage
  function uploadFile(targetPathName, file, onOk, onError, onProgress) {
    if(editor.config.thaditor) {
      thaditor.postServer("write", targetPathName, file, onProgress).then(() => {
        if(onOk) onOk(targetPathName, file);
      }).catch(() => {
         if(onError) onError(targetPathName, file);
      })
    } else { // Editor webserver
      var xhr = new XMLHttpRequest();
      xhr.onprogress = (e) => {
        if(onProgress) {
          onProgress(targetPathName, file, e && (e.loaded * 100.0 / e.total) || 100)
        }
      }
      xhr.onreadystatechange = ((xhr, file) => () => {
        if (xhr.readyState == XMLHttpRequest.DONE) {
          if (xhr.status == 200 || xhr.status == 201) {
            onOk ? onOk(targetPathName, file) : 0;
          } else {
            console.log("Error while uploading picture or file", xhr);
            onError ? onError(targetPathName, file) : 0;
          }
        }
      })(xhr, file);
      xhr.open("POST", targetPathName, false);
      xhr.setRequestHeader("write-file", file.type);
      xhr.send(file);
    }
  }
  editor.uploadFile = uploadFile;
  
  function pasteHtmlAtCaret(html) {
    var sel, range;
    if (window.getSelection) {
        // IE9 and non-IE
        sel = window.getSelection();
        // do not paste html into modify menu
        if (sel.anchorNode.offsetParent && sel.anchorNode.offsetParent.id === "modify-menu") {
          return;
        }
        if (sel.getRangeAt && sel.rangeCount) {
            range = sel.getRangeAt(0);
            range.deleteContents();
            // Range.createContextualFragment() would be useful here but is
            // only relatively recently standardized and is not supported in
            // some browsers (IE9, for one)
            var div = document.createElement("div");
            div.innerHTML = html;
            var frag = document.createDocumentFragment(), node, lastNode;
            while ( (node = div.firstChild) ) {
                lastNode = frag.appendChild(node);
            }
            range.insertNode(frag);
            // Preserve the selection
            if (lastNode) {
                range = range.cloneRange();
                range.setStartAfter(lastNode);
                range.collapse(true);
                sel.removeAllRanges();
                sel.addRange(range);
            }
        }
    } else if (document.selection && document.selection.type != "Control") {
        // IE < 9
        document.selection.createRange().pasteHTML(html);
    }
  }
  editor.pasteHtmlAtCaret = pasteHtmlAtCaret;
  
  // Given some files, uploads them and place them at cursor. Images are inserted as <img>, whereas other files just have the <a href=>)
  function uploadFilesAtCursor(files) { 
    // files is a FileList of File objects. List some properties.
    for (var i = 0, file; file = files[i]; i++) {
      var targetPathName =  editor.getStorageFolder(file) + file.name;
      // if(file.size < 30000000)
      editor.uploadFile(targetPathName, file, (targetPathName, file) => {
        if(file.type.indexOf("image") == 0) {
          editor.pasteHtmlAtCaret(`<img src="${targetPathName}" alt="${file.name}">`);
        } else {
          editor.pasteHtmlAtCaret(`<a href="${targetPathName}">${targetPathName}</a>`); 
        }
      });
    }
  }
  editor.uploadFilesAtCursor = uploadFilesAtCursor;
  
  // Returns the storage folder that will prefix a file name on upload (initial slash excluded, final slash included)
  editor.getStorageFolder = function(file) {
    var storageOptions = document.querySelectorAll("meta[editor-storagefolder]");
    for(let s of storageOptions) {
      if(file.type.startsWith(s.getAttribute("file-type") || "")) {
        let sf = storageOptions.getAttribute("editor-storagefolder") || "";
        if(!sf.endsWith("/")) sf = sf + "/";
        return sf;
      }
    }
    let extension = "";
    if(file && file.type.startsWith("image")) {
      var otherImages = document.querySelectorAll("img[src]");
      for(let i of otherImages) {
         extension = (i.getAttribute("src") || "").replace(/[^\/]*$/, "");
         break;
      }
      if(extension[0] == "/") { // Absolute URL
        return extension;
      }
    }
    if(extension != "" && extension[extension.length-1] != "/") {
      extension = extension + "/";
    }
    // extension ends with a / or is empty
    var tmp = location.pathname.split("/");
    tmp = tmp.slice(0, tmp.length - 1);
    storageFolder = tmp.join("/") + (extension != "" ?  "/" + extension : "/");
    return storageFolder;
  }
  editor.fs = { 
    listdir: 
      async function(dirname) {
        return JSON.parse(await getServer("listdir", dirname) || "[]");
      }
  };

  function getSelectorOf(clickedElem) {
    let curSelector = clickedElem.tagName.toLowerCase();
    if(curSelector === "html" || curSelector === "body" || curSelector === "head") return curSelector;
    if(clickedElem.getAttribute("id")) {
      curSelector += "#" + clickedElem.getAttribute("id");
    }
    if (clickedElem.getAttribute("class") && clickedElem.getAttribute("class") != "") {
      curSelector += (" " + clickedElem.getAttribute("class").trim()).replace(/\s+/g, ".");
    }
    return curSelector;
  }
  
  function getBareSelectorOf(t) {
    return t.nodeType === document.ELEMENT_NODE ? t.tagName.toLowerCase() : "/*" + t.nodeType + "*/";
  }
  
  // Given a node, computes a way to retrieve this node if the page was reloaded.
  // That's a treasure map.
  // TreasureMap.bareSelectorArray does not contain "~" and they'll need to be reinserted in case we use the selector as a CSS selector
  editor.toTreasureMap = function toTreasureMap(oldNode, ancestorToStopAt) {
    if(!oldNode) return undefined;
    let foundParentWithId = false;
    let tentativeSelector = [];
    let bareSelectorArray = [];
    let t = oldNode;
    
    while(t) {
      let bareSelector = getBareSelectorOf(t);
      let extSelector = undefined;
      if(!foundParentWithId && !bareSelector.startsWith("/*")) {
        if(tentativeSelector.length) {
          tentativeSelector.unshift(">");
        }
        extSelector = getSelectorOf(t);
        tentativeSelector.unshift(extSelector); // We pile more information in the tentativeSelector to find the elemnt
      }
      if(bareSelectorArray.length) {
        bareSelectorArray.unshift(">");
      }
      bareSelectorArray.unshift(bareSelector);
      if(ancestorToStopAt == t || t == document.head.parentElement) break;
      // Emulate :nth-of-type but for a class of siblings having the same selector.
      let s = t.previousSibling;
      foundParentWithId = foundParentWithId || t.nodeType === document.ELEMENT_NODE && t.hasAttribute("id");
      let gotOneElementBefore = t.nodeType === document.ELEMENT_NODE;
      while(s) {
        let isSimilarSibling = typeof s.matches == "function" && t.nodeType === document.ELEMENT_NODE && s.matches(extSelector);
        let selector = isSimilarSibling ? extSelector : getBareSelectorOf(s);
        if(isSimilarSibling && !foundParentWithId) {
          tentativeSelector.unshift(extSelector, "~");
        } else if(s.nodeType != document.ELEMENT_NODE && !foundParentWithId) {
          tentativeSelector.unshift(selector);
        }
        bareSelectorArray.unshift(getBareSelectorOf(s));
        gotOneElementBefore = gotOneElementBefore || s.nodeType === document.ELEMENT_NODE;
        s = s.previousSibling;
      }
      t = t.parentElement;
    }
    return {tentativeSelector: tentativeSelector, bareSelectorArray: bareSelectorArray};
  }
  // Given a tentative selector and the element to start from, returns the precise element designated by this selector if it exists.
  function queryBareSelector(bareSelectorArray, fromElement) {
    var i = 0;
    var n = bareSelectorArray.length;
    while(fromElement && i < n) {
      let selector = bareSelectorArray[i];
      let m = selector.match(/\/\*(\d+)\*\//);
      // In case there is no match with the expected selector, let's try to go further.
      while(fromElement && !(!m && fromElement.matches && fromElement.matches(selector) || m && Number(m[1]) == fromElement.nodeType)) {
        fromElement = fromElement.nextSibling;
      }
      if(fromElement) {
        i++;
        if(i >= n) {
          return fromElement;
        } else if(bareSelectorArray[i] == ">") {
          fromElement = fromElement.firstChild;
          i++;
        } else {
          fromElement = fromElement.nextSibling;
        }
      } else {
        return null;
      }
    }
    return null;
  }
  editor._internals.queryBareSelector = queryBareSelector;
  
  // Returns the new node that matches the old node the closest.
  // For text nodes, try to recover the text node, if not, returns the parent node;
  editor.fromTreasureMap = function(data, source) {
    if(!data) return undefined;
    if(typeof data === "object" && data.id) {
      return document.getElementById(data.id);
    }
    if(typeof data == "object" && Array.isArray(data.tentativeSelector)) {
      let tentativeSelector = [...data.tentativeSelector];
      let result = queryBareSelector(data.bareSelectorArray, source || document.head.parentElement);
      if(result) {
        return result;
      }
      // Fallback, we try to reduce the tentative selector until we find the node.
      while(tentativeSelector.length >= 1) {
        if(tentativeSelector[0] !== "~" && tentativeSelector[0] !== ">") {
          let result = document.querySelector(tentativeSelector.join(" "));
          if(result) {
            let x = tentativeSelector.length - 1;
            while(result && tentativeSelector[x].match(/\/\*(\d+)\*\//)) {
              result = result.previousSibling;
              x--;
            }
            return result;
          }
        }
        tentativeSelector.shift();
      }
      return undefined;
    }
  }

  // Helper to create an element with attributes, children and properties
  function el(tag, attributes, children, properties) {
    let tagClassIds = tag.split(/(?=#|\.)/g);
    let x;
    for(let attr of tagClassIds) {
      if(x && attr.startsWith(".")) {
        x.classList.toggle(attr.substring(1), true);
      } else if(x && attr.startsWith("#")) {
        x.setAttribute("id", attr.substring(1));
      } else if(!x) {
        x = document.createElement(attr);
      }
    }
    if(typeof attributes == "object") {
      for(let k in attributes) {
        let v = attributes[k];
        if(typeof v != "undefined") {
          x.setAttribute(k, v);
        }
      }
    }
    if(Array.isArray(children)) {
      for(let child of children) {
        if(typeof child === "string") {
          x.append(child)
        } else if(typeof child !== "undefined")
          x.appendChild(child);
      }
    } else if(typeof children !== "undefined") {
      x.append(children);
    }
    if(typeof properties == "object") {
      for(let k in properties) {
        x[k] = properties[k];
      }
    }
    return x;
  }
  editor.el = el;
  
  // Returns true if the element matches the selector
  // Equivalent curried call: editor.matches(selector)(elem)
  editor.matches = function(elem, selector) {
    if(typeof selector == "undefined") { // Then selector is in the elem variable
      return (selector => function(elem) {
        return editor.matches(elem, selector);
      })(elem);
    }
    if(elem && elem.matches) {
      try {
        return elem.matches(selector);
      } catch(e) {
        console.log("error while matching selector " + selector, e);
        return false;
      }
    }
    return false;
  }

  // An array of (node => {innerHTML, attributes, properties}) that can be defined by plug-ins.
  editor.customContextMenuButtons = [];
  
  // Creates an SVG icon from the given path. If fill is true, will have the path filled.
  function svgFromPath(path, fill, width, height, viewBox) {
    return `<svg class="context-menu-icon${fill ? " fill": ""}" 
          width="${width ? width : 40}" height="${height ? height : 30}" 
          ${viewBox ? "viewBox=\"" + viewBox[0] + " "+ viewBox[1] + " "+ viewBox[2] + " "+ viewBox[3] +"\"" : "viewBox=\"0 0 40 30\""}>
          <path d="${path}"></path></svg>`
  }
  editor.svgFromPath = svgFromPath;
  
  /***************************
   * Ghost & ignored API
   ****************************/
  
  // Returns true if this element is marked as ghost
  function isGhostNode(elem) {
    return elem && (elem.isghost || (elem.nodeType == 1 &&
      (elem.tagName == "GHOST" || elem.getAttribute("isghost") == "true")));
  }
  editor.isGhostNode = isGhostNode;
  
  // Returns the child's index among its parent, not counting ghost nodes.
  function childToInsertionIndex(parent, element) {
    while(isGhostNode(element)) {
      element = element.nextSibling;
    }
    var i = 0;
    var tmp = parent.firstChild;
    while(tmp) {
      if(!isGhostNode(tmp)) {
        if(tmp === element) {
          return i;
        }
        i += 1;
      }
      tmp = tmp.nextSibling;
    }
    return i;
  }
  // Returns the child associated to the index, not counting ghost nodes.
  function insertionIndexToChild(parent, index) {
    var tmp = parent.firstChild;
    while(index != 0) {
      if(!isGhostNode(tmp)) {
        index--;
      }
      tmp = tmp.nextSibling;
    }
    return tmp;
  }
  
  // Returns true if all children of this elements are marked as ghosts
  function areChildrenGhosts(n) {
    return n && n.getAttribute && (
      n.getAttribute("children-are-ghosts") == "true" ||
      n.getAttribute("children-are-ghost") == "true"
    ) ||
    editor.ghostChildNodes.find(f => f(n));
  }
  editor.areChildrenGhosts = areChildrenGhosts;
  
  // Returns true if this element has a node which implicitly marks it as a ghost
  function hasGhostAncestor(htmlElem) {
    if(htmlElem == null) return false;
    if(isGhostNode(htmlElem)) return true;
    return areChildrenGhosts(htmlElem.parentNode) ||
      /*(htmlElem.parentNode == null && htmlElem.nodeType !== document.DOCUMENT_NODE) || */hasGhostAncestor(htmlElem.parentNode);
  }
  editor.hasGhostAncestor = hasGhostAncestor;
  
  function isGhostAttributeKey(name) {
    return name.startsWith("ghost-");
  }
  editor.isGhostAttributeKey = isGhostAttributeKey;

  // Array of functions on nodes returning an array of attributes that should be ghosts (i.e. removed on back-propagation)
  editor.ghostAttrs = [];
  editor.ghostAttrs.push(n =>
    ((n && n.getAttribute && n.getAttribute("list-ghost-attributes")) || "").split(" ").concat(
      ((n && n.getAttribute && n.getAttribute("save-ghost-attributes")) || "").split(" ")).filter(a => a != "")
  );
  // attribute of some chrome extensions
  editor.ghostAttrs.push(n => ["bis_skin_checked"]);
  
  // Array of functions on nodes returning an array of predicates such that if one is true, the children of this element will be ignored (i.e. their old value is always returned on back-propagation)
  editor.ignoredChildNodes = [];
  
  // Returns truesy if the element n is ignoring child nodes.
  function isIgnoringChildNodes(n) {
    return editor.ignoredChildNodes.find(f => f(n));
  }
  editor.isIgnoringChildNodes = isIgnoringChildNodes;
  // Assuming that n is ignoring children, stores the encoding of the original children
  function storeIgnoredChildNodes(n) {
    let res = [];
    for(let k = 0; k < n.childNodes.length; k++) {
      res.push(domNodeToNativeValue(n.childNodes[k]));
    }
    // Let's store this element's array of children
    n.__editor__ = n.__editor__ || {};
    n.__editor__.ignoredChildNodes = res;
  }
  editor.storeIgnoredChildNodes = storeIgnoredChildNodes;

  // Return true if this htmlElem is inside an element that ignores its children.
  function hasIgnoringAncestor(htmlElem) {
    if(htmlElem == null) return false;
    return isIgnoringChildNodes(htmlElem.parentNode) || hasIgnoringAncestor(htmlElem.parentNode);
  }
  editor.hasIgnoringAncestor = hasIgnoringAncestor;
  
  // Array of functions on nodes returning an array of attributes that should be ignored (i.e. old value returned on back-propagation)
  editor.ignoredAttrs = [];
  editor.ignoredAttrs.push(n =>
    ((n && n.getAttribute && n.getAttribute("list-ignored-attributes")) || "").split(" ").concat(
      ((n && n.getAttribute && n.getAttribute("save-ignored-attributes")) || "").split(" ")).filter(a => a != "")
  );
  editor.ignoredAttrs.push(n => editor.matches(n, "body") ? ["contenteditable", "data-gr-c-s-loaded"] : []);
  editor.ignoredAttrs.push(n => editor.matches(n, "html") ? ["class"] : []);
  
  
  // Returns a method that, for each key name, return true if it is a ghost attribute for the node
  function isSpecificGhostAttributeKeyFromNode(n) {
    var additionalGhostAttributes = [];
    for(let k = 0; k < editor.ghostAttrs.length; k++) {
      additionalGhostAttributes = additionalGhostAttributes.concat(editor.ghostAttrs[k](n))
    }
    return (a => name => a.indexOf(name) != -1)(additionalGhostAttributes);
  }
  editor.isSpecificGhostAttributeKeyFromNode = isSpecificGhostAttributeKeyFromNode;

  // Returns a method that, for each key name, return true if it is an ignored attribute for the node
  function isIgnoredAttributeKeyFromNode(n) {
    var additionalIgnoredAttributes = [];
    for(var k = 0; k < editor.ignoredAttrs.length; k++) {
      additionalIgnoredAttributes = additionalIgnoredAttributes.concat(editor.ignoredAttrs[k](n))
    }
    return ((a, n) => (name, oldValue) => {
      let result = a.indexOf(name) != -1;
      if(result) { // let's store the previous attribute's value, even if it did not exist.
        n.__editor__ = n.__editor__ || {};
        n.__editor__.ignoredAttrMap = n.__editor__.ignoredAttrMap || {};
        if(!(name in n.__editor__.ignoredAttrMap)) {
          n.__editor__.ignoredAttrMap[name] = oldValue;
        }
      }
      return result;
    })(additionalIgnoredAttributes, n);
  }
  editor.isIgnoredAttributeKeyFromNode = isIgnoredAttributeKeyFromNode;
  function ignoredAttributeValue(n, name) {
    let result = n.__editor__.ignoredAttrMap[name];
    if(name in n.__editor__.ignoredAttrMap) {
      return result;
    }
    return n.getAttribute(name);
  }
  
  // Array of predicates that, if they return true on a node, Editor will mark this node as ghost.
  editor.ghostNodes = [];
  // Array of predicates that, if they return true on a node, Editor will mark all the children of this node as ghosts
  editor.ghostChildNodes = [];

  // Analytics scripts
  editor.ghostNodes.push(insertedNode =>
    editor.matches(insertedNode, "script[src]") &&
       (insertedNode.getAttribute("src").indexOf("google-analytics.com/analytics.js") != -1 ||
        insertedNode.getAttribute("src").indexOf("google-analytics.com/gtm/js") != -1 ||
        insertedNode.getAttribute("src").indexOf("googletagmanager.com/gtm.js") != -1)
  );

  // For for ace styles in header
  editor.ghostNodes.push(insertedNode => {
      if(insertedNode.tagName == "STYLE" && typeof insertedNode.getAttribute("id") == "string" &&
       (insertedNode.getAttribute("id").startsWith("ace-") ||
        insertedNode.getAttribute("id").startsWith("ace_"))) {
        insertedNode.setAttribute("save-ghost", "true"); 
        return true;
      } else {
        return false;
      }
    }
  );
  // For Google sign-in buttons and i-frames
  editor.ghostNodes.push(
    editor.matches("div.abcRioButton, iframe#ssIFrame_google")
  );
  // For anonymous styles inside HEAD (e.g. ace css themes and google sign-in)
  editor.ghostNodes.push(insertedNode => 
     insertedNode.tagName == "STYLE" && insertedNode.getAttribute("id") == null && insertedNode.attributes.length == 0 &&
     insertedNode.parentElement.tagName == "HEAD" && typeof insertedNode.isghost === "undefined"&& insertedNode.textContent.match("error_widget\\.ace_warning")
     && (insertedNode.setAttribute("save-ghost", "true") || true)
   );
  // For ace script for syntax highlight
  editor.ghostNodes.push(insertedNode =>
    editor.matches(insertedNode, "script[src]") &&
       insertedNode.getAttribute("src").startsWith("https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.6/mode-j")
  );
  // For ace script for syntax highlight
  editor.ghostNodes.push(insertedNode =>
    insertedNode.tagName == "ACE_OUTER"
  );
  // For the grammarly extension
  editor.ghostNodes.push(
    editor.matches(".gr-top-z-index, .gr-top-zero")
  );

  function domNodeToNativeValue(n, postProcessing) {
    let result;
    if(n.nodeType == document.TEXT_NODE) {
      result = ["TEXT", n.textContent];
    } else if(n.nodeType == document.COMMENT_NODE) {
      result = ["COMMENT", n.textContent];
    } else if(n.nodeType === document.DOCUMENT_TYPE_NODE) {
      result = ["!DOCTYPE", n.name, n.publicId ? n.publicId : "", n.systemId ? n.systemId : ""];
    } else {
      var attributes = [];
      var tagName = n.nodeType === document.DOCUMENT_NODE ? "#document" : n.tagName.toLowerCase();
      if(n.nodeType == 1) {
        var isSpecificGhostAttributeKey = isSpecificGhostAttributeKeyFromNode(n);
        var isIgnoredAttributeKey =  isIgnoredAttributeKeyFromNode(n); // TODO recover ignored value
        for(var i = 0; i < n.attributes.length; i++) {
          var key = n.attributes[i].name;
          var ignored = isIgnoredAttributeKey(key);
          if(!isGhostAttributeKey(key) && !isSpecificGhostAttributeKey(key) || ignored) {
            var value = ignored ? ignoredAttributeValue(n, key) : n.attributes[i].value;
            if(typeof value == "undefined") continue;
            if(key == "style") {
              value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2)
            }
            attributes.push([key, value]);
          }
        }
      }
      var children = [];
      if(n.__editor__ && n.__editor__.ignoredChildNodes) {
        children = n.__editor__.ignoredChildNodes;
      } else {
        var childNodes = n.childNodes;
        if(tagName.toLowerCase() === "noscript" && n.childNodes.length === 1 && n.childNodes[0].nodeType === 3) {
          // We'll recover the associated HTML node
          childNodes = el("div", {}, [], {innerHTML: n.childNodes[0].textContent, parentNode: n}).childNodes;
        }
        if(!areChildrenGhosts(n)) {
          for(i = 0; i < childNodes.length; i++) {
            if(!isGhostNode(childNodes[i])) {
              children.push(domNodeToNativeValue(childNodes[i], postProcessing));
            }
          }
        }
      }
      result = [tagName, attributes, children];
    }
    if(postProcessing) { result = postProcessing(result, n) || result }
    return result;
  }
  editor.domNodeToNativeValue = domNodeToNativeValue;
  
  // Returns the closest ancestor of the selection having the given tagName
  function getElementArCaret(tagName) {
    var node = document.getSelection().anchorNode;
    var w = node != null && node.nodeType == 3 ? node.parentNode : node;
    if(!tagName) return w;
    while(w != null && w.tagName.toLowerCase() != tagName.toLowerCase()) {
      w = w.parentNode;
    }
    return w;
  }
  editor.getElementArCaret = getElementArCaret;

  // Removes all the text from a node (not the text nodes themselves)
  function emptyTextContent(node) {
    editor.userModifies();
    if(node != null) {
      if(node.nodeType == 3) {
        node.textContent = "";
      } else {
        for(let i = 0; i < node.childNodes.length; i++) {
          emptyTextContent(node.childNodes[i]);
        }
      }
    }
    return node;
  }
  editor.emptyTextContent = emptyTextContent;

  // Duplicates a node. Returns the cloned element that has been inserted.
  // options: {
  //   onBeforeInsert: callback to transform cloned element before it is inserted (identity by default)
  //   target: Node before which to place the duplicated node (original node by default)
  //   after: If true, place the cloned node after the target node. (false by default)
  //   ignoreText: if true, will not attempt to duplicate text / indentation. (false by default)
  // }
  function duplicate(node, options) {
    editor.userModifies();
    if(typeof options == "undefined") options = {}
    if(typeof options.onBeforeInsert != "function") options.onBeforeInsert = e => e;
    if(node != null && node.parentNode != null) {
      var parentInsertion = options.target ? options.target.parentElement : node.parentElement;
      var insertBeforeNode = options.after ? options.target ? options.target.nextSibling : node.nextSibling :
                                             options.target ? options.target             : node;
      if(node.nextSibling != null && !options.target && !options.ignoreText) {
        var next = node.nextSibling;
        if(next.nodeType == 3 && next.nextSibling != null &&
           next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "TH" || node.tagName == "LI" || node.tagName == "TD")) {
          var textElement = next.cloneNode(true);
          node.parentNode.insertBefore(textElement, options.after ? node.nextSibling : node);
          if(options.after) {
            insertBeforeNode = textElement.nextSibling;
          } else {
            insertBeforeNode = textElement
          }
        }
      }
      var duplicated = node.cloneNode(true);
      function removeEditorAttributes(node) {
        if(node.nodeType != 1) return;
        let attrs = [...node.attributes];
        for(var i = 0; i < attrs.length; i++) {
          if(attrs[i].name.match(/^ghost-clicked$|^translate-id.*$/)) {
            node.removeAttribute(attrs[i].name);
          }
        }
        for(var i = 0; i < node.childNodes; i++) {
          removeEditorAttributes(node.childNodes[i]);
        }
      }
      removeEditorAttributes(duplicated);
      
      var cloned = options.onBeforeInsert(duplicated);
      parentInsertion.insertBefore(cloned, insertBeforeNode);
      return cloned;
    }
  }
  editor.duplicate = duplicate;

  // Removes a node from the DOM
  // Attempts to remove the associated text element in the case of table elements or list items unless options.ignoreText is true
  function remove(node, options) {
    editor.userModifies();
    if(typeof options == "undefined") options = {}
    if(node.previousSibling != null && !options.ignoreText) { // Remove whitespace as well
      var next = node.nextSibling;
      if(next.nodeType == 3 && next.nextSibling != null &&
         next.nextSibling.tagName == node.tagName && (node.tagName == "TR" || node.tagName == "TH" || node.tagName == "LI" || node.tagName == "TD")) {
        next.remove();
      }
    }
    node.remove();
  }
  editor.remove = remove;

  editor._internals.mutationCallbacks = {};
   
  // TODO: In the future, only determine if the user was either using the interface, or modifying a selected element.
  // And then mark the mutation as computer or user. No more ghosts.
  editor._internals.mutationCallbacks.handleScriptInsertion = 
    // Mark nodes as ghost on insertion, if they are so.
    function handleScriptInsertion(mutations) {
      for(var i = 0; i < mutations.length; i++) {
        // A mutation is a ghost if either
        // -- The attribute starts with 'ghost-'
        // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
        // -- It is the modification of a node or an attribute inside a ghost node.
        var mutation = mutations[i];
        if(editor.hasGhostAncestor(mutation.target) || editor.hasIgnoringAncestor(mutation.target)) continue;
        if(mutation.type == "childList") {
          for(var j = 0; j < mutation.addedNodes.length; j++) {
            var insertedNode = mutation.addedNodes[j];
            if(editor.hasGhostAncestor(insertedNode)) {
              insertedNode.isghost = true;
            } else {
              if(typeof insertedNode.isghost === "undefined" && (insertedNode.nodeType == 1 && insertedNode.getAttribute("isghost") != "true" || insertedNode.nodeType == 3 && !insertedNode.isghost) && editor.ghostNodes.find(pred => pred(insertedNode, mutation))) {
               if(insertedNode.nodeType == 1) insertedNode.setAttribute("isghost", "true");
               insertedNode.isghost = true;
              } else { // Record ignored attributes
                if(insertedNode.nodeType == 1) {
                  var isIgnoredAttributeKey = editor.isIgnoredAttributeKeyFromNode(insertedNode);
                  for(var k = 0; k < insertedNode.attributes.length; k++) {
                    var attr = insertedNode.attributes[k];
                    isIgnoredAttributeKey(attr.name, attr.value);
                  }
                }
              }
            }
          }
        }
      }
    }
    
  
  editor.ui = typeof editor.ui == "object" ? editor.ui : { _internals: {}, model: undefined };
  editor.ui._internals.getLosslessCssParser = new Promise((resolve, reject) => {
      editor.ui._internals.setLosslessCssParser = x => { editor.ui.CSSparser = new x(); resolve(x) };
  });
  
  // Display an box to switch to edit mode.
  // This is the only item available for UI even if edit=false
  editor.ui._internals.switchEditBox = 
     function switchEditBox(toEdit) {
      let prev = toEdit ? "=false" : "(=true|=?$|=?(?=&))",
          next = toEdit ? ""  : "=false",
          icon = toEdit ? editor.svgFromPath("M 30.85,10.65 19.56,21.95 19.56,21.95 16.96,19.34 28.25,8.05 30.85,10.65 30.85,10.65 Z M 31.56,9.94 33.29,8.21 C 33.68,7.82 33.67,7.19 33.28,6.8 L 32.1,5.62 C 31.71,5.23 31.08,5.22 30.68,5.62 L 28.96,7.34 31.56,9.94 31.56,9.94 Z M 16.31,20.11 15.67,23.22 18.81,22.61 16.31,20.11 16.31,20.11 16.31,20.11 Z M 26.41,16.5 26.41,26.5 C 26.41,27.61 25.51,28.5 24.41,28.5 L 9.4,28.5 C 8.3,28.5 7.41,27.6 7.41,26.49 L 7.41,3.51 C 7.41,2.4 8.31,1.5 9.41,1.5 L 19.41,1.5 19.41,7.5 C 19.41,8.61 20.3,9.5 21.41,9.5 L 25.41,9.5 29.99,4.92 C 30.78,4.13 32.04,4.13 32.82,4.91 L 34,6.09 C 34.77,6.87 34.77,8.14 33.99,8.92 L 26.41,16.5 26.41,16.5 Z M 20.41,1.5 20.41,7.5 C 20.41,8.05 20.86,8.5 21.4,8.5 L 26.41,8.5 20.41,1.5 20.41,1.5 Z", true, 40, 40)  : "x",
         title = toEdit ? "Edit this page" : "Preview this page";
      return el("div#editbox.editor-interface", {title: title}, [
        el("style.editor-interface", {}, `
        #editbox {
          ${toEdit ?
         `position: fixed;
          ${editor.config.onMobile() ? "bottom" : "top"}: 0px;
          right: 0px;
          margin-${editor.config.onMobile() ? "bottom" : "top"}: 25px;
          margin-right: 25px;
          border-radius: 60px;
          background: var(--context-button-color);
          ` :
         `display: none`
         }z-index: 2000000;
          opacity: 1;
          cursor: pointer;
        }
        #editbox svg.context-menu-icon.fill>path {
          fill: #ffffff;
          fill-rule: evenodd;
          stroke: #FFFFFF;
          stroke-width: 0px;
          stroke-linecap: none;
          -linejoin: miter;
          stroke-opacity: 0;
        }
        #editbox:hover {
          background: var(--context-button-color-hover)
        }`),
        el("div.editor-interface", {}, [], { innerHTML: icon })
      ], {
        isghost: true,
        onclick(event) {
          if(!location.search.match(new RegExp("edit" + prev))) {
             if(editor.ui.init) {
               editor.config.canEditPage = true;
               editor.ui.init();
               document.body.setAttribute("contenteditable", "true");
               document.querySelector("#editbox").remove();
               if(typeof editor.config.onInit == "function") {
                 editor.config.onInit()
               }
               setTimeout(() => {
                 editor.ui._internals.restoreUndoRedo();
               }, 100);
             } else {
               location.search = location.search.startsWith("?") ? location.search + "&" + "edit" + next : "?edit" + next
             }
          } else {
             location.search = location.search.replace(new RegExp("edit" + prev, "g"), "edit" + next);
          }
        }
        });
    } // editor.ui._internals.switchEditBox

  // Hook Editor's core to the web window, add event listeners.
  editor.init = function() {
    
    /*
      Pretend loading a page using Editor's commands.
      Does not attempt to load Editor's interface.
    */
    window.onpopstate = function(e){
        console.log("onpopstate", e);
        if(e.state && e.state.localURL) {
          editor._internals.doReloadPage(String(location), true);
        } else {
          editor._internals.doReloadPage(location.pathname + location.search, true);
        }
    };
    
    var onCopy = function(event) {
      const selection = document.getSelection();
      if(selection.rangeCount && (!document.activeElement || (!document.activeElement.matches("#modify-menu *") && !document.activeElement.matches("textarea.ace_text-input") && !document.activeElement.matches("textarea, input")))) {
        let range = selection.getRangeAt(0); // Let's put the correct stuff in the clipboardData.
        let contents = range.cloneContents();
        let newHtmlData = "";
        for(let i = 0; i < contents.childNodes.length; i++) {
          let n = contents.childNodes[i];
          newHtmlData += n.nodeType == 1 ? n.outerHTML : el("div", {}, n.textContent).innerHTML;
        }
        let textContent = el("div", {}, [], {innerHTML: newHtmlData}).textContent;
        event.clipboardData.setData('text/plain', textContent);
        event.clipboardData.setData('text/html', newHtmlData);
        event.preventDefault();
      }
    };
    
    var onPaste = function(e) {
      if(editor.userModifies && e.clipboardData.types.indexOf("text/html") >= 0 && (!document.activeElement || !document.activeElement.matches("#modify-menu *, textarea, input"))) {
        e.preventDefault();
        e.stopPropagation();
        let content = e.clipboardData.getData("text/html").replace(/^\s*<html>\s*<body>\s*(<!--[^\-]*-->\s*)?|(\s*<!--[^\-]*-->)?\s*<\/body>\s*<\/html>\s*$/g, "");
        pasteHtmlAtCaret(content);
        return true;
      }
    };
    
    var onDocLoad = function(event) { 
      document.body.addEventListener("copy", onCopy);
      document.body.addEventListener("paste", onPaste, {capture: true});
      
      if(typeof editor.config.canEditPage == "boolean" && !editor.config.canEditPage && !editor.config.varls) {
        document.body.insertBefore(editor.ui._internals.switchEditBox(true), document.body.childNodes[0]);
      }
      
      if(!editor.config.thaditor && editor.config.editIsFalseButDefaultIsTrue) {
        // Special case when ?edit=false but the default behavior is edit=true if nothing is set.
        // Happens only in editor webserver.
        // It continues to add edit=false to any clicked links.
        document.onclick = function (e) {
            var addEditEqualToUrl = function(href, what) {
              if(href.indexOf("://") == -1) { // Instrument the relative link so that it is edit=true
                if(href.indexOf("?") >= 0) {
                  if(href.endsWith("?")) {
                    href = href + "edit=" + what
                  } else {
                    href = href + "&edit=" + what
                  }
                } else {
                  href = href + "?edit=" + what
                }
              }
              return href;
            }
            e = e ||  window.event;
            var node = e.target || e.srcElement;
            while(node) {
              if(node.tagName == "A" && node.getAttribute("href") && !node.onclick && !node.getAttribute("onclick")) {
               var newLocation = addEditEqualToUrl(node.getAttribute("href"), "false");
               console.log(newLocation);
               window.location.href = newLocation;
               e.stopPropagation();
               return false;
              } else {
                node = node.parentNode;
              }
            }
          }
      } // end if !editor.config.thaditor && editor.config.editIsFalseButDefaultIsTrue
    };
    
    // Events handler for copying and pasting, so that we have the least surprise.
    // TODO: When the document is loaded, inject the interface?
    document.addEventListener("DOMContentLoaded", onDocLoad); // onDOMContentLoaded
    
    // Removes all callbacks
    editor.uninit = function() {
      window.onpopstate = undefined;
      document.removeEventListener("DOMContentLoaded", onDocLoad);
      document.body.removeEventListener("copy", onCopy);
      document.body.removeEventListener("paste", onPaste);
      document.body.insertBefore(editor.ui._internals.switchEditBox(true), document.body.childNodes[0]);
      document.onclick = undefined;
      if (typeof editor._internals.automaticGhostMarker !== "undefined") {
        // console.log("automaticGhostMarker.disconnect()");
        editor._internals.automaticGhostMarker.disconnect();
      }
    }
    
    // Immediately start listening to insertions to mark them as ghosts.
    // TODO: In the future, we won't care anymore.
    if (typeof editor._internals.automaticGhostMarker !== "undefined") {
      // console.log("automaticGhostMarker.disconnect()");
      editor._internals.automaticGhostMarker.disconnect();
    }

    editor._internals.automaticGhostMarker = new MutationObserver(editor._internals.mutationCallbacks.handleScriptInsertion);
    editor._internals.automaticGhostMarker.observe( document.head.parentElement
     , { attributes: false
       , childList: true
       , characterData: false
       , attributeOldValue: false
       , characterDataOldValue: false
       , subtree: true
       }
     );
   } // editor.init
  
  editor.config.onMobile = () => window.matchMedia("(max-width: 800px)").matches;
  editor.config.buttonHeight = () => editor.config.onMobile() ? 48 : 30;
  editor.config.buttonWidth  = () => editor.config.onMobile() ? 48 : 40;

  // Helpers: Text preview and summary
  function textPreview(element, maxLength) {
    let x = element.textContent;
    let result = "'" + x + "'";;
    if(x == "") {
      if(element.tagName === "META") {
        result = element.getAttribute("charset") ? "charset:" + element.getAttribute("charset")  :
                (element.getAttribute("name") || element.getAttribute("http-equiv") || "(name?)") + ": " + (element.getAttribute("content") || "(content?)");
      } else if(element.tagName === "SCRIPT" || element.tagName === "IMG") {
        result = typeof element.getAttribute("src") === "string" ? (element.getAttribute("src") || "(src?)").replace(/(https?:\/\/)?(www\.)?/, "") : "empty script";
      } else if(element.tagName === "LINK") {
        result = typeof element.getAttribute("href") === "string" ? (element.getAttribute("href") || "(src?)").replace(/(https?:\/\/)?(www\.)?/, "") : "empty script";
      }
    }
    if(typeof maxLength !== "undefined" && result.length > maxLength) {
      return result.substring(0, maxLength) + "...'";
    }
    return result;
  }
  function summary(element, idAndClasses, maxLength) {
    var summary = element.tagName.toLowerCase();
    if(idAndClasses && element.getAttribute("id")) {
      summary += "#" + element.getAttribute("id");
    }
    var elemClass = element.getAttribute("class");
    if(idAndClasses && elemClass && elemClass.trim().length) {
      summary += "." + elemClass.split(/\s+/g).join(".");
    }
    summary += " " + textPreview(element);
    maxLength = maxLength || 80;
    summary = summary.substring(0, maxLength || 80) + (summary.length > 80 ? "..." : "");
    return summary;
  }
  
  function getTempCSSName(CSSFilePath) {
    let newFilePath = CSSFilePath.split("/");
    let newFileName = `tmp-${editor.config.userName}-${newFilePath[newFilePath.length - 1]}`;
    newFilePath[newFilePath.length - 1] = newFileName;
    newFilePath = newFilePath.join("/");
    return newFilePath;
  }
  
  function editor_stopWatching() {
    editor.ui.model.outputObserver.disconnect();
  }
  
  function editor_resumeWatching() {
    editor.ui.model.outputObserver.observe
      ( document.body.parentElement
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: true
        });
  }
  
  function removeTimestamp(path) {
    var dummyIndex = path.indexOf("?");
    if(dummyIndex > -1) {
      path = path.slice(0, dummyIndex);
    }
    return path;
  }
  function setTimestamp(path) {
    path = removeTimestamp(path);
    path += "?timestamp=" + (+new Date());
    return path;
  }
  
  function isAbsolute(url) {
    return url.match(/^https?:\/\/|^www\.|^\/\//);
  }
  function linkToEdit(link) {
    return link && !isAbsolute(link) ? link.match(/\?/) ? link + mbEditA : link + mbEditQ: link;
  }
  
  // Helper.
  function relativeToAbsolute(url) {
    if(isAbsolute(url) || url && url.length && url[0] == "/") return url;
    let u =  new URL(location.href);
    if(url[0] === "#") {
      return u.pathname + url; 
    }
    else {
      return u.pathname.replace(/[^\/]*$/, "") + url;
    }
  }

  // Saves the Document Object Model, bare version.
  editor.saveDOM = function saveDom() {
    if(document.getElementById("notification-menu") != null) {
      //document.getElementById("notification-menu").innerHTML = `Please wait until previous saving completes.`
      // TODO: Listen and gather subsequent modifications when it is loading
      return;
    }
    editor.ui.model.isSaving = true;
    var newMenu = el("menuitem#notification-menu.to-be-selected", {isghost: true});
    if(document.getElementById('lastaction')) {
      document.getElementById('lastaction').remove();
    }
    if(document.getElementById("modify-menu")) {
      document.getElementById("modify-menu").append(newMenu);
    }
    if(editor.config.thaditor) {
      editor.ui.model.actionsAfterSaveState = [];
    }
    editor.ui.refresh();
    editor.ui.sendNotification("Saving...");
    let doSendDom = domToSend => {
      if(domToSend) {
        const toSend = JSON.stringify(domToSend);
        editor.ui._internals.notifyServer({"question": editor.ui.model.askQuestions ? "true" : "false"}, toSend, "Save")
      }
    }
    if(typeof thaditor == "object" && typeof thaditorfast == "object") {
      thaditorfast.pagePromise.then(page => {
        let domToSend;
        try {
          domToSend = bam.apply(thaditorfast.changesToSave(), page);
        } catch(e) {
          console.log("Problem while trying to save changes in Thaditorfast", e);
          if(!window.confirm("I could not save the changes using the fast engine. See console for details.\nUse the regular engine?")) {
            editor.ui.model.isSaving = false;
            editor.refresh();
            return; 
          }
          domToSend = editor.domNodeToNativeValue(document);
        }
        return domToSend;
      }).then(doSendDom);
    } else {
      doSendDom(editor.domNodeToNativeValue(document));
    }
  } //editor.saveDOM
  
  
  function isLive() {
    return !(editor.config.path.includes("Thaditor/versions/"));
  }

  //Version used
  function computeDraftVersion() {
    return isLive() ? "Live" : editor.config.path.slice(editor.config.path.lastIndexOf("versions/")+9, editor.config.path.lastIndexOf("/"));
  }
  
  var ifAlreadyRunning = typeof editor == "object" && typeof editor.ui === "object" && typeof editor.ui.model === "object";   
  
  /******************************
        Editor's interface.
  ******************************/
  editor.ui._internals.loadInterface = function() {
    // Insets the modification menu.
    editor.ui._internals.contextMenu = document.querySelector("div#context-menu");
    if(!editor.ui._internals.contextMenu) {
      editor.ui._internals.contextMenu = el("div#context-menu", {contenteditable: "false", isghost: "true"}, [], {isghost: true});
      document.body.insertBefore(editor.ui._internals.contextMenu, document.body.childNodes[0]);
    }
    editor.ui._internals.modifyMenu = document.querySelector("div#modify-menu");
    if(!editor.ui._internals.modifyMenu) {4
      editor.ui._internals.modifyMenu = el("div#modify-menu", {contenteditable: "false", isghost: "true"}, [], {isghost:true})
      document.body.insertBefore(editor.ui._internals.modifyMenu, document.body.childNodes[0]);
    }
    prevStyleElem = editor.ui._internals.styleElem;
    editor.ui._internals.styleElem = document.querySelector("link#server-elm-style");
    if(!editor.ui._internals.styleElem) {
      if(prevStyleElem) {
        document.head.append(prevStyleElem);
        editor.ui._internals.styleElem = prevStyleElem;
      } else {
        editor.ui._internals.styleElem = el("link#server-elm-style",{rel:"stylesheet", type:"text/css",href:"/server-elm-style.css",class:"editor-interface",isghost:"true"});
        document.head.append(editor.ui._internals.styleElem);
      }
    }    
    /*
      Pushes the notification msg to the log & displays it for 3 seconds directly left of the moidfymenu.
      css for notification box is textarea .notif
    */
    editor.ui.sendNotification = function sendNotification(msg, timeout) {
      let modifyMenuDiv = editor.ui._internals.modifyMenu;
      if (!modifyMenuDiv) {
        console.log("Notifications havent been set up for use outside of editor, like in the filesystem");
        console.log (msg);
        return;
      }
      let notifBox = modifyMenuDiv.querySelector("#notif-box");
      if (!notifBox) {
        notifBox = el("textarea", {id:"notif-box", class:"textarea notifs editor-interface", visibility:true, readonly:true, isghost:true}, [], {value:msg});
        editor.ui._internals.modifyMenu.append(notifBox);
      }
      notifBox.value = msg;
      notifBox.style.display = "block";
      notifBox.classList.toggle("visible", true);
      notifBox.style.zIndex = 100;
      notifBox.style.visibility = true;
      editor.ui.model.editor_log.push(msg);
      var logWindow = getEditorInterfaceByTitle("Log");
      if(logWindow) logWindow.refresh();
      setTimeout(function hideNotification() {
        let notifBox = document.getElementById("notif-box");
        if (notifBox) {
          notifBox.classList.toggle("visible", false);
          setTimeout(() => notifBox.remove(), 500);
        }
      }, timeout ? timeout : 3000);
    };
    
    // Sub-interfaces can register functions that will store some information that will be inserted at the same key in the record editor.ui.model.restoredAfterReload.
    editor.ui._internals.saveBetweenReloads = {};
    
    // Same as editor._internals.writeDocument, but tries to preserve as much of the DOM and toolbar state as possible.
    editor.ui._internals.writeDocument = (function() {
      // Save/Load ghost/ignored attributes/nodes for when a page is reloaded, only if elements have an id.
      // Same for some attributes
      function saveGhostAttributes() {
        var ghostModified = document.querySelectorAll("[ghost-visible]");
        var savedGhostAttributes = [];
        for(var i = 0; i < ghostModified.length; i++) {
          var elem = ghostModified[i];
          savedGhostAttributes.push([editor.toTreasureMap(elem),
              "ghost-visible", ghostModified[i].getAttribute("ghost-visible")]);
        }

        function saveAttributes(name) {
          var ghostAttributesModified = document.querySelectorAll("["+name+"]");
          for(var i = 0; i < ghostAttributesModified.length; i++) {
            var elem = ghostAttributesModified[i];
            var toSave = elem.getAttribute(name).split(" ");
            for(j in toSave) {
              var key = toSave[j];
              savedGhostAttributes.push([editor.toTreasureMap(elem), key, elem.getAttribute(key)]);
            }
          }
        }
        saveAttributes("save-ghost-attributes");
        saveAttributes("save-ignored-attributes");  
      
        var elemsWithAttributesToSave = document.querySelectorAll("[save-properties]");
        var savedProperties = [];
        for(var i = 0; i < elemsWithAttributesToSave.length; i++) {
          var elem = elemsWithAttributesToSave[i];
          var toSave = elem.getAttribute("save-properties").split(" ");
          for(j in toSave) {
            var key = toSave[j];
            savedProperties.push([editor.toTreasureMap(elem), key, elem[key]])
          }
        }
        var parentsGhostNodes = [];
        var ghostElemsToReinsert = document.querySelectorAll("[save-ghost]");
        for(var i = 0; i < ghostElemsToReinsert.length; i++) {
          var elem = ghostElemsToReinsert[i];
          parentsGhostNodes.push({parent: editor.toTreasureMap(elem.parentNode), node: elem});
        }
        return [savedGhostAttributes, savedProperties, parentsGhostNodes];
      }
      function applyGhostAttributes(attrs) {
        var [savedGhostAttributes, savedProperties, parentsGhostNodes] = attrs;
        for(var i = 0; i < savedGhostAttributes.length; i++) {
          var [data, key, attr] = savedGhostAttributes[i];
          var elem = editor.fromTreasureMap(data);
          if(elem != null) {
            elem.setAttribute(key, attr);
          }
        }
        for(var i = 0; i < savedProperties.length; i++) {
          var [data, key, value] = savedProperties[i];
          var elem = editor.fromTreasureMap(id);
          if(elem != null) {
            elem[key] = value;
          }
        }
        for(var i = 0; i < parentsGhostNodes.length; i++) {
          var {parent: data, node: elem} = parentsGhostNodes[i];
          var parent = editor.fromTreasureMap(data);
          if(parent != null) {
            if(!elem.getAttribute("id") || !document.getElementById(elem.getAttribute("id"))) {
              parent.appendChild(elem);
            }
          }
        }
      }
      
      function saveBeforeReloadingToolbar() {
        editor.ui.model.restoredAfterReload = {};
        for(let k in editor.ui._internals.saveBetweenReloads) {
          editor.ui.model.restoredAfterReload[k] = editor.ui._internals.saveBetweenReloads[k]();
        }
        console.log("saved before reloading toolbar", editor.ui.model.restoredAfterReload);
      }
      
      return function writeDocument(newContent) {
        let saved = saveGhostAttributes();
        saveBeforeReloadingToolbar();
        if(editor.ui.model.caretPosition) {
          editor.ui.model.caretPosition = dataToRecoverCaretPosition(editor.ui.model.caretPosition);
        }
        if(editor.ui.model.selectionRange) {
          editor.ui.model.selectionRange = dataToRecoverSelectionRange(editor.ui.model.selectionRange);
        }
        if(editor.ui.model.clickedElem) {
          editor.ui.model.clickedElem = editor.toTreasureMap(editor.ui.model.clickedElem);
        }
        let scrollX = window.scrollX;
        let scrollY = window.scrollY;
        editor._internals.writeDocument(newContent);
        applyGhostAttributes(saved);
        setTimeout(() => window.scroll(scrollX, scrollY), 10);
      };
    })(); // editor.ui._internals.writeDocument 
    
      // Handle a rewrite message from the worker
    editor.ui._internals.handleRewriteMessage = function(e) {
      //Rewrite the document, restoring some of the UI afterwards.
      if(editor.config.fast) {
        // TODO: Change the query parameters.
        var newQueryStr = e.data.newQueryStr;
        if(newQueryStr) {
          var strQuery = "";
          if(newQueryStr != null) { //newQueryStr = undefined ==> (newQueryStr !== null) ==> false;
            var newQuery = JSON.parse(newQueryStr);
            for(var i = 0; i < newQuery.length; i++) {
              var {_1: key, _2: value} = newQuery[i];
              strQuery = strQuery + (i == 0 ? "?" : "&") + key + (value === "" && key == "edit" ? "" : "=" + value)
            } 
          }
          document.location.search = strQuery;
        } else {
          document.location.reload();
        }
      } else {
        editor.ui._internals.handleRewriteMessageWithText(e, e.data.text);
      }
    }
    
    editor.ui._internals.handleRewriteMessageWithText = function(e, text) {
      let editor_model = editor.ui.model;
      editor_model.isSaving = false;
      editor.ui._internals.writeDocument(text);
      
      var newLocalURL = e.data.newLocalURL;
      var newQueryStr = e.data.newQueryStr;
      var ambiguityKey = e.data.ambiguityKey;
      var ambiguityNumber = e.data.ambiguityNumber;
      var ambiguitySelected = e.data.ambiguitySelected;
      var ambiguityEnd = e.data.ambiguityEnd;
      var ambiguitySummaries = e.data.ambiguitySummaries;
      var opSummaryEncoded = e.data.opSummaryEncoded;
      var replaceState = e.data.customRequestHeaders && e.data.customRequestHeaders.replaceState == "true";
      if(ambiguityKey !== null && typeof ambiguityKey != "undefined" &&
         ambiguityNumber !== null && typeof ambiguityNumber != "undefined" &&
         ambiguitySelected !== null && typeof ambiguitySelected != "undefined") {
        var n = JSON.parse(ambiguityNumber);
        console.log ("editor.ui._internals.handleRewriteMessage ambiguity");
        var selected = JSON.parse(ambiguitySelected);
        var summaries = JSON.parse(ambiguitySummaries);
        
        var disambiguationMenuContent = [];
        disambiguationMenuContent.push(el("span#ambiguity-id", {v: ambiguityKey}, "Choose the update you prefer, and click the save button:"));
        // Find the common path of all files so that we don't need to repeat its name or path.
        var fileOf = x => x.replace(/^(.*): *\n[\s\S]*$/, "$1")
        var commonPrefix = fileOf(summaries[1]);
        for(var i = 1; i <= n; i++) {
          while(!fileOf(summaries[i - 1]).startsWith(commonPrefix)) {
            let n = commonPrefix.replace(/^(.*)(?:\/|\\).*$/, "$1");
            commonPrefix = n === commonPrefix ? "" : n;
          }
        }
        if(commonPrefix) {
          disambiguationMenuContent.push(el("span#ambiguity-prefix", {}, commonPrefix + ":"));
        }
        for(var i = 1; i <= n; i++) {
          var summary = summaries[i-1].substring(commonPrefix.length).
                replace(/"/g,'&quot;').
                replace(/</g, "&lt;").
                replace(/---\)|\+\+\+\)/g, "</span>").
                replace(/\(---/g, "<span class='remove'>").
                replace(/\(\+\+\+/g, "<span class='add'>").
                replace(/(\nL\d+C\d+:)(.*)/, "$1<span class='codepreview'>$2</span>");
          disambiguationMenuContent.push(el("span.solution" + (i == selected ? ".selected" : "") + (i == n && ambiguityEnd != 'true' ? '.notfinal' : ''), {
          title: i == selected ? "Currently displaying this solution" : "Select this solution" + (i == n && ambiguityEnd != 'true' ? " (compute further solutions after if any)" : ""), onclick: i == selected ? `` : `this.classList.add('to-be-selected'); editor.ambiguity.select('${ambiguityKey}', ${i})`}, "", {innerHTML: "#" + i + " " + summary}));
        }
        disambiguationMenuContent.push(el("button#ambiguityCancel.action-button", {title: "Revert to the original version", onclick: `editor.ambiguity.cancel("${ambiguityKey}", ${selected})`}, "Cancel"));
        editor_model.disambiguationMenu = el("div.disambiguationMenu", {}, disambiguationMenuContent);
        editor_model.disambiguationMenu.ambiguityKey = ambiguityKey;
        editor_model.disambiguationMenu.selected = selected;
        editor_model.clickedElem = undefined;
        editor_model.displayClickedElemAsMainElem = true;
        editor_model.notextselection = false;
        editor_model.caretPosition = undefined;
        editor_model.link = undefined;
        var advancedBlock = getEditorInterfaceByTitle("Advanced");
        if(advancedBlock) advancedBlock.minimized = false;
        editor_model.visible = true;
        //editor_model.displaySource: false, // Keep source opened or closed
        // TODO: Disable click or change in DOM until ambiguity is resolved.
      } else { //no ambiguity
        if(editor_model.disambiguationMenu && editor_model.disambiguationMenu.replayActionsAfterSave) {
          console.log("disambiguationMenu was there. replaying actions");
          editor_model.disambiguationMenu.replayActionsAfterSave("Modifications applied");
        } else {
          console.log("disambiguationMenu is not there.");
        }
        editor_model.disambiguationMenu = undefined;
        if(opSummaryEncoded) {
          var opSummary = decodeURI(opSummaryEncoded);
          opSummary =
            opSummary.
            replace(/</g, "&lt;").
            replace(/---\)/g, "</span>").
            replace(/\(---/g, "<span class='remove'>").
            replace(/\+\+\+\)/g, "</span>").
            replace(/\(\+\+\+/g, "<span class='add'>");
          editor_model.editor_log.push(opSummary);
          // editor.ui.sendNotification(opSummary);
        }
      } // /noambiguity
      var strQuery = "";
      if(newQueryStr != null) { //newQueryStr = undefined ==> (newQueryStr !== null) ==> false;
        var newQuery = JSON.parse(newQueryStr);
        for(var i = 0; i < newQuery.length; i++) {
          var {_1: key, _2: value} = newQuery[i];
          strQuery = strQuery + (i == 0 ? "?" : "&") + key + (value === "" && key == "edit" ? "" : "=" + value)
        } 
      }
      if(newLocalURL) { // Overrides query parameters
        window.history[replaceState ? "replaceState" : "pushState"]({localURL: newLocalURL}, "Nav. to " + newLocalURL, newLocalURL);
      } else if(strQuery) {
        window.history.replaceState({}, "Current page", strQuery);
      }
      // editor.ui.refresh(); // The interface will be automatically refreshed after loading.
    }; // editor.ui._internals.handleRewriteMessage
    
    // Used only by the Editor webserver (editor.config.thaditor == false)
    editor.ui._internals.handleServerResponse = xmlhttp => function () {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
          editor.ui._internals.handleRewriteMessage({
            data: {
              newLocalURL: xmlhttp.getResponseHeader("New-Local-URL"),
              newQueryStr: xmlhttp.getResponseHeader("New-Query"),
              ambiguityKey: xmlhttp.getResponseHeader("Ambiguity-Key"),
              ambiguityNumber: xmlhttp.getResponseHeader("Ambiguity-Number"),
              ambiguitySelected: xmlhttp.getResponseHeader("Ambiguity-Selected"),
              ambiguityEnd: xmlhttp.getResponseHeader("Ambiguity-End"),
              ambiguitySummaries: xmlhttp.getResponseHeader("Ambiguity-Summaries"),
              opSummaryEncoded: xmlhttp.getResponseHeader("Operations-Summary"),
              customRequestHeaders: xmlhttp.customRequestHeaders,
              text: xmlhttp.responseText
            }
          })
        } //xhr.onreadystatechange == done
    }; //editor.ui._internals.handleServerResponse
    
    editor.ui._internals.handleSendRequestFinish = function(data) {
      console.log("editor.ui._internals.handleSendRequestFinish", data);
      // TODO: In case of ambiguity, only replay undo/redo after ambiguity has been resolved.
      
      editor.ui.model.outputObserver.disconnect();
      editor.ui._internals.handleRewriteMessage({data: data});
      document.addEventListener("DOMContentLoaded", function(event) {
      // Now the page is reloaded, but the scripts defining Editor have not loaded yet.
        var replayActionsAfterSave = msg => function(msgOverride) {
          console.log("replaying actions after save");
          editor.removeExplicitNodes();
          editor.ui._internals.replayActions(editor.ui.model.actionsAfterSaveState, editor.ui.model.undoStack, editor.ui.model.redoStack);
          if(editor.ui.model.actionsAfterSaveState.length) {
            editor.ui.refresh();
          }
          // We're done.
          editor.ui.model.actionsAfterSaveState = [];
          if(msg) {
            setTimeout(function saveCompleted(count) {
               if(editor.ui.sendNotification) {
                 editor.ui.sendNotification(editor.ui.model.actionsAfterSaveState.length === 0 || !msgOverride ? msg : msgOverride);
               } else {
                 setTimeout(() => saveCompleted((count || 0) + 1), (count||0)*10);
               }
            }, 0);
          }
        }
        let what = data.what ? data.what + " completed." : undefined;
        if(!data.isAmbiguous || !editor.ui.model.disambiguationMenu) {
          replayActionsAfterSave(what)();
        } else {
          editor.ui.model.disambiguationMenu.replayActionsAfterSave = replayActionsAfterSave(what);
        }
      });
    }; // editor.ui._internals.handleSendRequestFinish
    
    // The "what" is so that we can show a notification when this is done
    editor.ui._internals.notifyServer = function(requestHeaders, toSend, what) {
      if(editor.config.thaditor) {
        thaditor.do( {action:"sendRequest",
                    toSend: toSend || "{\"a\":2}",
                    aq:editor.ui.model.askQuestions,
                    loc: location.pathname + location.search,
                    requestHeaders: requestHeaders,
                    what: what}
        ).then(editor.ui._internals.handleSendRequestFinish);
      } else {
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.onreadystatechange = editor.ui._internals.handleServerResponse(xmlhttp);
        xmlhttp.open("POST", location.pathname + location.search);
        xmlhttp.setRequestHeader("Content-Type", "application/json");
        if(requestHeaders) {
          for(let k in requestHeaders) {
            xmlhttp.setRequestHeader(k, requestHeaders[k]);
          }
        }
        xmlhttp.customRequestHeaders = requestHeaders;
        xmlhttp.send(toSend || "{\"a\":2}");
      }
    }; // editor.ui._internals.notifyServer
    
    // Recomputes the page and display it entirely.
    editor.reload = function reloadPage() {
      editor.ui.sendNotification("Reloading...");
      editor.ui._internals.notifyServer({reload: "true"}, undefined, "Reload");
    }; // editor.reload
    
    editor.ui.reload = editor.reload;
    
    // Computes the file at the given URL and display it with Editor.
    // If replaceState it true, the back button will not work.
    editor.navigateTo = function navigateTo(url, replaceState) {
      url = relativeToAbsolute(url);
      if(editor.config.fast) {
        location.href = url;
      } else {
        editor.ui.sendNotification("Loading...");
        editor.ui._internals.notifyServer({reload: "true", url: url, replaceState: ""+replaceState}, undefined, "Page load");
      }
    }; // editor.navigateTo
    
    editor.ui.navigateTo = editor.navigateTo;
    
    // API to deal with ambiguity
    editor.ambiguity = {};
    editor.ui.ambiguity = editor.ambiguity;
    
    // Select and display the num-th alternative of the ambiguity indexed by key.
    editor.ambiguity.select = function(key, num) {
      editor.ui._internals.notifyServer({"ambiguity-key": key, "select-ambiguity": JSON.stringify(num), "question": "true"});
    }; // editor.ambiguity.select
    
    // Select the ambiguity result If the page displayed after an ambiguity is detected.
    // For Thaditor, this changes nothing because files are already written, it only clears the ambiguity data.
    editor.ambiguity.accept = function(key, num) {
      editor.ui._internals.notifyServer({"ambiguity-key": key, "accept-ambiguity": JSON.stringify(num)});
    }; // editor.ambiguity.accept
    
    // Cancels the entire change that presented an ambiguity, indexed by key.
    editor.ambiguity.cancel = function(key, num) {
      editor.ui._internals.notifyServer({"ambiguity-key": key, "cancel-ambiguity": JSON.stringify(num)});
    }; // editor.ambiguity.cancel
    
    // Saves the CSS and stores undo/redo before saving the DOM.
    editor.ui.save = function save() {
      if (editor.ui.model.isSaving) {
        editor.ui.sendNotification("Can't save while save is being undertaken");
      } else {
        //temp place to put CSS file loading stuff (may well be moved later)
        let allPageLinks = document.querySelectorAll("link[rel=stylesheet]");
        (async () => {
          for(let e = 0; e < allPageLinks.length; e++) {
            let linkNode = allPageLinks[e];
            if(!editor.isGhostNode(linkNode) && linkNode.getAttribute("ghost-href")) {
              let linkNodeTmpHref = linkNode.getAttribute("href");
              let linkNodeOriginalHref = linkNode.getAttribute("ghost-href");
              let trueTempPath = removeTimestamp(linkNodeTmpHref); // This one is absolute normally
              let originalAbsPath = relativeToAbsolute(removeTimestamp(linkNodeOriginalHref));
              let newValue = (await editor.getServer("read", trueTempPath)) || "";
              await editor.postServer("write", originalAbsPath, newValue);
              linkNode.cachedContent = newValue;
              linkNode.tmpCachedContent = newValue;
              // Since we delete the temporary file, we cannot revert to the temporary href.
              // Problem: it's not possible to undo small changes.
              // Worse: The deletion of the tmp css file will make it impossible to undo to states before of before this saving.
              // Ghost attributes are not restored when doing undo.
              editor_stopWatching();
              linkNode.setAttribute("href", setTimestamp(linkNodeOriginalHref));
              linkNode.removeAttribute("ghost-href");
              editor_resumeWatching();
              if(editor.config.fast) {
                editor.userModifies();
                thaditorfast.handleMutations([
                  { type: "attributes",
                    attributeName: "href",
                    target: linkNode },
                  { type: "attributes",
                    attributeName: "ghost-href",
                    target: linkNode },
                ]);
              }
              await editor.postServer("unlink", trueTempPath);
            }                 
          }
          editor.ui.model.actionsAfterSaveState = [];
          editor.ui._internals.saveUndoRedo();
          if(!this || typeof this != "object" || typeof this.classList == "undefined" || !this.classList.contains("disabled")) {
            editor.saveDOM();
          }
        })();
      } // if editor.ui.model.isSaving
    };  // editor.ui.save
    
    let nodeCacheTag = Symbol("nodeCacheTag");
    editor.ui._internals.nodeCacheTag = nodeCacheTag;
    
    // Converts an element to the ArrayNode which holds a pointer to the element in a Symbol.
    // Every ArrayNode contains a pointer to the element it represents.
    function domNodeToNativeValueWithCache(x) {
      return domNodeToNativeValue(x, (arrayNode, c) => {
        arrayNode[nodeCacheTag] = c;
        return arrayNode;
      })
    }
    let forEachArrayNode = function forEachArrayNode(callback, initNode) {
      function aux(arrayNode) {
        callback(arrayNode);
        if(Array.isArray(arrayNode) && arrayNode.length == 3) {
          for(var i = 0; i < arrayNode[2].length; i++) {
            aux(arrayNode[2][i]);
          }
        }
      }
      aux(initNode || document);
    }

    // Given ArrayNode, creates and cache an associated element to that.
    function arrayNodeToDomNode(arrayNode) {
      if(nodeCacheTag in arrayNode) {
        return arrayNode[nodeCacheTag];
      }
      let r;
      if(arrayNode.length == 2) {
        if(arrayNode[0] == "TEXT") {
          r = document.createTextNode(arrayNode[1]);
        } else if(arrayNode[0] == "COMMENT") {
          r = document.createComment(arrayNode[1]);
        }
      } else if(arrayNode.length == 3) {
        r = document.createElement(arrayNode[0]);
        let attrsArray = arrayNode[1];
        let childrenArray = arrayNode[2];
        for(let i = 0; i < attrsArray.length; i++) {
          let keyValue = attrsArray[i];
          let key = keyValue[0];
          if(key == "style") {
            r.setAttribute(key, keyValue[1].map(([kv, vv]) => kv + ":" + vv).join(";"));
          } else {
            r.setAttribute(key, keyValue[1]);
          }
        }
        for(let i = 0; i < childrenArray.length; i++) {
          r.append(arrayNodeToDomNode(childrenArray[i]));
        }
      } else {
        console.log("Don't know how to create an element like this: ", arrayNode);
      }
      arrayNode[nodeCacheTag] = r;
      return r;
    }
    
    // Converts the node to a treasure map so that it can be retrieved later if the node is replaced.
    // Uses the current state of the DOM.
    function toTreasureCache(node, serialized) {
      let result = { [nodeCacheTag]: node, treasureMap: editor.toTreasureMap(node) };
      if(serialized && node) {
        result.serialized = domNodeToNativeValueWithCache(node);
      }
      return result;
    }
    // Returns a connected node associated to a treasure cache, using the treasure map to find it if necessaru
    function fromTreasureCache(treasureCache, isUndo) {
      let cache = treasureCache[nodeCacheTag];
      if(cache) return cache;
      let e, treasureMap;
      if(isUndo) {
        treasureMap = treasureCache.treasureMap;
        if(treasureMap) {
          e = editor.fromTreasureMap(treasureMap, treasureMap.source);
        } else if(treasureCache.serialized) {
          e = arrayNodeToDomNode(treasureCache.serialized);
        }
      } else {
        treasureMap = treasureCache.treasureMapRedo;
        if(treasureMap) {
          e = editor.fromTreasureMap(treasureMap, treasureMap.source);
        } else if(treasureCache.serialized) {
          e = arrayNodeToDomNode(treasureCache.serialized);
        }
      }
      treasureCache[nodeCacheTag] = e;
      return e;
    }
    // type ArrayAttribute = [String \ "style", String] | ["style", [String, String][]]
    // type ArrayChildren = ArrayNode[]
    // type ArrayAttributes = ArrayAttribute[]
    // type ArrayNode = [String, ArrayAttributes, ArrayChildren]
    //                | ["TEXT", String]
    //                | ["COMMENT", String]
    //                | ["!DOCTYPE", String, String, String]
    // // Afterwards and before means in the same batch fo mutations
    // type TreasureMapRemovedAfterwards = TreasureMap & { source?: {next: Int, indexRemoved:Int} } // Source if element in removed elments afterwards
    // type TreasureMapAddedBefore       = TreasureMap & { source?: {prev: Int, indexAdded :Int} }   // Source if element in added elmeents before.
    // type TreasureMapAddedAfterwards   = TreasureMap & { source?: {next: Int, indexAdded :Int} } // Source if element in removed elments afterwards
    // type TreasureMapRemovedBefore     = TreasureMap & { source?: {prev: Int, indexRemoved:Int} }   // Source if element in added elmeents before.
    // type CachedNodeRemoved = { treasureMapRedo: TreasureMapAddedBefore,     // If removed node was created before, when we stand before replaying the actions
    //                            treasureMap:     TreasureMapAddedAfterwards, // If removed node was reinserted afterwards, when we stand before undoing the actions.
    //                            serialized: ArrayNode,                       // If needs to be restored in an undo, when the cache is empty
    //                            Symbol(nodeCacheTag)?: HTMLNode }
    // type CachedNodeAdded = {   treasureMapRedo: TreasureMapRemovedBefore,    // If added node was removed before, when we stand before replaying the actions
    //                            treasureMap:     TreasureMapRemovedAfterwards,// If added node was removed afterwards, when we stand before undoing the actions
    //                            serialized: ArrayNode,                        // If needs to be restored in a redo, when the cache is empty
    //                            Symbol(nodeCacheTag)?: HTMLNode }
    // type CachedTreasureMap = { treasureMap:     TreasureMapRemovedAfterwards, // Where this element was removed afterwards, where it is located. when we stand before undoing the actions.
    //                            treasureMapRedo: TreasureMapAddedBefore,       // Where this element was added before, where it is locaed, when we stand before replaying the actions.
    //                            Symbol(nodeCacheTag)?: HTMLNode }
    // type StoredMutation =
    //          {type: "childList",     target: CachedTreasureMap, removedNodes: CachedNodeRemoved[], addedNodes: CachedNodeAdded[],
    //                                  previousSibling: CachedTreasureMap, nextSibling: CachedTreasureMap }
    //        | {type: "characterData", target: CachedTreasureMap, oldValue: String, newValue: String }
    //        | {type: "attributes",    target: CachedTreasureMap, oldValue: String | null, newValue: String | null, attributeName: String }
    //        | {type: "linkHrefCSS",   target: CachedTreasureMap, oldValue: String, newValue: String }
    // Store the mutation in the undo buffer as a storedMutation
    // Gather undos if they happen within 100ms;
    editor.ui._internals.makeMutationUndoable = function makeMutationUndoable(m) {
      var editor_model = editor.ui.model;
      let time = +new Date();
      let storedMutation = { type: m.type, target: {[nodeCacheTag]: m.target}, timestamp: time };
      if(m.type === "childList") {
        storedMutation.removedNodes = [...m.removedNodes].map(r => ({[nodeCacheTag]: r}));
        storedMutation.addedNodes   = [...m.addedNodes  ].map(a => ({[nodeCacheTag]: a}));
        storedMutation.previousSibling =  {[nodeCacheTag]: m.previousSibling};
        storedMutation.nextSibling = {[nodeCacheTag]: m.nextSibling};
      } else {
        storedMutation.oldValue = m.oldValue;
      }
      if(m.type == "characterData") {
        storedMutation.newValue = m.target.textContent;
      } else if(m.type == "attributes") {
        storedMutation.newValue = m.target.getAttribute(m.attributeName);
        storedMutation.attributeName = m.attributeName;
      } else if(m.type == "linkHrefCSS") {
        storedMutation.newValue = m.newValue;
      }
      //check if the last element on currently on the stack is operating on the same "information", i.e. oldValue or nodelists
      //and should be combined together when undoing/redoing
      
      let lastUndo = editor_model.undoStack[editor_model.undoStack.length-1];
      //makes single actions that are recorded as multiple mutations a single action
      //true here ==> mutation is separate action
      if(!lastUndo || (lastUndo[0].timestamp < (time - 100))) {
        editor_model.undoStack.push([storedMutation]);
        editor_model.redoStack = [];
        editor_model.actionsAfterSaveState.push({type: "do", mutations: [storedMutation]});
      } else { // We just append the mutation to the last undo because it's too short.
        lastUndo.push(storedMutation);
        // We group this action with the previous one also with actionsAfterSaveState
        // For every batch action in actionsAfterSaveState there is the same in undoStack or redoStack or vice-versa
        editor_model.actionsAfterSaveState[editor_model.actionsAfterSaveState.length - 1].mutations.push(storedMutation);
      }
      return storedMutation; // For post-processing.
    }; //editor.ui._internals.makeMutationUndoable

    // Removes all nodes (added elements, removed elements, target, previous/nextSibling)
    // Undo/Redo must still work after this function is run, to ensure we can redo changes if session is lost.
    editor.removeExplicitNodes = function removeExplicitNodes() {
      function removeTreasureCache(treasureCache) {
        if(!treasureCache) return;
        delete treasureCache[nodeCacheTag];
      }
      function removeTreasureCaches(undoOrRedoStack) {
        undoOrRedoStack.forEach(storedMutations => {
          storedMutations.forEach(storedMutation => {
            switch(storedMutation.type) {
              case "childList":
                removeTreasureCache(storedMutation.previousSibling);
                removeTreasureCache(storedMutation.nextSibling);
                storedMutation.addedNodes.forEach(removeTreasureCache);
                storedMutation.removedNodes.forEach(removeTreasureCache);
              case "attributes":
              case "characterData":
              case "linkHrefCSS":
              default:
               removeTreasureCache(storedMutation.target);
            }
          });
        });
      }
      removeTreasureCaches(editor.ui.model.undoStack);
      removeTreasureCaches(editor.ui.model.redoStack);
    }
    
    // POST-PROCESSING of a batch of mutations.
    // 1. Create a forest representing the trees as it is now after the mutations.
    // 2. Undo all the mutations on the forest
    // 3. Use the information on the initial and final forest to store indicators on how to recover changes in case nodes are not there anymore.
    editor.ui._internals.postProcessMutations = function postProcessMutations(storedMutations) {
      let noChild = true;
      for(let i = storedMutations.length - 1; i >= 0; i--) {
        let storedMutation = storedMutations[i];
        let mutType = storedMutation.type;
        if(mutType == "childList") noChild = false;
      }
      function foreachTreasureCache(callback) {
        for(let i = storedMutations.length - 1; i >= 0; i--) {
          let storedMutation = storedMutations[i];
          let mutType = storedMutation.type;
          callback(storedMutation.target, storedMutations, i, "target")
          
          if(mutType == "childList") {
            if(storedMutation.previousSibling[nodeCacheTag]) callback(storedMutation.previousSibling, storedMutations, i, "previousSibling"); 
            if(storedMutation.nextSibling[nodeCacheTag]) callback(storedMutation.nextSibling, storedMutations, i, "nextSibling");
            
            for(var k = 0; k < storedMutation.addedNodes.length; k++) {
              let treasureCache = storedMutation.addedNodes[k];
              callback(treasureCache, storedMutations, i, "addedNodes", k);
            }
            for(var k = 0; k < storedMutation.removedNodes.length; k++) {
              let treasureCache = storedMutation.removedNodes[k];
              callback(treasureCache, storedMutations, i, "removedNodes", k); 
            }
          }
        }
      }
      
      // Build the forest
      foreachTreasureCache(treasureCache => {
        let node = treasureCache[nodeCacheTag];
        node[nodeCacheTag] = [];
      });
      let addToForest = forest => function(treasureCache) {
        let node = treasureCache[nodeCacheTag];
        if(node[nodeCacheTag].length) return;
        let tmp = node;
        let topMostMentionnedParent = node;
        while(tmp) {
          if(tmp[nodeCacheTag]) topMostMentionnedParent = tmp;
          tmp = tmp.parentElement;
        }
        if(topMostMentionnedParent != node) {
          // This ancestor will be added later. The index of ancestor will be added later.
          let indexInsertion;
          if(!topMostMentionnedParent[nodeCacheTag].length) {
            indexInsertion = forest.length;
            forest.push({root: topMostMentionnedParent, pathToDocument: topMostMentionnedParent.isConnected ? editor.toTreasureMap(topMostMentionnedParent) : undefined});
            topMostMentionnedParent[nodeCacheTag][storedMutations.length] = {source: indexInsertion, treasureMap: editor.toTreasureMap(topMostMentionnedParent, topMostMentionnedParent)};
          } else {
            indexInsertion = topMostMentionnedParent[nodeCacheTag][storedMutations.length].source;
          }
          node[nodeCacheTag][storedMutations.length] = { source: indexInsertion, treasureMap: editor.toTreasureMap(node, topMostMentionnedParent) };
          return;
        }
        // No ancestor in the forest.
        let indexInsertion = forest.length;
        node[nodeCacheTag][storedMutations.length] = {source: indexInsertion, treasureMap: editor.toTreasureMap(node, node)};
        forest.push({root: node, pathToDocument: node.isConnected ? editor.toTreasureMap(node) : undefined});
      }
      let forests = [];
      let lastForest = [];
      forests[storedMutations.length] = lastForest;
      // 1. We build the partial forest of nodes
      foreachTreasureCache(addToForest(lastForest));
      
      // 2. Second, we replace those nodes with arrays representing them, so that we can move these nodes around.
      for(let i = 0; i < lastForest.length; i++) {
        lastForest[i].root = domNodeToNativeValueWithCache(lastForest[i].root);
      }
      
      let undoChanges = childNodes => function undoChangesRec(arrayNode, index, bareSelectorArray, storedMutation) {
        let result = arrayNode;
        if(index >= bareSelectorArray.length - 1) {
          let oldValue = storedMutation.oldValue;
          let newValue = storedMutation.newValue;
          if(storedMutation.type == "attributes") {
            if(typeof newValue == "string") {
              for(var k = 0; k < arrayNode[1].length; k++) {
                if(arrayNode[1][k] == storedMutation.attributeName) {
                  if(typeof oldValue == "string") { // Update the attribute
                    result = [arrayNode[0], arrayNode[1].slice(0, k).concat([[arrayNode[1][k], oldValue]]).concat(arrayNode[1].slice(k+1)), arrayNode[2]];
                    break;
                  } else { // Remove the attribute
                    result = [arrayNode[0], arrayNode[1].slice(0, k).concat(arrayNode[1].slice(k+1)), arrayNode[2]];
                    break;
                  }
                }
              }
            }
            if(typeof oldValue == "string" && typeof newValue == "undefined") {
              result = [arrayNode[0], arrayNode[1].concat([[storedMutation.attributeName, oldValue]]), arrayNode[2]];
            }
          } else if (storedMutation.type == "characterData") {
            result = [arrayNode[0], oldValue];
          } else if (storedMutation.type == "childList") {
            let previousSibling = storedMutation.previousSibling ? storedMutation.previousSibling[nodeCacheTag] : undefined;
            let indexOfPreviousSibling = 0;
            while(previousSibling && indexOfPreviousSibling < arrayNode[2].length && arrayNode[2][indexOfPreviousSibling][nodeCacheTag] != previousSibling) {
              indexOfPreviousSibling++;
            }
            if(previousSibling && indexOfPreviousSibling == arrayNode[2].length) {
              console.trace("Error: could not find the previous sibling in arrayNode", {arrayNode, previousSibling});
            }
            let numberOfPreviousSiblings = previousSibling ? indexOfPreviousSibling + 1 : 0; 
            let addedCount = storedMutation.addedNodes.length;
            // We return the added nodes as a field of childNodes
            childNodes.addedNodes = arrayNode[2].slice(numberOfPreviousSiblings, numberOfPreviousSiblings + addedCount);
            // TODO: Put all added nodes as new nodes inside the forest
            result = [arrayNode[0], arrayNode[1], arrayNode[2].slice(0, numberOfPreviousSiblings).concat(childNodes.removedNodes).concat(arrayNode[2].slice(numberOfPreviousSiblings + addedCount))];
          }
        } else if (bareSelectorArray[index + 1] == ">") {
          let indexChild = 0;
          while(index + 2 + indexChild + 1 <= bareSelectorArray.length - 1 &&
                bareSelectorArray[index + 2 + indexChild + 1] != ">") {
            indexChild++;
          }
          let newChildSerialized =
            undoChangesRec(arrayNode[2][indexChild], index + 2 + indexChild, bareSelectorArray, storedMutation);
          result = [arrayNode[0], arrayNode[1], arrayNode[2].slice(0, indexChild).concat([newChildSerialized]).concat(arrayNode[2].slice(indexChild + 1))];
        } else {
          console.log("path not found within mutation to change attributes",
            {index: index, bareSelectorArray: bareSelectorArray, storedMutation: storedMutation, arrayNode: arrayNode});
          return arrayNode;
        }
        result[nodeCacheTag] = arrayNode[nodeCacheTag];
        return result;
      }
      let nodeType = {TEXT: document.TEXT_NODE, COMMENT: document.COMMENT_NODE, "!DOCTYPE": document.DOCUMENT_TYPE_NODE, "#document": document.DOCUMENT_NODE};
      // 3. We back-propagate every mutation to build the original forest as it was before the mutations.
      for(i = storedMutations.length - 1; i >= 0; i--) {
        let storedMutation = storedMutations[i];
        let mutType = storedMutation.type;
        let target = storedMutation.target[nodeCacheTag];
        let p = target[nodeCacheTag][i+1];
        let deltaTargetIndexInForest = 0;
        let removedNodesSerialized = mutType == "childList" ? storedMutation.removedNodes.map(treasureCache => {
          let node = treasureCache[nodeCacheTag];
          let pRemoved = node[nodeCacheTag][i + 1];
          if(pRemoved < p.source) deltaTargetIndexInForest++;
          return forests[i+1][pRemoved.source].root;
        }) : undefined;
        let forestCutRemoved =  mutType == "childList" ? forests[i+1].filter((el, j) => {
          for(let n = 0; n < storedMutation.removedNodes.length; n++) {
            if(storedMutation.removedNodes[n][nodeCacheTag][nodeCacheTag][i+1].source == j) {
              return false;
            }
          }
          return true;
        }) : forests[i+1];
        let childNodes = {removedNodes: removedNodesSerialized, addedNodes: []};
        let newTargetSerialized = undoChanges(childNodes)(forests[i + 1][p.source].root, 0, p.treasureMap.bareSelectorArray, storedMutation);
        forests[i] = forestCutRemoved.slice(0, p.source - deltaTargetIndexInForest).concat([{root: newTargetSerialized, pathToDocument: forests[i+1][p.source].pathToDocument}]).concat(forestCutRemoved.slice(p.source - deltaTargetIndexInForest + 1)).concat(childNodes.addedNodes.map(n => ({root: n, pathToDocument: undefined})));
        let newForest = forests[i];
        let getBareSelectorOfArrayNode = function (arrayNode) {
          return arrayNode.length != 3 ? "/*" + nodeType[arrayNode[0]]  + "*/" : arrayNode[0];
        }
        let mark = function (arrayNode, k, bareSelectorArray) {
          if(arrayNode[nodeCacheTag][nodeCacheTag]) { // The node is one that we trace.
            arrayNode[nodeCacheTag][nodeCacheTag][i] = { source: k, treasureMap: {bareSelectorArray: bareSelectorArray} }
          }
          if(Array.isArray(arrayNode[2]) && arrayNode[2].length) {
            let currentChildSelector = bareSelectorArray.concat([">"]);
            for(var m = 0; m < arrayNode[2].length; m++) {
              currentChildSelector = currentChildSelector.concat([getBareSelectorOfArrayNode(arrayNode[2][m])]);
              mark(arrayNode[2][m], k, currentChildSelector)
            }
          }
        }
        for(let k = 0; k < newForest.length; k++) {
          let root = newForest[k].root;
          mark(root, k, [getBareSelectorOfArrayNode(root)]);
        }
      }
      let firstForest = forests[0];
      // Clean up. We don't need the arrayNodes of nodes which can be found in the document at the time of recovery;
      // That way, we save a lot of space.
      for(var k = 0; k < firstForest.length; k++) {
        if(firstForest[k].pathToDocument) firstForest[k].root = undefined;
      }
      for(var k = 0; k < lastForest.length; k++) {
        if(lastForest[k].pathToDocument) lastForest[k].root = undefined;
      }
      // Ok, now the forests contains all the successive representation of the elements.
      // For each treasureCache now, we can find absolute treasureMap and the treasureMapRedo or serialized versions if necessary.
      // To ensure we create elements only once, we can just store the first forest in the first storedMutation and the last forest in the last storedMutation
      storedMutations[0].firstForest = firstForest;
      storedMutations[storedMutations.length-1].lastForest = lastForest;
      foreachTreasureCache((treasureCache, storedMutations, i, name, subIndex) => {
        let node = treasureCache[nodeCacheTag];
        treasureCache.forRedo = node[nodeCacheTag][0]; // A source in the forest as well as a treasureMap to find the node from the source.
        treasureCache.forUndo = node[nodeCacheTag][storedMutations.length];
      });
      
      
      // When we are done with back-propagation, we just remove all the nodes.
      foreachTreasureCache(treasureCache => {
        delete treasureCache[nodeCacheTag][nodeCacheTag];
      });
      
      editor.ui._internals.saveUndoRedo();
    }
    
    
    editor.ui._internals.saveUndoRedo = function saveUndoRedo() {
      let numberOfUndosSinceLastSave = editor.ui.model.actionsAfterSaveState.filter(x => x.type == "undo").length;
      let numberOfDosSinceLastSave = editor.ui.model.actionsAfterSaveState.length - numberOfUndosSinceLastSave;
      let undoStackToSave = editor.ui.model.undoStack.slice(0, editor.ui.model.undoStack.length - numberOfDosSinceLastSave);
      let actionsAfterSaveStateToSave = editor.ui.model.actionsAfterSaveState;
      let redoStackToSave = editor.ui.model.redoStack;
      while(true) {
        let actionsAfterSaveStateString = JSON.stringify(actionsAfterSaveStateToSave);
        let undoString = JSON.stringify(undoStackToSave);
        let redoString = JSON.stringify(redoStackToSave);
        try {
          localStorage.removeItem(editor.config.path + ' editor.ui.model.actionsAfterSaveState');
          localStorage.removeItem(editor.config.path + ' editor.ui.model.undoStack');
          localStorage.removeItem(editor.config.path + ' editor.ui.model.redoStack');
          // The number of undo/redos we can save is dependent on the memory 
          localStorage.setItem(editor.config.path + ' editor.ui.model.actionsAfterSaveState', actionsAfterSaveStateString);
          localStorage.setItem(editor.config.path + ' editor.ui.model.undoStack', undoString);
          localStorage.setItem(editor.config.path + ' editor.ui.model.redoStack', redoString);
        } catch(e) { // Not enough place.
          for(let key of Object.keys(localStorage)) {
            if(key.indexOf("editor.ui.model") >= 0) {
              localStorage.removeItem(key);
            }
          }
          console.log("[error] Saved undo/redo. Size: " + (undoString.length + redoString.length + actionsAfterSaveStateString.length));
          // Strategy to prune the number of actions to save
          if(undoStackToSave.length) { // First thing to forget: the oldest undo stack.
            console.log("[Warning] Out of memory to save oldest actions. Removing one oldest action.")
            undoStackToSave = undoStackToSave.slice(1);
          } else if(redoStackToSave.length) { // Second thing to forget: things that were undone when the page was closed unexpectedly
            console.log("[Warning] Out of memory to save most recent undone actions. Removing one undo actions.");
            redoStackToSave = redoStackToSave.slice(0, redoStackToSave.length - 1);
          } else if(actionsAfterSaveStateToSave.length) { // Actions made since last save
            console.log("[Warning] Out of memory to save some actions done since last save. Removing the most recent one.");
            editor.ui.sendNotification("Reaching memory limit. Please save changes and refresh.");
            actionsAfterSaveStateToSave = actionsAfterSaveStateToSave.slice(0, actionsAfterSaveStateToSave.length - 1);
          } else { // Nothing to save.
            break;
          }
          continue;
        }
        break;
      }
    }
    
    // Given a list of actions newUndo, redo, and undo, first rewind the actions to replay them.
    editor.ui._internals.replayActions = function replayActions(actionsAfterSaveState, undoStack, redoStack) {
      // At this point, the undo and redo stack were restored like they were when the page was last closed or refreshed.
      let undosAfterSaveState = actionsAfterSaveState.filter(x => x.type == "undo").map(y => y.mutations);
      undosAfterSaveState.reverse();
      let dosAfterSaveState = actionsAfterSaveState.slice(undosAfterSaveState.length).map(y => y.mutations);
      editor.ui.model.actionsAfterSaveState = [];
      editor.ui.model.undoStack = undoStack;
      undoStack.push(...undosAfterSaveState); // We put back the undos performed after the saved state
      editor.ui.model.redoStack = [];         // We set redoStack to empty.
      for(var i = 0; i < undosAfterSaveState.length; i++) { // We undo the actions undone after the saved state
        editor.ui.undo();
      }
      editor.ui.model.redoStack = dosAfterSaveState.concat(redoStack);
      for(var i = 0; i < dosAfterSaveState.length; i++) {
        editor.ui.redo();
      }
    }
    
    editor.ui._internals.restoreUndoRedo = function restoreUndoRedo() {
      editor.ui.model.undoStack = JSON.parse(localStorage.getItem(editor.config.path + " editor.ui.model.undoStack")) || editor.ui.model.undoStack;
      editor.ui.model.redoStack = JSON.parse(localStorage.getItem(editor.config.path + " editor.ui.model.redoStack")) || editor.ui.model.redoStack;
      editor.ui.model.actionsAfterSaveState = JSON.parse(localStorage.getItem(editor.config.path + " editor.ui.model.actionsAfterSaveState")) || editor.ui.model.actionsAfterSaveState;
      
      // If we are restoring it, it means that we are out of sync with the current document.
      // We assume that the document is at the state it was when (undoBeforeSave) undos were executed.
      
      // If there are more undos, we should ask to restore the changes or not.
      if(editor.ui.model.actionsAfterSaveState.length > 0) {
        let msg = //editor.ui.model.actionsAfterSaveState.length ?
                     //"We recovered the changes you did while the page was saved." :
                     "We recovered some unsaved changes since when you last saved the page."
        if(confirm(msg + "\nDo you want to restore them?")) {
          let actionsAfterSaveState = editor.ui.model.actionsAfterSaveState;
          editor.ui._internals.replayActions(actionsAfterSaveState, editor.ui.model.undoStack, editor.ui.model.redoStack);
          editor.ui.refresh();
        } else {
          editor.ui.model.undoStack = [];
          editor.ui.model.redoStack = [];
          editor.ui.model.actionsAfterSaveState = [];
        }
        editor.ui._internals.saveUndoRedo();
      }
    }

    //debugging function for printing both teh undo and redo stacks.
    editor.ui._internals.printstacks = function printstacks() {
      console.log("-----------------------------");
      let i, j;
      console.log("UNDO STACK:");
      for(i = 0; i < editor.ui.model.undoStack.length; i++) {
        console.log(i + ".");
        for(j = 0; j < editor.ui.model.undoStack[i].length; j++) {
          console.log(editor.ui.model.undoStack[i][j]);
        }
      }
      console.log("REDO STACK:");
      for(i = 0; i < editor.ui.model.redoStack.length; i++) {
        console.log(i + "."); 
        for(j = 0; j < editor.ui.model.redoStack[i].length; j++) {
          console.log(editor.ui.model.redoStack[i][j]);
        }
      }
      console.log("-----------------------------");
    }; // editor.ui._internals.printstacks
    
    // Returns true if Editor's undo feature should be enabled.
    editor.ui.canUndo = function canUndo() {
      return editor.ui.model.undoStack.length > 0;
    }; // editor.ui.canUndo

    //undo function: handles undo feature
    editor.ui.undo = function undo() {
      editor.userModifies();
      let storedMutations = editor.ui.model.undoStack.pop();
      //need to check if undoStack is empty s.t. we can set the "savability" of the document accurately
      if(storedMutations == undefined) {
        return 0;
      }
      (async () => {
      //TODO prevent pressing the undo button while save underway while letting Editor use the undo function. (just not the user);
      //need to disconnect the MutationObserver such that our undo does not get recorded as a mutation
      editor_stopWatching();
      const quicker = node => !node || node.isConnected ? node : editor.fromTreasureMap(editor.toTreasureMap(node));
      let k;
      // Undo's batches can be merged, hence we need to make sure we always take the most recent last forest.
      let indexOfLastForest = storedMutations.length - 1;
      function recover(treasureCache) {
        if(!treasureCache) return null;
        if(nodeCacheTag in treasureCache) { // We are good there.
          return treasureCache[nodeCacheTag];
        }
        if(treasureCache.forUndo === undefined) return;
        // Node is missing. Let's recover it.
        let p = treasureCache.forUndo;
        let forest = storedMutations[indexOfLastForest].lastForest;
        let topNode;
        if(forest[p.source].pathToDocument) { // This element exists, we need to recover it.
          topNode = editor.fromTreasureMap(forest[p.source].pathToDocument);
        } else {
          topNode = arrayNodeToDomNode(forest[p.source].root);
        }
        let finalNode = queryBareSelector(p.treasureMap.bareSelectorArray, topNode);
        treasureCache[nodeCacheTag] = finalNode;
        return finalNode;
      }
      for(k = storedMutations.length - 1; k >= 0; k--) {
        let storedMutation = storedMutations[k];
        if("lastForest" in storedMutation) indexOfLastForest = k;
        let mutType = storedMutation.type;
        let target = recover(storedMutation.target);
        //in each case, we reverse the change, setting the URValue/oldValue as the current value
        //at the target, and replacing the URValue/oldValue with the current value present in target
        if(mutType === "attributes") {
          if(storedMutation.oldValue === null) {
            target.removeAttribute(storedMutation.attributeName); 
          } else { 
            target.setAttribute(storedMutation.attributeName, storedMutation.oldValue);
          }
        } else if(mutType === "characterData") {
          target.textContent = storedMutation.oldValue;
        } else if(mutType === "linkHrefCSS") { // There should be only one such even
          var keepUndo = storedMutation;
          await assignTmpCss(target, keepUndo.oldValue, true);
        } else { // childList
          let removedNodesToAdd = storedMutation.removedNodes;
          let addedNodesToRemove = storedMutation.addedNodes;
          let nextSibling = recover(storedMutation.nextSibling);
          if(!nextSibling) {
            let previousSibling = recover(storedMutation.previousSibling);
            let count = addedNodesToRemove.length;
            nextSibling = previousSibling ? previousSibling.nextSibling : target.firstChild;
            while(count > 0 && nextSibling) {
              count--;
              nextSibling = nextSibling.nextSibling;
            }
          }
          for(i = 0; i < removedNodesToAdd.length; i++) { 
            let toReinsert = recover(removedNodesToAdd[i]);
            if(toReinsert) target.insertBefore(toReinsert, nextSibling); 
          }
          for(i = 0; i < addedNodesToRemove.length; i++) {
            let r = recover(addedNodesToRemove[i]);
            if(r) r.remove();
          }
        }
      } //storedMutation looper
      editor.ui.model.redoStack.push(storedMutations);
      if(editor.ui.model.actionsAfterSaveState.length && editor.ui.model.actionsAfterSaveState[editor.ui.model.actionsAfterSaveState.length - 1].type == "do") {
        editor.ui.model.actionsAfterSaveState = editor.ui.model.actionsAfterSaveState.slice(0, editor.ui.model.actionsAfterSaveState.length - 1);
      } else {
        editor.ui.model.actionsAfterSaveState.push({type: "undo", mutations: storedMutations});
      }
      editor.ui._internals.saveUndoRedo();
      //TODO make sure save button access is accurate (i.e. we should ony be able to save if there are thigns to undo)
      //turn MutationObserver back on
      editor_resumeWatching();
      editor.ui.refresh();
      //editor.ui._internals.printstacks();
      })();
      return "Undo launched";
    }; //editor.ui.undo

    // Returns true if Editor's redo feature should be enabled.
    editor.ui.canRedo = function canRedo() {
      return editor.ui.model.redoStack.length > 0;
    }; // editor.ui.canRedo
    
    // Redo an undone mutation array.
    editor.ui.redo = function redo() {
      editor.userModifies();
      let storedMutations = editor.ui.model.redoStack.pop();
      if(storedMutations === undefined) {
        return 0;
      }
      (async () => {
      editor_stopWatching();
      let k;
      let indexOfFirstForest = 0;
      // mutations can happen in batches, each batch starts with a .firstForest attribute.
      function recoverNode(treasureCache) {
        if(!treasureCache) return null;
        if(nodeCacheTag in treasureCache) { // We are good there.
          return treasureCache[nodeCacheTag];
        }
        if(treasureCache.forRedo === undefined) return undefined;
        // Node is missing. Let's recover it.
        let p = treasureCache.forRedo;
        let forest = storedMutations[indexOfFirstForest].firstForest;
        let topNode;
        if(forest[p.source].pathToDocument) { // This element exists, we need to recover it.
          topNode = editor.fromTreasureMap(forest[p.source].pathToDocument);
        } else {
          topNode = arrayNodeToDomNode(forest[p.source].root);
        }
        let finalNode = queryBareSelector(p.treasureMap.bareSelectorArray, topNode);
        treasureCache[nodeCacheTag] = finalNode;
        return finalNode;
      }
      for(k = 0; k < storedMutations.length; k++) {
        let storedMutation = storedMutations[k];
        if("firstForest" in storedMutation) {
          indexOfFirstForest = k;
        }
        let mutType = storedMutation.type;
        let target = recoverNode(storedMutation.target);
        //in each case, we reverse the change, setting the URValue/oldValue as the current value
        //at the target, and replacing the URValue/oldValue with the current value present in target
        if(mutType === "attributes") {
          if(storedMutation.newValue === null) {
            target.removeAttribute(storedMutation.attributeName); 
          } else { 
            target.setAttribute(storedMutation.attributeName, storedMutation.newValue);
          }
        } else if(mutType === "characterData") {
          target.textContent = storedMutation.newValue;
        } else if(mutType === "linkHrefCSS") { // There should be only one such even
          var keepUndo = storedMutation;
          await assignTmpCss(target, keepUndo.newValue, true);
        } else { // childList
          let nodesToRemove = storedMutation.removedNodes;
          let nodesToReadd = storedMutation.addedNodes;
          let nextSibling = recoverNode(storedMutation.nextSibling);
          if(!nextSibling) {
            let previousSibling = recoverNode(storedMutation.previousSibling);
            let count = nodesToRemove.length;
            nextSibling = previousSibling ? previousSibling.nextSibling : target.firstChild;
            while(count > 0 && nextSibling) {
              count--;
              nextSibling = nextSibling.nextSibling;
            }
          }
          for(i = 0; i < nodesToReadd.length; i++) {
            let toReAdd = recoverNode(nodesToReadd[i]);
            if(toReAdd) target.insertBefore(toReAdd, nextSibling); 
          }
          for(i = 0; i < nodesToRemove.length; i++) {
            let r = recoverNode(nodesToRemove[i]);
            if(r) r.remove();
          }
        }
      } //mut looper
      editor.ui.model.undoStack.push(storedMutations);
      if(editor.ui.model.actionsAfterSaveState.length && editor.ui.model.actionsAfterSaveState[editor.ui.model.actionsAfterSaveState.length - 1].type == "undo") {
        editor.ui.model.actionsAfterSaveState = editor.ui.model.actionsAfterSaveState.slice(0, editor.ui.model.actionsAfterSaveState.length - 1);
      } else {
        editor.ui.model.actionsAfterSaveState.push({type: "do", mutations: storedMutations});
      }
      editor.ui._internals.saveUndoRedo();
      editor_resumeWatching();
      editor.ui.refresh();
      //editor.ui._internals.printstacks();
      })();
      return 1;
    }; //editor.ui.redo

    // Synchronizes the state of undo/redo buttons with the stacks.
    editor.ui.syncUndoRedoButtons = function syncUndoRedoButtons() {
      let undoButton = document.querySelector("#undobutton");
      let redoButton = document.querySelector("#redoButton");
      if(undoButton) undoButton.classList.toggle("disabled", !editor.ui.canUndo());
      if(redoButton) redoButton.classList.toggle("disabled", !editor.ui.canRedo());
      let saveButton = document.querySelector(".saveButton");
      if(saveButton) saveButton.classList.toggle("disabled", !editor.ui.canSave() && !editor.ui.model.disambiguationMenu);
    }; 
    
    // Returns true if the save button should be enabled.
    editor.ui.canSave = function editor_canSave() {
      return editor.ui.model.actionsAfterSaveState.length > 0;
    };

    function isDescendantOf(a, b) {
      while(a && a != b) {
        a = a.parentElement;
      }
      return a == b;
    }
    
    editor.ui._internals.handleMutations = function handleMutations(mutations, observer) {
      var processedMutations = [];
      if(editor.config.fast) {
        if(editor.ui.model.userIsModifying) {
          for(var i = 0; i < mutations.length; i++) {
            let mutation = mutations[i];
            if(editor.hasGhostAncestor(mutation.target)) {
              continue;
            }
            if(mutation.type == "attributes") {
              var isSpecificGhostAttributeKey = editor.isSpecificGhostAttributeKeyFromNode(mutation.target);
              var isIgnoredAttributeKey = editor.isIgnoredAttributeKeyFromNode(mutation.target);
              if(editor.isGhostAttributeKey(mutation.attributeName) || isSpecificGhostAttributeKey(mutation.attributeName) ||
               mutation.target.getAttribute(mutation.attributeName) === mutation.oldValue ||
               isIgnoredAttributeKey(mutation.attributeName)
              ) {
                continue;
              }
            } else if(mutation.type == "childList") {
              if(editor.areChildrenGhosts(mutation.target)) {
                continue;
              }
            }
            processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
          }
        }
      } else {
        for(var i = 0; i < mutations.length; i++) {
          // A mutation is a ghost if either
          // -- The attribute starts with 'ghost-'
          // -- It is the insertion of a node whose tag is "ghost" or that contains an attribute "isghost=true"
          // -- It is the modification of a node or an attribute inside a ghost node.
          /*  
           * Add mutations to undo list if they are not ghosts and if they are really doing something.
           */
          let mutation = mutations[i];
          if(editor.hasGhostAncestor(mutation.target)) {
            continue;
          }
          if(mutation.type == "attributes") {
            var isSpecificGhostAttributeKey = editor.isSpecificGhostAttributeKeyFromNode(mutation.target);
            var isIgnoredAttributeKey = editor.isIgnoredAttributeKeyFromNode(mutation.target);
            if(editor.isGhostAttributeKey(mutation.attributeName) || isSpecificGhostAttributeKey(mutation.attributeName) ||
               mutation.target.getAttribute(mutation.attributeName) === mutation.oldValue ||
               isIgnoredAttributeKey(mutation.attributeName)) {
            } else {
              processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
              // Please do not comment out this line until we get proper clever save.
              console.log("Attribute is not ghost so the change will be saved", mutation);
              console.log("TIP: Use this script if you want to mark it as ghost:");
              let sel = getShortestUniqueSelector(mutation.target);
              if(typeof mutation.oldValue === "undefined") {
                console.log("editor.ghostAttrs.push(n => editor.matches(n, '"+sel+"') ? ['"+mutation.attributeName+"'] : []);")
              } else {
                console.log("editor.ignoredAttrs.push(n => editor.matches(n, '"+sel+"') ? ['"+mutation.attributeName+"'] : []);")
              }
            }
          } else if(mutation.type == "childList") {
            if(!editor.areChildrenGhosts(mutation.target)) {
              for(var j = 0; j < mutation.addedNodes.length; j++) {
                if(!editor.hasGhostAncestor(mutation.addedNodes[j]) && !editor.hasIgnoringAncestor(mutation.addedNodes[j])) {
                  processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
                  // Please do not comment out this line until we get proper clever save.
                  console.log(`Added node ${j} does not have a ghost ancestor`, mutation);
                  console.log("TIP: Ignore node and siblings with this script:");
                  let sel = getShortestUniqueSelector(mutation.target);
                  console.log("editor.ignoredChildNodes.push(editor.matches('"+sel+"'));");
                  if(mutation.addedNodes[j].nodeType === 1) {
                    console.log("TIP: Mark this element as ghost:");
                    sel = getShortestUniqueSelector(mutation.addedNodes[j]);
                    console.log("editor.ghostNodes.push(editor.matches('"+sel+"'));");
                  }
                }
              }
              for(var j = 0; j < mutation.removedNodes.length; j++) {
                if(!editor.isGhostNode(mutation.removedNodes[j]) && !editor.isIgnoringChildNodes(mutation.target) && !editor.hasIgnoringAncestor(mutation.target)) {
                  processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
                  // Please do not comment out this line until we get proper clever save.
                  console.log(`Removed node ${j} was not a ghost`, mutation);
                  console.log("TIP: Mark this element as ghost:");
                  let sel = getShortestUniqueSelector(mutation.target);
                  console.log("editor.ignoredChildNodes.push(editor.matches('"+sel+"'));");
                }
              }
            }
          } else if(mutation.type === "characterData") {
            processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
            // Please do not comment out this line until we get proper clever save.
            console.log("Text modified not by user", mutation);
          } else {
            processedMutations.push(editor.ui._internals.makeMutationUndoable(mutation));
            if(!editor.ui.model.userIsModifying) {
              // Please do not comment out this line until we get proper clever save.
              console.log("mutations other than attributes, childList and characterData are not ghosts", mutation);
            }
          }
        }
      }
      if(processedMutations.length == 0) {
        return;
      } // Send in post the new HTML along with the URL
      editor.ui._internals.postProcessMutations(processedMutations);
      // Set undo/redo state
      editor.ui.syncUndoRedoButtons();
      
      if(editor.ui.model.autosave && !editor.ui.model.disambiguationMenu) {
        editor._internals.ifIdleAfter(typeof editor.config.editdelay == "number" ? editor.config.editdelay : 1000, "autosave",
          function() {
            editor._internals.autosavetimer = undefined;
            editor.ui.save();
          });
      }
    } //editor.ui._internals.handleMutations
    
    // When selecting some text, mouse up on document, the focus node can be outside of the anchor node. We want to prevent this from happening
    // This is because triple click in Chrome selects the whitespace after the last word as well.
    editor.ui.fixSelection = function fixSelection() {
      if(+new Date() < fixSelection.lastChanged + 1000) return;
      var sel = window.getSelection();
      if(!sel || !sel.rangeCount) return;
      sel = sel.getRangeAt(0);
      if(sel.startContainer.nodeType !== 3) return;
      if(sel.endContainer.nodeType !== 1) return;
      // We'll ensure that the end of selection is inside a text node and that it does not goes ouside a boundary.
      let tmp = sel.startContainer;
      let finalTextNode = tmp.parentNode.childNodes[tmp.parentNode.childNodes.length - 1];
      while(finalTextNode.nodeType != 3) {
        finalTextNode = finalTextNode.previousSibling;
      }
      if(finalTextNode) { // finalTextNode.nodeType === 3
        var range = document.createRange();
        range.setStart(sel.startContainer, sel.startOffset);
        range.setEnd(finalTextNode, finalTextNode.textContent.length);
        editor.ui.clearTextSelection();
        window.getSelection().addRange(range);
        fixSelection.lastChanged = +new Date();
      }
    }; // editor.ui.fixSelection
    
    
    // Returns a copy of the selection before clearing it.
    editor.ui.clearTextSelection = function clearTextSelection() {
      var sel = window.getSelection();
      if(!sel || !sel.rangeCount) return;
      var selection = sel.getRangeAt(0);
      if (window.getSelection) {
        if (window.getSelection().empty) {  // Chrome
          window.getSelection().empty();
        } else if (window.getSelection().removeAllRanges) {  // Firefox
          window.getSelection().removeAllRanges();
        }
      } else if (document.selection) {  // IE?
        document.selection.empty();
      }
      return selection;
    }; // editor.ui.clearTextSelection
    
    // Handle a file event (e.g. uploaded files)
    editor.ui.handleDroppedFiles =
      function handleDroppedFiles(evt) {
        evt.stopPropagation();
        evt.preventDefault();
        var files = evt.dataTransfer.files; // FileList object
        editor.uploadFilesAtCursor(files);
      }
    
    editor.ui.handleDragOver =
      function handleDragOver(evt) {
        evt.stopPropagation();
        evt.preventDefault();
        evt.dataTransfer.dropEffect = 'copy'; // Explicitly show this is a copy.
      }
    
    // Prevent mouse down on modify-menu that end outside modify-menu to trigger onclick
    editor.ui._internals.onMouseDown = function(event) {
      var tmp = event.target;
      while(tmp) {
        if(tmp.getAttribute && tmp.getAttribute("id") == "modify-menu") {
          editor.ui.model.dismissNextClick = true;
          return;
        }
        tmp = tmp.parentElement;
      }
    }   
    editor.ui._internals.onClick = function(event) {
      if(editor.ui.model.dismissNextClick) {
        editor.ui.model.dismissNextClick = false;
        return;
      }
      var clickedElem = event.target;
      //console.log("click event", event.target);
      var editorSelectOptions = document.querySelectorAll("meta[editor-noselect],meta[editor-doselect]");
      var matchOptions = function(clickedElem) {
        var result = true;
        for(let i = 0; i < editorSelectOptions.length; i++) {
          let negativeSelector = editorSelectOptions[i].getAttribute("editor-noselect"),
              positiveSelector = editorSelectOptions[i].getAttribute("editor-doselect");
          if(result && negativeSelector) {
            result = !editor.matches(clickedElem, negativeSelector);
          }
          if(!result && positiveSelector) {
            result = editor.matches(clickedElem, positiveSelector);
          }
        }
        return result;
      }
      while(clickedElem && editorSelectOptions && !matchOptions(clickedElem)) {
        clickedElem = clickedElem.parentElement;
      }
      var ancestors = [];
      var tmp = clickedElem;
      var aElement;
      var buttonElement;
      var ancestorIsModifyBox = false;
      var ancestorIsContextMenu = false;
      var link = undefined;
      while(tmp) {
        ancestors.push(tmp);
        if(tmp.getAttribute && tmp.getAttribute("id") == "modify-menu") {
          ancestorIsModifyBox = true;
        }
        if(tmp.getAttribute && tmp.getAttribute("id") == "context-menu") {
          ancestorIsContextMenu = true;
        }
        if(!aElement && tmp.tagName === "A") { // First link.
          aElement = tmp;
          link = aElement.getAttribute("href");
        }
        if(!buttonElement && tmp.tagName === "BUTTON") {
          buttonElement = tmp;
        }
        tmp = tmp.parentElement;
      }
      var return_value = undefined;
      document.querySelectorAll("[ghost-hovered=true]").forEach(e => e.removeAttribute("ghost-hovered"));
      if(ancestorIsModifyBox || ancestorIsContextMenu || ancestors[ancestors.length - 1].tagName != "HTML") return;
      if(aElement && link || buttonElement && !buttonElement.matches(".editor-interface") && editor.config.fast) { // Prevents navigating to clicks.
        // We could prevent navigating to buttons as well, but that should be an option. Or a button like for the link
        event.stopPropagation();
        event.preventDefault();
        return_value = false;
      }
      if(buttonElement && buttonElement.matches(".editor-interface")) {
        editor.userModifies();
      }
      //console.log("not modify box", ancestors)
      document.querySelector("#context-menu").classList.remove("visible");
      
      editor.ui.model.clickedElem = clickedElem;
      editor.ui.model.link = link;
      editor.ui.model.link_href_source = aElement; // So that we can modify it
      editor.ui.model.insertElement = false;
      editor.ui.model.notextselection = false;
      editor.ui.refresh();
      return return_value;
      // Check if the event.target matches some selector, and do things...
    } //end of editor.ui._internals.onClick

    // Icons:
    var icons = {};
    editor.ui.icons = icons;
    icons.parentUp = editor.svgFromPath("M 20,5 20,25 M 15,10 20,5 25,10");
    icons.boxArrowDown = editor.svgFromPath("M 10,17 13,14 17,18 17,4 23,4 23,18 27,14 30,17 20,27 Z", true, 30, 20, [0, 0, 40, 30]);
    icons.boxArrowUp = editor.svgFromPath("M 10,14 13,17 17,13 17,27 23,27 23,13 27,17 30,14 20,4 Z", true, 30, 20, [0, 0, 40, 30]);
    icons.boxArrowExpand = editor.svgFromPath("M 9.5,22 9.5,17 20.5,7 30.5,17 30.5,22 20.5,12 Z", true, 30, 20, [0, 0, 40, 30]);
    icons.arrowDown = editor.svgFromPath("M 10,17 13,14 17,18 17,4 23,4 23,18 27,14 30,17 20,27 Z", true);
    icons.arrowRight = editor.svgFromPath("M 21,25 18,22 22,18 8,18 8,12 22,12 18,8 21,5 31,15 Z", true);
    icons.arrowUp = editor.svgFromPath("M 10,14 13,17 17,13 17,27 23,27 23,13 27,17 30,14 20,4 Z", true);
    icons.arrowLeft = editor.svgFromPath("M 19,25 22,22 18,18 32,18 32,12 18,12 22,8 19,5 9,15 Z", true);
    icons.clone = editor.svgFromPath("M 19,8 31,8 31,26 19,26 Z M 11,4 23,4 23,8 19,8 19,22 11,22 Z");
    icons.save = editor.svgFromPath("M 10,5 10,25 30,25 30,9 26,5 13,5 Z M 13,6 25,6 25,12 13,12 Z M 22,7 22,11 24,11 24,7 Z M 13,15 27,15 27,24 13,24 Z M 11,23 12,23 12,24 11,24 Z M 28,23 29,23 29,24 28,24 Z", true);
    icons.openLeft = editor.svgFromPath("M 27.5,4 22.5,4 12.5,15 22.5,25 27.5,25 17.5,15 Z", true);
    icons.closeRight = editor.svgFromPath("M 12.5,4 17.5,4 27.5,15 17.5,25 12.5,25 22.5,15 Z", true);
    icons.openTop = editor.svgFromPath("M 9.5,22 9.5,17 20.5,7 30.5,17 30.5,22 20.5,12 Z", true);
    icons.closeBottom = editor.svgFromPath("M 9.5,7 9.5,12 20.5,22 30.5,12 30.5,7 20.5,17 Z", true);
    icons.wasteBasket = editor.svgFromPath("m 24,11.5 0,11 m -4,-11 0,11 m -4,-11 0,11 M 17,7 c 0,-4.5 6,-4.5 6,0 m -11,0.5 0,14 c 0,3 1,4 3,4 l 10,0 c 2,0 3,-1 3,-3.5 L 28,8 M 9,7.5 l 22,0");
    icons.plus = editor.svgFromPath("M 18,5 22,5 22,13 30,13 30,17 22,17 22,25 18,25 18,17 10,17 10,13 18,13 Z", true);
    icons.liveLink = link => `<a class="livelink" href="javascript:if(editor.confirmLeaving()) { editor.navigateTo('${link}'); }">${editor.svgFromPath("M 23,10 21,12 10,12 10,23 25,23 25,18 27,16 27,24 26,25 9,25 8,24 8,11 9,10 Z M 21,5 33,5 33,17 31,19 31,9 21,19 19,17 29,7 19,7 Z", true)}</a>`;
    icons.gear = editor.svgFromPath("M 17.88,2.979 14.84,3.938 15.28,7.588 13.52,9.063 10,8 8.529,10.83 11.42,13.1 11.22,15.38 7.979,17.12 8.938,20.16 12.59,19.72 14.06,21.48 13,25 15.83,26.47 18.1,23.58 20.38,23.78 22.12,27.02 25.16,26.06 24.72,22.41 26.48,20.94 30,22 31.47,19.17 28.58,16.9 28.78,14.62 32.02,12.88 31.06,9.84 27.41,10.28 25.94,8.52 27,5 24.17,3.529 21.9,6.42 19.62,6.219 17.88,2.979 Z M 20,11 A 4,4 0 0 1 24,15 4,4 0 0 1 20,19 4,4 0 0 1 16,15 4,4 0 0 1 20,11 Z", true);
    icons.folder = editor.svgFromPath("M 8,3 5,6 5,26 10,10 32,10 32,6 18,6 15,3 8,3 Z M 5,26 10,10 37,10 32,26 Z");
    icons.reload = editor.svgFromPath("M 32.5,8.625 30.25,15.25 24.75,11.125 M 6.75,20 9.875,14.5 15.125,19 M 29.5,18 C 28.25,22.125 24.375,25 20,25 14.5,25 10,20.5 10,15 M 10.5,12 C 11.75,7.875 15.625,5 20,5 25.5,5 30,9.5 30,15");
    icons.log = editor.svgFromPath("M 17.24,16 A 1.24,2 0 0 1 16,18 1.24,2 0 0 1 14.76,16 1.24,2 0 0 1 16,14 1.24,2 0 0 1 17.24,16 Z M 20,16 21.24,16 21.24,16 A 1.24,2 0 0 1 20,18 1.24,2 0 0 1 18.76,16 1.24,2 0 0 1 20,14 1.33,2.16 0 0 1 21,15 M 12,14 12,18 14,18 M 10,12 23,12 23,20 10,20 Z M 23,6 23,11 28,11 M 14,6 14,12 10,12 10,20 14,20 14,25 28,25 28,11 23,6 14,6 Z");
    icons.source = editor.svgFromPath("M 22.215125,2 25,3 18.01572,27 15,26 Z M 12,19 12,25 2,14 12,4 12,9 7,14 Z M 28,9 28,4 38,15 28,25 28,20 33,15 Z", true);
    icons.undo = editor.svgFromPath("M 9.5,12.625 11.75,19.25 17.25,15.125 M 31.5,16 C 30.25,11.875 26.375,9 22,9 16.5,9 12,13.5 12,19");
    icons.redo = editor.svgFromPath("M 31.5,12.625 29.25,19.25 23.75,15.125 M 9.5,16 C 10.75,11.875 14.625,9 19,9 24.5,9 29,13.5 29,19");
    icons.isDraft = editor.svgFromPath("M 2,7 2,25 38,25 38,7 M 36,6 C 32,6 29.1,3.9 26.1,3.9 23.1,3.9 22,5 20,6 L 20,23 C 22,22 23.1,20.9 26.1,20.9 29.1,20.9 32,22.9 36,22.9 Z M 4,6 C 8,6 10.9,3.9 13.9,3.9 16.9,3.9 18,5 20,6 L 20,23 C 18,22 16.9,20.9 13.9,20.9 10.9,20.9 8,22.9 4,22.9 Z");
    icons.escape = editor.svgFromPath("M 7.5 4 L 17.5 15 L 7.5 25 L 12.5 25 L 20 17.5 L 27.5 25 L 32.5 25 L 22.5 15 L 32.5 4 L 27.5 4 L 20 12.25 L 12.5 4 L 7.5 4 z", true);
    icons.linkMode = editor.svgFromPath("M 14,3 14,23 19,19 22,27 25,26 22,18 28,18 Z");
    icons.check = editor.svgFromPath("M 10,13 13,13 18,21 30,3 33,3 18,26 Z", true);
    
    /***********************
      Editor's main toolbar
    ************************/

    var thaditor_files = [
      "Thaditor", "Makefile", "ThaditorPackager.py", "ThaditorInstaller.py", "ThaditorInstaller.php",
      "ThaditorInstaller.htaccess", "composer.json", "composer.lock", "credentials.json", "cacert.pem", "versions",
      "vendor", "ssg", "cache"
    ];
    
    var verz = computeDraftVersion();
    //hover mode functions for linkSelectMode
    function escapeLinkMode() {
      document.body.removeEventListener('mouseover', linkModeHover1, false);
      document.body.removeEventListener('mouseout', linkModeHover2, false);
      //removing the hovered element (which is retained if the escape key is hit)
      document.querySelectorAll("[ghost-hovered=true]").forEach(e => e.removeAttribute("ghost-hovered"));
      editor.ui.model.visible = false;
      editor.ui.model.linkSelectMode = false;
      editor.ui.model.linkSelectCallback = undefined;
      editor.ui.model.linkSelectOtherMenus = undefined;
      editor.ui.refresh();
    }
    function noGhostHover(node) {
      curClass = node.getAttribute("class")
      if(curClass === "modify-menu-icon-label-link" ||
        curClass === "context-menu-icon" ||
        curClass === "context-menu-icon fill") {
          return false;
        }
      else if(node.tagName === "path" || node.tagName === "PATH") {
        return false;
      }
      return true;
    }
    function linkModeHover1(event) {
      //console.log(event.target);
      //console.log(event.target.tagName);
      //console.log(event.target.getAttribute("class"));
      if(noGhostHover(event.target)) { 
        event.target.setAttribute("ghost-hovered", true);
        editor.ui.refresh();
        //console.log("hey!");
      }
    }
    function linkModeHover2(event) {
      if(noGhostHover(event.target)) {
        event.target.removeAttribute("ghost-hovered");
        editor.ui.refresh();
      }
    }

    function confirmLeaving() {
      if(editor.ui.canSave()) {
        var x = confirm("There are unsaved modifications. Do you want to discard them?");
        if(x) {
          editor.ui.model.undoStack = [];
          editor.ui.model.redoStack = [];
          return true;
        } else {
          return false;
        }
      }
      editor.ui.model.undoStack = [];
      editor.ui.model.redoStack = [];
      return true;
    }
    editor.confirmLeaving = confirmLeaving;
    //(outer lastEditScript)

    // Wraps a portion of code so that it and its mutation observers are executed with the flag editor.ui.model.userIsModifying set to true.
    editor.userModifies = function userModifies(callback) {
      editor.ui.model.userIsModifying = true;
      if(callback) callback();
      setTimeout(() => {
        editor.ui.model.userIsModifying = false;
      }, 0); // Will be invoked after mutation handlers.
    }
    editor.userStartsModifying = () => { editor.ui.model.userIsModifying = true; }
    editor.userStopsModifying = () => { editor.ui.model.userIsModifying = false; }
        
    // newValue can be a function, in which it should be applied on the current content.
    // Returns the old value.
    async function assignTmpCss(linkNode, newValue, notUndoable) {
      if(!notUndoable) { // We send this to undo.
        editor_stopWatching();
        let oldValue = await assignTmpCss(linkNode, newValue, true);
        let m = {type: "linkHrefCSS", target: linkNode, oldValue: oldValue, newValue: newValue};
        editor.ui._internals.postProcessMutations([editor.ui._internals.makeMutationUndoable(m)]);
        editor.ui.syncUndoRedoButtons();
        editor_resumeWatching();
        return;
      }
      // Here the change should not take care of doing the undo/redo part. 
      let ghostHref = linkNode.getAttribute("ghost-href");
      let hasGhostHref = typeof ghostHref === "string";
      let oldHref = hasGhostHref ? ghostHref : linkNode.getAttribute("href")
      let CSSFilePath = relativeToAbsolute(removeTimestamp(oldHref));
      let currentContent = typeof linkNode.cachedContent === "string" ? linkNode.cachedContent :
                               await editor.getServer("read", CSSFilePath);
      if(typeof linkNode.cachedContent !== "string") {
        linkNode.cachedContent = currentContent;
      }
      if(hasGhostHref) { // Proxied
        console.log("Was proxied");
        let tmpCachedContent = typeof linkNode.tmpCachedContent == "string" ? linkNode.tmpCachedContent :
                               await editor.getServer("read", linkNode.getAttribute("href"));
        if(typeof newValue === "function") {
          newValue = newValue(tmpCachedContent);
        }
        if(currentContent === newValue) { // We can remove the proxy
          if(!notUndoable) {
            editor.ui.model.outputObserver.disconnect();
          }
          let CSSTmpFilePath = linkNode.getAttribute("href");
          await editor.postServer("unlink", removeTimestamp(CSSTmpFilePath));
          editor.userModifies();
          linkNode.setAttribute("href", oldHref);
          linkNode.removeAttribute("ghost-href");
          linkNode.tmpCachedContent = newValue;
        } else { // We keep the proxy, just update the href
          let CSSTmpFilePath = linkNode.getAttribute("href");
          await editor.postServer("write", removeTimestamp(CSSTmpFilePath), newValue);
          editor.userModifies();
          linkNode.setAttribute("href", setTimestamp(CSSTmpFilePath));
          linkNode.tmpCachedContent = newValue;
        }
        return tmpCachedContent;
      } else {// Unproxied
        console.log("Was not proxied")
        if(typeof newValue === "function") {
          newValue = newValue(currentContent);
        }
        if(currentContent !== newValue) { // Create the proxy file
          console.log("Value updated")
          //add dummy counter, force reload
          let CSSTmpFilePath = getTempCSSName(CSSFilePath);
          await editor.postServer("write", CSSTmpFilePath, newValue);
          editor.userModifies();
          linkNode.setAttribute("ghost-href", oldHref);
          linkNode.setAttribute("href", setTimestamp(CSSTmpFilePath));
          linkNode.tmpCachedContent = newValue;
        } // else nothing to change, leave unproxied.
        return currentContent;
      }
    }
    
    function init_css_parser() {
      let thaditorLossLessCss = document.querySelector("script#thaditor-losslesscss");
      if(!thaditorLossLessCss) {
        let script = el("script", {id: "thaditor-losslesscss", class:"editor-interface", type:"text/javascript", src:"https://cdn.jsdelivr.net/gh/MikaelMayer/lossless-css-parser@d4d64a4a87f64606794a47ab58428900556c56dc/losslesscss.js", async:"true", onload:"editor.ui._internals.setLosslessCssParser(losslesscssjs);", isghost: "true"});
        document.head.appendChild(script);
      } else {
        console.log("thaditorLossLessCss found", thaditorLossLessCss);
      }
    }
         
    function init_interfaces() {
      init_css_parser();
      let linkSelect = function() {
        activateNodeSelectionMode("to link to",
          (linkFrom => linkTo => {
            let targetID = linkTo.getAttribute("id");
            if(!targetID) {
              targetID = "ID" + editor.ui.model.idNum
              linkTo.setAttribute("id", targetID);
              editor.ui.model.idNum += 1;
            }
            else if(targetID.length > 100) {
              targetID = targetID.trim();
              linkTo.setAttribute("id", targetID);
            }
            linkFrom.setAttribute("href", "#" + targetID);
          })(editor.ui.model.clickedElem)
        );
      } 
      let createButton = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("modify-menu-button");
        button.innerHTML = innerHTML;
        return button;
      } //you can append a createbutton to the element returning in render
      let add_btn_to_div = (div, innerHTML, attributes, properties) => {
        div.append(createButton(innerHTML, attributes, properties));
      };
      if(typeof simple_editor_interface === "object") {
        let x = simple_editor_interface[editor.config.EDITOR_VERSION];
        if(typeof x === "object") {
          editor.ui.model.interfaces.push(x);
          if(x.alone) {
            return;
          }
        }
      }
      editor.ui.model.interfaces.push({
        title: "Selected Element Tree",
        minimized: true,
        priority(editor_model) {
          return undefined;
        },
        enabled(editor_model) {
          return editor_model.clickedElem;
        },
        render: function render(editor_model, innerBox) {
          let domSelector = el("div.dom-selector.noselect"); // create dom selector interface
          const clickedElem = editor_model.clickedElem;
          if (!clickedElem) return "Click on an element to view its location in DOM tree";
          domSelector.classList.add("dom-selector-style");
          let mainElemDiv = el("div.mainElem");
          let childrenElemDiv = el("div.childrenElem");
          domSelector.append(
            mainElemDiv, childrenElemDiv
          );
          let displayMainElem = function(elem) {
            mainElemDiv.append(
              el("div", {"class":"mainElemName", "type":"text", value: elem.tagName.toLowerCase()}, "<" + elem.tagName.toLowerCase() + ">", {
                onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(elem),
                onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(elem)
              }),
              el("div", {"class": "mainElemInfo"}, textPreview(elem, 50))
            );
          }
          let displayChildrenElem = function(elem) {
            childrenElemDiv.append(
              el("div", {
                  "class": "childrenSelector" + (elem.matches(".editor-interface") ? " editor-interface-dom-selector" : "") +
                    (editor.isGhostNode(elem) ? " editor-recorded-ghost-node" : ""),
                  title: elem.matches(".editor-interface") ? "This is part of Editor" : (editor.isGhostNode(elem) ? "(temporary) " : "") + textPreview(elem, 20)
                  },
                [
                  el("div", {"class": "childrenSelectorName"}, "<" + elem.tagName.toLowerCase() + ">", {}),
                  // el("div", {"class": "childrenSelectorInfo"}, textPreview(elem, 20))
                ], 
                {
                  onmouseenter: (c => () => { c.setAttribute("ghost-hovered", "true") })(elem),
                  onmouseleave: (c => () => { c.removeAttribute("ghost-hovered") })(elem)
                }
              )
            );
          }
          // show attributes of element on the dom selector
          let displayElemAttr = function(targetDiv, elem) {
            for (let i = 0; elem && elem.attributes && i < elem.attributes.length; i++) {
              let name = elem.attributes[i].name;
              let value = elem.attributes[i].value;
              if (name === "ghost-clicked" || name === "ghost-hovered") continue;
              targetDiv.append(
                el("div", { "class": "elementAttr" },
                  [
                    el("span", { title: "This element has attribute name '" + name + "'" }, name + ": "),
                    el("span", { title: "This element has attribute value '" + value + "'" }, value)
                  ]
                )
              );
            }
          }
          // display children and siblings in the second part of selector
          let displayChildrenSiblings = function(middleChild, selectMiddleChild) {
            // display clicked element's previous sibling, clicked element, clicked element's next sibling
            let cnt = 0;
            // display previous sibling
            if (middleChild.previousElementSibling && 
                (middleChild.previousElementSibling.id !== "context-menu" || middleChild.previousElementSibling.id !== "modify-menu" || middleChild.previousElementSibling.id !== "editbox")) {
              displayChildrenElem(middleChild.previousElementSibling);
              let qs = childrenElemDiv.querySelectorAll(".childrenElem > .childrenSelector");
              console.log ({qs});
              qs[cnt].onclick = function () {
                let c = middleChild.previousElementSibling;
                if ((c.tagName && c.tagName === "HTML") || !c.tagName) {
                  return;
                }
                // still in status 2, but clicked element change to previous sibling
                editor_model.displayClickedElemAsMainElem = false;
                editor_model.previousVisitedElem = []; // clear the stack
                editor_model.clickedElem.removeAttribute("ghost-hovered");
                editor_model.clickedElem = c;
                editor_model.notextselection = true;
                if(editor.config.onMobile()) editor_model.savedTextSelection = editor.ui.clearTextSelection();
                editor.ui.refresh();
              }
            } else {
              childrenElemDiv.append(
                el("div", {"class": "childrenSelector no-sibling"}, "no sibling")
              );
            }
            cnt++;
            // display certain child in the middle
            displayChildrenElem(middleChild);
            childrenElemDiv.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
              let c = middleChild;
              if (!c.tagName) {
                return;
              }

              if (!c.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
                // still in status 2
                editor_model.displayClickedElemAsMainElem = false;
              } else {
                // switch to status 1
                editor_model.displayClickedElemAsMainElem = true;
              }
              editor_model.clickedElem.removeAttribute("ghost-hovered");
              editor_model.clickedElem = c;
              editor_model.notextselection = true;
              if(editor.config.onMobile()) editor_model.savedTextSelection = editor.ui.clearTextSelection();
              editor.ui.refresh();
            }
            if (selectMiddleChild) {
              childrenElemDiv.querySelectorAll(".childrenElem > .childrenSelector")[cnt].classList.add("selectedDom");
            }
            cnt++;
            // display next sibling
            if (middleChild.nextElementSibling && 
              (middleChild.nextElementSibling.id !== "context-menu" || middleChild.nextElementSibling.id !== "modify-menu" || middleChild.nextElementSibling.id !== "editbox")) {
              displayChildrenElem(middleChild.nextElementSibling);
              childrenElemDiv.querySelectorAll(".childrenElem > .childrenSelector")[cnt].onclick = function () {
                let c = middleChild.nextElementSibling;
                if ((c.tagName && c.tagName === "HTML") || !c.tagName) {
                  return;
                }
                // still in status 2, but clicked element change to next sibling
                editor_model.displayClickedElemAsMainElem = false;
                editor_model.previousVisitedElem = []; // clear the stack
                editor_model.clickedElem.removeAttribute("ghost-hovered");
                editor_model.clickedElem = c;
                editor_model.notextselection = true;
                if(editor.config.onMobile()) editor_model.savedTextSelection = editor.ui.clearTextSelection();
                editor.ui.refresh();
              }
            } else {
              childrenElemDiv.append(
                el("div", {"class": "childrenSelector no-sibling"}, "no sibling")
              );
            }
          }
          // editor itself should be invisible
          if (clickedElem.id !== "context-menu" || clickedElem.id !== "modify-menu" || clickedElem.id !== "editbox") {
            console.log(clickedElem);
            if (!clickedElem.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
              editor_model.displayClickedElemAsMainElem = false;
            }
            // status 1. display clicked element in main part
            if (editor_model.displayClickedElemAsMainElem) {
              displayMainElem(clickedElem);
              domSelector.classList.add("selectedDom");
              mainElemDiv.onclick = function () {
                if (!clickedElem.tagName) {
                  return;
                }
                // When the main element in selector is clicked, selector switch to status 2 so that user can see its parent element
                editor_model.displayClickedElemAsMainElem = false;
                editor_model.clickedElem.removeAttribute("ghost-hovered");
                editor_model.clickedElem = clickedElem;
                editor_model.notextselection = true;
                if(editor.config.onMobile()) editor_model.savedTextSelection = editor.ui.clearTextSelection();
                editor.ui.refresh();
              }
              displayElemAttr(mainElemDiv, clickedElem);
              // display children, if no previous selected child, display first 3 children elements in second part of selector
              if (editor_model.previousVisitedElem.length < 2 ||
                  (editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1] != clickedElem)) {
                if (editor_model.previousVisitedElem.length !== 0) {
                  editor_model.previousVisitedElem = [];
                }
                if (clickedElem.children.length > 0) {
                  // only display first 3 children elements
                  let childrenElem = clickedElem.children;
                  for (let i = 0, cnt = 0; i < childrenElem.length && cnt < 3; ++i) {
                    // prevent displaying editor itself
                    if (cnt === 0 && (childrenElem[i].matches(".editor-interface") || editor.isGhostNode(childrenElem[i]))) {
                      continue;
                    }
                    displayChildrenElem(childrenElem[i]);
                    let qs = childrenElemDiv.querySelectorAll(".childrenElem > .childrenSelector");
                    console.log ({qs});
                    qs[cnt].onclick = function () {
                      let c = childrenElem[i];
                      if (!c.tagName) {
                        return;
                      }
                      if (!c.hasChildNodes() || (clickedElem.childNodes.length == 1 && clickedElem.childNodes[0].nodeType === 3)) {
                        editor_model.displayClickedElemAsMainElem = false;
                      } else {
                        // still in status 1
                        editor_model.displayClickedElemAsMainElem = true;
                      }
                      editor_model.clickedElem.removeAttribute("ghost-hovered");
                      editor_model.clickedElem = c;
                      editor_model.notextselection = true;
                      editor.ui.refresh();
                    }
                    cnt++;
                  }
                }
                // else: bottom of DOM tree
              } else {
                editor_model.previousVisitedElem.pop();
                let middleChild = editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1];
                displayChildrenSiblings(middleChild, false);
              }
            } else {
              // status 2. display clicked element's parent element in main part
              // <html> has no parent element
              if(clickedElem.parentElement) {
                displayMainElem(clickedElem.parentElement);
                mainElemDiv.onclick = function () {
                  if (!clickedElem.parentElement.tagName) {
                    return;
                  }
                  // still in status 2 while current clicked element's parent element becomes clicked element so that user can see grandparent element
                  editor_model.displayClickedElemAsMainElem = false;
                  // memoization. when user click parent element:
                  if (editor_model.previousVisitedElem.length === 0) {
                    editor_model.previousVisitedElem.push(clickedElem);
                    editor_model.previousVisitedElem.push(clickedElem.parentElement);
                  } else {
                    if (editor_model.previousVisitedElem[editor_model.previousVisitedElem.length - 1] == clickedElem) {
                      editor_model.previousVisitedElem.push(clickedElem.parentElement);   // continuous storing path
                    } else {
                      editor_model.previousVisitedElem = []; // clear the stack
                    }
                  }
                  editor_model.clickedElem.removeAttribute("ghost-hovered");
                  editor_model.clickedElem = clickedElem.parentElement;
                  editor_model.notextselection = true;
                  if(editor.config.onMobile()) editor_model.savedTextSelection = editor.ui.clearTextSelection();
                  editor.ui.refresh();
                }
                displayElemAttr(mainElemDiv, clickedElem.parentElement);
              } else {
                // for <html>
                displayMainElem(clickedElem);
                displayElemAttr(mainElemDiv, clickedElem);
              } 
              displayChildrenSiblings(clickedElem, true);
            }
          }
          return domSelector;
        }
      });
      editor.ui.model.interfaces.push({
        title: "Attributes",
        minimized: true,
        priority(editor_model) {
          return undefined;
        },
        enabled(editor_model) {
          return editor_model.clickedElem;
        },
        render: function render(editor_model, innerBox) {
          let keyvalues = el("div", {"class":"keyvalues"});
          const clickedElem = editor_model.clickedElem;
          if (!clickedElem) return "Click on an element to see its attributes";
          // modify tagname
          keyvalues.append(
            el("div", {"class": "keyvalue"}, [
              el("span", {title: "This element has tag name '" + clickedElem.tagName.toLowerCase() + "'"}, "Tag: "),
              el("span", {class:"attribute-key-value"}, [
                el("input", {"type": "text", value: clickedElem.tagName.toLowerCase(), "id": "newTagName"}, 
                  [], {
                    oninput() {
                      let applyNewTagNameButton = document.querySelector("#applyNewTagName");
                      applyNewTagNameButton.classList.toggle("visible", this.value !== this.getAttribute("value") && this.value.match(/^\w*$/));
                      applyNewTagNameButton.value = this.value === "" ? "-" : "Set";
                      applyNewTagNameButton.setAttribute("title", this.value === "" ? "Lift element's children and delete element" :  "Change tag name to '"+this.value+"'");
                    }
                  }),
                  el("input", {"type": "button", id: "applyNewTagName", value: "Set", title: "Apply new tag name"}, [], {onclick() {
                        editor.userModifies(() => {
                          let newTagName = document.querySelector("#newTagName").value;
                          let newel;
                          if(newTagName === "") {
                            while(clickedElem.childNodes.length) {
                              newel = clickedElem.childNodes[0];
                              clickedElem.parentElement.insertBefore(newel, clickedElem);
                            }
                            clickedElem.remove();
                          } else {
                            newel = el(document.querySelector("#newTagName").value);
                            let elements = clickedElem.childNodes;
                            for(let i = 0; i < elements.length; i++) {
                              newel.append(elements[i].cloneNode(true));
                            }
                            for(let i = 0; i < clickedElem.attributes.length; i++) {
                              newel.setAttribute(clickedElem.attributes[i].name, clickedElem.attributes[i].value);
                            }
                            clickedElem.parentElement.insertBefore(newel, clickedElem);
                            clickedElem.remove();
                          }
                          editor_model.clickedElem = newel;
                          editor.ui.refresh();
                        });
                      }
                    }
                  ),
                  el("div", {id:"newtagname-align-placeholder"}, " ")
                ]
              )
            ])
          );
          let isSpecificGhostAttributeKey = editor.isSpecificGhostAttributeKeyFromNode(clickedElem);
          let isIgnoredAttributeKey = editor.isIgnoredAttributeKeyFromNode(clickedElem);

          for(let i = 0; clickedElem.attributes && i < clickedElem.attributes.length; i++) {
            let name = clickedElem.attributes[i].name;
            if(name === "ghost-clicked" || name === "ghost-hovered") continue;
            let value = clickedElem.attributes[i].value;
            // Inline styles incoporated into CSS display editor
            if(name !== "style") {
              let isGhost = editor.isGhostAttributeKey(name) || isSpecificGhostAttributeKey(name);
              let isIgnored = isIgnoredAttributeKey(name);
              let isHref = name === "href" && clickedElem.tagName === "A";
              keyvalues.append(
                el("div", {"class": "keyvalue" + (isGhost ? " editor-recorded-ghost-attribute" : "")
                                              + (isIgnored ? " editor-ignored-attribute" : ""),
                          "title": isGhost ? "Key/value generated by a script" : isIgnored ? "key/value ignored after being modified by a script" : undefined
                }, [
                  el("span", {title: "Element attribute name"}, name + ": "),
                  el("span", {class: "attribute-key-value", title: "Element attribute value of " + name}, [
                    el("input", {"type": "text", value: value, "id": ("dom-attr-" + name)}, [], {
                        oninput: ((name, isHref) => function () {
                          editor._internals.ifIdleAfter(200, "attribute " + name, () => {
                            editor.userModifies(() => {
                                clickedElem.setAttribute(name, this.value);
                                if(isHref) {
                                  let livelinks = document.querySelectorAll(".livelink");
                                  for(let livelink of livelinks) {
                                    let finalLink = livelink.matches("#context-menu *") ?
                                      `javascript:if(editor.confirmLeaving()) { editor.navigateTo('${linkToEdit(this.value)}'); }` : this.value;
                                    livelink.setAttribute("href", finalLink);
                                    livelink.setAttribute("title", "Go to " + this.value);
                                  }
                                }
                              });
                          });
                        })(name, isHref)
                      }),
                    isHref ? el("div", {title: "Go to " + value, "class": "modify-menu-icon inert"}, [], {
                      innerHTML: editor.ui.icons.liveLink(value)
                    }) : undefined,
                    isHref ? el("div", {title: "Select a node on the page to refer to", "class": "modify-menu-icon inert"}, [], { 
                      innerHTML: editor.ui.icons.linkMode,
                      onclick: linkSelect
                    }) : undefined,
                    el("div", {"class":"modify-menu-icon", title: "Delete attribute '" + name + "'"}, [], {
                      innerHTML: editor.ui.icons.wasteBasket,
                      onclick: ((name) => function() {
                        editor.userModifies(() => {
                          clickedElem.removeAttribute(name);
                          editor_model.clickedElem = clickedElem;
                          editor.ui.refresh();
                        });
                        })(name)
                      })
                    ]
                  )
                ]
              ));
            }
            else {
              let styleInterface = getEditorInterfaceByTitle("Style");
              if(styleInterface) {
                styleInterface.minimized = false;
                editor.ui.model.inline = clickedElem.getAttribute("style");
                styleInterface.priority(editor.ui.model);
              }
            }
          }
          let highlightsubmit = function() {
            let attrName = this.parentElement.parentElement.querySelector("[name=name]").value;
            this.parentElement.parentElement.querySelector("div.modify-menu-icon").disabled =
              attrName === "" || attrName.trim() !== attrName
          }

          if(clickedElem.nodeType === 1) {
            keyvalues.append(
              el("div", {"class": "keyvalue keyvalueadder"}, [
                el("span", {class: "attribute-key"}, el("input", {"type": "text", placeholder: "key", value: "", name: "name"}, [], {oninput: highlightsubmit})),
                el("span", {class: "attribute-key-value"}, [
                  el("span", {}, el("input", {"type": "text", placeholder: "value", value: "", name: "value"}, [], {
                    onfocus: function() {
                      let keyInput = document.querySelector("div.keyvalueadder input[name=name]");
                      if(keyInput && keyInput.value != "") {
                        let name = document.querySelector("div.keyvalueadder input[name=name]").value;
                        editor.userModifies(() => {
                          clickedElem.setAttribute(
                            name,
                            document.querySelector("div.keyvalueadder input[name=value]").value
                          );
                        });
                        editor.ui.refresh();
                        let d =  document.querySelector("div.keyvalue input#dom-attr-" + name);
                        if(d) d.focus();
                      }
                    },
                    oninput: highlightsubmit})),
                  el("div", {"class":"modify-menu-icon", title: "Add this name/value attribute"}, [], {innerHTML: editor.ui.icons.plus,
                    disabled: true,
                    onclick() {
                      editor.userModifies(() => {
                        clickedElem.setAttribute(
                          this.parentElement.querySelector("[name=name]").value,
                          this.parentElement.querySelector("[name=value]").value
                        );
                      });
                      editor.ui.refresh();
                    },
                    oninput: highlightsubmit })])
              ])
            );
          }
          return keyvalues;
        }
      });
      
      editor.ui.model.interfaces.push({
        title: "Style",
        minimized: true,
        priority(editor_model) {
          if(editor_model.inline) return 1;
          return undefined;
        },
        enabled(editor_model) {
          return editor_model.clickedElem;
        },
        render: function render(editor_model, innerBox) {
          const clickedElem = editor_model.clickedElem;
          if(!clickedElem) {
            return "Click on an element to see its style";
          }
          const do_css = (clickedElem && clickedElem.id !== "context-menu" && clickedElem.id !== "modify-menu" && clickedElem.id !== "editbox" &&
                          !editor_model.insertElement);
          let CSSarea = el("div", {id: "CSS-modification", value: ""}, [], {}); 
          if (!do_css) return CSSarea;
          //parse relevant CSS, recording prior and post CSS text as well 

          async function fullParseCSS() {
            var fullCSS = [], keyframes = [], rawCSS = [];
            //console.log("All style tags:", document.querySelectorAll("style"));
            let CSSstyles = document.querySelectorAll("link[rel=stylesheet], style");
            for(let i = 0; i < CSSstyles.length; i++) {
              let linkOrStyleNode = CSSstyles[i];
              if(linkOrStyleNode.tagName === "LINK" && linkOrStyleNode.getAttribute("rel") === "stylesheet" &&
                 linkOrStyleNode.getAttribute("href") && !linkOrStyleNode.getAttribute("isghost")) {
                   let href = linkOrStyleNode.getAttribute("href");
                   if(isAbsolute(href)) continue;
                let CSSFilePath = relativeToAbsolute(removeTimestamp(href));
                if(!(linkOrStyleNode.className && linkOrStyleNode.className === "editor-interface") && (CSSFilePath.indexOf("http") < 0)) {
                  let CSSvalue = typeof linkOrStyleNode.tmpCachedContent === "string" ?
                        linkOrStyleNode.tmpCachedContent :
                        await editor.getServer("read", CSSFilePath);
                  rawCSS.push({text: CSSvalue, tag: linkOrStyleNode});
                }
              }
              else if(linkOrStyleNode.tagName === "STYLE" && !linkOrStyleNode.getAttribute("isghost")) {
                rawCSS.push({text: linkOrStyleNode.textContent, tag: linkOrStyleNode});
              }
            }
            let CSSparserBuilder = await editor.ui._internals.getLosslessCssParser;
            let CSSparser = new CSSparserBuilder();
            function findText(parsed, startIndex, endIndex) { //for css + img replacement
              let textSegment = "";
              for(let i = startIndex; i < endIndex; i++) {
                textSegment += parsed ? parsed[0].selector ? CSSparser.unparseCSS([parsed[i]]) :
                  (parsed[0].directive ? CSSparser.unparseRules([parsed[i]]) : "") : "";
                //console.log(textSegment);
              }
              return textSegment;
            }
            for(let z in rawCSS) {  
              var parsedCSS = CSSparser.parseCSS(rawCSS[z].text);
              for(let i in parsedCSS) {
                if(parsedCSS[i].kind === 'cssBlock' && editor.matches(clickedElem, parsedCSS[i].selector.replace(/:(?=(:?after|:?before|:?hover))[^,]*(?=,|$)/g, ""))) {
                  let content = CSSparser.unparseCSS([parsedCSS[i]]);
                  let wsBefore = content.replace(/^(\s*\n|)[\s\S]*$/g, (m, ws) => ws);
                  let contentTrimmed = content.replace(/\s*\n/,"");
                  //calculating before and after text
                  fullCSS.push({type: 'cssBlock', content: contentTrimmed, 
                    before: findText(parsedCSS, 0, Number(i)) + wsBefore, after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag});
                }
                else if(parsedCSS[i].kind === '@media' && window.matchMedia(parsedCSS[i].atNameValue).matches) {
                  let curMedia = parsedCSS[i];
                  for(let j in curMedia.content) {
                    if(curMedia.content[j].kind === 'cssBlock' && editor.matches(clickedElem, curMedia.content[j].selector.replace(/:(?=(:?after|:?before|:?hover))[^,]*(?=,|$)/g, ""))) {
                      var insertMedia = {type: '@media', content: CSSparser.unparseCSS([curMedia.content[j]]), 
                        mediaSelector: curMedia.wsBefore + curMedia.selector + curMedia.wsBeforeAtNameValue + curMedia.atNameValue + curMedia.wsBeforeOpeningBrace + "{",
                        innerBefore: findText(curMedia.content, 0, j), innerAfter: findText(curMedia.content, Number(j) + 1, curMedia.content.length),
                        before: findText(parsedCSS, 0, Number(i)), after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag, bracketAfter: curMedia.wsBeforeClosingBrace + "}"};
                      //console.log("Insert media:");
                      //console.log(insertMedia);
                      fullCSS.push(insertMedia);
                      //console.log("got here first!");
                    }
                  }
                }
                else if(parsedCSS[i].kind === '@charset') {
                  if(!(parsedCSS[i].wsBefore === "" && parsedCSS[i].wsBeforeAndSemicolon === ";" && parsedCSS[i].wsBeforeValue === " "
                    && parsedCSS[i].value.startsWith("\"") && parsedCSS[i].value.endsWith("\""))) {
                    editor.ui.sendNotification("CSS @charset declaration is invalid due to extraneous white space.");	
                  }
                  if(editor_model.clickedElem.tagName != "STYLE" && editor_model.clickedElem.tagName != "LINK") {
                    fullCSS.push({type: '@charset', content: CSSparser.unparseCSS([parsedCSS[i]]), 
                      before: findText(parsedCSS, 0, i), after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag});
                  }
                }
                else if(parsedCSS[i].kind === '@keyframes') {
                  keyframes.push({type: '@keyframes', content: CSSparser.unparseCSS([parsedCSS[i]]), 
                    before: findText(parsedCSS, 0, Number(i)), after: findText(parsedCSS, Number(i) + 1, parsedCSS.length), orgTag: rawCSS[z].tag,
                    animationName: parsedCSS[i].atNameValue});
                }
                else if(parsedCSS[i].kind === 'whitespace') { 
                  continue;
                }
                if(i === parsedCSS.length - 1 && !fullCSS.length) {
                  console.log("Nothing relevant in style tag: ", rawCSS[z].tag);
                }
              }
              //console.log("The parsed text looks like:", curCSS);
            }
            for(i in keyframes) {
              for(j in fullCSS) {
                let parsedSection = CSSparser.parseCSS(fullCSS[j].content);
                for(k in parsedSection.content) {
                  for(l in parsedSection.content[k].rules) {
                    if(Number(parsedSection.content[k].rules[l].search(keyframes[i].animationName)) >= 0) {
                      fullCSS.push(keyframes[i]);
                    }
                  }
                }
                for(k in parsedSection.rules) {
                  if(Number(parsedSection.rules[k].search(keyframes[i].animationName)) >= 0) {
                    fullCSS.push(keyframes[i]);
                  }
                }
              }
            }
            //console.log(fullCSS);
            return fullCSS;
          } // fullParseCSS
          
          function fullUnparseCSS(curCSS) {
            let curTag = curCSS.orgTag;
            let CSSString = "";
            if(curCSS.type === 'cssBlock' || curCSS.type === "@charset") {
              //console.log(curCSS.content);
              CSSString = curCSS.before + curCSS.content + curCSS.after;
              //console.log(CSSString);
            }
            else if(curCSS.type === '@media') { 
              console.log(curCSS);
              CSSString = curCSS.before + curCSS.mediaSelector + curCSS.innerBefore + curCSS.content + curCSS.innerAfter + curCSS.bracketAfter + curCSS.after;   
            }
            if(curTag.tagName === "LINK") {
              return CSSString;
            }
            // Style elements
            //console.log("Text is:" + CSSString);
            editor.userModifies(() => {
              curTag.textContent = CSSString;
            });
            //debugger
            //consolw.log("After");
          } // fullUnparseCSS
          var curCSSWindow = undefined;

          function setCSSAreas() {
            //console.log(CSSarea.firstChild);
            while(CSSarea.firstChild) {
              console.log("Removed child:", CSSarea.firstChild);
              CSSarea.removeChild(CSSarea.firstChild);
            }
            //if there is linked CSS text
            if(clickedElem.tagName === "LINK" && clickedElem.getAttribute("rel") === "stylesheet" && clickedElem.getAttribute("href")) {
              let oldHref = clickedElem.getAttribute("href"); // Even if it's a temporary href
              let CSSFilePath = relativeToAbsolute(oldHref);
              (async () => {
                let CSSvalue = await editor.getServer("read", "CSSFilePath");
                CSSarea.append(el("div", {"class": "CSS-chain"}, [], {innerHTML: "STYLE TEXT:"}));
                CSSarea.append(
                  el("div", {"class": "CSS-modify-unit"}, [
                    el("textarea", {"class": "linked-CSS"}, [], {
                      value: CSSvalue,
                      onfocusout() {
                        setCSSAreas();
                      },
                      oninput() {
                        editor._internals.ifIdleAfter(200, "linked-css", () => {
                          (async () => { // Maybe create a new temporary CSS file.
                            await assignTmpCss(clickedElem, this.value);
                          })();
                        });
                      }
                    })
                  ])
                );
              })();
            }
            //inline styles 
            editor_model.inline = clickedElem.getAttribute("style"); //? CSSparser.parseCSS(clickedElement.getAttribute("style")) : undefined;
            if(editor_model.inline) {
              console.log("We have inline CSS!");
              let inlineCSS = el("div", {"class": "CSS-modify-unit"}, [
                el("textarea", {"class": "inline-CSS"}, [], {
                  value: editor_model.inline,
                  onfocusout() {
                    setCSSAreas();
                  },
                  oninput() {
                    editor._internals.ifIdleAfter(200, "inline-style", () => {
                      editor.userModifies(() => {
                        clickedElem.setAttribute("style", this.value);
                      });
                    });
                  }
                }),
                el("div", {"class": "CSS-buttons", title: "Apply this style to other elements by creating a rule"}, [
                  el("div", {"class": "CSS-action-button"}, [], {
                    innerHTML: editor.ui.icons.clone,
                    onclick() {
                      let stylesLinks = document.querySelectorAll("style, link[rel=stylesheet]");
                      let i = stylesLinks.length - 1;
                      let lastStyleLink = stylesLinks[i];
                      while(i >= 0 && lastStyleLink.matches(".editor-interface, .editor-interface *")) {
                        i--;
                        lastStyleLink = stylesLinks[i];
                      }
                      if(lastStyleLink && (lastStyleLink.isghost || lastStyleLink.tagName === "LINK" && lastStyleLink.tagName.indexOf("http") >= 0)) {
                        lastStyleLink = undefined;
                      }
                      console.log("Closest CSS source:", lastStyleLink);
                      let inline_CSS = document.querySelectorAll(".inline-CSS");
                      console.log("Finding inline CSS textarea:", inline_CSS);  
                      let postIndentCSS = "";
                      let preIndentCSS = inline_CSS[0].value.split("\n");
                      for(let i = 0; i < preIndentCSS.length; i++) {
                        if(i !== preIndentCSS.length-1) {
                          postIndentCSS += "  " + preIndentCSS[i] + "\n";
                        }
                        else {
                          postIndentCSS += "  " + preIndentCSS[i]; 
                        }
                      }
                      let curSelector = getShortestUniqueSelector(clickedElem);
                      postIndentCSS = "\n" + curSelector + " {\n" + postIndentCSS + "\n}";   
                      console.log("lastStyleLink is:", lastStyleLink);     
                      if(lastStyleLink) {
                        if(lastStyleLink.tagName === "LINK") {
                          (async () => {
                            await assignTmpCss(lastStyleLink, oldValue => oldValue + postIndentCSS);
                            editor.userModifies(() => {
                              clickedElem.removeAttribute("style");
                            });
                            setCSSAreas();
                          })();
                        }
                        else { // lastStyleLink is a <style>
                          let curValue = lastStyleLink.textContent;
                          editor.userModifies();
                          lastStyleLink.textContent = curValue + postIndentCSS;
                          clickedElem.removeAttribute("style");
                          setCSSAreas();
                        }
                      }
                      else {
                        //just default to style node for now
                        editor.userModifies(() => {
                          document.body.appendChild(el("style.inserted-CSS", {}, postIndentCSS));
                          clickedElem.removeAttribute("style");
                        });
                        setCSSAreas();
                      }
                    }
                  }),
                  el("div", {"class": "CSS-action-button"}, [], {
                    innerHTML: editor.ui.icons.wasteBasket,
                    onclick() {
                      editor.userModifies(() => {
                        let inline_CSS = document.querySelectorAll(".inline-CSS");
                        inline_CSS.value = "";
                        clickedElem.setAttribute("style", inline_CSS.value);
                      });
                      setCSSAreas();
                    }
                  })
                ])
              ]);
              CSSarea.append(el("div", {"class": "CSS-chain"}, [], {innerHTML: "Inline styles:"}));
              CSSarea.append(inlineCSS);
            } // inline style present
            else{
              CSSarea.append(el("button.action-button#add-inline-style", {}, [], {
                innerHTML: "Add inline style",
                onclick() {
                  editor.userModifies(() => {
                    clickedElem.setAttribute("style", " ");
                  });
                  editor.ui.refresh();
                }}));
            }
            (async () => {
            //rest of CSS
            editor_model.CSSState = await fullParseCSS();
            //console.log("CSS state is:", editor_model.CSSState);
            const count = (str) => {
              const re = /\n/g
              return ((str || '').match(re) || []).length
            }
            for(let i = 0; i < editor_model.CSSState.length; i++) {
              let cssState = editor_model.CSSState[i];
              let orgTag = cssState.orgTag;
              //console.log("cssState", cssState);
              let headerStr = orgTag.tagName.toLowerCase() + (orgTag.tagName === "LINK" ? " (" + removeTimestamp(orgTag.getAttribute("ghost-href") || orgTag.getAttribute("href"))+":" + (count(cssState.before) + 1) + ")" : "");
              for(let curElem = orgTag.parentElement; curElem; curElem = curElem.parentElement) {
                headerStr =  curElem.tagName.toLowerCase() + " > " + headerStr; 
              }
              CSSarea.append(el("div", {"class": "CSS-chain"}, [], {
                innerHTML: headerStr,
                onclick: () => {
                  editor_model.clickedElem = orgTag;
                  editor.ui.refresh();
                }
                }));
              if(cssState.type === '@media') {
                CSSarea.append(el("div", {"class": "@media-selector", "contenteditable": true}, [], {
                oninput: (cssState => function() {
                  editor._internals.ifIdleAfter(200, "media-css-update", () => {
                    (async () => {
                      if(window.matchMedia(cssState.selector).matches ? editor.matches(clickedElem, cssState.content.selector) : false) {
                        //implement throwError;
                      }
                      cssState.mediaSelector = this.textContent;
                      if(cssState.orgTag.tagName != "LINK") {
                        fullUnparseCSS(cssState);
                      } else {
                        await assignTmpCss(cssState.orgTag, fullUnparseCSS(cssState));
                      }
                    })();
                  });
                })(cssState),
                innerHTML: cssState.mediaSelector
                }))
              }
              let eachCSS = el("div", {"class": "CSS-modify-unit"}, [
                el("textarea", {"class": "CSS-selectors" }, [], {
                  defaultValue: cssState.content,
                  onfocusout() {
                    if(this.storedCSS.orgTag.tagName != "LINK") {
                      setCSSAreas();
                    }
                  },
                  oninput: function() {
                    editor._internals.ifIdleAfter(200, "css-link-update", () => {
                      (async () => {
                        if(this.storedCSS.orgTag.tagName != "LINK") { // style node
                          let throwError = false;
                          let CSSparserBuilder = await editor.ui._internals.getLosslessCssParser;
                          let CSSparser = new CSSparserBuilder();
                          let curCSSState = CSSparser.parseCSS(this.value);
                          //console.log(curCSSState);
                          //check to make sure CSS is still relevant to clicked element.
                          if(curCSSState && curCSSState.length && curCSSState[0].kind === 'cssBlock' && !editor.matches(clickedElem, curCSSState[0].selector)) {
                            editor.ui.sendNotification("CSS selector does not match");
                            this.setAttribute("wrong-selector", true);
                            this.setAttribute("title", "The first CSS selector does not apply to the selected element!");
                          }
                          else {
                            this.setAttribute("wrong-selector", false);
                            this.removeAttribute("title");
                          }
                          this.storedCSS.content = this.value;
                          fullUnparseCSS(this.storedCSS);
                          //setCSSAreas();
                        }
                        else { // Link
                          this.storedCSS.content = this.value;
                          await assignTmpCss(this.storedCSS.orgTag, fullUnparseCSS(this.storedCSS));
                        }
                      })();
                    });
                  },
                  storedCSS: cssState
                }),
                orgTag.tagName === "LINK" ?
                  el("div", {"class": "CSS-action-button", "title": "Delete this snippet of CSS"}, [], {
                    innerHTML: editor.ui.icons.wasteBasket,
                    onclick() {
                      (async () => {
                        let linked_CSS = this.parentElement.childNodes[0];
                        linked_CSS.value = "";
                        linked_CSS.storedCSS.content = linked_CSS.value;
                        await assignTmpCss(linked_CSS.storedCSS.orgTag, fullUnparseCSS(linked_CSS.storedCSS));
                        setCSSAreas();
                      }) ();
                    }
                  }) : // inline style case
                  el("div", {"class": "CSS-action-button", "title": "Delete this CSS snippet"}, [], {
                    innerHTML: editor.ui.icons.wasteBasket,
                    onclick() {
                      let linked_CSS = this.parentElement.childNodes[0];
                      //console.log(this.parentElements.childNodes);
                      linked_CSS.value = "";
                      linked_CSS.storedCSS.content = linked_CSS.value;
                      fullUnparseCSS(linked_CSS.storedCSS);
                      setCSSAreas();
                    }
                  })
              ]);
              CSSarea.append(eachCSS);
            }
            })(); // Async css set.
          } // function setCSSAreas()
          setCSSAreas();   
          return CSSarea;
        }
      });
      editor.ui.model.interfaces.push({
        title: "Image Tools",
        minimized: true,
        priority(editor_model) {
          return this.enabled(editor_model) ? 1 : undefined; // It's likely we want to modify this image above all.
        },
        findURLS(styleStr) {
          var urls = [];
          var diffPics = styleStr.split(",");
          for(let k in diffPics) {
            //extracts only url(...)
            let regex = new RegExp("(url\\([\"']?)([^\\)'\"]+?)([\"']?\\))", "g");
            let m;
            let remainStr = diffPics[k];
            while(m = regex.exec(remainStr)) {
              let sIndex = m.index + m[1].length;
              //extracting the rest of the string 
              let afterStr = remainStr.slice(sIndex + m[2].length);
              let beforeStr = remainStr.slice(0, sIndex);
              urls.push({remainderBefore: beforeStr, url: m[2], remainderAfter: afterStr});  
            }
          }
          return urls;
        },
        //checks the inline CSS of the clicked node/element to see if background or background-image is a rule, and if 
        //a link to an image is provided as part of the value for this rule;
        //TODO: expand the set of CSS being checked to any style tags as well.
        checkForBackgroundImg(clickedElem, findURLS) {
          function findText(parsed, startIndex, endIndex) { //for css + img replacement
            let textSegment = "";
            for(let i = startIndex; i < endIndex; i++) {
              textSegment += parsed ? parsed[0].selector ? editor.ui.CSSparser.unparseCSS([parsed[i]]) :
                (parsed[0].directive ? editor.ui.CSSparser.unparseRules([parsed[i]]) : "") : "";
              //console.log(textSegment);
            }
            return textSegment;
          }
          //console.log("clicked element is:", clickedElem);
          //clickedElem ? console.log(clickedElem.getAttribute("style")) : console.log("nothing clicked");
          var clickedStyle = clickedElem  && clickedElem.hasAttribute && clickedElem.hasAttribute("style") ? editor.ui.CSSparser.parseRules(clickedElem.getAttribute("style")) : []; 
          //console.log(clickedStyle);
          //inefficient way of doing things, but since background takes precedence over background-image, we need to process the 
          //former first, if it contains a url. for now, I am looping through the CSS rules twice.
          //console.log("^parsed rules ");
          for(let i = 0; i < clickedStyle.length; i++) {
            for(let j = 0; j < clickedStyle[i].length; j++) {
              if(clickedStyle[i][j].directive === "background") {
                clickedStyle[i][j].value = findURLS(clickedStyle[i][j].value);  
                if(clickedStyle[i][j].value.length) {
                  //console.log(clickedStyle[i][j]);
                  return {beforeCSS: findText(clickedStyle[i], 0, Number(j)), relCSS: clickedStyle[i][j], 
                    imageSelection: 0, afterCSS: findText(clickedStyle[i], Number(j) + 1, clickedStyle[i].length)};
                }
              }
            }
          }
          for(let i = 0; i < clickedStyle.length; i++) {
            for(let j = 0; j < clickedStyle[i].length; j++) {
              if(clickedStyle[i][j].directive === "background-image") {
                //console.log("hello?");
                //console.log(clickedStyle[i][j].value);
                clickedStyle[i][j].value = findURLS(clickedStyle[i][j].value);  
                if(clickedStyle[i][j].value.length) {
                  return {beforeCSS: findText(clickedStyle[i], 0, Number(j)), relCSS: clickedStyle[i][j], 
                    imageSelection: 0, afterCSS: findText(clickedStyle[i], Number(j) + 1, clickedStyle[i].length)};
                }
              }
            }
          } 
          //console.log("unsuccessful");
          return undefined;
        },
        enabled(editor_model) {
          let clickedElem = editor_model.clickedElem;
          if(clickedElem && clickedElem.tagName === "IMG") return true;
          if(!editor.ui.CSSparser) return false;
          let backgroundImgSrc = this.checkForBackgroundImg(clickedElem, this.findURLS);
          const do_img_rpl = (clickedElem && (clickedElem.tagName === "IMG" || backgroundImgSrc));
          this.backgroundImgSrc = backgroundImgSrc;
          return do_img_rpl;
        },
        
        // enabled has been called before, clickedElem is not empty and it contains a background image
        render: function render(editor_model, innerBox) {
          if(!this.enabled(editor_model)) {
            return "Click an element with a background image.";
          }
          //extract url and extraneous text from specified CSS value (which is originally part of a rule)
          const clickedElem = editor_model.clickedElem;
          let ret = el("div", {"class": "information"});
          if (!clickedElem) return ret;
          let backgroundImgSrc = this.backgroundImgSrc;
          //unparse the background/background-image object
          function unparseBackgroundImg(backImgObj) {
            var textSegment = "";
            let valueText = "";
            for(let i in backImgObj.relCSS.value) {
              valueText += (Number(i) !== 0 ? ", " : "") + backImgObj.relCSS.value[i].remainderBefore + backImgObj.relCSS.value[i].url + backImgObj.relCSS.value[i].remainderAfter;
              //console.log(valueText);
            }
            //console.log("Object about to be unparsed:");
            //console.log(backImgObj);
            return backImgObj.beforeCSS + findText([{...backImgObj.relCSS, value: valueText}], 0, 1) + backImgObj.afterCSS;
          }
          function uploadImagesAtCursor(files, srcName, backImgObj) {
            for (var i = 0, file; file = files[i]; i++) {
              var targetPathName =  editor.getStorageFolder(file) + file.name;
              editor.uploadFile(targetPathName, file, (targetPathName, file) => {
                if(backImgObj) {
                  backImgObj.imageSelection = backImgObj.relCSS.value.length == 1 ? 0 : (() => {
                    let radios = document.querySelectorAll(".background-img-radio");
                    let defaultValue = 0;
                    for (let i in radios) {
                      //hopefully there aren't more than 10 images!
                      if (radios[i].checked) defaultValue = Number(radios[i].getAttribute("value").match(/[0-9]/g));
                    }
                    return defaultValue;
                  })();
                  backImgObj.relCSS.value[backImgObj.imageSelection].url = targetPathName;
                  editor.userModifies(() => {
                    clickedElem.setAttribute("style", unparseBackgroundImg(backImgObj));
                  });
                }
                else {
                  let d = document.querySelector("#modify-menu #dom-attr-src");
                  if(d) { d.setAttribute("value", file.name); }
                  editor.userModifies(() => {
                    clickedElem.setAttribute("src", targetPathName);
                  });
                }
                // adapt to HTML5 new attribute 'srcset'
                // IF website use 'srcset', we force to set this attribute to null then replace image using 'src'
                if (clickedElem.getAttribute("srcset") != undefined) {
                  editor.userModifies(() => {
                    clickedElem.setAttribute("srcset", "");
                  });
                }
              });
            }
            // refresh images list
            showListsImages(targetPathName);  // targetPathName is the last file of files array, but it seems that user can only upload one file once
            // automatically select upload image
            let selectedImage = document.querySelectorAll("div#modify-menu .imgFolder > img");
            for (let i = 0; i < selectedImage.length; ++i) {
              let imgName = selectedImage[i].getAttribute("src").split("/").pop();
              if (imgName === files[files.length - 1].name) {
                selectedImage[i].parentElement.classList.add("highlight-select-image");
              } else {
                selectedImage[i].parentElement.classList.remove("highlight-select-image");
              }
            }
          }
          async function showListsImages(srcName, backImgObj, checkBackImgObj, findURLS) {
            if (isAbsolute(srcName)) {
              return;
            }
            console.log("Source name is:", srcName);
            srcName = relativeToAbsolute(srcName)
            let dir = "";
            for(let i = 0, arr = srcName.split(/\\|\//); i < arr.length - 1; ++i) {
              dir += (arr[i] + "/");
            }
            files = await editor.fs.listdir(dir);
            
            let images = [];
            let currentSelectedImage;
            files.forEach(file => {
              let ext = file.split('.').pop().toLowerCase();
              if (ext == 'jpeg' || ext == 'jpg' || ext == 'png' || ext == 'gif' || ext == 'svg' || ext == 'bmp') {
                if (file.split('/').pop() === srcName.split("/").pop().split("?")[0]) {   // note that srcName maybe "/1.jpg?raw=true"
                  currentSelectedImage = file;
                } else {
                  images.push(file);
                }
              }
            });
            // sometimes website use 'srcset' as the url of image, we cannot find currentSelectedImage precisely
            if (currentSelectedImage != null) {
              images.unshift(currentSelectedImage);   // currentSelectedImage should be placed as the first one
            }

            // init: clear image list
            let selectedImage = document.querySelectorAll("#modify-menu .imgFolder");
            selectedImage.forEach(e => e.remove());

            let imgDiv = el("div", { "id": "imgGallery" });
            if (!document.getElementById("imgGallery")) {
              ret.append(imgDiv);
            } else {
              imgDiv = document.getElementById("imgGallery");
            }

            for (let i = 0; i < images.length; ++i) {
              imgDiv.append(
                el("div", { "class": "imgFolder" }, el("img", { "src": dir + images[i], "title": images[i], "alt": images[i] },  [], {}), {
                  onclick() {
                    //console.log("At the beginning:");
                    //console.log(JSON.stringify(backImgObj));
                    // highlight the selected image
                    let otherImages = document.querySelectorAll("#modify-menu .imgFolder");
                    console.log ({otherImages, document});
                    for (let i = 0; i < otherImages.length; ++i) {
                      otherImages[i].classList.remove("highlight-select-image");
                    }
                    console.log ("thru");
                    // replace image
                    if(backImgObj) {
                      backImgObj.imageSelection = (() => {
                        let radios = document.querySelectorAll("#modify-menu .background-img-radio");
                        let defaultValue = 0;
                        for (let i in radios) {
                          //hopefully there aren't more than 10 images!
                          if (radios[i].checked) defaultValue = Number(radios[i].getAttribute("value").match(/[0-9]/g));
                        }
                        return defaultValue;
                      })();
                      //console.log("Here?");
                      //console.log(JSON.stringify(backImgObj));
                      if(!(typeof backImgObj.relCSS.value === 'string')){
                        //console.log("Here?");
                        //console.log(JSON.stringify(backImgObj));
                        //console.log(backImgObj.relCSS.value.length);
                        backImgObj.relCSS.value[backImgObj.imageSelection].url = this.children[0].getAttribute("src");
                      }
                      else {
                        console.log("Second time around:");
                        backImgObj = checkBackImgObj(clickedElem, findURLS);
                        backImgObj.relCSS.value[backImgObj.imageSelection].url = this.children[0].getAttribute("src");
                      }
                      //console.log("current link", this.children[0].getAttribute("src"));
                      //console.log("current section number is:", backImgObj.imageSelection);
                      //console.log("current selection is:", backImgObj.relCSS.value[backImgObj.imageSelection].url); 
                      editor.userModifies(() => {
                        clickedElem.setAttribute("style", unparseBackgroundImg(backImgObj));
                      });
                      //console.log("new style attribute is:", clickedElem.getAttribute("style"));

                      console.log(JSON.stringify(backImgObj));

                    }
                    // adapt to HTML5 new attribute 'srcset'
                    // IF website use 'srcset', we force to set this attribute to null then make image replacemenet
                    else if (clickedElem.getAttribute("srcset") != undefined) {
                      editor.userModifies(() => {
                        clickedElem.setAttribute("srcset", "");
                      });
                    }
                    else {
                      editor.userModifies(() => {
                        clickedElem.setAttribute("src", this.children[0].getAttribute("src"));
                      document.querySelector("#modify-menu #dom-attr-src").setAttribute("value", this.children[0].getAttribute("src"));
                      });
                    }
                    // this.style.outline = "2px solid white";
                    console.log ("pre1");
                    this.classList.add("highlight-select-image");
                    console.log ("post1");
                  }
                })
              );
            }
            if (currentSelectedImage != null) {
              console.log ("pre2");
              ret.querySelectorAll(".imgFolder")[0].classList.add("highlight-select-image");
              console.log ("post2");
            }
          
          }
          let srcName = backgroundImgSrc ? backgroundImgSrc.relCSS.value[0].url : clickedElem.getAttribute("src");

          //console.log(srcName);
          //console.log(backgroundImgSrc.relCSS.value[0].url);
          clickedElem.ondragover = function (e) {
            e.preventDefault();
          }
          clickedElem.ondrop = function (e) {
            // upload and replace the image 
            e.stopPropagation();
            e.preventDefault();
            var files = e.dataTransfer.files; // FileList object
            if (files && files[0]) {
              uploadImagesAtCursor(files, srcName, backgroundImgSrc);
            }
          }
          // radio buttons for cases when there are two background images
          if(backgroundImgSrc && backgroundImgSrc.relCSS.value.length > 1) {
            for(let i in backgroundImgSrc.relCSS.value) {
              ret.append(el("span", {class: "insertOption"}, [
                el("input", {type: "radio", class: "background-img-radio", id: `radio${i}`, name: "", value: `Image {i}`}, [], {checked: Number(i) === 0}),
                el("label", {"for": "radio${i}"}, `Image {i}`)]),);
            }         
          }
          // upload image button
          ret.append(
            el("a", 
              { "id": "upload-image-btn-a" }, 
              el(
                "input", {"id": "upload-image-btn-input", "type": "file", value: "Please upload images..."}, 
                [], 
                { onchange: function(evt) { uploadImagesAtCursor(evt.target.files, srcName, backgroundImgSrc); }}
              ), 
              {}
            )
          );
          if(srcName == undefined) {
            ret.append(
              el("button", {}, "Add src attribute", {onclick: () => {
                editor.userModifies(() => {
                  clickedElem.setAttribute("src", "");
                });
                editor.ui.refresh();
              }}));
          } else {
            showListsImages(srcName, backgroundImgSrc, this.checkForBackgroundImg, this.findURLS);
            // show lists of images in selected image's folder
          }
          return ret;
        }
      });
      editor.ui.model.interfaces.push({
        title: "Text Editing",
        minimized: true,
        priority(editor_model) {
          return undefined;
        },
        enabled(editor_model) {
          const clickedElem = editor_model.clickedElem;
          if(!clickedElem) return false;
          for(let i = 0; i < clickedElem.childNodes.length; i++) {
            let node = clickedElem.childNodes[i];
            if(node.nodeType === 3 && node.textContent.trim() !== "") {
              return true;
            }
          }
        },
        render: function render(editor_model, innerBox) {
          if(!this.enabled(editor_model)) {
            delete editor.ui._internals.saveBetweenReloads["TextEditing"];
            return "Click on an element that contains some text.";
          }
          const clickedElem = editor_model.clickedElem;
          //textarea textChildNodeContent
          let ret = el("div", {id: "textChildNodeContentDiv"}, []);
          for(let i = 0; i < clickedElem.childNodes.length; i++) {
            let node = clickedElem.childNodes[i];
            if(node.nodeType === 3 && (node.textContent.trim() !== "" || node.textContent.trim() === "" && clickedElem.childNodes.length == 0)) { // Non-empty text nodes.
              let txtAreaNode = el("textarea", {class:"textChildNodeContent"},
                [], {
                  value: node.textContent,
                  oninput: (node => function() {
                    editor._internals.ifIdleAfter(200, "textContent", () => {
                    editor.userModifies(() => { node.textContent = this.value;}); 
                    });
                  })(node)
                });
              ret.append(txtAreaNode)
              console.log("appending text area node", txtAreaNode);
            } else if(node.nodeType === 1) { // Make this a shortcut for the node
              ret.append(
                el("div.childrenSelector", {}, 
                  el("div.childrenSelectorName", {}, "<" + node.tagName + ">"),
                  {
                    onclick: (node => () => {
                      editor_model.clickedElem = node;
                      editor.ui.refresh();
                    })(node)
                  }
                )
              )
            }
          }
          
          if("TextEditing" in editor_model.restoredAfterReload) {
            let restored = editor_model.restoredAfterReload["TextEditing"];
            //
            console.log("restored", restored);
            console.log("ret.childNodes", ret.childNodes);
            setTimeout((ret => () => {
              var tmp = ret;
              while(tmp && tmp.tagName != "BODY") tmp = tmp.parentNode;
              if(!tmp) return;
              console.log("ret", ret);
              for(let i = 0; i < restored.length && i < ret.childNodes.length; i++) {
                var child = ret.childNodes[i];
                if(child.tagName === "TEXTAREA") {
                  console.log("restoring selection on ", child);
                  console.log("data ", restored[i]);
                  child.scrollTop = restored[i].scrollTop;
                  var minimum = Math.min(restored[i].selectionStart, restored[i].selectionEnd);
                  var maximum = Math.max(restored[i].selectionStart, restored[i].selectionEnd);
                  var direction = restored[i].selectionStart < restored[i].selectionEnd ? "forward" : "backward";
                  if(restored[i].focus) {
                    child.focus();
                  }
                  child.setSelectionRange(minimum, maximum, direction);
                }
              }
              delete editor_model.restoredAfterReload["TextEditing"];
            })(ret), 0);
          } else {
            console.log("No restoration data");
          }
          
          editor.ui._internals.saveBetweenReloads["TextEditing"] = (ret => () => {
            let res = [];
            for(let i = 0; i < ret.childNodes.length; i++) {
              if(ret.childNodes[i].tagName === "TEXTAREA") {
                res[i] = {
                  scrollTop: ret.childNodes[i].scrollTop,
                  selectionEnd: ret.childNodes[i].selectionStart,
                  selectionStart: ret.childNodes[i].selectionEnd,
                  focus: ret.childNodes[i] === document.activeElement
                };
              }
            }
            return res;
          })(ret);
          
          return ret;
        }
      });
      editor.ui.model.interfaces.push({
        title: "Create",
        minimized: true,
        priority(editor_model) {
          return editor_model.insertElement ? 1 : undefined;
        },
        enabled(editor_model) {
          return editor_model.clickedElem;
        },
        render: function render(editor_model, innerBox) {
          if(!this.enabled(editor_model)) {
            return "Click on an element to view insert options.";
          }
          let ret = el("div", {"class": "information"});
          const clickedElem = editor_model.clickedElem;
          if (!clickedElem) return ret;
          ret.classList.add("insert-information-style");
          ret.classList.add("information-style");
          let insertOption = function(value, msg, checked, title) {
            return el("span", {class: "insertOption"}, [
              el("input", {type: "radio", id: "radioInsert" + value, name: "insertionPlace", value: value}, [], {checked: checked || false}),
              el("label", {"for": "radioInsert" + value, title: title}, msg)], {onclick: restoreCaretPosition});
          }
          let t = clickedElem.tagName;
          let isHTML = t === "HTML";
          let isTop = isHTML || t === "BODY" || t === "HEAD";
          let caretBlinks = editor_model.caretPosition;
          ret.append(el("div", {id: "insertionPlace"}, [
            isTop ? undefined : insertOption("before", "Before node"),
            isHTML ? undefined : insertOption("first-child", "As first child"),
            isHTML || !caretBlinks ? undefined : insertOption("caret", "At caret", !isTop && caretBlinks),
            isHTML ? undefined : insertOption("last-child", "As last child", isTop || !caretBlinks),
            isTop ? undefined : insertOption("after", "After node"),
            isTop ? undefined : insertOption("wrap", "Wrap node", false, "Put the selected node inside the newly inserted node"),
            clickedElem.childNodes && clickedElem.childNodes.length ? insertOption("wrap-children", "Wrap children", false, "Insert all node's children as children of element, then add element as a child.") : undefined
          ]));
          let getInsertionPlace = () => {
            let radios = document.querySelectorAll('#insertionPlace input[name=insertionPlace]');
            let value = "after";
            for (let i = 0, length = radios.length; i < length; i++) {
              if (radios[i].checked) return radios[i].getAttribute("value");
              value = radios[i].getAttribute("value");
            }
            return value;
          };
          let insertTag = function(event, newElement, insertionStyle) {
            newElement = newElement || (() => {
              let parent = this;
              while(parent && !parent.classList.contains("tagName")) parent = parent.parentElement;
              let m = parent.querySelector(".templateengine");
              if(typeof m.innerHTMLCreate === "string") return m.innerHTMLCreate;
              return el(m.createParams.tag, m.createParams.attrs, m.createParams.children, m.createParams.props);
            })();
            editor.userModifies(() => {
              if(insertionStyle === "after") {
                if(typeof newElement === "string") {
                  clickedElem.insertAdjacentHTML("afterend", newElement);
                  newElement = clickedElem.nextElementSibling;
                } else {
                  clickedElem.parentElement.insertBefore(newElement, clickedElem.nextSibling);
                }
              } else if(insertionStyle === "before") {
                if(typeof newElement === "string") {
                  clickedElem.insertAdjacentHTML("beforebegin", newElement);
                  newElement = clickedElem.previousElementSibling;
                } else {
                  clickedElem.parentElement.insertBefore(newElement, clickedElem);
                }
              } else if(insertionStyle === "wrap") {
                if(typeof newElement === "string") {
                  clickedElem.insertAdjacentHTML("beforebegin", newElement);
                  newElement = clickedElem.previousElementSibling;
                } else {
                  clickedElem.parentElement.insertBefore(newElement, clickedElem);
                }
                newElement.appendChild(clickedElem);
                console.log("newElement's parent HTML", newElement.parentElement.outerHTML);
              } else if(insertionStyle === "wrap-children") {
                if(typeof newElement === "string") {
                  clickedElem.insertAdjacentHTML("afterbegin", newElement);
                  newElement = clickedElem.children[0];
                } else {
                  clickedElem.insertBefore(newElement, clickedElem.childNodes[0]);
                }
                while(newElement.nextSibling) {
                  newElement.append(newElement.nextSibling);
                }
              } else if(insertionStyle === "caret") {
                let s = editor_model.caretPosition;
                let txt = s.startContainer;
                if(txt.textContent.length > s.startOffset && s.startOffset > 0) { // split
                  // Need to split the text node.
                  txt.parentElement.insertBefore(document.createTextNode(txt.textContent.substring(s.startOffset)), txt.nextSibling);
                  txt.textContent = txt.textContent.substring(0, s.startOffset);
                }
                if(typeof newElement === "string") {
                  let tmpSpan = el("span");
                  txt.parentElement.insertBefore(tmpSpan, txt.nextSibling)
                  tmpSpan.insertAdjacentHTML("afterend", newElement);
                  newElement = tmpSpan.nextElementSibling;
                  tmpSpan.remove();
                } else {
                  txt.parentElement.insertBefore(newElement, txt.nextSibling)
                }
              } else if(insertionStyle === "last-child") { // Insert at the end of the selected element, inside.
                if(typeof newElement === "string") {
                  let tmpSpan = el("span");
                  clickedElem.insertBefore(tmpSpan, null);
                  tmpSpan.insertAdjacentHTML("afterend", newElement); // afterend or beforeend same, tmpSpan to be removed.
                  newElement = tmpSpan.nextElementSibling;
                  tmpSpan.remove();
                } else {
                  console.log("insert at the end");
                  // Insert at the end.
                  clickedElem.insertBefore(newElement, null);
                }
              } else if(insertionStyle === "first-child") { // Insert at the end of the selected element, inside.
                if(typeof newElement === "string") {
                  let tmpSpan = el("span");
                  clickedElem.insertBefore(tmpSpan, clickedElem.children[0]);
                  tmpSpan.insertAdjacentHTML("afterend", newElement);// afterend or beforeend same, tmpSpan to be removed.
                  newElement = tmpSpan.nextElementSibling;
                  tmpSpan.remove();
                } else {
                  console.log("insert at the beginning");
                  // Insert at the beginning.
                  clickedElem.prepend(newElement);
                }
              }
            });
            editor_model.insertElement = false;
            editor_model.visible = true;
            editor_model.clickedElem  = typeof newElement !== "string" && typeof newElement !== "undefined" ?
              newElement : clickedElem;
            editor.ui.refresh();
          }
          let addElem = function(name, createParams) {
            ret.append(
              el("div", {"class": "tagName", title: createParams.title},
                el("span", { "class": "templateengine"}, name, {createParams: createParams}), {
                    onclick: function(event) {
                      let insertionStyle = getInsertionPlace();
                      insertTag.call(this, event, undefined, insertionStyle);
                  }}
              )
            );
          }
          if(clickedElem.tagName === "HEAD") {
            addElem("Title", {tag:"title", children: "Page_title", title: "Insert <title>"});
            addElem("Meta", {tag:"meta", attrs:{name:"", content: ""}, props: {}, title: "Insert <meta>"});
            addElem("Link", {tag:"link", attrs:{rel:"", href: ""}, props: {}, title: "Insert <link>"});
          }
          if(clickedElem.tagName !== "HEAD") {
            ret.append(el("input", {"type": "file", multiple: "", value: "Images or files..."}, [], {
              onchange: function(evt) { editor.uploadFilesAtCursor(evt.target.files); }})
            );
            ret.append(
              el("div", {"class":"modify-menu-icon", id: "selectExistingNodeToMove", title: "Select an existing node to move"}, [], {
                  innerHTML: editor.ui.icons.linkMode + "<span>Move node</span>",
                  onclick: function(event) {
                    editor_model.insertElement = false;
                    let insertionStyle = getInsertionPlace();
                    activateNodeSelectionMode(
                      "to move",
                      node => insertTag.call(this, event, node, insertionStyle),
                      addPinnedModifyMenuIcon => {
                        addPinnedModifyMenuIcon(editor.ui.icons.clone + "<span class='modify-menu-icon-label-link'>Clone</span>", 
                          {"class": "link-select-button", title: "Confirm to clone",
                            id: "selectbutton"
                          },
                          {onclick: function(event) {
                            let node = editor_model.clickedElem;
                            let clonedNode = editor.duplicate(node, {ignoreText: true});
                            insertTag.call(this, event, clonedNode, insertionStyle);
                            escapeLinkMode();
                            editor_model.clickedElem = clonedNode;
                            }
                          }
                        );
                      }
                    )
                  }
                })
            )
            // TODO: Filter and sort which one we can add, also depending on where to insert.
            addElem("List item", {tag:"li", props: { innerHTML: "<br>" }, title: "Insert <li>"});
            addElem("Bulleted list", {tag:"ul", props: { innerHTML: "<ul>\n<li><br></li>\n</ul>" }, title: "Insert <ul>"});
            addElem("Numbered list", {tag:"ol", props: { innerHTML: "<ol>\n<li><br></li>\n</ol>" }, title: "Insert <ol>"});
            addElem("Button", {tag: "button", props: {innerHTML: "Button name" }, title: "Insert <button>"});
            addElem("Link", {tag: "a", props: { innerHTML: "Link name", href: "" }, title: "Insert <a href=''>"});
            addElem("Paragraph", {tag: "p", props: { innerHTML: "Your text here" }, title: "Insert <p>"});
            addElem("Division content", {tag: "div", title: "Insert <div>"});
            addElem("Section", {tag: "section", title: "Insert <section>"});
            addElem("Image", {tag: "img", title: "Insert <img>", attrs: {src: ""}});
            addElem("Preformatted text", {tag: "pre", title: "Insert <pre>"});
            for(let i = 1; i <= 6; i++) {
              addElem("Header " + i, {tag:"h" + i, props: { innerHTML: "Title" + i }, title: "Insert <h"+i+">"});
            }
            addElem("Newline", {tag: "br", title: "Insert <br>"});
          }
          addElem("Stylesheet", {tag:"style", children: "/*Your CSS there*/", title: "Insert <style>"});
          addElem("JavaScript", {tag:"script", children: "/*Your CSS below*/", title: "Insert <script>"});

          ret.append(
            el("div", {"class": "tagName", id: "customHTML"}, [
              el("textarea", {id: "customHTMLToInsert", placeholder: "Custom HTML here...", "class": "templateengine", oninput: "this.innerHTMLCreate = this.value"}),
              el("div", {"class":"modify-menu-icon", title: "Insert HTML", style: "display: inline-block"}, [], {
                  innerHTML: editor.ui.icons.plus, 
                  onclick: function(event) {
                      let insertionStyle = getInsertionPlace();
                      insertTag.call(this, event, undefined, insertionStyle);
                  }
                }
              )
            ])
          );
          //document.querySelector("#modify-menu").classList.toggle("visible", true);
          return ret;
        }
      });
      if (editor.config.thaditor) {
        editor.ui.model.interfaces.push({
          title: "Drafts",
          minimized: true,
          priority(editor_model) {
            return undefined;
          },
          enabled(editor_model) {
            return true;
          },
          render: (editor_model, innerBox) => {
            
            let draftListDiv = el("div", {"class":"draftList"}, [], {});

            (async () => {
            const verzExist = JSON.parse(await editor.getServer("isdir", "Thaditor/versions"));

            const get_switch_btn_for = (nm) => {
              return el("button", {"class":"draft-switch", title: "Open version '" + nm + "'"}, [nm], 
              {
                onclick: (event) => {
                  editor_model.version = nm;
                  editor.navigateTo("/Thaditor/versions/" + nm + "/" + mbEditQ);
                  setTimeout(() => editor.ui.sendNotification("Switched to " + nm), 2000);
                }
              });
            };

            const get_switch_btn_live = () => {
              return el("button", {class:"draft-switch-live draft-switch"}, ["Open live website"],
              {
                onclick: (event) => {
                  editor_model.version = "Live";
                  editor.navigateTo("/" + mbEditQ);
                  setTimeout(() => editor.ui.sendNotification("Switched to Live version"), 2000);
                }
              })
            }

            const get_clone_btn_for = (nm) => {
              return el("button", {"class":"draft-clone", title: "Clone " + nm + " to a new version"}, ["Clone"],
              {
                onclick: (event) => {
                  cloneSite(nm, verzExist); //confirms + sends notif inside method
                }
              })  
            }

            const get_delete_btn_for = (nm) => {
              return el("button", {"class":"draft-delete", title: "Delete version " + nm}, ["Delete"],
              {
                onclick: (event) => {
                  deleteDraft(nm); //confirms + sends notif inside the method
                }
              })  
            }

            const get_rename_btn_for = (nm) => {
              return el("button", {"class":"draft-publish", title: "Rename " + nm}, ["Rename"],
              {
                onclick: (event) => { 
                  renameDraft(nm, verzExist); //confirms + sends notif inside
                }
              })
            }

            const get_publish_btn_for = (nm) => {
              return el("button", {"class":"draft-publish", title: "Publish " + nm + " to live"}, ["Publish"],
              {
                onclick: (event) => { 
                  publishDraft(nm); //confirms + sends notif inside
                }
              })
            };

            const get_current_label = () => {
              return el("div", {"class":"draft-row", "id": "draft-title"},
                      [
                        el("label", {}, [editor_model.version], {}),
                        (isLive() ? el("label", {}, [""]) : get_rename_btn_for(editor_model.version)),
                        get_clone_btn_for(editor_model.version),
                        (isLive() ? el("label", {}, ["Can't delete live"]):
                                                          el("button", {}, ["Delete"],
                                                          {
                                                            onclick: (event) => {
                                                              deleteDraft(editor_model.version);
                                                            }
                                                          })),

                      ],
                      {
                        onclick: (event) => {
                          //pass
                        },
                      })
            };

            const get_current_label_live = () => {
              return el("div", {"class":"draft-row", "id": "draft-title"},
                      [
                        el("label", {style:"font-style:italic"}, ["Currently viewing live website"], {}),
                        get_clone_btn_for("Live"),
                        

                      ],
                      {
                        onclick: (event) => {
                          //pass
                        },
                      })
            };

            const get_current_label_for = (nm) => {
              return el("div", {"class":"draft-row", "id": "draft-title"},
                      [
                        el("label", {title: "Currently viewing " + nm + " version"}, [nm], {}),
                        get_rename_btn_for(editor_model.version),
                        get_clone_btn_for(nm),
                        get_delete_btn_for(nm),
                        get_publish_btn_for(nm),
                      ],
                      {
                        onclick: (event) => {
                          //pass
                        },
                      })
            };
            

            const get_row_for_draft = (nm) => {
              return el("div", {"class": "draft-row"},
              [
                get_switch_btn_for(nm),
                get_rename_btn_for(nm),
                get_clone_btn_for(nm),
                get_delete_btn_for(nm),
              ]);
            };

            const get_row_for_live = () => {
              return el("div", {"class": "draft-row", "id": "draft-row-live"},
              [
                get_switch_btn_live(),
                get_clone_btn_for("Live")
              ])
            }

            if (isLive()) {
              draftListDiv.append(get_current_label_live());
            } else {
              draftListDiv.append(get_current_label_for(editor_model.version));
              draftListDiv.append(get_row_for_live());
            }
            if (verzExist) {
              const vers = JSON.parse(await editor.getServer("listdir", "Thaditor/versions/"));
              vers.forEach(ver => {
                if (!(ver == editor_model.version)){
                  draftListDiv.append(get_row_for_draft(ver));
                }
              });
            }
            })();
            return draftListDiv;
          }
        });
      } // End of if apache_server
      
      editor.ui.model.interfaces.push({
        title: "SEO",
        minimized: true,
        priority(editor_model) {
          if(!document.querySelector("meta[name=viewport]")) {
            return 1;
          }
          return undefined;
        },
        enabled(editor_model) {
          return true;
        },
        render: function render(editor_model, innerBox) {
          function oneClickFix(msg, buttonName, callback, parameters) {
            return el("div", {class:"seo-fix"}, [
              el("p", {class:"seo-fix-description"}, msg),
              parameters,
              el("button.action-button", {type: ""}, buttonName, {
                onclick: function() {
                  callback();
                  editor.ui.refresh();
                }
              })]);
          }
          let title = document.querySelector("head > title");
          let description = document.querySelector("head > meta[name=description]")
          let ret = el("div", {}, [
            document.querySelector("head > meta[name=viewport]") ? undefined :
            oneClickFix("Viewport not set on this page. This might make this page not display properly on mobile devices.",
              "Add missing <meta name='viewport'...>", () => 
                editor.userModifies(() => 
                  document.head.appendChild(el("meta", {name: "viewport", content:"width=device-width, initial-scale=1.0"})))),
            document.querySelector("head > meta[charset]") ? undefined :
            oneClickFix("Character encoding not set on this page. The display of non-breaking spaces would be compromized on many browsers.", "Add missing <meta charset='UTF-8'>", () =>
                  editor.userModifies(() => {
                  document.head.insertBefore(el("meta", {charset: "UTF-8" }), document.head.childNodes[0])})),
            el("div", {class:"seo-fix"}, [
              el("p", {class:"seo-fix-description"}, !title ?
                "Page title not set. Search engines do prefer a title." :
                "Title of the page:"
              ),
              el("input", {type:"text", value: title ? title.textContent : "", placeholder: "Title of the page"}, [], {
                onchange: function() {
                  editor.userModifies(() => {
                    if(!title) {
                      title = el("title");
                      document.head.appendChild(title);
                    }
                    title.textContent = this.value;
                  });
                }
              })
            ]),
            el("div", {class:"seo-fix"}, [
              el("p", {class:"seo-fix-description"}, !description ?
                "Page description not set. Search engines do prefer a page description to show on their results." :
                "Description of the page:"
              ),
              el("textarea", {type:"text", class: "textChildNodeContent", placeholder: "Description of the page"}, [], {
                oninput: function() {
                  editor._internals.ifIdleAfter(200, "meta.description", () => 
                  editor.userModifies(() => {
                    if(!description) {
                      description = el("meta", {name: "description"});
                      document.head.appendChild(description);
                    }
                    description.setAttribute("content", this.value);
                  }));
                },
                value: description ? description.getAttribute("content") || "" : ""
              })
            ]),
          ]);
          return ret;
        }
      });
      editor.ui.model.interfaces.push({ 
        title: "Advanced",
        minimized: true,
        priority(editor_model) {
          return editor_model.disambiguationMenu ? 0 : undefined;
        },
        enabled(editor_model) {
          return true;
        },
        render: function render(editor_model, innerBox) {
          let retDiv = el("div", {"class":"modify-menu-icons"});
          //We need 3 btns: refresh, filesystem + help.
          add_btn_to_div(retDiv, editor.ui.icons.reload,
            {"class": "tagName", title: "Reload the current page"},
              {onclick: function(event) {
                if(editor.confirmLeaving()) {
                  editor.reload();
                }
              } }
            );
          add_btn_to_div(retDiv, editor.ui.icons.folder,
            {"class": "tagName", title: "List files in current directory"},
              {onclick: function(event) {
                let u =  new URL(location.href);
                u.pathname = u.pathname.replace(/[^\/]*$/, "");
                u.searchParams.set("ls", "true");
                if(editor.confirmLeaving()) {
                  editor.navigateTo(u.href);
                }
              }
            }
          );
          if(editor.config.thaditor) {
            retDiv.append(
              el("button.action-button#update-thaditor-btn", {type: ""}, "Update Thaditor", {onclick() {
                if(confirm("Are you ready to upgrade Thaditor?")) {
                  editor._internals.doWriteServer("updateversion", "latest", "", response => {
                    console.log("Result from Updating Thaditor to latest:");
                    console.log(response);
                    if(response.endsWith("Done.")) {
                      location.reload(true);
                    } else {
                      alert("Update failed. Please navigate to /ThaditorInstaller.php and re-install Thaditor. You will not loose any drafts.");
                    }
                  });
                }
              } })
            );
          }
          retDiv.append(
            el("label", {class:"switch", title: "If off, ambiguities are resolved automatically. Does not apply for HTML pages"},
              [el("input", {class: "global-setting", id: "input-question", type: "checkbox"}, [], {
                onchange: function() { editor_model.askQuestions = this.checked; },
                checked: editor_model.askQuestions}),
              el("span", {class:"slider round"})]));
          retDiv.append(
            el("label", {"for": "input-question", class: "label-checkbox"}, "Ask questions"));
          
          retDiv.append(
            el("label", {class:"switch", title: "If on, changes are automatically propagated 1 second after the last edit"}, [
              el("input", {class: "global-setting", id: "input-autosave", type:"checkbox"}, [], {
                onchange: function() { editor_model.autosave = this.checked; },
              checked: editor_model.autosave}),
              el("span", {class:"slider round"})])
          );
          retDiv.append(
            el("label", {"for": "input-autosave", class: "label-checkbox"}, "Auto-save"));
          
          if(editor.config.thaditor) {
            retDiv.append(
              el("a", {href:"javascript:0", id:"thaditor-sign-out-button", style:"display:block"}, "Sign out of Google", {
                onclick() {
                  let onOk = () => thaditor_sign_out(() => {
                    retDiv.append(
                      el("a", {href:"javascript:0", id:"thaditor-google-log-in-button", style:"display:block"}, "Sign in with Google",
                      {onclick: thaditor_sign_in()}));
                    document.querySelector("#thaditor-sign-out-button").remove();
                  });
                  if(!gapi.auth2) {
                    thaditor_gapi_onload(onOk);
                  } else {
                    onOk();
                  }
                }})
            );
          }
          if(editor_model.disambiguationMenu) {
            retDiv.append(editor_model.disambiguationMenu);
          }
          return retDiv;
        }
      });
      
      editor.ui.model.interfaces.push({
        title: "Log",
        minimized: true,
        priority(editor_model) {
          return undefined;
        },
        enabled(editor_model) {
          return true;
        },
        currentBox: undefined,
        render: function render(editor_model, innerBox) {
          let retDiv = el("div#fullLog", {"class":"modify-menu-icons"});
          let logtxt = "";
          const elog = editor_model.editor_log;
          for (let i = 0; i < elog.length; i++) {
            const l = elog[i];
            logtxt = logtxt + (i > 0 ? "<br>" : "") + l;
          }
          retDiv.innerHTML = logtxt;
          this.currentBox = retDiv;
          return retDiv;
        },
        refresh() {
          let currentBox = this.currentBox;
          let newBox = this.render(editor.ui.model);
          currentBox.parentNode.insertBefore(newBox, currentBox);
          currentBox.remove();
        }
      });
      if(typeof thaditor !== "undefined" && thaditor.customInterfaces) {
        editor.ui.model.interfaces.push(...thaditor.customInterfaces);
      }
    }
    
    function getEditorInterfaceByTitle(title) {
      return editor.ui.model.interfaces.find(x => x.title == title);
    }
 
    // First time: We add the interface containers.
    if(!ifAlreadyRunning) {
      init_interfaces();
    }
    
    //if no ID, tag/name, or class (if h1, h2, etc..., probably fine)
    //split between common (section, div, span, p, ul,  etc...) and rare/better semantically defined tags (pre)
    //check if selector applies to any ancestors or descendants, then its ok
    //else add class or use > selector until it is precise 
    function getShortestUniqueSelector(clickedElem) {
      console.log("clickedElem", clickedElem);
      let curSelector = getSelectorOf(clickedElem);
      //checking ancestors
      let consideredParent = clickedElem.parentNode;
      do {
        var selectorIsOrg = true;
        for(let curAncestor = clickedElem.parentNode; curAncestor; curAncestor = curAncestor.parentNode) {
          if(editor.matches(curAncestor, curSelector)) {
            selectorIsOrg = false;
          }
        }
        //checking descendants
        if(clickedElem.querySelector(curSelector)) {
          selectorIsOrg = false;
        }
        if(!selectorIsOrg) {
          curSelector =  consideredParent.tagName.toLowerCase() + " > " + curSelector; 
          consideredParent = consideredParent.parentNode;
        }
      } while(!selectorIsOrg && consideredParent);
      return curSelector;
    }
    

    function reorderCompatible(node1, node2){
      let topLevelOrderableTags = {TABLE:1, P:1, LI:1, UL:1, OL:1, H1:1, H2:1, H3:1, H4:1, H5:1, H6:1, DIV:1, SECTION: 1, IMG: 1, PRE: 1};
      let metaOrderableTags = {META:1, TITLE:1, SCRIPT: 1, LINK: 1, STYLE: 1};
      return node1.tagName === node2.tagName && node1.tagName !== "TD" && node1.tagName !== "TH" ||
        topLevelOrderableTags[node1.tagName] && topLevelOrderableTags[node2.tagName] ||
        metaOrderableTags[node1.tagName] && metaOrderableTags[node2.tagName];
    }
    function preventTextDeselection(e){
      e = e || window.event;
      e.preventDefault();
    }
    function restoreCaretPosition() {
      if(typeof editor.ui.model.caretPosition != "undefined") {
        var sel = window.getSelection();
        sel.removeAllRanges();
        var range = document.createRange();
        range.setStart(editor.ui.model.caretPosition.startContainer, editor.ui.model.caretPosition.startOffset);
        range.setEnd(editor.ui.model.caretPosition.endContainer, editor.ui.model.caretPosition.endOffset);
        sel.addRange(range);
      }
    }
    // This function activates the node selection mode, in which one DOM node can be selected,
    // After clicking on confirm, the callback is called with the selected node.
    // callbackUI is invoked to render other buttons along with the confirmation button.
    function activateNodeSelectionMode(msg, callback, callbackUI) {
      editor.ui.model.visible = false;
      
      editor.ui.model.linkSelectMode = true;
      editor.ui.model.clickedElem = document.body; //"center" clicked element on document body
      //removes all context menu stuff 
      document.querySelector("#context-menu").classList.remove("visible");
      editor.ui.model.linkSelectCallback = callback;
      editor.ui.model.linkSelectMsg = "Confirm " + msg;
      editor.ui.model.linkSelectOtherMenus = callbackUI;
      editor.ui.refresh();
      editor.ui.sendNotification(editor.ui.model.linkSelectMsg);
      document.body.addEventListener('mouseover', linkModeHover1, false);
      document.body.addEventListener('mouseout', linkModeHover2, false);
    }

    

    function copy_website(source, dest) {
      let website_files = JSON.parse(editor._internals.doReadServer("fullListDir", source));
      let is_dest_valid = editor._internals.doReadServer("isdir", dest)
      if (!website_files) throw "copy_website(): invalid source";
      if (!is_dest_valid) throw "copy_website(): invalid dest";
      
      //filter out Thaditor files
      website_files = website_files.filter(val => !thaditor_files.includes(val[0]));
      website_files = website_files.filter(val => val[0][0] != ".");
      //cpy website_files to to dest
      website_files.forEach(val => {
        let [nm, isdir] = val;
        const s = (source + nm);
        const d = (dest + nm);
        if (isdir) {
          editor._internals.doWriteServer("fullCopy", s, d);
        } else {
          editor._internals.doWriteServer("copy", d, s);
        }
      });
      let dh = editor._internals.doReadServer("read", source + "/.thaditor_meta");
      let draft_history = (dh == "" ? undefined : JSON.parse(dh));
      const get_date_meta = () => (new Date).toString();
      if (draft_history == undefined) {
        draft_history = ["live:" + get_date_meta()];
      } else {
        draft_history.push(editor.ui.model.version + ":" + get_date_meta());
      }
      editor._internals.doWriteServer("write", dest + "/.thaditor_meta", JSON.stringify(draft_history));
      return 1;
    }
    
    function deleteDraftDef(nm) { //definitely delete the draft, without a prompt
      //the path of the folder we want to delete is and always will be Thaditor/versions/$nm/
      const pth_to_delete = "Thaditor/versions/" + nm + "/";
      //here we want to hand editor._internals.doWriteServer to the worker in editor.js

      const data = {action:"drafts",
                    subaction:"deletermrf",
                    pth_to_delete:pth_to_delete,
                    nm:nm, thaditor_files:thaditor_files, version:editor.ui.model.version};
      if (editor.ui.model.version == nm) {
        editor._internals.doWriteServer("deletermrf", pth_to_delete);
        editor.navigateTo("/" + mbEditQ);
        editor.ui.refresh();
      } else {
        thaditor.do(data).then(data => {
          editor.ui.sendNotification("Permanently deleted draft named: " + data.nm);
          editor.ui.refresh()});
      }
    }

    function deleteDraft(nm) {
      if (nm == "Live") throw "Shouldn't be able to call deleteDraft on live";
      const ans = window.confirm("Are you sure you want to permanently delete " + nm + "?");
      if (!ans) return;
      deleteDraftDef(nm);
    }


    function getNewDraftName(nm, verzExist) {
      const draft_name = window.prompt ("Please provide the name for the new draft. Leave blank to cancel");
      if (!draft_name) {
        return 0;
      }
      
      let is_draft_name_valid = (nm) => {
        return !(nm.startsWith("[^a-zA-Z0-9]"));
      };

      if (!is_draft_name_valid(draft_name)) {
        window.alert("Invalid draft name");
        return 0;
      }
      
      let fail = false;
      if (!verzExist) {
        editor._internals.doWriteServer("mkdir", "Thaditor/versions");
      } else {
        let versionsList = JSON.parse(editor._internals.doReadServer("fullListDir", "Thaditor/versions/"));
        versionsList.forEach(val => {
          let [nm, isdir] = val;
          if (isdir) {
            if (nm == draft_name) {
              fail = window.confirm("Overwrite existing draft?");
            }
          }
        });
      }
      if (fail) return 0;
      return draft_name;
    }


    function cloneSite(nm, verzExist) {
      //verzExist tells us if we need to mkdir versions
      //nm could be live or any draft ==> make f_pth
      const draft_name = getNewDraftName(nm, verzExist);
      if (!draft_name) return 0;
      //all of that above ^^ needs to happen in the UI thread.
      const t_pth = "Thaditor/versions/" + draft_name + "/"
      const f_pth = (nm == "Live" ? "" : "Thaditor/versions/" + nm + "/");
      const data = {action:"drafts", subaction:"clone",
                    draft_name:draft_name,
                    t_pth:t_pth, f_pth:f_pth,
                    nm:nm,thaditor_files:thaditor_files,version:editor.ui.model.version};
      editor.ui.sendNotification("Creating draft " + draft_name + " from " + nm);
      thaditor.do(data).then(data => {
        //just send a notif, no more naving to the clone
        editor.ui.refresh();
        editor.ui.sendNotification("Successfully cloned " + data.nm + " to " + data.draft_name);
      });
    }
    
    function renameDraft(nm, verzExist) {
      //verzExist tells us if we need to mkdir versions
      //nm could be live or any draft ==> make f_pth
      const draft_name = getNewDraftName(nm, verzExist);
      if (!draft_name) return 0;
      //all of that above ^^ needs to happen in the UI thread.
      const t_pth = "Thaditor/versions/" + draft_name + "/"
      const f_pth = (nm == "Live" ? "" : "Thaditor/versions/" + nm + "/");
      const data = {action:"drafts", subaction:"rename",
                    draft_name:draft_name,
                    t_pth:t_pth, f_pth:f_pth,
                    nm:nm,thaditor_files:thaditor_files,version:editor.ui.model.version};
      editor.ui.sendNotification("Renaming draft " + nm + " to " + draft_name);
      thaditor.do(data).then(data => {
        let marker = false;
        if (data.nm == data.version) {
          editor.navigateTo("/Thaditor/versions/" + data.draft_name + "/" + mbEditQ);
          marker = true;
        }
        if(marker) {
          setTimeout(editor.ui.sendNotification("Successfully renamed " + data.nm + " to " + data.draft_name), 2000)
        } else {
          editor.ui.refresh();
          editor.ui.sendNotification("Successfully renamed " + data.nm + " to " + data.draft_name);
        }
      });
    }

    function publishDraft(nm) {
      //We're copying out Thaditor/versions/$nm/ to "".
      if (nm == "Live") throw "Can't publish live to live";
      const conf = window.confirm("Are you sure you want to publish " + nm + " to live?");
      if (!conf) {
        return;
      }
      let t_src = "Thaditor/versions/" + nm + "/";
      const data = {action:"drafts",
                    subaction:"publish",
                    t_src:t_src,
                    nm:nm,thaditor_files:thaditor_files,
                    version:editor.ui.model.version};
      thaditor.do(data).then(data => {
        editor.ui.sendNotification("Successfully published " + data.nm + " to live.");
      });
    }
    
    editor.ui.refresh = function refresh() {
      const menuholder = document.querySelector("#modify-menu-holder");
      const old_scroll = menuholder ? menuholder.scrollTop : 0;
      
      // Set up
      let editor_model = editor.ui.model;
      var clickedElem = editor_model.clickedElem;
      var contextMenu = document.querySelector("#context-menu");
      var modifyMenuDiv = document.querySelector("#modify-menu");
      
      if(!modifyMenuDiv || !contextMenu) { // After some reloading, none of them might exist.
        editor.ui.init();
        contextMenu = document.querySelector("#context-menu");
        modifyMenuDiv = document.querySelector("#modify-menu");
      }
      modifyMenuDiv.classList.toggle("editor-interface", true);
      contextMenu.classList.toggle("editor-interface", true);

      // Display the full interface (visible) or just the save/undo/redo buttons (not visible)
      modifyMenuDiv.classList.toggle("visible", editor_model.visible);

      // Make sure at most one element is marked as ghost-clicked.
      document.querySelectorAll("[ghost-clicked=true]").forEach(e => e.removeAttribute("ghost-clicked"));
      if(clickedElem && clickedElem.nodeType === 1) {
        clickedElem.setAttribute("ghost-clicked", "true");
      }
      
      // Recover selection if it exists
      editor_model.selectionRange = editor_model.notextselection ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0); 
        if(!f || !f.getBoundingClientRect ||
            f.startOffset === f.endOffset && f.startContainer === f.endContainer) return;
        return f;
      })();
      
      // Recover caret position if it exists
      editor_model.caretPosition = editor_model.notextselection || clickedElem && clickedElem.tagName === "HEAD" ? undefined : (() => {
        let selection = window.getSelection();
        if(!selection || !selection.rangeCount) return;
        let f = selection.getRangeAt(0);
        if(!f || f.startOffset !== f.endOffset && f.startContainer !== f.endContainer) return;
        return f;
      })();
      
      // We render the content of modifyMenuDiv from scratch
      modifyMenuDiv.innerHTML = "";
      let modifyMenuPinnedIconsDiv = el("div", {"class":"modify-menu-icons pinned", isghost:"true"}, [], {isghost: true}); // Icons always visible
      let modifyMenuIconsDiv = el("div", {"class":"modify-menu-icons", isghost:"true"}, [], {isghost: true}); // Top-level icons on the top bar
      let domSelector = el("div", {"class": "dom-selector noselect", isghost:"true"}); // create dom selector interface
      let modifyMenuHolder = el("div", {"class": "modify-menu-holder", "id":"modify-menu-holder"});
      modifyMenuDiv.append(modifyMenuPinnedIconsDiv); // Keep this one as it.
      
      /*
        Render interfaces / containers
      */
      for(let i = 1; i < editor_model.interfaces.length; i++) {
        let x = editor_model.interfaces[i];
        let priority = x.priority(editor_model);
        if(i > 0 && typeof priority === "number") {
          x.minimized = false;
          let previous = editor_model.interfaces[i-1]
          let beforePriority = previous.priority(editor_model);
          if(typeof beforePriority === "undefined" && (!previous.enabled(editor_model) || previous.minimized)) {
            var tmp = editor_model.interfaces[i];
            editor_model.interfaces[i] = editor_model.interfaces[i-1];
            editor_model.interfaces[i-1] = tmp;
            i -= 2; // Bubble up
          }
        }
      }
      for(let i = 0; i < editor_model.interfaces.length; i++) {
        let x = editor_model.interfaces[i];
        let priority = x.priority(editor_model);
        let initMinimized = typeof priority == "number" ? false :
                            x.enabled(editor_model) ? x.minimized : true;
        let renderedContent = x.render(editor_model);
        let class_str = x.title.replace(" ", "_");
        let menu = el(
          "div", {
            class:"editor-container" + (x.enabled(editor_model) ? "" : " disabled") + (x.minimized ? " minimized" : "") + " " + class_str},
          [ el("div.editor-container-title", {
                 title: typeof renderedContent === "string" ? renderedContent : undefined
               },
               [ el("div", {title: "Expand menu", class: "expand-menu"}, x.title),
                 el("div.editor-container-icon#displayarrow", {}, [], {innerHTML: editor.ui.icons.boxArrowExpand}),
                 el("div.editor-container-icon.arrowdown", {title: "Move menu down"}, [], {innerHTML: editor.ui.icons.boxArrowDown,
                   onclick: function(event) {
                     let d = this.parentElement.parentElement;
                     var tmp = editor_model.interfaces[d.i];
                     editor_model.interfaces[d.i] = editor_model.interfaces[d.i+1];
                     editor_model.interfaces[d.i+1] = tmp;
                     d.nextElementSibling.i = d.i;
                     d.i = d.i + 1;
                     d.parentElement.insertBefore(d.nextElementSibling, d);
                     event.preventDefault();
                     event.stop = true;
                     return false;
                   }}),
                 el("div.editor-container-icon.arrowup", {title: "Move menu up"}, [], {innerHTML: editor.ui.icons.boxArrowUp,
                   i: i,
                   onclick: function(event) {
                     let d = this.parentElement.parentElement;
                     var tmp = editor_model.interfaces[d.i];
                     editor_model.interfaces[d.i] = editor_model.interfaces[d.i-1];
                     editor_model.interfaces[d.i-1] = tmp;
                     d.previousElementSibling.i = d.i;
                     d.i = d.i - 1;
                     d.parentElement.insertBefore(d, d.previousElementSibling);
                     event.preventDefault();
                     event.stop = true;
                     return false;
                   }})
               ],
               {
                onclick: ((x) => event => {
                  console.log(event);
                  if(event.stop) return;
                  let target = event.target;
                  while(!target.matches(".editor-container")) target = target.parentNode;
                  //console.log("onclick", event.target);
                  x.minimized = target.classList.contains("minimized");
                  x.minimized = !x.minimized;
                  target.classList.toggle("minimized", x.minimized);
                })(x)
               }),
            el("div.editor-container-content", {}, renderedContent),
          ],
        {i: i});
        modifyMenuHolder.append(menu);
      }
      
      //console.log ({old_scroll, modifyMenuHolder});
      modifyMenuDiv.append(modifyMenuHolder);
      if(modifyMenuHolder) modifyMenuHolder.scrollTop = old_scroll;

      let createButton = function(innerHTML, attributes, properties) {
        let button = el("div", attributes, [], properties);
        button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
        button.classList.add("modify-menu-button");
        button.innerHTML = innerHTML;
        return button;
      }
      let addPinnedModifyMenuIcon = function(innerHTML, attributes, properties) {
        modifyMenuPinnedIconsDiv.append(createButton(innerHTML, attributes, properties));
      }
      var panelOpenCloseIcon = function() {
        return document.querySelector("#modify-menu").classList.contains("visible") ?
            editor.config.onMobile() ? editor.ui.icons.closeBottom : editor.ui.icons.closeRight + "<span class='modify-menu-icon-label'>Close</span>"
          : editor.config.onMobile() ? editor.ui.icons.openTop : editor.ui.icons.openLeft + "<span class='modify-menu-icon-label'>Open</span>";
      }
      var alwaysVisibleButtonIndex = 0;
      function nextVisibleBarButtonPosStyle() {
        let result = "position: absolute;" +
          (editor.config.onMobile() ? "top:-"+editor.config.buttonHeight()+"px;left:"+alwaysVisibleButtonIndex*editor.config.buttonWidth()+"px" :
                        "left:-"+editor.config.buttonWidth()+"px;top:"+alwaysVisibleButtonIndex*editor.config.buttonHeight()+"px")
        alwaysVisibleButtonIndex++;
        return result;
      }
      if(!editor_model.linkSelectMode) {
        addPinnedModifyMenuIcon(
          panelOpenCloseIcon(),
          {title: "Open/close settings tab", "class": "inert" },
          {onclick: function(event) {
              document.querySelector("#modify-menu").classList.toggle("visible");
              editor_model.visible = !editor_model.visible;
              setTimeout(maybeRepositionContextMenu, 500);
              this.innerHTML = panelOpenCloseIcon();
              if(editor.config.onMobile() && editor_model.savedTextSelection) {
                window.getSelection().addRange(editor_model.savedTextSelection);
                editor_model.savedTextSelection = undefined;
              }
            }
        });
        if(editor.config.EDITOR_VERSION & 1) {
          addPinnedModifyMenuIcon(editor.ui.icons.undo + "<span class='modify-menu-icon-label'>Undo</span>", 
            {"class": "inert" + (editor.ui.canUndo() ? "" : " disabled"), title: "Undo most recent change",
              id: "undobutton"
            },
            {onclick: function(event) {
              if(!editor.ui.undo()) editor.ui.sendNotification("Nothing to undo!");
              }
            }   
          );
          addPinnedModifyMenuIcon(editor.ui.icons.redo + "<span class='modify-menu-icon-label'>Redo</span>",
            {"class": "inert" + (editor.ui.canRedo() ? "" : " disabled"), title: "Redo most recent undo",
              id: "redobutton"
            },
            {onclick: function(event) {
             if(!editor.ui.redo()) editor.ui.sendNotification("Nothing to redo!");
              }
            }
          );
        }
        addPinnedModifyMenuIcon(editor.ui.icons.save + "<span class='modify-menu-icon-label'>Save</span>",
        {title: editor_model.disambiguationMenu ? "Accept proposed solution" : "Save", "class": "saveButton" + (editor.ui.canSave() || editor_model.disambiguationMenu ? "" : " disabled") + (editor_model.isSaving ? " to-be-selected" : ""),
          id: "savebutton"
        },
          {onclick: editor_model.disambiguationMenu ? 
            ((ambiguityKey, selected) => () => editor.ambiguity.accept(ambiguityKey, selected))(
              editor_model.disambiguationMenu.ambiguityKey, editor_model.disambiguationMenu.selected)
            : editor.ui.save
          }
        )
      }
      else {
        addPinnedModifyMenuIcon(editor.ui.icons.escape + "<span class='modify-menu-icon-label-link'>Cancel</span>", 
          {"class": "link-select-button", title: "Go back to original screen",
            id: "escapebutton"
          },
          {onclick: function(event) {
              escapeLinkMode();
            }
          }
        );
        addPinnedModifyMenuIcon(editor.ui.icons.check + "<span class='modify-menu-icon-label-link'>Select</span>", 
          {"class": "link-select-button", title: editor_model.linkSelectMsg || "Select target",
            id: "selectbutton"
          },
          {onclick: function(event) {
              editor_model.linkSelectCallback(editor_model.clickedElem);
              escapeLinkMode();
            }
          }
        );
        if(editor_model.linkSelectOtherMenus) {
          editor_model.linkSelectOtherMenus(addPinnedModifyMenuIcon)
        }
      }

      if(!editor_model.linkSelectMode) {
        contextMenu.innerHTML = "";
        var whereToAddContextButtons = contextMenu;
        var noContextMenu = false;
        // What to put in context menu?
        if(editor.config.onMobile() || (editor_model.clickedElem && editor_model.clickedElem.matches("html, head, head *, body")) || !editor_model.selectionRange && !editor_model.clickedElem) {
          whereToAddContextButtons = modifyMenuIconsDiv;
          noContextMenu = true;
        }
        let numButtons = 0;
        let addContextMenuButton = function(innerHTML, attributes, properties) {
          let button = el("div", attributes, [], properties);
          button.onmousedown = button.onmousedown ? button.onmousedown : preventTextDeselection;
          button.classList.add("context-menu-button");
          button.innerHTML = innerHTML;
          whereToAddContextButtons.append(button);
          numButtons++;
        }
        if(editor_model.link) {
          addContextMenuButton(editor.ui.icons.liveLink(linkToEdit(editor_model.link)),
            {title: "Go to " + editor_model.link, "class": "inert"});
        }
        if(!editor_model.selectionRange && clickedElem && clickedElem.parentNode && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(editor.ui.icons.parentUp,
          {title: "Select parent", "class":"inert"},
            {onclick: (c => event => {
              editor_model.clickedElem = c;
              refresh();
            })(clickedElem.parentElement)}
          );
        }
        
        var computedStyle = clickedElem && window.getComputedStyle(clickedElem);
        var isDisplayInline = computedStyle && (computedStyle.display.startsWith("inline") || computedStyle.display === "table-cell");
        if(!editor_model.selectionRange && clickedElem && clickedElem.matches && !clickedElem.matches(".editor-interface") && clickedElem.previousElementSibling && !clickedElem.previousElementSibling.matches(".editor-interface") && reorderCompatible(clickedElem.previousElementSibling, clickedElem) && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(isDisplayInline ? editor.ui.icons.arrowLeft : editor.ui.icons.arrowUp,
          {title: "Move selected element " + (isDisplayInline ? "to the left" : "up")},
          {onclick: (c => event => {
              let wsTxtNode = c.previousSibling && c.previousSibling.nodeType == 3 &&
                c.previousSibling.textContent.trim() === "" ? c.previousSibling : undefined;
              // There is whitespace before this element, we try to reinsert
              editor.userModifies();
              c.parentElement.insertBefore(c, c.previousElementSibling);
              if(wsTxtNode) { // We move the whitespace as well.
                c.parentElement.insertBefore(wsTxtNode, c.previousElementSibling);
              }
              editor_model.clickedElem = c;
              refresh();
            })(clickedElem)
          });
        }
        if(!editor_model.selectionRange && clickedElem && clickedElem.matches && !clickedElem.matches(".editor-interface") && clickedElem.nextElementSibling && !clickedElem.nextElementSibling.matches(".editor-interface") && reorderCompatible(clickedElem, clickedElem.nextElementSibling) && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(isDisplayInline ? editor.ui.icons.arrowRight : editor.ui.icons.arrowDown,
          {title: "Move selected element " + (isDisplayInline ? "to the right" : "down")},
          {onclick: (c => (event) => {
              let wsTxtNode = c.nextSibling && c.nextSibling.nodeType == 3 && 
                c.nextSibling.textContent.trim() === "" ? c.nextSibling : undefined;
              let nodeToInsertAfter = c.nextElementSibling;
              editor.userModifies();
              nodeToInsertAfter.insertAdjacentElement("afterend", c);
              if(wsTxtNode) { // We move the whitespace as well
                nodeToInsertAfter.parentElement.insertBefore(wsTxtNode, nodeToInsertAfter.nextSibling);
              }
              editor_model.clickedElem = c;
              refresh();
            })(clickedElem)
          });
        }
        if(!editor_model.selectionRange && clickedElem && clickedElem.tagName !== "HTML" && clickedElem.tagName !== "BODY" && clickedElem.tagName !== "HEAD" && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(editor.ui.icons.clone,
            {title: "Clone selected element"},
            {onclick: ((c, contextMenu) => event => {
                c.removeAttribute("ghost-clicked");
                let cloned = duplicate(c);
                if(cloned) {
                  editor_model.clickedElem = cloned;
                  refresh();
                } else contextMenu.classList.remove("visible");
              })(clickedElem, contextMenu)
            });
          addContextMenuButton(editor.ui.icons.wasteBasket,
            {title: "Delete selected element"},
            {onclick: (c => event => {
                if(editor_model.clickedElem.nextElementSibling) editor_model.clickedElem = editor_model.clickedElem.nextElementSibling;
                else editor_model.clickedElem = editor_model.clickedElem.previousElementSibling;
                editor.userModifies();
                c.remove();
                refresh();
              })(clickedElem)
            });
        }
        if(editor_model.selectionRange && (editor_model.selectionRange.startContainer === editor_model.selectionRange.endContainer || editor_model.selectionRange.startContainer.parentElement === editor_model.selectionRange.commonAncestorContainer && editor_model.selectionRange.endContainer.parentElement === editor_model.selectionRange.commonAncestorContainer) && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(editor.ui.icons.plus,
              {title: "Wrap selection"},
              {onclick: (s => event => {
                let elements = [];
                let tmp = s.startContainer;
                let nodeToInsertAfter = s.startContainer;
                let parent = nodeToInsertAfter.parentElement;
                editor.userModifies();
                while(tmp && tmp !== s.endContainer.nextSibling) {
                  if(tmp.nodeType === 3) {
                    elements.push(tmp === s.startContainer ? tmp === s.endContainer ? tmp.textContent.substring(s.startOffset, s.endOffset) : tmp.textContent.substring(s.startOffset) :
                      tmp === s.endContainer ? tmp.textContent.substring(0, s.endOffset) :
                      tmp.textContent);
                    if(tmp === s.startContainer) {
                      if(tmp === s.endContainer && tmp.textContent.length > s.endOffset) {
                        // Need to split the text node.
                        tmp.parentElement.insertBefore(document.createTextNode(tmp.textContent.substring(s.endOffset)), tmp.nextSibling);
                      }
                      if(s.startOffset === 0) {
                        nodeToInsertAfter = nodeToInsertAfter.previousSibling;
                        tmp.remove();
                      } else {
                        tmp.textContent = tmp.textContent.substring(0, s.startOffset);
                      }
                    } else if(tmp === s.endContainer) {
                      if(s.endOffset === s.endContainer.textContent.length) {
                        tmp.remove();
                      } else {
                        tmp.textContent = tmp.textContent.substring(s.endOffset);
                      }
                    } else {
                      tmp.remove();
                    }
                  } else {
                    elements.push(tmp);
                    tmp.remove();
                  }
                  tmp = tmp.nextSibling;
                }
                let insertedNode = el("span", {"ghost-clicked": "true"});
                for(let k of elements) {
                  insertedNode.append(k);
                }
                let nodeToInsertBefore = nodeToInsertAfter ? nodeToInsertAfter.nextSibling : parent.childNodes[0];
                parent.insertBefore(insertedNode, nodeToInsertBefore);
                document.querySelector("#modify-menu").classList.toggle("visible", true);
                editor_model.visible = true;
                editor_model.clickedElem = insertedNode;
                refresh();
              })(editor_model.selectionRange)}
              )
        }
        if(!editor_model.selectionRange && editor_model.clickedElem && editor.config.EDITOR_VERSION & 1) {
          addContextMenuButton(editor.ui.icons.plus,
              {title: "Insert element", contenteditable: false},
              {onclick: event => {
                editor_model.clickedElem = clickedElem;
                editor_model.displayClickedElemAsMainElem = true;
                editor_model.insertElement = true;
                editor_model.visible = true;
                getEditorInterfaceByTitle("Create").minimized = false;
                refresh();
                restoreCaretPosition();
              }});
        }
        if(editor_model.clickedElem) {
          // Thaditor-defined custom context menu buttons
          if(typeof thaditor === "object") {
            for(let button of thaditor.customContextMenuButtons(editor_model.clickedElem)) {
              addContextMenuButton(button.innerHTML, button.attributes, button.properties)
            }
          }
          // Page-defined custom context menu buttons
          for(let custom of editor.customContextMenuButtons) {
            for(let button of custom(editor_model.clickedElem)) {
              addContextMenuButton(button.innerHTML, button.attributes, button.properties)
            }
          }
        }

        let baseElem = clickedElem;
        while(baseElem && (baseElem.tagName == "SCRIPT" || baseElem.tagName == "STYLE")) {
          baseElem = baseElem.nextElementSibling;
        }
        baseElem = editor_model.selectionRange || baseElem || clickedElem;
      
        if(baseElem && !noContextMenu) {
          let clientRect = baseElem.getBoundingClientRect();
          // Find out where to place context menu.
          let clickedElemLeft = window.scrollX + clientRect.left;
          let clickedElemTop = window.scrollY + clientRect.top;
          let clickedElemBottom = window.scrollY + clientRect.bottom;
          let clickedElemRight = window.scrollX + clientRect.right;
          let desiredWidth = numButtons * editor.config.buttonWidth();
          let desiredLeft = (clickedElemLeft + clickedElemRight) / 2 - desiredWidth;
          if(desiredLeft < clickedElemLeft) desiredLeft = clickedElemLeft;
          let desiredTop = clickedElemTop - editor.config.buttonHeight(); 
          if(desiredTop - window.scrollY < 9) {
            desiredTop = clickedElemBottom;
            if(desiredTop + editor.config.buttonHeight() > window.innerHeight) {
              desiredTop = window.innerHeight - editor.config.buttonHeight(); 
            }
          }
          if(desiredLeft < 0) desiredLeft = 0;
          if(desiredTop < 0) desiredTop = 0;
          contextMenu.style.left = desiredLeft + "px";
          contextMenu.style.top = desiredTop + "px";
          contextMenu.style.width = desiredWidth + "px";
          contextMenu.classList.add("visible");
          setTimeout(maybeRepositionContextMenu, 0);
        }
        if(noContextMenu) {
          contextMenu.classList.remove("visible");
          if(modifyMenuIconsDiv.childNodes.length) {
            modifyMenuPinnedIconsDiv.parentElement.insertBefore(modifyMenuIconsDiv, modifyMenuPinnedIconsDiv.nextSibling);
          }
        }
      }
      
      return true;

    } // editor.ui.refresh

    editor.refresh = function() {
      console.log("Please prefer editor.ui.refresh() instead of editor.refresh()");
      editor.ui.refresh()
    };
    editor.ui.refresh();

    function maybeRepositionContextMenu() {
      //move the context menu if overlaps with modify-menu
       let contextMenu = document.querySelector("#context-menu");
       let modifyMenuDiv = document.querySelector("#modify-menu");
       let pinnedIcons = document.querySelector(".modify-menu-icons.pinned");
       if(!pinnedIcons) return;
       let pcr = pinnedIcons.getBoundingClientRect();
       let ccr = contextMenu.getBoundingClientRect();
       let mcr = modifyMenuDiv.getBoundingClientRect();
       if(editor.config.onMobile()) {
         if(ccr.bottom > pcr.top) {
           contextMenu.style.top = (ccr.y - (ccr.bottom - pcr.top)) + "px"
         } else if(ccr.bottom > mcr.top) {
           contextMenu.style.top = (ccr.y - (ccr.bottom - mcr.top)) + "px"
         }
       } else {
         if(ccr.right > pcr.left && ccr.top < pcr.bottom) { // Overlap with icons.
           contextMenu.style.left = (ccr.x - (ccr.right - pcr.left)) + "px"
         } else if(ccr.right > mcr.left) {
           contextMenu.style.left = (ccr.x - (ccr.right - mcr.left)) + "px"
         }
       }
    } //maybeRepositionContextMenu
    
    editor.ui.close = function editor_close() {
      if(editor.ui.model.visible) {
        editor.ui.model.visible = false;
        editor.ui.refresh();
        //Hide the menu
        //This is also working fine
        return false;
      }
      return true;
    } //editor.ui.close 
    editor.close = editor.ui.close;
    
    editor.ui._internals.onBeforeuninit = function editor_onbeforeuninit(e) {
      e = e || window.event;
      if(editor.config.onMobile() && editor.ui.model.visible) { // Hack to ask before saving.
        e.preventDefault();
        e.returnValue = '';
        return editor.ui.close();
      }
      var askConfirmation = editor.ui.canSave() || editor.ui.model.isSaving || editor.ui.model.disambiguationMenu;
      const confirmation = 'You have unsaved modifications. Do you still want to exit?';
      // For IE and Firefox prior to version 4
      if (e) {
        if(askConfirmation) {
          e.returnValue = confirmation;
        }
      }
      if(askConfirmation) {
        // For Safari
        return confirmation;
      } else if(!editor.config.thaditor) { // Send a close message in case this was a file opened from Desktop
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.onreadystatechange = editor.ui._internals.handleServerResponse(xmlhttp);
        xmlhttp.open("POST", location.pathname + location.search, false); // Async
        xmlhttp.setRequestHeader("close", "true");
        xmlhttp.send("{\"a\":3}");
      }
    } // End of editor.ui._internals.onBeforeuninit
    
  }; // editor.ui._internals.loadInterface

  function setCaretPositionIn(node, position) {
    position = Math.min(position, node.textContent.length);
    if (node.nodeType == 3) {
      let sel  = window.getSelection()
      setTimeout( () => sel.collapse(node, position), 0);
    } else {
      let p = position
      let n = node.firstChild
      while(n != null && p > n.textContent.length) {
        p = p - n.textContent.length
        n = n.nextSibling
      }
      if(n != null) {
        setCaretPositionIn(n, p)
      } else {
        console.log("Could not find position. Reached node and position ", [n, p])
      }
    }
  }
  function dataToRecoverCaretPosition(caretPosition) {
    if(!caretPosition) return undefined;
    return {target: editor.toTreasureMap(caretPosition.startContainer), startOffset: caretPosition.startOffset};
  }
  function recoverCaretPositionFromData(data) {
    if(!data) return;
    let newTextNodeOrParent = editor.fromTreasureMap(data.target);
    if(newTextNodeOrParent) setCaretPositionIn(newTextNodeOrParent, data.startOffset)
  }
  function dataToRecoverSelectionRange(selectionRange) { // TODO
    if(!selectionRange) return undefined;
    return undefined;
  }
  function recoverSelectionRangeFromData(data) { // TODO
    if(!data) return;
    return undefined;
  }

  // Initialize Editor's interface.
  editor.ui.init = function() {
    editor.ui.model = { // Change this and call editor.ui.refresh() to get something consistent.
      opened: true,
      visible: ifAlreadyRunning ? editor.ui.model.visible : false,
      clickedElem: ifAlreadyRunning ? editor.fromTreasureMap(editor.ui.model.clickedElem) : undefined,
      displayClickedElemAsMainElem: true, // Dom selector status switch signal
      previousVisitedElem: [], // stack<DOM node> which helps showing previous selected child in the dom selector
      notextselection: false, // When using the relative DOM selector, set to true to avoid considering the caret (e.g. for insertions and deletions)
      savedTextSelection: undefined, // Text range to restore when the edition bar closes, on mobile
      restoredAfterReload: ifAlreadyRunning ? editor.ui.model.restoredAfterReload : {},
      selectionRange: ifAlreadyRunning ? recoverSelectionRangeFromData(editor.ui.model.selectionRange) : undefined,
      caretPosition: ifAlreadyRunning ? recoverCaretPositionFromData(editor.ui.model.caretPosition) : undefined,
      link: undefined,
      disambiguationMenu: undefined, //here
      isSaving: false,
      //data structures to represent undo/redo "stack"
      undoStack: ifAlreadyRunning ? editor.ui.model.undoStack : [],
      redoStack: ifAlreadyRunning ? editor.ui.model.redoStack : [],
      // ({type: "undo" | "do", mutations: StoredMutation[]})[] such that there is no "do" followed by "undo"
      actionsAfterSaveState: ifAlreadyRunning ? editor.ui.model.actionsAfterSaveState : [],
      isDraftSwitcherVisible : ifAlreadyRunning ? editor.ui.model.isDraftSwitcherVisible : false,
      //observer to listen for muts
      outputObserver: ifAlreadyRunning ? editor.ui.model.outputObserver : undefined,
      //worker for interface with the server
      send_notif:ifAlreadyRunning ? editor.ui.model.send_notif : "",
      //editor log
      editor_log: ifAlreadyRunning ? editor.ui.model.editor_log : [],
      show_log: ifAlreadyRunning ? editor.ui.model.show_log : false, //here
      linkSelectMode: false, //here
      linkSelectCallback: undefined, // Callback that is going to be called with the selected node.
      idNum: ifAlreadyRunning ? editor.ui.model.idNum : 1,
      //new attribute to keep menu state after reload
      textareaPropertiesSaved: ifAlreadyRunning ? editor.ui.model.textareaPropertiesSaved : [],
      askQuestions: ifAlreadyRunning ? editor.ui.model.askQuestions : editor.config.askQuestions,
      autosave: ifAlreadyRunning ? editor.ui.model.autosave : editor.config.autosave,
      path: editor.config.path,
      version : computeDraftVersion(),
      interfaces: ifAlreadyRunning ? editor.ui.model.interfaces : [],
      disambiguationMenu: ifAlreadyRunning ? editor.ui.model.disambiguationMenu : undefined,
      userIsModifying: false // Set to true only when user is clearly modifying something
    }
    
    // Loads all the Editor interfaces.
    editor.ui._internals.loadInterface();
    
    // Register the output observer
    if (typeof editor.ui.model === "object" && typeof editor.ui.model.outputObserver !== "undefined") {
      editor.ui.model.outputObserver.disconnect();
    }
    editor.ui.model.outputObserver = new MutationObserver(editor.ui._internals.handleMutations);
    editor.ui.model.outputObserver.observe
      ( document.body.parentElement
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: true
        }
      );
    
    // Opens Editor's uI
    editor.ui.refresh();
    
    // Register navigation to compute pages in background.
    window.onpopstate = function(e){
      console.log("onpopstate", e);
      if(e.state && e.state.localURL) {
        editor.navigateTo(location, true);
      } else {
        editor.navigateTo(location.pathname + location.search, true);
      }
    };
    
    // Fix selection when it happens
    document.addEventListener("selectionchange", editor.ui.fixSelection);
    
    // File dropping
    var dropZone = document.body;
    dropZone.addEventListener('dragover', editor.ui.handleDragOver, false);
    dropZone.addEventListener('drop', editor.ui.handleDroppedFiles, false);
      
    // Key Shortcuts
    var lastKeyPress = 0;
    var onKeyDown = function(e) {
      editor.userModifies();
      var key = e.which || e.keyCode;
      if (e.which == 83 && (e.ctrlKey || e.metaKey)) { // CTRL+S or CMD+S: Save
        if(document.getElementById("savebutton") && document.getElementById("savebutton").onclick) {
          document.getElementById("savebutton").onclick();
        }
        e.preventDefault();
      }
      if(e.which == 75 && (e.ctrlKey || e.metaKey)) { // CTRL+K: Insert link
        if(new Date().valueOf() - lastKeyPress > 100) {
          document.execCommand('createLink', false, 'http://');
          e.preventDefault();
          var s = getSelection();
          s = s ? s.anchorNode : s;
          s = s ? s.parentNode : s;
          lastKeyPress = new Date().valueOf();
          editor.ui._internals.onClick({target: s, modify: true});
        }
        // Open link.
      }
      // CTRL+Z: Undo
      if(e.which == 90 && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        if(!editor.ui.undo()) editor.ui.sendNotification("Nothing to undo!");
      }
      // CTRL+Y: Redo
      if(e.which == 89 && (e.ctrlKey || e.metaKey)) {
        e.preventDefault();
        if(!editor.ui.redo()) editor.ui.sendNotification("Nothing to redo!");
      }
      //in link select mode, escape on the keyboard can be
      //used to exit the link select mode (same as escape button)
      if(editor.ui.model.linkSelectMode) {
        if(e.which == 27) { // Escape, Esc
          escapeLinkMode();
        }
      } else {
        if(e.which == 27) { // Escape, Esc.
          if(editor.ui.model.clickedElem) {
            editor.ui.model.clickedElem = undefined;
            editor.ui.refresh();
          } else {
            editor.ui.uninit();
          }
        }
      }
    };
    document.addEventListener("keydown", onKeyDown);
    
    // Events after a key is pressed.
    var bodyeditable = document.querySelector("body");
    var onKeypress = e => {
      if(e.keyCode==13 && !e.shiftKey){ // [Enter] key without SHIFT
          // If we are inside a paragraph, we split the paragraph.
          // If we are directly inside a div, we add a <br> separator.
          // We delete everything between anchorNode and focusNode
          // TODO: Handle ul and li
          // TODO: Handle tab to indent and dedent.
          var caretSelection = document.getSelection();
          var x = caretSelection.anchorNode;
          if(x && x.nodeType == 3 && caretSelection.rangeCount) { // text node
            if(x.parentNode && getComputedStyle(x.parentNode).display == "block") {
              e.preventDefault(); //Prevent default browser
              var range = caretSelection.getRangeAt(0);
              range.deleteContents();
              caretSelection = document.getSelection();
              x = caretSelection.anchorNode;
              if(x.parentNode.tagName == "p") { // Split the p
                var newPar = document.createElement("p");
                
                var fo = caretSelection.anchorOffset;
                if(fo < x.text.length) {
                  newPar.append(document.createTextNode(x.text.substring(fo)));
                  x.deleteData(fo,x.text.length - fo);
                }
                var y = x.nextSibling;
                while(y) {
                  var yy = y;
                  y = y.nextSibling;
                  newPar.append(yy); // Moves yy
                }
                x.parentNode.insertAdjacentElement("afterend", newPar);
              } else { // insert br
                range.insertNode(document.createElement("br"))
              }
            }
          }
      }
    }
    if(bodyeditable && !bodyeditable.configured) {
      bodyeditable.configured = true;
      bodyeditable.addEventListener("keypress", onKeypress, true);
    }
    
    document.addEventListener('mousedown', editor.ui._internals.onMouseDown, false);
    document.addEventListener('click', editor.ui._internals.onClick, true);
    
    
    var onDeviceReady = function onDeviceReady(){
      document.addEventListener("backbutton", editor.ui.close, false);
    };
    // Mobile only. Experiment not working. We want the back button to close the editor when it is opened.
    document.addEventListener("deviceready", onDeviceReady, false);

    var onError = function (message, source, lineno, colno, error) {
      let msg;
      if(message instanceof ErrorEvent) {
        msg = message.message;
      } else {
        msg = message + " from " + source + " L" + lineno + "C" + colno;
      }
      editor.ui.model.editor_log.push(msg);
    };
    window.addEventListener("error", onError);
    
    window.onbeforeuninit = editor.ui._internals.onBeforeuninit;
    
    editor.ui.uninit = function() {
      if(!editor.confirmLeaving()) return;
      if(editor.ui._internals.contextMenu) {
        editor.ui._internals.contextMenu.remove();
      }
      if(editor.ui._internals.modifyMenu) {
        editor.ui._internals.modifyMenu.remove();
      }
      // Register the output observer
      if (typeof editor.ui.model === "object" && typeof editor.ui.model.outputObserver !== "undefined") {
        editor.ui.model.outputObserver.disconnect();
      }
      editor.ui.model.opened = false;
      window.onpopstate = undefined;
      document.removeEventListener("selectionchange", editor.ui.fixSelection);
      dropZone.removeEventListener('dragover', editor.ui.handleDragOver);
      dropZone.removeEventListener('drop', editor.ui.handleDroppedFiles);
      document.removeEventListener("keydown", onKeyDown);
      bodyeditable.removeEventListener("keypress", onKeypress);
      
      document.removeEventListener('mousedown', editor.ui._internals.onMouseDown, false);
      document.removeEventListener('click', editor.ui._internals.onClick, true);
      
        // Mobile only. Experiment not working. We want the back button to close the editor when it is opened.
      document.removeEventListener("backbutton", editor.ui.close);
      document.removeEventListener("deviceready", onDeviceReady);
      window.removeEventListener("error", onError);
      
      window.onbeforeuninit = undefined;
      
      document.body.removeAttribute("contenteditable");
      document.body.insertBefore(editor.ui._internals.switchEditBox(true), document.body.childNodes[0]);
    };
    
    // Store the current child list of nodes that ignore their children totally
    (function() {
      var elems = document.querySelectorAll("*");
      for(var i = 0; i < elems.length; i++) {
        if(editor.isIgnoringChildNodes(elems[i])) {
          editor.storeIgnoredChildNodes(elems[i]);
        }
      }
    })();
    
    // The thing that makes everything live editable.
    // Make sure we give the time to the page to register the change
    setTimeout(() => {
      if(typeof editor.config.canEditPage == "boolean" && editor.config.canEditPage) {
        document.body.setAttribute("contenteditable", "true");
      }
    }, 1);
  } // editor.ui.init

  document.addEventListener("DOMContentLoaded", function(event) { 
    if(editor.config.canEditPage) {
      editor.ui.init();
    }
  });
})(editor);

editor.init(); // Hook into navigation, copy/paste, listen to insertions.

// editor.ui.init() can be called afterwards.