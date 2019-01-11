update:
	npm update sketch-n-sketch
	git diff --quiet && git diff --staged --quiet || git commit -am "Updated sketch-n-sketch's version"
	node bundle.js
	git diff --quiet && git diff --staged --quiet || git commit -am "Updated the binary"
	npm version patch
	npm publish
	git push origin master
	npm install -g http-server-editor

publish:
	node bundle.js
	git diff --quiet && git diff --staged --quiet || git commit -am "Updated the binary"
	npm version patch
	npm publish
	git push origin master
	npm install -g http-server-editor
