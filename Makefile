update:
	npm update sketch-n-sketch
	git commit -am "updated sketch-n-sketch's version"
	npm version patch
	npm publish
	git push origin master