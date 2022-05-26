backupEnv :
	cp src/Env.elm src/Env.bak

restoreEnv :
ifneq (,$(wildcard src/Env.bak))
	mv src/Env.bak src/Env.elm
else
	$(info >>> src/Env.bak not found, using src/Env/Development.env)
	cp src/Env/Development.env src/Env.elm
endif


release :
	make backupEnv
	cp src/Env/Production.env src/Env.elm
	npx rollup --config rollup.conf.js
	npx uglifyjs public/app.js --compress \
	"pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | \
	npx uglifyjs --mangle --output build/app.min.js
	sha1sum build/app.min.js
	node genReleaseHtml.js build/app.min.js
	rm public/app.js
	make restoreEnv
