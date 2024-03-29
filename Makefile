dev-server :
	npx elm-live src/Main.elm \
	--host=0.0.0.0 \
	--start-page=src/Native/app.html \
	-- --output=src/Native/elmapp.js --optimize

debug-server :
	npx elm-live src/Main.elm \
	--host=0.0.0.0 \
	--start-page=src/Native/app.html \
	-- --output=src/Native/elmapp.js --debug

backup-env :
	cp src/Env.elm src/Env.bak

restore-env :
ifneq (,$(wildcard src/Env.bak))
	mv src/Env.bak src/Env.elm
else
	$(info >>> src/Env.bak not found, using src/Env/Development.env)
	cp src/Env/Development.env src/Env.elm
endif

_prod :
	cp src/Env/Production.env src/Env.elm
	npx elm-esm make src/Main.elm --output=src/Native/elmappesm.js --optimize
	npx rollup src/Native/main.js --format=iife --file=current/prod/app.js
	rm src/Native/elmappesm.js
	npx uglifyjs current/prod/app.js --compress \
		"pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | \
	npx uglifyjs --mangle --output current/prod/app.min.js
	node genHtml.js current/prod/app.min.js
	rm current/prod/app.js

_stag :
	cp src/Env/Staging.env src/Env.elm
	npx elm-esm make src/Main.elm --output=src/Native/elmappesm.js --optimize
	npx rollup src/Native/main.js --format=iife --file=current/stag/app.js
	rm src/Native/elmappesm.js
	npx uglifyjs current/stag/app.js --compress \
		"pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | \
	npx uglifyjs --mangle --output current/stag/app.min.js
	node genHtml.js current/stag/app.min.js
	rm current/stag/app.js

prod :
	-mkdir -p current/prod
	make backup-env
	-make _prod
	make restore-env

stag :
	-mkdir -p current/stag
	make backup-env
	-make _stag
	make restore-env
