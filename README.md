Source code of [TheSpace](https://thespace.game) official app.

## Development

### Install Tools

- Elm v0.19 https://guide.elm-lang.org/install/
- Node/NPM (recent version) https://docs.npmjs.com/cli/v8/configuring-npm/install
- make

### Install project development dependencies

```
npm install --only-dev
```

### Auto reload dev server

```
npx elm-live src/Main.elm --host=127.0.0.1 --start-page=src/Native/app.html --open  -- --output=src/Native/elmapp.js
```

*Note: ```src/Native/elmapp.js``` is in .gitignore, host using ```127.0.0.1``` instead of ```localhost``` due to rpc provider whitelist.*

### Env Management (production/staging/development)

### Switch env

```
cp src/Env/(Production|Staging|Development).env src/Env.elm
```

### Backup current env

```
make backupEnv
```

This will back up ```src/Env.elm``` to ```src/Env.bak```.


### Restore backup env

```
make restoreEnv
```

This will restore ```src/Env.bak``` to ```src/Env.elm```, and use ```src/Env/Development.env``` if ```src/Env.bak``` not exists.


## Release

### Production Release

```
make prod
```

### Staging Release

```
make stag
```

These commands will:
1. backup current env
2. switch to production/staging env
3. compile current Elm code with ```--optimize``` flag to ```src/Native/elmappesm.js```
4. bundle ```elmappesm.js``` with js files to ```current/(prod|stag)/app.js```
5. uglify and minify ```current/(prod|stag)/app.js``` to ```current/(prod|stag)/app.min.js```
6. generate ```current/(prod|stag)/index.html``` using MD5 checksum of ```app.min.js``` as version(```?v=version```)
7. remove ```current/(prod|stag)/app.js``` and ```src/Native/elmappesm.js```
8. restore backed up env


## App Architecture

![Diagram for TheSpace App Architecture](doc/arch.svg "TheSpace App Architecture")
