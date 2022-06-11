Source code of [TheSpace](https://thespace.game) official app.

## Development

### Install Tools

- Elm v0.19 https://guide.elm-lang.org/install/
- Node/NPM (recent version) https://docs.npmjs.com/cli/v8/configuring-npm/install

### Install project development dependencies

```
npm install --only-dev
```

### Auto reload dev server

```
make dev-server
```

or, in debug mode

```
make debug-server
```

#### Notes
- tmp build elmapp to ```src/Native/elmapp.js``` (in .gitignore).
- host using ```127.0.0.1:8000``` (not ```localhost``` due to rpc provider whitelisting).

### Env Management (production/staging/development)

### Switch env

```
cp src/Env/(Production|Staging|Development).env src/Env.elm
```

### Backup current env

```
make backup-env
```

This will back up ```src/Env.elm``` to ```src/Env.bak```.


### Restore backup env

```
make restore-env
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
3. compile current Elm code with ```--optimize``` flag to ```src/Native/elmappesm.js``` (ES6 Module).
4. bundle ```elmappesm.js``` with js files to ```current/(prod|stag)/app.js```
5. uglify and minify ```current/(prod|stag)/app.js``` to ```current/(prod|stag)/app.min.js```
6. generate ```current/(prod|stag)/index.html``` using MD5 checksum of ```app.min.js``` as version(```?v=version```)
7. remove ```current/(prod|stag)/app.js``` and ```src/Native/elmappesm.js```
8. restore backed up env


## App Architecture

![Diagram for TheSpace App Architecture](doc/arch.svg "TheSpace App Architecture")
