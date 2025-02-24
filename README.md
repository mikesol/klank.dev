# klank.dev

A media creation studio written in PureScript.

## Getting started

Each directory in `klank-studio` is a fully-elaborated project. For example, to build `01-hello-world`, you would start by running:

```bash
cd klank-studio/01-hello-world
npm install
```

Then, you can create an interactive session with hot reloading by running `npm run start`.

## Publishing

The current recommended publishing technique is to use [surge.sh](https://surge.sh). You can install surge by running `npm install -g surge`. Make sure to run `npx surge` at least once before publishing to log into the service.

To publish your klank, run `npm install && npm run build && cd dist && npx surge`.

## Creating a new project

To create a new project, you can edit one of the examples or copy one of them to a new example in the `klank-studio` folder. You can also link to klank.dev's libraries from the `packages.dhall` of any PureScript project using `spago`.

## Documentation

`klank.dev` is mostly a wrapper around the following libraries. Please visit their GitHub projects for more information and to browse their APIs.

- [`purescript-audio-behaviors`](https://github.com/mikesol/purescript-audio-behaviors)
- [`purescript-painting`](https://github.com/mikesol/purescript-painting)
- [`purescript-behaviors`](https://github.com/mikesol/purescript-behaviors)
