# klank.dev

A media creation studio written in PureScript.

## Getting started

Each directory in `klank-studio` is a fully-elaborated project. In each directory, you should start by running:

```bash
npm install
```

Then, you can create a live session by running `npm run start`.

## Publishing

The current recommended publishing technique is to use [surge.sh](https://surge.sh). You can install surge by running `npm install -g surge`. Make sure to run `npx surge` at least once before publishing to log into the service.

To publish your klank, run `npm install && npm run build && cd dist && npx surge`.
