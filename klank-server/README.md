# klank-server

The `klank.dev` server.

## To run locally

```
npm install
cp -r ../klank-lib klank-lib
NODE_ENV=development node devserver.js
```

## To deploy to AWS via serverless

```
npm install
cp -r ../klank-lib klank-lib
serverless deploy
```
