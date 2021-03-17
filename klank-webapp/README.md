# klank-webapp

## To run in studio mode

In studio mode, your klank lives in the `./studio` folder. When you save your klank as you're working on it, the site will hot-reload.

```bash
npm install
git clone --depth 1 --branch v1.4.12 https://github.com/ajaxorg/ace-builds
git clone --depth 1 --branch v0.0.12 https://github.com/shawwn/chalkie
npx spago build
npm run studio
```

## To run the klank.dev site locally

```bash
npm install
git clone --depth 1 --branch v1.4.12 https://github.com/ajaxorg/ace-builds
git clone --depth 1 --branch v0.0.12 https://github.com/shawwn/chalkie
npx spago build
npm run serve
```
