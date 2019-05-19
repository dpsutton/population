# Population

modeling of virus interactions with host from MIT

Has the "simple virus" part. Drug resistant virus remains to be done.

## Run
You will need node and Java installed.
``` shell
npm install
npx shadow-cljs watch app
```

Once it finishes compiling, visit [http://localhost:8700/](http://localhost:8700/)

## Clean

``` shell
yarn clean
```

## Release
Commits to master automatically trigger a build on netlify, and it is viewable [here](https://cranky-rosalind-769727.netlify.com/).
``` shell
yarn release
```

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
