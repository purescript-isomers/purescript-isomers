# isomers

WIP!

Use the same building blocks to create fully isomorphic apps (SPA with SSR or traditional server side apps) and APIs in PureScript. Everything is derived from value level spec and renderers.

## Objectives

Once again - the whole spec is just a value. Values are much easier to compose.

## Credits

The core pieces of `Isomers.Request.Duplex` were copied from `routing-duplex` library by @natefaubion. I wasn't able to extend its recursive AST and change was [too invasive](https://github.com/natefaubion/purescript-routing-duplex/issues/19) to be included in the original lib.
