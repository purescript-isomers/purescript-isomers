# purescript-isomers

WIP!

Use the same building blocks to create fully isomorphic apps (SPA with SSR or traditional server side apps) and APIs in PureScript. Everything is derived from value level spec and renderers.

## Objectives

Once again - the whole spec is just a value. Values are much easier to transform and compose.

## Design

The main idea behind this framework is quite simple. Let's prototype it quickly here.

As a base layer this lib provides a pair of composable codec types. We can think about them as encoding and decoding functions for simplicity here. Let say that we use just `Tuple` to keep them together:

```purescript
type RequestCodec a =  (a -> HTTPRequest) /\ (HTTPRequest -> Maybe a)

type ResponseCodec a = (a -> HTTPResponse) /\ (HTTPResponse -> Maybe a)
```

These codecs (I'm using `Duplex` term in the codebase following Nate's convention) allow us to send and receive data through HTTP channel. In the lib the representation is a bit [more complicated than that][1].

### Single endpoint

Given the above types we can define a simple, single endpoint API just by providing a pair:

```
type Api i o = RequestCodec i /\ ResponseCodec o
```

So client would be just:

```purescript
client :: forall i o m. Monad m => Api i o -> i -> m (Maybe o)
client ((reqEnc /\ _) /\ (_ /\ resDec)) i = do
  httpRes <- httpFetch (reqEnc i)
  pure $ resDec httpRes
```

To build a server we need a function which actually computes the `o` given the `i`:

```purescript
server :: forall i o n. Monad n => Api i o -> (i -> n o) -> HTTPRequest -> n HTTPResponse
server ((_ /\ reqDec) /\ (resEnc /\ _)) handler httpReq = do
  reqDec httpReq >>= case _ of
    Just i -> resEnc <$> handler i
    Nothing -> -- handle bad request
```

Please ignore the details like moands which we work in or error handling because they not important now. I want to only describe this idea briefly and clearly.

### Multiple endpoints

But a single endpoint APIs are a really rare thing. Usually we want to be able to provide multiple functions which serve different types of values for different types of inputs carried by requests. Server should be able to pick a request, decode it and pass the data to the appropriate handler and encode the result using appropriate encoding function. Similar thinking should be applied to the client function which should accept a request in the form of an application level **value** turn it into `HTTPRequest` and wait for a response which should be decoded by appropriate codec which is dedicated for this particular response type. On both ends of the wire we want to use aligning functions in this case of course and our codecs keep them together for us.

To fulfill this "dispatch" requirement we use "compatible" `Variant`s and `Record`s types. So let me introduce `RealWorldApi` type :-P which can be used by both the client and the server to describe multiple endpoints safely:

```purescript
type RealWorldApi req res = RequestCodec (Variant req) /\ { | res }
```

What we have above is a codec which encodes / decodes requests into a `Variant` on the first position of our tuple. The `{ | res }` type represents a record of response codecs which we use to turn results into a `HTTPResponse`s. In this record we have codecs which should be used to encode / decode responses for particular requests. We are able to pick appropriate response codec or handler on the server based on the label included in the `Variant` label from the request. These labels don't carry any HTTP semantic meaning by themselves - they are only a dispatch layer. On the application layer you work with data directly and use these labels / paths only to pick endpoints which you want to use or to create URLs.

So the simple client can be sketched as:

```purescript
client ::
  forall i m o req req_ res res_.
  Monad m =>
  Row.Cons endpoint i req_ req =>
  Row.Cons endpoint (ResponseCodec o) res_ res =>
  RealWorldApi req res ->
  SProxy endpoint ->
  i ->
  m (Maybe o)
client ((reqEnc /\ _) /\ resCodecs) endpoint i = do
  let
    httpReq = reqEnc (Variant.inj l i)
    resDec = snd (Record.get l resCodecs)

  httpRes <- httpFetch httpReq
  pure $ resDec httpRes
```

On the server we additionally have to pick the hander from provided handlers record which resides under the appropriate label (simple `hmap` is enough there).

### Stay tuned

So that was the general idea. Based on it I can also define and combine rendering functions with particular data layer endpoints and extend an API and build a fully isomorphic SSR solution and SPA web routing. Of course SSR / SPA "routes" are addition to the API which can be still seen as just the above pair when we ignore rendering record.

I'm trying to cover this and many more things in this attempt: "heterogeneous DSL" for defining your specs which is easy to compose, nested routes handilng but also flattening, easy to use combinators for codeces, HTTP semantics preserving response and request wrappers... please stay tuned!


## Credits

The core pieces of `Isomers.Request.Duplex` were copied from `routing-duplex` library by @natefaubion. I wasn't able to extend its recursive AST and this change was [too invasive](https://github.com/natefaubion/purescript-routing-duplex/issues/19) to be included in the original lib.

--

[1] We have different encodings on the client than on the server of `HTTPRequest` and `HTTPResponse`. Additionally parsers work in an effectful monad - currently hardcoded `Aff` but should be parametrized in the future.
We also care about portability / extensibility  of our encodings on the server side so we don't loose the power of the HTTP itself and underlining PS backend at hand. We want to preserve possibly to handle binary data transfers and their decoders [think `multipart` + `formidable` on `node.js` as an example], we want to allow streaming, we don't want to restricts our backend to be only `JS` etc.).
