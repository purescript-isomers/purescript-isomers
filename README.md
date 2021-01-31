# webrow-hybrid

Can we create a library in PureScript for type-safe routing and hybrid apps which operates from the users perspective purely on the value level? Do we really have to clone (_servant_ like) heavy type-level machinery and fight with piles of constraints and ad hoc polymorphism?

We have `Record` and `Variant*` (and `Rows` in general). We have wonderful libs like _run_ ,  _routing-duplex_ or _wai_ at our disposal.
So let's make a little proof that PureScript is not just a "simpler Haskell for the frontend".
