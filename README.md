# webrow-hybrid

Can we create a library in PureScript for type safe routing and hybrid apps which operates purely on the value level? Do we really have to clone (_servant_ like) heavy typelevel machinery and fight with piles of constraints and ad hoc polymorphism?

I don't think so.

We have `Record` and `Variant*` (and `Rows` in general). We have wonderful libs like _run_ and _routing-duplex_ at our disposal.
So let's make a little proof that PureScript is not just a Haskell for the frontend.

