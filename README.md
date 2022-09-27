# Eiger: Auditable, executable, flexible legal regulations

This repo is the online home of *Eiger*, a system developed by Alexander Bernauer (of PricewaterhouseCoopers Switzerland) and Richard Eisenberg (developed while Richard was
at Tweag) to encode legal regulations as Haskell code.

The [writeup of the Eiger approach](https://arxiv.org/abs/2209.04939) is on arXiv,
and our work was presented as an system demonstration at Haskell Symposium 2022.

This repo contains the core Eiger implementation in `eiger`, support for JSON (de)serialization
in `eiger-json`, and some tests (that depend on JSON deserialization) in `eiger-test`. You can
also look in `eiger-test` for some inspiration for how to write your own Eiger programs.

We do *not* expect to invest time in maintaining this public version of Eiger, though if you
are interested in adopting Eiger for your work (whether it be for profit or research or other
goals), we may be open to partnering with you. We both believe strongly in the general ideas
behind Eiger and hope to see more regulation formally specified in code.