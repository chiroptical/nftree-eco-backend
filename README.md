# nftree-eco-backend

The backend for nftree.eco

## What is nftree.eco?

nftree.eco is a tree adoption website with one goal: plant more trees. I will
work with cities who have documented planted trees in their neighborhoods and
turn them into non-fungible tokens for users to purchase. After you purchase a
token you can choose to trade or make friends with other trees. To trade your
trees you will need its' unique address and optionally you can plant more
trees via [The Good Api][good-api].

Ultimately, nftree.eco will be run as a non-profit with the goal of increasing
tree equity in city landscapes.

## What it is not

This is **not** a blockchain project and never will be.

## TODO

- [ ] Verify email on register (hailgun?)
- [ ] Request access to tree database from contact
- [ ] Implement login via oauth? maybe with GitHub?

## Adding a haskell package from GitHub

- `niv add {githubUsername}/{packageName}`
- In `default.nix`, add the following in `myHaskellPackages`
```
"{packageName}" =
  hself.callCabal2nix
    "{packageName}"
    (sources.{packageName})
    {};
```
## Useful links

- [The Good Api][good-api]
- [The Eden Project](https://www.edenproject.com/)
- [Jake Archibald's CORS Resource](https://jakearchibald.com/2021/cors)

[good-api]: https://docs.thegoodapi.com
