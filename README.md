# nftree-eco-backend

The backend for nftree.eco

## TODO

- Serve the API in src/Api.hs https://docs.servant.dev/en/stable/tutorial/Server.html#a-first-example
- Add persistent and sqlite for storing database
- Add password for hashing and storing passwords

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
