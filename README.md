# nftree-eco-backend

The backend for nftree.eco

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
