# Gorn

```text
gorn git:(master) gorn build -i ./src
src
|
+- LCvlP1Xe: Duplicates copy.hs
|
+- LCvlP1Xe: Duplicates.hs
|
+- TNRtyPPT: Tree.hs
|
`- Y4toR+9y: Utils.hs
```

```text
gorn git:(master) gorn duplicates -i ./src
LCvlP1Xe
|
+- ./src/Duplicates.hs
|
`- ./src/Duplicates copy.hs
```

## Roadmap

- [x] Build tree.
    - [x] Parallel.
    - [x] Fault tolerant: catch and collect all FS/IO exceptions.
- [x] Print tree.
- [x] Find duplicates.
- [x] Print duplicates forest.
- [x] Make cool CLI.
- [ ] Make duplicate finder work on directories.

## Bourgeois excesses

- [ ] Nix CD.
- [ ] Duplicate finder machinery speedup.
- [ ] TUI (?).

## Install

### Bare metal

| Tested with | version |
|-------------|---------|
| GHC         | 9.2.5   |
| Cabal       | 3.8.1.0 |

```bash
git clone https://github.com/npatsakula/haskell_gorn.git; cd haskell_gorn
cabal install exe:gorn -O2 --overwrite-policy=always
```

### Nix

Local shell:

```bash
# sh <(curl -L https://nixos.org/nix/install) --no-daemon
# mkdir -p ~/.config/nix
# echo "experimental-features = flakes nix-command" >> ~/.config/nix/nix.conf
nix shell github:npatsakula/haskell_gorn
which gorn
```

Docker:

```sh
nix build .#container
docker load < result
docker images
```