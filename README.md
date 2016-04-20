## xdcc - A wget-like utility for retrieving files from XDCC bots on IRC

[![Build Status](https://travis-ci.org/JanGe/xdcc.svg?branch=master)](https://travis-ci.org/JanGe/xdcc)
[![xdcc on Hackage](https://img.shields.io/hackage/v/xdcc.svg?maxAge=2592000)](https://hackage.haskell.org/package/xdcc)
[![xdcc on Stackage Nightly](http://stackage.org/package/xdcc/badge/nightly)](http://stackage.org/nightly/package/xdcc)

XDCC (eXtended DCC) is a protocol used by IRC bots to make files
available for transfer. This utility can be used to retrieve such files.

See https://en.wikipedia.org/wiki/XDCC for more details.

### Supported DCC Variants:

* (Standard) DCC
* Reverse DCC

## Installation

### OS X & Linux

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Add `~/.local/bin` to your `PATH` environment variable.
1. Run `stack setup --resolver nightly`.
1. Run `stack install xdcc --resolver nightly`.

### Windows

1. Install [Stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows) using its Windows Installer.
1. Open a [Command Prompt](http://www.digitalcitizen.life/7-ways-launch-command-prompt-windows-7-windows-8).
1. Run `stack setup --resolver nightly`.
1. Run `stack install xdcc --resolver nightly`.

## Usage
```
xdcc HOST CHANNEL USER #PACK
```

See `xdcc --help` for more options.

## Development

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Run `stack build`.
