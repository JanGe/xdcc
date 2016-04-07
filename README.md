## xdcc - A wget-like utility for retrieving files from XDCC bots on IRC

[![Build Status](https://travis-ci.org/JanGe/xdcc.svg?branch=master)](https://travis-ci.org/JanGe/xdcc)

XDCC (eXtended DCC) is a protocol used by IRC bots to make files
available for transfer. This utility can be used to retrieve such files.

See https://en.wikipedia.org/wiki/XDCC for more details.

### Supported DCC Variants:

* (Standard) DCC
* Reverse DCC

## Installation

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Add `~/.local/bin` to your `PATH` environment variable.
1. Run `stack install xdcc --resolver nightly-2016-04-06`.

## Usage
```
xdcc HOST CHANNEL USER #PACK
```

See `xdcc --help` for more options.

## Development

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Run `stack build`.
