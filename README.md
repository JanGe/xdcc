## xdcc - A wget-like utility for retrieving files from XDCC bots on IRC

[![Build Status](https://travis-ci.org/JanGe/xdcc.svg?branch=master)](https://travis-ci.org/JanGe/xdcc)

XDCC (eXtended DCC) is a protocol used by IRC bots to make files
available for transfer. This utility can be used to retrieve such files.

See https://de.wikipedia.org/wiki/XDCC for more details.

## Installation

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Add `~/.local/bin` to your `PATH` environment variable.
1. Clone this repository.
1. Run `stack install` from the root folder of this repository.

## Usage
```
xdcc HOST CHANNEL USER #PACK
```

See `xdcc --help` for more options.

## Development

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Run `stack build`.
