# alexa.pl

Alexa skill development with SWI-Prolog. Uses the [Attempto Reasoner](http://attempto.ifi.uzh.ch/race/) (RACE) as an example application.

## Installation

First, you need [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

We make use of the following packages:
- [`library(race)`](https://github.com/fnogatz/race)

It can be installed by calling `?- pack_install(race).` in SWI-Prolog.

## Usage

```sh
make server
```
