# hJOPserver

This repository contains source codes of the main technological server of [hJOP
project](https://hjop.kmz-brno.cz/).

Description of whole hJOP project is available in Czech at
[http://hjop.kmz-brno.cz](http://hjop.kmz-brno.cz/).

 * Programming language: Object Pascal
 * Environment: Delphi 2009
 * For: Microsoft Windows OS

## Using

Executable Windows files could be downloaded in
[Releases](https://github.com/kmzbrnoI/hJOPserver/releases) section.  This
project is nowadays distributed in x86 (32-bit) version only.

## Basic information

hJOPserver connects to the model railroad via 2 dynamic libraries:

 * RCS ([API specification](https://github.com/kmzbrnoI/mtb-lib/wiki))
 * Trakce ([API specification](https://github.com/kmzbrnoI/xn-lib-cpp-qt/wiki))

It creates TCP server for connecting of the model railroad operators ([API
specification](https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer)), it
creates *UDP discover server* ([API
specification](https://github.com/kmzbrnoI/hJOPserver/wiki/udpDiscover)) and it
also features a *PT server* ([API
specification](https://github.com/kmzbrnoI/hJOPserver/wiki/ptServer)), which is
used for remote diagnostics of the server.

It maintains many databases and allows operators to control model railroad
like [JOP](https://cs.wikipedia.org/wiki/Jednotné_obslužné_pracoviště) system
does on the real railroad in the Czech Republic.

The whole hJOP project targets mainly on czech users, so the whole GUI and
some parts of the source codes are in czech language.

## Development

Simply clone this repository (with submodules!) and build `exe` via Delphi
compiler. Nowadays, the author does now know about any free compiler this
project could be compiled with, Delphi 2009 and all newer versions are
non-free.

This repository has (due to historical reasons) all commit messages in czech
language.

Main project file: [`src/hJOPserver.dpr`](src/hJOPserver.dpr)

### Special components & libraries

* [dcpcrypt2](http://www.cityinthesky.co.uk/opensource/dcpcrypt/)
* [JEDI Code Library](http://wiki.delphi-jedi.org/index.php?title=JEDI_Code_Library)

## Authors

This project was created by:

 * Jan Horacek ([jan.horacek@kmz-brno.cz](mailto:jan.horacek@kmz-brno.cz))

Do not hesitate to contact author in case of any troubles!

## License

This application is released under the [Apache License v2.0
](https://www.apache.org/licenses/LICENSE-2.0).
