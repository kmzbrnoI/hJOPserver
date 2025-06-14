hJOPserver
==========

This repository contains source codes of the main technological server of [hJOP
project](https://hjop.kmz-brno.cz/).

Description of whole hJOP project is available in Czech at
[http://hjop.kmz-brno.cz](http://hjop.kmz-brno.cz/).

 * Programming language: Object Pascal
 * Environment: CodeGear RAD Studio (available for free under [Community edition](http://docwiki.embarcadero.com/RADStudio/Athens/en/Community_Edition))
 * For: Microsoft Windows OS

## Using

Windows executable files can be downloaded in
[Releases](https://github.com/kmzbrnoI/hJOPserver/releases) section. This
project is nowadays distributed in x86-64 (64-bit) version only.

## Basic information

hJOPserver connects to the model railroad via 2 dynamic libraries:

 * RCS ([API specification](https://github.com/kmzbrnoI/mtb-lib/wiki))
 * Trakce ([API specification](https://github.com/kmzbrnoI/xn-lib-cpp-qt/wiki))

It creates:

 1. TCP server for connecting of the model railroad operators ([API
    specification](https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer)),
 2. creates *UDP discover server* ([API
    specification](https://github.com/kmzbrnoI/hJOPserver/wiki/udpDiscover)),
 3. *PT server* ([API
    specification](https://github.com/kmzbrnoI/hJOPserver/wiki/ptServer)), which is
    used for remote diagnostics of the server.

It maintains several databases and allows operators to control model railroad
like [JOP](https://cs.wikipedia.org/wiki/Jednotné_obslužné_pracoviště) system
does on the real railroad in the Czech Republic.

The whole hJOP project targets mainly czech users, so the whole GUI is in
czech language only.

## Development

Simply clone this repository (with submodules!) and build `exe` via Delphi
compiler. CodeGear RAD Studio is nowadays free for non-commercial purposes.

Main project file: [`src/hJOPserver.dpr`](src/hJOPserver.dpr)

### Special components & libraries

* [JEDI Code Library](http://wiki.delphi-jedi.org/index.php?title=JEDI_Code_Library)

## Authors

This project was created by:

 * Jan Horacek ([jan.horacek@kmz-brno.cz](mailto:jan.horacek@kmz-brno.cz))

Do not hesitate to contact author in case of any troubles!

## License

This application is released under the [Apache License v2.0
](https://www.apache.org/licenses/LICENSE-2.0).
