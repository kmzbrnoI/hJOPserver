#!/usr/bin/env python3

"""
Calculate elapsed distance based on hJOP state ini record.
"""

import sys
import csv
import configparser
import os


class HV:
    def __init__(self, addr: int, name: str, owner: str) -> None:
        self.address: int = addr
        self.name: str = name
        self.owner: str = owner
        self.start_forward: float = 0
        self.start_backward: float = 0
        self.end_forward: float = 0
        self.end_backward: float = 0


def hv_from_db(lokdir: str, addr: int) -> HV:
    lok_fn = os.path.join(lokdir, f'L_{addr}.2lok')
    assert os.path.isfile(lok_fn), f'Lok {addr} not present in {lokdir}!'
    lok = configparser.ConfigParser()
    lok.read(lok_fn, encoding='utf-8-sig')
    lokdict = lok[str(addr)]
    return HV(addr, lokdict['nazev'], lokdict['majitel'])


if __name__ == '__main__':
    if len(sys.argv) < 5:
        sys.stderr.write(f'Usage: {sys.argv[0]} loks-path start.ini end.ini output.csv\n')
        sys.exit(1)

    lokdir, start_fn, end_fn, out_fn = sys.argv[1:]
    hvs = {}

    start = configparser.ConfigParser()
    start.read(start_fn)
    for section in start.sections():
        addr = int(section)
        assert addr not in hvs, f'{addr} duplicity'
        hvs[addr] = hv_from_db(lokdir, addr)
        hvs[addr].start_forward = float(start[section]['najeto_vpred_metru'])
        hvs[addr].start_backward = float(start[section]['najeto_vzad_metru'])

    end = configparser.ConfigParser()
    end.read(end_fn)
    for section in end.sections():
        addr = int(section)
        if addr not in hvs:
            hvs[addr] = hv_from_db(lokdir, addr)
        hvs[addr].end_forward = float(end[section]['najeto_vpred_metru'])
        hvs[addr].end_backward = float(end[section]['najeto_vzad_metru'])

    with open(out_fn, 'w', encoding='utf-8', newline='') as out_file:
        writer = csv.writer(out_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

        writer.writerow(['adresa', 'nazev', 'majitel', 'najeto_metru_vpred', 'najeto_metru_vzad'])
        for hv in hvs.values():
            forward: float = round(hv.end_forward - hv.start_forward, 2)
            backward: float = round(hv.end_backward - hv.start_backward, 2)
            assert (forward*backward) >= 0, f'HV {hv.address} has different signs for directions!'
            if forward > 0 or backward > 0:
                writer.writerow([hv.address, hv.name, hv.owner, forward, backward])
            elif forward < 0 or backward < 0:
                print(f'Omitting {hv.address} ({hv.name}) - negative diff')
