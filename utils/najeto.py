#!/usr/bin/env python3

"""
Calculate elapsed distance based on hJOP HV csv output.
"""

import sys
import csv


class HV:
    def __init__(self, addr: int, name: str, owner: str) -> None:
        self.address: int = addr
        self.name: str = name
        self.owner: str = owner
        self.start_forward: float = 0
        self.start_backward: float = 0
        self.end_forward: float = 0
        self.end_backward: float = 0


if __name__ == '__main__':
    if len(sys.argv) < 4:
        sys.stderr.write(f'Usage: {sys.argv[0]} start.csv end.csv output.csv\n')
        sys.exit(1)

    start_fn, end_fn, out_fn = sys.argv[1:]
    hvs = {}

    with open(start_fn, encoding='cp1250') as start_file:
        start_reader = csv.reader(start_file, delimiter=';')

        for i, line in enumerate(start_reader):
            if i == 0:
                continue
            addr, name, owner, forward, backward = int(line[0]), line[1], line[2], float(line[3]), float(line[4])
            assert addr not in hvs, f'{addr} duplicity'
            hvs[addr] = HV(addr, name, owner)
            hvs[addr].start_forward = forward
            hvs[addr].start_backward = backward

    with open(end_fn, encoding='utf8') as end_file:
        end_reader = csv.reader(end_file, delimiter=',')

        for i, line in enumerate(end_reader):
            if i == 0:
                continue
            addr, name, owner, forward, backward = int(line[0]), line[1], line[2], float(line[3]), float(line[4])

            if addr in hvs:
                if hvs[addr].name != name:
                    print(f'Warning: {addr}: name[begin] = {hvs[addr].name} != {name} = name[end]')
                if hvs[addr].owner != owner:
                    print(f'Warning: {addr}: owner[begin] = {hvs[addr].owner} != {owner} = owner[end]')
                hvs[addr].end_forward = forward
                hvs[addr].end_backward = backward
            else:
                hvs[addr] = HV(addr, name, owner)
                hvs[addr].end_forward = forward
                hvs[addr].end_backward = backward

    with open(out_fn, 'w', encoding='utf-8', newline='') as out_file:
        writer = csv.writer(out_file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

        writer.writerow(['adresa', 'nazev', 'majitel', 'najeto_metru_vpred', 'najeto_metru_vzad'])
        for hv in hvs.values():
            forward: float = round(hv.end_forward - hv.start_forward, 2)
            backward: float = round(hv.end_backward - hv.start_backward, 2)
            assert (forward*backward) >= 0, f'HV {hv.address} has different signs for directions!'
            if forward > 0 and backward > 0:
                writer.writerow([hv.address, hv.name, hv.owner, forward, backward])
            elif forward < 0 and backward < 0:
                print(f'Omitting {hv.address} ({hv.name}) - negative diff')
