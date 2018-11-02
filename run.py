import argparse
from system import System

def main(rom_path):
    sys = System(rom_path)
    sys.start()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='NES Emulator')
    parser.add_argument('--rom',
                        metavar='rom',
                        type=str,
                        help='path to nes rom')
    args = parser.parse_args()
    main(args.rom)