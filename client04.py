import time
from erlport import Port, Protocol
from erlport import String, Atom
from lxml import etree


class ClientProtocol(Protocol):

    def handle_fix_request1(self, count, arg1, arg2, arg3):
        #
        # Add your processing here.
        #
        arg1_str = String(arg1)
        arg2_atom = Atom(arg2)
        dbg_write("handling request1 -- count: $s  arg1: %s arg2: %s\n" % (
            count, arg1_str, arg2_atom, ))
        #
        # Add your processing here.
        #
        result = None
        return Atom('fix_request1'), count, result

    def handle_fix_request2(self, count, arg1, arg2, arg3):
        #
        # Add your processing here.
        #
        arg1_str = String(arg1)
        arg2_atom = Atom(arg2)
        dbg_write("handling request2 -- count: $s  arg1: %s arg2: %s\n" % (
            count, arg1_str, arg2_atom, ))
        #
        # Add your processing here.
        #
        result = None
        return Atom('fix_request2'), count, result

    def handle_fix_request3(self, count, arg1, arg2, arg3):
        #
        # Add your processing here.
        #
        arg1_str = String(arg1)
        arg2_atom = Atom(arg2)
        dbg_write("handling request3 -- count: $s  arg1: %s arg2: %s\n" % (
            count, arg1_str, arg2_atom, ))
        #
        # Add your processing here.
        #
        result = None
        return Atom('fix_request3'), count, result


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
