import time
from erlport import Port, Protocol
from erlport import String, Atom


class ClientProtocol(Protocol):

    def handle_fix_request1(self, arg1):
        #
        # Add your processing here.
        #
        arg1 = String(arg1)
        dbg_write('handling arg1: %s' % arg1)
        result = [arg1.upper(), ]
        return Atom('fix_request1'), result

    def handle_fix_request2(self, arg1):
        #
        # Add your processing here.
        #
        arg1 = String(arg1)
        dbg_write('handling arg1: %s' % arg1)
        result = [arg1.upper(), arg1.upper(), ]
        return Atom('fix_request2'), result

    def handle_fix_request3(self, arg1):
        #
        # Add your processing here.
        #
        arg1 = String(arg1)
        dbg_write('handling arg1: %s' % arg1)
        result = [arg1.upper(), arg1.upper(), arg1.upper(), ]
        return Atom('fix_request3'), result


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
