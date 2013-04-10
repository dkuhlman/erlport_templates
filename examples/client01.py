import time
from erlport import Port, Protocol
from erlport import String
from lxml import etree


class ClientProtocol(Protocol):

    def handle_names(self, filename):
        filename = String(filename)
        dbg_write('handling filename: %s' % filename)
        doc = etree.parse(filename)
        root = doc.getroot()
        names = root.xpath('.//client/name/text()')
        dbg_write('returning names: %s' % (names, ))
        return names

    def handle_interestnames(self, filename):
        filename = String(filename)
        dbg_write('handling filename: %s' % filename)
        doc = etree.parse(filename)
        root = doc.getroot()
        names = root.xpath('.//client/interest/name/text()')
        dbg_write('returning names: %s' % (names, ))
        return names

    def handle_interestdescriptions(self, filename):
        filename = String(filename)
        dbg_write('handling filename: %s' % filename)
        doc = etree.parse(filename)
        root = doc.getroot()
        names = root.xpath('.//client/interest/description/text()')
        size = 0
        for name in names:
            size += len(name)
        dbg_write('returning size: %d' % (size, ))
        return size, names


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
