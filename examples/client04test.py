import time
from erlport import Port, Protocol
from erlport import String, Atom
from lxml import etree


class ClientProtocol(Protocol):

    def handle_search(self, count, file_name, search_str):
        #
        # Add your processing here.
        #
        file_name = String(file_name)
        search_str = String(search_str)
        dbg_write('handling count: %s  file_name: %s  search_str: "%s"' % (
            count, file_name, search_str, ))
        doc = etree.parse(file_name)
        root = doc.getroot()
        result = root.xpath(search_str)
        return Atom('search'), count, result


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
