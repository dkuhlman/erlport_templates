import time
from erlport import Port, Protocol
from erlport import String, Atom
from lxml import etree


class ClientProtocol(Protocol):

    def handle_name_address(self, filename):
        filename = String(filename)
        dbg_write('handling filename: %s' % filename)
        doc = etree.parse(filename)
        root = doc.getroot()
        clients = root.xpath('./client')
        result = []
        for client in clients:
            name = client.xpath('./name/text()')
            if name:
                name = name[0]
            address = client.xpath('./address/text()')
            if address:
                address = address[0]
            result.append((name, address))
        return Atom('name_address'), result

    def handle_name_interest(self, filename):
        filename = String(filename)
        dbg_write('handling filename: %s' % filename)
        doc = etree.parse(filename)
        root = doc.getroot()
        clients = root.xpath('./client')
        result = []
        for client in clients:
            name = client.xpath('./name/text()')
            if name:
                name = name[0]
                interests = client.xpath('./interest/name/text()')
                if interests:
                    result.append((name, interests))
        return Atom('name_interest'), result


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
