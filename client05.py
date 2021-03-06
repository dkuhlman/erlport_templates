"""
This is the client/python side of the worker processes implemented
in request05.erl.  This template contains one handler for one specific
type of request.  You can copy and modify that handler so as to handle
other requests (commands, whatever).  The sample handler below, does
an xpath search on a specified XML file and returns the results (a list).
"""

import time
from erlport import Port, Protocol
from erlport import String, Atom
from lxml import etree


class ClientProtocol(Protocol):

    def handle_fix_request1(
            self,
            process_count,
            task_count,
            file_name,
            search_str):
        #
        # Add your processing here.
        #
        file_name = String(file_name)
        search_str = String(search_str)
##         dbg_write('1. process: %d  task: %s  file: %s  search: "%s"' % (
##             process_count, task_count, file_name, search_str, ))
        doc = etree.parse(file_name)
        root = doc.getroot()
        result = root.xpath(search_str)
##         dbg_write('2. process: %d  task: %s  file: %s  result len: %s' % (
##             process_count, task_count, file_name, len(result), ))
        return (Atom('task_response'),
                Atom('fix_request1'),
                process_count,
                task_count,
                result)


def dbg_write(msg):
    outfile = open("debug.log", 'a')
    outfile.write('-----\n%s\n%s\n' % (time.ctime(), msg, ))
    outfile.close()


if __name__ == "__main__":
    proto = ClientProtocol()
    proto.run(Port(use_stdio=True, packet=4, compressed=True))
