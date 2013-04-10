.. vim: set ft=rst:

=====================================
Erlang + Erlport + Python templates
=====================================

:author: Dave Kuhlman
:address: dkuhlman@rexx.com
    http://www.rexx.com/~dkuhlman

:revision: 1.0a                                                                     

:date: |date|

.. |date| date:: %B %d, %Y             

:copyright: Copyright (c) 2013 Dave Kuhlman. This documentation
    and the software it describes is covered by The MIT License:
    http://www.opensource.org/licenses/mit-license.php.

:abstract: This document describes a set of templates that can be
    used to ease the task of creating Erlang/erlport/Python modules.

.. sectnum::    :depth: 4

.. contents::
    :depth: 4


Description
=============

This package contains a set of templates that can be used to
interface Erlang to Python using Erlport.

Using one pair of these files (requestor + client), you should be
able to quickly create a harness for the Python processing that you
want to request from within an Erlang module.

For information about Erlport, see:
https://github.com/hdima/erlport.git


Instructions
==============

1. Examine the requestors (``requestorN.erl``).  Choose the style of
   interface that suits your needs.

2. Copy and rename the requestor that your have chosen and the
   corresponding client: client01.py for requestor01.py, client02,py
   for requestor02.py, etc.

3. Edit the requestor and the client:

   - In the requestor, change the module name.

   - In both the requestor and client, change all variable names and
     constants that are prefixed with "fix\_".

   - In the client, add the Python processing that you wish to
     perform in response to each different request.  Add new
     handlers or delete unneeded ones.  The handler should return
     whatever data structure that you want to use to pass data back
     to the requestor, for example, you can use Python lists and
     tuples, which will be converted into Erlang lists and tuples.
     To return an Erlang atom, use ``erlport.Atom('my_atom_name')``.

   - In the requestor, add the processing that you wish to perform
     to the data returned by the client.

4. Compile the requestor for Erlang.

5. Test your code.  For example::

       1> c(my_requestor).
       2> P = my_requestor:start()
       3> my_requestor:rpc(P, {get_value, "some_file.xml"}).
       o
       o
       o
       9> my_requestor:quit(P).

Requests
----------

A word about the requests and their format.  These templates assume
that we need a way to communicate several different kinds of
requests to the Python process.  So, we send a tuple in which the
first item (an atom) specifies the kind of request and in which the
remaining arguments are arguments to the Python handler.  So, for
example, if the request is::

    {op_code_1, arg_1, arg_2, arg_3}

then the Python handler (method) should have the following header::

    def handle_op_code_1(self, arg1, arg2, arg3):


The specific templates
========================

requestor01 and client01
--------------------------

This template creates a new port and a new Python process for each
request.  Remember that Python processes are "heavy weight": the are
operating system processes that show up in the Task Manager on MS
Windows and in the System Monitor on Linux.

Use this template if your requests to Python are few and where you
want those Python/OS processes to disappear between requests.


requestor02 and client02
--------------------------

This one creates opens a port (and creates the associated Python
process), then returns that port to the caller.  You can then call
one of several functions in the module to make separate and
different requests using the same Python process.  This template
reuse the same Python process so the use of that process is
synchronous.  However, you can open more that one port and use them
concurrently.  And, you can close the port, destroying the Python
process, when you are finished with it.

Use this template when you need to make a number of requests to your
Python module in sequence and do *not* want to re-create the Python
process for each request.


requestor03 and client03
--------------------------

This one is similar to requestor02/client02 in that the start/0
function creates and gives you a port/process that you can use
repeatedly (sequentially), then later destroy when you no longer
need it.  However, the user interface uses the rpc request style.
(See Joe Armstrong, "Programming Erlang" for more on the ``rpc``
style of interface.)  Using this template, you would interface with
your Python process by calling the ``rpc`` function passing in the
port and an Erlang term that specifies the request.

This template might be particularly well suited in an application
where a sequence of tasks are specified in consistently structured
data objects.


requestor04 and client04
--------------------------

This template creates a set of Python processes of a fixed size.  It
then runs the requests from a list of requests on those processes,
starting up a new task on an existing process when that process
completes its previous task.

Use this template when the processing on the Python side is likely
to be intensive and compute bound, but the processing on the Erlang
side (e.g. saving the results) is likely to be light weight.
Applications built with this template should be able to keep
multiple cores busy when most of the work is on the Python side.

Also consider using this template when the processing on the Erlang
side needs coordination, for example, when you need to merge results
from multiple processes on the Python side, when you need to
compare the results from multiple processes on the Python side
(e.g. to select an optimum of some kind, etc.


requestor05 and client05
--------------------------

This template creates a fixed size set of processes and runs
(distributes) tasks across those processes.  Each of these processes
is composed of an Erlang process and an associated Erlport Python
(OS) process.

Use this template when the processing on both the Erlang side and
the Python side of the port is likely to be intensive (or maybe even
when the processing on the Erlang side will be intensive and the
processing on the Python side is not).  Since this template treats
the Erlang code and the Python code as part of the same (Erlang)
process and since it parallelizes those process pairs, applications
built with this template will keep multiple cores busy even when
some (or even most) of the heavy processing is on the Erlang side of
that pair.

Remember that, since the Erlang processing is being done in separate
processes, these processes can communicate with each other or
with the supervisor process that started them only through messages.


requestor06 and client06 -- An Erlang behavior
------------------------------------------------

This template creates a fixed size set of processes and keeps them
in a "pool" of processes.  Whenever a task is requested, a process
is taken out of the pool and given that task to process.  When the
Python process is complete (specifically, when it returns the atom
"finisted", the process is put back in the pool and made available.

This "template" is implemented with an Erlang behavior.  This
behavior enables you to implement the erang process controller with
a very small number of lines of code.

An example of an Erlang process controller that uses this behavior
is provided by ``requestor06test.erl``.

You can try out this template/behavior by running ``test06_run``,
for example::

    $ ./test06_run 3 5

By default, this example uses the file ``client01.xml`` for input.


Possible, needed additions and extensions
===========================================

- Support for Python processes where the processing on the Python
  side needs to return results multiple times or needs to return its
  results in a sequence of chunks (a sequence of messages), rather
  than in a single chunk (message).

- Enable user to feed tasks using a function (generator?) rather
  than a list, so that tasks can be generated dynamically, perhaps
  even in response to previous computed results.
