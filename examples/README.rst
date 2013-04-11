.. vim: set ft=rst:

============================
erlport_templates examples
============================


The example code in this directory was created using the templates
and the behavior in the parent directory.

Instructions on how to run them is below.


Compile the example code
==========================

You will need to compile the .erl files to .beam.  The following
should do that::

    $ erlc *.erl


Run the examples
==================

request01test.erl
-------------------

Here is an example of running test #1 inside of erl shell::

    41> c(requestor01test).
    {ok,requestor01test}
    42> requestor01test:runclient("client01.xml").
    1. Dave Kuhlman
    2. Mona Knight
    ok
    43> requestor01test:runinterest("client01.xml").
    1. birding
    2. computer_programming
    3. birding
    4. gardening
    5. volunteering
    ok


request02test.erl
-------------------

Here is an example of running test #2 inside of erl shell::

    31> P = requestor02test:start().
    #Port<0.2383>
    32> requestor02test:requestnames(P, "client01.xml").
    1. Dave Kuhlman
    2. Mona Knight
    ok
    33> requestor02test:requestinterest(P, "client01.xml").
    1. birding
    2. computer_programming
    3. birding
    4. gardening
    5. volunteering
    ok
    34> requestor02test:stop(P).
    ok


request03test.erl
-------------------

To run test #3 you can use the following in the erl shell::

    18> c(requestor03test).                                     
    {ok,requestor03test}
    19> f(P).                                                   
    ok
    20> P = requestor03test:start().                            
    #Port<0.2375>
    21> requestor03test:rpc(P, {interestnames, "client01.xml"}).
    Interests
    ----------------------
    1. birding
    2. computer_programming
    3. birding
    4. gardening
    5. volunteering
    ok
    22> requestor03test:stop(P).                                
    ok


The other tests
-----------------

In some cases there is a runnable escript file.  For example, you
can run the following::

    $ ./test04_run
    $ ./test05_run
    $ ./test06_run


