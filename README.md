The irresponsible guild proudly presents...

Squeel
=====

[![Build Status](https://travis-ci.org/irresponsible/squeel.svg?branch=master)](https://travis-ci.org/irresponsible/squeel)

Squeel is a simple wrapper over epgsql, adding nicer return values for records (maps) 
and error emssages.

Usage
-----

```erlang
%% 1 item was inserted
1> {ok, 1} = squeel:exec(Conn, "insert into my_table (a, b) values ($1, $2)", ["one", "two"]).
2> {ok, [Record]} = squeel:exec(Conn, "select * from my_table LIMIT 1").
3> Record.
#{"a" := <<"one">>, "b" := <<"two">>}
```

Build
-----

    $ rebar3 compile
    
Test
----

    $ ./scripts/setup_test_db.sh
    $ ./scripts/start_test_db.sh
    $ ./scripts/populate_test_db.sh
    $ rebar3 eunit

License
-------

```
Copyright (c) 2017 Antonis Kalou <kalouantonis@protonmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
