Erlang Game Server
==============

### Requirements
1. Git
2. Erlang R16B03
3. Ruby   1.9.3
4. Rails  3.2.9

### Setup
Checkout game_server

```
git clone https://github.com/mafei198/game_server.git
```

Create Your Game Server

```
$ cd game_server
$ ./setup /path/to/your/server
```

Compile Game Server

```
$ cd /path/to/your/server
$ make
$ make test
```

Start Game Server

```
$ make console
```

More commands please see the Makefile.

## License
Erlang Game Server is under The MIT License (MIT)

Copyright (c) 2014-2024
Savin Max <mafei.198@gmail.com>

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
