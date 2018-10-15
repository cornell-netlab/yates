
[![Build Status](https://travis-ci.org/cornell-netlab/yates.svg?branch=master)](https://travis-ci.org/cornell-netlab/yates)

[<img src= "http://www.cs.cornell.edu/~praveenk/yates/img/yates-logo.png" width=50%>](http://www.cs.cornell.edu/~praveenk/yates/yates)

## [Yet Another Traffic Engineering System](https://cornell-netlab.github.io/yates/)

YATES is a software framework that enables rapid prototyping and evaluation of traffic engineering systems. It is open source under a GNU LGPLv3 license.
## Getting started

### Installation: 
**Quick start:**


1. Install [OPAM](https://opam.ocaml.org/)

2. Switch to OCaml version 4.06.0 or greater:
    ```
    opam switch 4.06.0    
    ```        

3. Install the basic required OCaml dependencies. The list of dependencies can be generated as:
    ```
    opam install dune
    dune external-lib-deps --missing @install
    ```
    Install these dependencies using Opam. For example, to install Frenetic,
    ```
    opam install frenetic    
    ```
    
4. Build YATES
    ```
    make && make install
    ```
    
For more details and complete install, please read the [installation guide](http://www.cs.cornell.edu/~praveenk/yates/installation/).


### Using YATES
An example run that evaluates performance of ECMP with Internet2's Abilene backbone network.
```
    $ yates data/topologies/abilene.dot \                                                 
      data/demands/actual/abilene.txt data/demands/predicted/abilene.txt \
      data/hosts/abilene.hosts -ecmp
```
The [usage page](http://www.cs.cornell.edu/~praveenk/yates/tutorial/) describes how to use YATES to evaluate TE systems in more detail.

### Extending YATES
If you would like to use YATES to prototype new TE system(s), please see an example on [extending YATES](http://www.cs.cornell.edu/~praveenk/yates/extending/).



## Credits

See the [list of initial contributors here](http://www.cs.cornell.edu/~praveenk/yates/yates#members).


## License

YATES is released under the GNU Lesser General Public License, version 3.  [See here](https://github.com/cornell-netlab/yates/blob/master/LICENSE)
