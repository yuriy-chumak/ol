Please, download source data from the http://yann.lecun.com/exdb/mnist/

This example requires external library "libol-ann" (a c library that does fast math). You can build it manually or install using 'kiss' package manager.
Please refer [this link](https://github.com/yuriy-chumak/libol-ann/) for further steps.

PREPARE
-------

* Download mnist sample files (train-images-idx3-ubyte.gz, train-labels-idx1-ubyte.gz, t10k-images-idx3-ubyte.gz, and t10k-labels-idx1-ubyte.gz) from http://yann.lecun.com/exdb/mnist/ (don't unpack, just download).
* Change the database path in mnist-check.lisp and mnist-train.lisp from "/media/uri/1TB/DATA/mnist/" to path to downloaded files.


USAGE
-----

1. Run mnist-train.lisp, wait, press LMB to dump an ann state ("syn0" and "syn1" files). Close mnist-train.lisp.
2. Run mnist-check.lisp, press LMB to test new randrom image.
