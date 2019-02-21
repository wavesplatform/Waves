@0xbd8392b832c63ee5;
using Scala = import "scala.capnp";
$Scala.package("com.wavesplatform.account.capnp");
$Scala.module("Recipient");

struct Recipient {
    chainId @0 :UInt32;

    union {
        address @1 :Data;
        alias @2 :Text;
    }
}