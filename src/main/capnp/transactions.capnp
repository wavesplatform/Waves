@0xbd8392b832c63ee5;
using Java = import "capnp/java.capnp";
$Java.package("com.wavesplatform.transactions.capnp");
$Java.outerClassname("Transactions");

struct Recipient {
    chainId @0 :UInt32;

    union {
        address @1 :Data;
        alias @2 :Text;
    }
}

struct Transaction {
    sender @0 :Data;
    chainId @1 :UInt8;

    fee :group {
        amount @2 :Int64;
        assetId @3 :Data;
    }

    timestamp @4 :Int64;
    version @5 :UInt8;

    data :union {
        genesis :group {
            recipient @6 :Recipient;
            amount @7 :Int64;
        }

        payment :group {
            recipient @8 :Recipient;
            amount @9 :Int64;
        }
    }
}