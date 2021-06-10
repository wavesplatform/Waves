package com.wavesplatform.crypto

import org.bouncycastle.crypto.digests.Blake2bDigest

object Blake2b256 extends BCDigest(() => new Blake2bDigest(256), 32)
