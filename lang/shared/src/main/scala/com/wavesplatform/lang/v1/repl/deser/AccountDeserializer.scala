package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonDeserializer
import java.nio.ByteBuffer

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.v1.repl.model.Account
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{Blake2bDigest, KeccakDigest, SHA256Digest}

