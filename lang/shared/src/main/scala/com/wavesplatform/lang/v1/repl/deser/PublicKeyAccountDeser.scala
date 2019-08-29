package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonProcessingException
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonDeserializer
import java.io.IOException
import java.nio.ByteBuffer

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.v1.repl.model.Account
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{Blake2bDigest, KeccakDigest, SHA256Digest}

class PublicKeyAccountDeser(chainId: Byte) extends JsonDeserializer[Account] {
  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext) =
    Account(
      jsonParser.getValueAsString,
      chainId,
      Base58.encode(address())
    )

  private def address(publicKey: Array[Byte], chainId: Byte) = {
    val buf = ByteBuffer.allocate(26)
    val hash = secureHash(publicKey, 0, publicKey.length)
    buf.put(1.toByte).put(chainId.toByte).put(hash, 0, 20)
    val checksum = secureHash(buf.array, 0, 22)
    buf.put(checksum, 0, 4)
    buf.array
  }

  private val BLAKE2B256 = new ThreadLocal[Digest]
  private val KECCAK256 = new ThreadLocal[Digest]
  private val SHA256 = new ThreadLocal[Digest]


  private def secureHash(message: Array[Byte], ofs: Int, len: Int): Array[Byte] = {
    val blake2b = hash(message, ofs, len, BLAKE2B256)
    hash(blake2b, 0, blake2b.length, KECCAK256)
  }

  private def hash(message: Array[Byte], ofs: Int, len: Int, alg: ThreadLocal[Digest]) = {
    val digest = getDigest(alg)
    val result = new Array[Byte](digest.getDigestSize)
    digest.update(message, ofs, len)
    digest.doFinal(result, 0)
    result
  }

  private def getDigest(cache: ThreadLocal[Digest]) = {
    var digest = cache.get
    if (digest == null) {
      if (cache eq BLAKE2B256) digest = new Blake2bDigest(256)
      else if (cache eq KECCAK256) digest = new KeccakDigest(256)
      else if (cache eq SHA256) digest = new SHA256Digest
      cache.set(digest)
    }
    digest
  }


}
