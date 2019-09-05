package com.wavesplatform.lang.v1.repl.model

import java.nio.ByteBuffer

import com.wavesplatform.common.utils.Base58
import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{Blake2bDigest, KeccakDigest, SHA256Digest}

case class Account(publicKey: Array[Byte]) {
  def address(chainId: Byte): Array[Byte] = Account.address(publicKey, chainId)
}

object Account {
  implicit val decoder: Decoder[Account] = (c: HCursor) => Right {
    val str = c.value.asString.get
    val pk = Base58.decode(str)
    Account(pk)
  }

  private val BLAKE2B256 = new ThreadLocal[Digest]
  private val KECCAK256 = new ThreadLocal[Digest]
  private val SHA256 = new ThreadLocal[Digest]

  private def address(publicKey: Array[Byte], chainId: Byte): Array[Byte] = {
    val buf = ByteBuffer.allocate(26)
    val hash = secureHash(publicKey, 0, publicKey.length)
    buf.put(1.toByte).put(chainId.toByte).put(hash, 0, 20)
    val checksum = secureHash(buf.array, 0, 22)
    buf.put(checksum, 0, 4)
    buf.array
  }

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