package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{Alias, PublicKeyAccount}
import com.wavesplatform.serialization.Deser
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519._

import scala.util.{Failure, Success, Try}

trait CreateAliasTransaction extends ProvenTransaction with VersionedTransaction {
  def alias: Alias
  def fee: Long
  def timestamp: Long

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"   -> version,
      "alias"     -> alias.name,
      "fee"       -> fee,
      "timestamp" -> timestamp
    ))

  val baseBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      Deser.serializeArray(alias.bytes.arr),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
}

object CreateAliasTransaction {
  val typeId: Byte = 10

  def parseBase(start: Int, bytes: Array[Byte]): Try[(PublicKeyAccount, Alias, Long, Long, Int)] = {
    for {
      sender <- Try(PublicKeyAccount(bytes.slice(start, start + KeyLength)))
      (aliasBytes, aliasEnd) = Deser.parseArraySize(bytes, start + KeyLength)
      alias     <- Alias.fromBytes(aliasBytes).fold(err => Failure(new Exception(err.toString)), Success.apply)
      fee       <- Try(Longs.fromByteArray(bytes.slice(aliasEnd, aliasEnd + 8)))
      timestamp <- Try(Longs.fromByteArray(bytes.slice(aliasEnd + 8, aliasEnd + 16)))
    } yield (sender, alias, fee, timestamp, aliasEnd + 16)
  }
}
