package com.wavesplatform.transaction.assets

import cats.data.State
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.{Script, ScriptReader}

import scala.util.{Failure, Success, Try}

case class SetAssetScriptTransaction private (version: Byte,
                                              chainId: Byte,
                                              sender: PublicKeyAccount,
                                              assetId: ByteStr,
                                              script: Option[Script],
                                              fee: Long,
                                              timestamp: Long,
                                              proofs: Proofs)
    extends FastHashId
    with VersionedTransaction
    with ChainSpecific {

  override val builder: TransactionParser        = SetAssetScriptTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version" -> version,
      "chainId" -> chainId,
      "assetId" -> assetId.base58,
      "script"  -> script.map(_.bytes().base64)
    ))
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
      Deser.serializeOption(script)(s => s.bytes().arr)
    ))
  override val bytes: Coeval[Array[Byte]]    = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
}

object SetAssetScriptTransaction extends TransactionParserFor[SetAssetScriptTransaction] with TransactionParser.MultipleVersions {

  val typeId: Byte                          = 15
  override val supportedVersions: Set[Byte] = Set(1)

  private def currentChainId = AddressScheme.current.chainId

  def create(
      version: Byte,
      chainId: Byte,
      sender: PublicKeyAccount,
      assetId: ByteStr,
      script: Option[Script],
      fee: Long,
      timestamp: Long,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(chainId == currentChainId,
                       (),
                       ValidationError.GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
    } yield SetAssetScriptTransaction(version, chainId, sender, assetId, script, fee, timestamp, proofs)

  }

  def signed(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, chainId, sender, assetId, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  override def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] = {
    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }
    def readUnsized[T](f: (Array[Byte], Int) => (T, Int)): State[Int, T] = State { from =>
      val (v, end) = f(bytes, from);
      (end, v)
    }
    def readEnd[T](f: Array[Byte] => T): State[Int, T] = State { from =>
      (from, f(bytes.drop(from)))
    }

    Try {
      val makeTransaction = for {
        chainId   <- readByte
        sender    <- read(PublicKeyAccount.apply, KeyLength)
        assetId   <- read(ByteStr.apply, AssetIdLength)
        fee       <- read(Longs.fromByteArray _, 8)
        timestamp <- read(Longs.fromByteArray _, 8)
        scriptOrE <- readUnsized((b: Array[Byte], p: Int) => Deser.parseOption(b, p)(ScriptReader.fromBytes))
        proofs    <- readEnd(Proofs.fromBytes)
      } yield {
        (scriptOrE match {
          case Some(Left(err)) => Left(err)
          case Some(Right(s))  => Right(Some(s))
          case None            => Right(None)
        }).flatMap(script => create(version, chainId, sender, assetId, script, fee, timestamp, proofs.right.get))
          .fold(left => Failure(new Exception(left.toString)), right => Success(right))
      }
      makeTransaction.run(0).value._2
    }.flatten
  }

}
