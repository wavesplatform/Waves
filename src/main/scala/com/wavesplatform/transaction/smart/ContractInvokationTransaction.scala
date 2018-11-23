package com.wavesplatform.transaction.smart

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.crypto.KeyLength
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.{ByteStr, _}
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.Json

import scala.util.{Failure, Success, Try}

case class ContractInvokationTransaction private (version: Byte,
                                                  chainId: Byte,
                                                  sender: PublicKeyAccount,
                                                  function: String,
                                                  args: List[ContractInvokationTransaction.Arg],
                                                  fee: Long,
                                                  timestamp: Long,
                                                  proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = ContractInvokationTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      Deser.serializeArray(function.getBytes),
      Shorts.toByteArray(args.size.toShort),
      Bytes.concat(args.map(_.bytes): _*),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val json = Coeval.evalOnce(
    jsonBase()
      ++ Json.obj("version" -> version, "function" -> function, "args" -> args.map(_.toString)))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object ContractInvokationTransaction extends TransactionParserFor[ContractInvokationTransaction] with TransactionParser.MultipleVersions {

  sealed trait Arg {
    def bytes: Array[Byte]
  }

  object Arg {
    private def deser(start: Int, a: Array[Byte]): (Arg, Int) = a(start) match {
      case Serde.E_LONG  => (I(Longs.fromByteArray(a.drop(start + 1))), start + 3)
      case Serde.E_TRUE  => (Bool(true), start + 1)
      case Serde.E_FALSE => (Bool(false), start + 1)
      case Serde.E_BYTES =>
        val (a, e) = Deser.parseArraySize(a, start + 1)
        (Arr(ByteStr(a)), e)
      case Serde.E_STRING => ???
    }

    def deserArgs(a: Array[Byte]): (List[Arg], Int) = {
      val amt = Shorts.fromByteArray(a.slice(0, 2))
      def aux(yet: Int, pos: Int): (List[Arg], Int) = {
        if (yet == 0) (List.empty, pos)
        else {
          val (parsed, offset) = deser(pos, a)
          val (l, f)           = aux(yet - 1, offset)
          (parsed +: l, f)
        }
      }
      aux(amt, 2)
    }
  }
  case class I(i: Long)       extends Arg { lazy val bytes = Serde.E_LONG +: Longs.toByteArray(i)               }
  case class Bool(b: Boolean) extends Arg { lazy val bytes = Array(if (b) Serde.E_TRUE else Serde.E_FALSE)      }
  case class Str(s: String)   extends Arg { lazy val bytes = Serde.E_STRING +: Deser.serializeArray(s.getBytes) }
  case class Arr(b: ByteStr)  extends Arg { lazy val bytes = Serde.E_BYTES +: Deser.serializeArray(b.arr)       }

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  private def networkByte = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId         = bytes(0)
      val sender          = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val (func, funcEnd) = Deser.parseArraySize(bytes, KeyLength + 1)
      val (args, argsEnd) = Arg.deserArgs(bytes.drop(funcEnd))


      val fee       = Longs.fromByteArray(bytes.slice(argsEnd, argsEnd + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(argsEnd + 8, argsEnd + 16))
      (for {
        _         <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        proofs    <- Proofs.fromBytes(bytes.drop(argsEnd + 16))
        tx        <- create(version, sender, new String(func, StandardCharsets.UTF_8), args, fee, timestamp, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             function: String,
             args: List[ContractInvokationTransaction.Arg],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
    } yield new ContractInvokationTransaction(version, networkByte, sender, function,args, fee, timestamp, proofs)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             function: String,
             args: List[ContractInvokationTransaction.Arg],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, sender, function,args, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 function: String,
                 args: List[ContractInvokationTransaction.Arg],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, sender, function,args, fee, timestamp, sender)
}
