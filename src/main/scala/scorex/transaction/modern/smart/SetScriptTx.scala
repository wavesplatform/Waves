package scorex.transaction.modern.smart

import com.google.common.primitives.Bytes
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.Deser
import scorex.transaction.base.SetScriptTxBase
import scorex.transaction.modern.{ModernTransaction, TxData, TxHeader}
import scorex.transaction.smart.script.{Script, ScriptReader}
import scorex.transaction.validation.ValidationError
import scorex.transaction.{AssetId, ChainSpecific, Proofs, TransactionParser}

import scala.util.{Failure, Success, Try}

final case class SetScriptPayload(chainId: Byte, script: Option[Script]) extends TxData {
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(chainId),
      Deser.serializeOption(script)(s => s.bytes().arr),
    )
  }
  override val json: Coeval[JsObject] = Coeval.evalOnce {
    Json.obj("script" -> script.map(_.bytes()))
  }
}

final case class SetScriptTx(header: TxHeader, payload: SetScriptPayload, proofs: Proofs)
  extends ModernTransaction(SetScriptTx)
    with SetScriptTxBase
    with ChainSpecific {
  override def assetFee: (Option[AssetId], Long) = (None, header.fee)

  override val script: Option[Script] = payload.script
  override val chainId: Byte = payload.chainId
}

object SetScriptTx extends TransactionParser.Modern[SetScriptTx, SetScriptPayload] {
  override val typeId: Byte = 13

  override val supportedVersions: Set[Byte] = Set(3)

  override def create(header: TxHeader, data: SetScriptPayload, proofs: Proofs): Try[SetScriptTx] =
    Try(SetScriptTx(header, data, proofs))

  override def parseTxData(version: Byte, bytes: Array[Byte]): Try[(SetScriptPayload, Int)] = {
    Try {
      val chainId = bytes(0)
      val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
        Deser.parseOption(bytes, 1)(ScriptReader.fromBytes)
      val scriptEiOpt = scriptOptEi match {
        case None            => Right(None)
        case Some(Right(sc)) => Right(Some(sc))
        case Some(Left(err)) => Left(err)
      }

      scriptEiOpt
        .fold(
          err => Failure(new Exception(err.toString)),
          scrOpt => Success((SetScriptPayload(chainId, scrOpt), scriptEnd))
        )
    }.flatten
  }
}
