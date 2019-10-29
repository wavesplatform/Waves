package com.wavesplatform.lang.v1.evaluator
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, Types}
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{Burn, CallableAction, DataItem, Issue, Reissue, AssetTransfer}

sealed trait ScriptResult
case class ScriptResultV3(ds: List[DataItem[_]], ts: List[AssetTransfer]) extends ScriptResult
case class ScriptResultV4(actions: List[CallableAction]) extends ScriptResult

object ScriptResult {
  type E[A] = Either[String, A]

  private def err(actual: AnyRef, version: StdLibVersion, expected: String = ""): Either[ExecutionError, Nothing] =
    Types.callableReturnType(version)
      .flatMap(t => Left(
        callableResultError(t, actual) + (if (expected.isEmpty) "" else s" instead of '$expected")
      ))

  private def processDataEntry(dataEntryFields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, DataItem[_]] =
    (dataEntryFields.get(FieldNames.Key), dataEntryFields.get(FieldNames.Value)) match {
      case (Some(CONST_STRING(k)), Some(CONST_BOOLEAN(b))) => Right(DataItem.Bool(k, b))
      case (Some(CONST_STRING(k)), Some(CONST_STRING(b)))  => Right(DataItem.Str(k, b))
      case (Some(CONST_STRING(k)), Some(CONST_LONG(b)))    => Right(DataItem.Lng(k, b))
      case (Some(CONST_STRING(k)), Some(CONST_BYTESTR(b))) => Right(DataItem.Bin(k, b))
      case other => err(s"can't reconstruct ${FieldNames.DataEntry} from $other", version)
    }

  private def processScriptTransfer(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, AssetTransfer] =
    (fields(FieldNames.Recipient), fields(FieldNames.Amount), fields(FieldNames.Asset)) match {
      case (CaseObj(at, fields2), CONST_LONG(b), maybeToken) if at.name == Types.addressType.name =>
        for {
          token <- maybeToken match {
            case CONST_BYTESTR(tokenId) => Right(Some(tokenId))
            case CaseObj(_, m) if m.isEmpty => Right(None)
            case other => err(s"can't reconstruct token from $other", version)
          }
          r <- fields2("bytes") match {
            case CONST_BYTESTR(addBytes) => Right(AssetTransfer(Address(addBytes), b, token))
            case other => err(s"can't reconstruct address from $other", version)
          }
        } yield r
      case other =>
        err(other, version, FieldNames.ScriptTransfer)
    }

  private def processWriteSetV3(fields: Map[String, EVALUATED]): Either[String, List[DataItem[_]]] =
    fields(FieldNames.Data) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(tpe, dataEntryFields) if tpe.name == FieldNames.DataEntry => processDataEntry(dataEntryFields, V3)
          case other                                                             => err(other, V3, FieldNames.DataEntry)
        }
      case other => err(other, V3, s"List(${FieldNames.Data})")
    }

  private def processTransferSetV3(fields: Map[String, EVALUATED]): Either[String, List[AssetTransfer]] =
    fields(FieldNames.Transfers) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(t, fields) if t.name == FieldNames.ScriptTransfer => processScriptTransfer(fields, V3)
          case other                                                     => err(other, V3, FieldNames.TransferSet)
        }
      case other => err(other, V3, s"List(${FieldNames.Transfers})")
    }

  private def processScriptResultV3(fields: Map[String, EVALUATED]): Either[String, ScriptResultV3] = {
    val writes = fields(FieldNames.ScriptWriteSet) match {
      case CaseObj(tpe, fields) if tpe.name == FieldNames.WriteSet => processWriteSetV3(fields)
      case other                                                   => err(other, V3, FieldNames.Data)
    }
    val payments = fields(FieldNames.ScriptTransferSet) match {
      case CaseObj(tpe, fields) if tpe.name == FieldNames.TransferSet => processTransferSetV3(fields)
      case other                                                      => err(other, V3, FieldNames.Transfers)
    }
    for {
      w <- writes
      p <- payments
    } yield ScriptResultV3(w, p)
  }

  private def processIssue(fields: Map[String, EVALUATED]): Either[String, Issue] = ???
  private def processReissue(fields: Map[String, EVALUATED]): Either[String, Reissue] = ???
  private def processBurn(fields: Map[String, EVALUATED]): Either[String, Burn] = ???

  private def processScriptResultV4(actions: Seq[EVALUATED]): Either[String, ScriptResultV4] =
    actions.toList
      .traverse {
         case CaseObj(t, fields) if t.name == FieldNames.ScriptTransfer => processScriptTransfer(fields, V4)
         case CaseObj(t, fields) if t.name == FieldNames.DataEntry      => processDataEntry(fields, V4)
         case CaseObj(t, fields) if t.name == FieldNames.Issue          => processIssue(fields)
         case CaseObj(t, fields) if t.name == FieldNames.Reissue        => processReissue(fields)
         case CaseObj(t, fields) if t.name == FieldNames.Burn           => processBurn(fields)
         case other => err(other, V4)
      }
      .map(ScriptResultV4)


  def fromObj(e: EVALUATED, version: StdLibVersion): Either[ExecutionError, ScriptResult] =
    (e, version) match {
      case (CaseObj(tpe, fields), V3) =>
        tpe.name match {
          case FieldNames.WriteSet     => processWriteSetV3(fields).map(ScriptResultV3(_, List.empty))
          case FieldNames.TransferSet  => processTransferSetV3(fields).map(ScriptResultV3(List.empty, _))
          case FieldNames.ScriptResult => processScriptResultV3(fields)
          case f                       => err(f, version)
        }
      case (ARR(actions), V4) => processScriptResultV4(actions)

      case c => err(c.toString, version)
    }

}
