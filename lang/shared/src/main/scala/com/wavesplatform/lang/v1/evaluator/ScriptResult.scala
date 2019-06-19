package com.wavesplatform.lang.v1.evaluator
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, Types}
import com.wavesplatform.lang.v1.traits.domain.DataItem
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address

case class ScriptResult(ds: List[DataItem[_]], ts: List[(Address, Long, Option[ByteStr])])

object ScriptResult {
  type E[A] = Either[String, A]

  private def err(actual: AnyRef, expected: String = "") =
    Left(
      s"${FieldNames.Error}, " +
        s"but got '$actual'${if (expected.isEmpty) "" else s"instead of '$expected"}")

  private def processWriteSet(c: CaseObj): Either[String, List[DataItem[_]]] = c match {
    case CaseObj(_, fields) =>
      fields(FieldNames.Data) match {
        case ARR(xs) =>
          val l: Vector[E[DataItem[_]]] = xs.map {
            case CaseObj(tpe, dataEntryFields) if tpe.name == FieldNames.DataEntry =>
              (dataEntryFields.get(FieldNames.Key), dataEntryFields.get(FieldNames.Value)) match {
                case (Some(CONST_STRING(k)), Some(CONST_BOOLEAN(b))) => Right(DataItem.Bool(k, b))
                case (Some(CONST_STRING(k)), Some(CONST_STRING(b)))  => Right(DataItem.Str(k, b))
                case (Some(CONST_STRING(k)), Some(CONST_LONG(b)))    => Right(DataItem.Lng(k, b))
                case (Some(CONST_STRING(k)), Some(CONST_BYTESTR(b))) => Right(DataItem.Bin(k, b))
                case other                                           => err(s"can't reconstruct ${FieldNames.DataEntry} from $other")
              }
            case other => err(other, FieldNames.DataEntry)
          }.toVector
          l.sequence.map(_.toList)
        case other => err(other, s"List(${FieldNames.Data})")
      }
    case other => err(other)
  }
  private def processTransferSet(c: CaseObj): Either[String, List[(Address, Long, Option[ByteStr])]] = c match {
    case CaseObj(tpe, fields) =>
      fields(FieldNames.Transfers) match {
        case ARR(xs) =>
          val l: Vector[E[(Address, Long, Option[ByteStr])]] = xs.map {
            case CaseObj(t, scriptTransferFields) if t.name == FieldNames.ScriptTransfer =>
              (scriptTransferFields(FieldNames.Recipient), scriptTransferFields(FieldNames.Amount), scriptTransferFields(FieldNames.Asset)) match {
                case (CaseObj(at, fields2), CONST_LONG(b), maybeToken) if at.name == Types.addressType.name =>
                  for {
                    token <- maybeToken match {
                      case CONST_BYTESTR(tokenId)     => Right(Some(tokenId))
                      case CaseObj(_, m) if m.isEmpty => Right(None)
                      case other                      => err(s"can't reconstruct token from $other")
                    }
                    r <- fields2("bytes") match {
                      case CONST_BYTESTR(addBytes) => Right((Address(addBytes), b, token))
                      case other                   => err(s"can't reconstruct address from $other")
                    }
                  } yield r
                case other =>
                  err(other, FieldNames.ScriptTransfer)
              }
            case other => err(other, FieldNames.TransferSet)
          }.toVector
          l.sequence.map(_.toList)
        case other => err(other, s"List(${FieldNames.Transfers})")
      }
    case other => err(other)
  }

  private def processContractSet(c: CaseObj) = c match {
    case CaseObj(_, fields) =>
      val writes = fields(FieldNames.ScriptWriteSet) match {
        case c @ CaseObj(tpe, _) if tpe.name == FieldNames.WriteSet => processWriteSet(c)
        case other                                                  => err(other, FieldNames.Data)
      }
      val payments = fields(FieldNames.ScriptTransferSet) match {
        case c @ CaseObj(tpe, _) if tpe.name == FieldNames.TransferSet => processTransferSet(c)
        case other                                                     => err(other, FieldNames.Transfers)
      }
      for {
        w <- writes
        p <- payments
      } yield ScriptResult(w, p)
  }

  def fromObj(e: EVALUATED): Either[ExecutionError, ScriptResult] =
    e match {
      case c @ CaseObj(tpe, _) =>
        tpe.name match {
          case FieldNames.WriteSet       => processWriteSet(c).map(ScriptResult(_, List.empty))
          case FieldNames.TransferSet    => processTransferSet(c).map(ScriptResult(List.empty, _))
          case FieldNames.ScriptResult => processContractSet(c)
          case f                         => err(f)
        }
      case c => err(c.toString)
    }

}
