package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, Types}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain._

sealed trait ScriptResult
case class ScriptResultV3(ds: List[DataItem[_]], ts: List[AssetTransfer]) extends ScriptResult
case class ScriptResultV4(actions: List[CallableAction])                  extends ScriptResult
case class IncompleteResult(expr: EXPR, unusedComplexity: Int)            extends ScriptResult

object ScriptResult {
  type E[A] = Either[String, A]

  private def err[A](actual: AnyRef, version: StdLibVersion, expected: String = ""): Either[ExecutionError, A] =
    Types
      .callableReturnType(version)
      .flatMap(
        t =>
          Left(
            callableResultError(t, actual) + (if (expected.isEmpty) "" else s" instead of '$expected")
          )
      )

  private def processDataEntryV3(fields: Map[String, EVALUATED]): Either[ExecutionError, DataItem[_]] =
    (processIntEntry orElse processBoolEntry orElse processBinaryEntry orElse processStringEntry)
      .lift((fields.get(FieldNames.Key), fields.get(FieldNames.Value)))
      .fold(err[DataItem[_]](s"can't reconstruct ${FieldNames.DataEntry} from $fields", V3))(Right(_))

  private def processDataEntryV4(
      fields: Map[String, EVALUATED],
      dataType: String,
      entryHandler: PartialFunction[(Option[EVALUATED], Option[EVALUATED]), DataItem[_]]
  ): Either[ExecutionError, DataItem[_]] =
    entryHandler
      .lift((fields.get(FieldNames.Key), fields.get(FieldNames.Value)))
      .fold(err[DataItem[_]](s"can't reconstruct $dataType from $fields", V4))(Right(_))

  private val processIntEntry =
    processDataEntryPartially(
      { case CONST_LONG(v) => v },
      (k, v) => DataItem.Lng(k, v)
    )

  private val processBoolEntry =
    processDataEntryPartially(
      { case CONST_BOOLEAN(v) => v },
      (k, v) => DataItem.Bool(k, v)
    )

  private val processBinaryEntry =
    processDataEntryPartially(
      { case CONST_BYTESTR(v) => v },
      (k, v) => DataItem.Bin(k, v)
    )

  private val processStringEntry =
    processDataEntryPartially(
      { case CONST_STRING(v) => v },
      (k, v) => DataItem.Str(k, v)
    )

  private def processDataEntryPartially[V, R <: DataItem[V]](
      valueExtractor: PartialFunction[EVALUATED, V],
      constructor: (String, V) => R
  ): PartialFunction[(Option[EVALUATED], Option[EVALUATED]), R] = {
    case (Some(CONST_STRING(key)), Some(value)) if valueExtractor.isDefinedAt(value) =>
      constructor(key, valueExtractor(value))
  }

  private def processDeleteEntry(fields: Map[String, EVALUATED]): Either[String, DataItem.Delete] =
    fields.get(FieldNames.Key) match {
      case Some(CONST_STRING(key)) => Right(DataItem.Delete(key))
      case other                   => err(other, V4, FieldNames.DeleteEntry)
    }

  private def processScriptTransfer(
      ctx: EvaluationContext[Environment, Id],
      fields: Map[String, EVALUATED],
      version: StdLibVersion
  ): Either[ExecutionError, AssetTransfer] =
    (fields(FieldNames.Recipient), fields(FieldNames.Amount), fields(FieldNames.Asset)) match {
      case (other @ CaseObj(at, fields2), CONST_LONG(b), maybeToken) if ctx.environment.multiPaymentAllowed || at.name == Types.addressType.name =>
        for {
          token <- maybeToken match {
            case CONST_BYTESTR(tokenId)     => Right(Some(tokenId))
            case CaseObj(_, m) if m.isEmpty => Right(None)
            case other                      => err(s"can't reconstruct token from $other", version)
          }
          r <- if (at.name == Types.addressType.name) {
            fields2("bytes") match {
              case CONST_BYTESTR(addBytes) => Right(AssetTransfer(Address(addBytes), b, token))
              case other                   => err(s"can't reconstruct address from $other", version)
            }
          } else if (at.name == Types.aliasType.name) {
            fields2("alias") match {
              case CONST_STRING(alias) => ctx.environment.resolveAlias(alias).map(a => AssetTransfer(a, b, token))
              case other               => err(s"can't reconstruct alias from $other", version)
            }
          } else {
            err(other, version, FieldNames.ScriptTransfer)
          }
        } yield r
      case other =>
        err(other, version, FieldNames.ScriptTransfer)
    }

  private def processWriteSetV3(fields: Map[String, EVALUATED]): Either[String, List[DataItem[_]]] =
    fields(FieldNames.Data) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(tpe, fields) if tpe.name == FieldNames.DataEntry => processDataEntryV3(fields)
          case other                                                    => err(other, V3, FieldNames.DataEntry)
        }
      case other => err(other, V3, s"List(${FieldNames.Data})")
    }

  private def processTransferSetV3(ctx: EvaluationContext[Environment, Id], fields: Map[String, EVALUATED]): Either[String, List[AssetTransfer]] =
    fields(FieldNames.Transfers) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(t, fields) if t.name == FieldNames.ScriptTransfer => processScriptTransfer(ctx, fields, V3)
          case other                                                     => err(other, V3, FieldNames.TransferSet)
        }
      case other => err(other, V3, s"List(${FieldNames.Transfers})")
    }

  private def processScriptResultV3(ctx: EvaluationContext[Environment, Id], fields: Map[String, EVALUATED]): Either[String, ScriptResultV3] = {
    val writes = fields(FieldNames.ScriptWriteSet) match {
      case CaseObj(tpe, fields) if tpe.name == FieldNames.WriteSet => processWriteSetV3(fields)
      case other                                                   => err(other, V3, FieldNames.Data)
    }
    val payments = fields(FieldNames.ScriptTransferSet) match {
      case CaseObj(tpe, fields) if tpe.name == FieldNames.TransferSet => processTransferSetV3(ctx, fields)
      case other                                                      => err(other, V3, FieldNames.Transfers)
    }
    for {
      w <- writes
      p <- payments
    } yield ScriptResultV3(w, p)
  }

  private def processIssue(ctx: EvaluationContext[Environment, Id], parentId: ByteStr, fields: Map[String, EVALUATED]): Either[String, Issue] = {
    (
      fields.get(FieldNames.IssueQuantity),
      fields.get(FieldNames.IssueDecimals),
      fields.get(FieldNames.IssueName),
      fields.get(FieldNames.IssueDescription),
      fields.get(FieldNames.IssueScriptField),
      fields.get(FieldNames.IssueIsReissuable),
      fields.get(FieldNames.IssueNonce)
    ) match {
      case (
          Some(CONST_LONG(quantity)),
          Some(CONST_LONG(decimals)),
          Some(CONST_STRING(name)),
          Some(CONST_STRING(description)),
          Some(script),
          Some(CONST_BOOLEAN(isReissuable)),
          Some(CONST_LONG(nonce))
          ) =>
        if (script == unit) {
          if (0 <= decimals && decimals <= 8) {
            Right(Issue.create(None, decimals.toInt, description, isReissuable, name, quantity, nonce, parentId))
          } else {
            Left(s"Invalid decimals $decimals")
          }
        } else {
          Left("Issuing scripted asset isn't supported")
        }
      case _ => Left(s"Invalid arguments")
    }
  }

  private def processReissue(fields: Map[String, EVALUATED]): Either[String, Reissue] =
    (
      fields.get(FieldNames.ReissueAssetId),
      fields.get(FieldNames.ReissueQuantity),
      fields.get(FieldNames.ReissueIsReissuable)
    ) match {
      case (
          Some(CONST_BYTESTR(assetId)),
          Some(CONST_LONG(quantity)),
          Some(CONST_BOOLEAN(isReissuable))
          ) =>
        Right(Reissue(assetId, isReissuable, quantity))
      case other =>
        err(other, V4, FieldNames.Reissue)
    }

  private def processBurn(fields: Map[String, EVALUATED]): Either[String, Burn] =
    (fields.get(FieldNames.BurnAssetId), fields.get(FieldNames.BurnQuantity)) match {
      case (Some(CONST_BYTESTR(assetId)), Some(CONST_LONG(quantity))) =>
        Right(Burn(assetId, quantity))
      case other =>
        err(other, V4, FieldNames.Burn)
    }

  private def processSponsorFee(fields: Map[String, EVALUATED]): Either[String, SponsorFee] =
    (fields.get(FieldNames.SponsorFeeAssetId), fields.get(FieldNames.SponsorFeeMinFee)) match {
      case (Some(CONST_BYTESTR(assetId)), Some(minFeeOpt)) =>
        val minFeeValueOpt = minFeeOpt match {
          case CONST_LONG(minFee) => Right(Some(minFee))
          case `unit`             => Right(None)
          case other              => err(s"can't reconstruct ${FieldNames.SponsorFeeMinFee} from $other", V4)
        }
        minFeeValueOpt.map(v => SponsorFee(assetId, v))
      case other =>
        err(other, V4, FieldNames.SponsorFee)
    }

  private def processScriptResultV4(ctx: EvaluationContext[Environment, Id], txId: ByteStr, actions: Seq[EVALUATED]): Either[String, ScriptResultV4] =
    actions.toList
      .traverse {
        case obj @ CaseObj(actionType, fields) =>
          v4ActionHandlers
            .get(actionType.name)
            .map(_(ctx, txId, fields))
            .getOrElse(err(obj, V4))

        case other => err(other, V4)
      }
      .map(ScriptResultV4)

  private val v4ActionHandlers
      : Map[String, (EvaluationContext[Environment, Id], ByteStr, Map[String, EVALUATED]) => Either[ExecutionError, CallableAction]] =
    Map(
      FieldNames.ScriptTransfer -> ((ctx, _, a) => (processScriptTransfer(ctx, a, V4))),
      FieldNames.IntegerEntry   -> ((_, _, a) => (processDataEntryV4(a, FieldNames.IntegerEntry, processIntEntry))),
      FieldNames.BooleanEntry   -> ((_, _, a) => (processDataEntryV4(a, FieldNames.BooleanEntry, processBoolEntry))),
      FieldNames.StringEntry    -> ((_, _, a) => (processDataEntryV4(a, FieldNames.StringEntry, processStringEntry))),
      FieldNames.BinaryEntry    -> ((_, _, a) => (processDataEntryV4(a, FieldNames.BinaryEntry, processBinaryEntry))),
      FieldNames.DeleteEntry    -> ((_, _, a) => processDeleteEntry(a)),
      FieldNames.Issue          -> processIssue,
      FieldNames.Reissue        -> ((_, _, a) => processReissue(a)),
      FieldNames.Burn           -> ((_, _, a) => processBurn(a)),
      FieldNames.SponsorFee     -> ((_, _, a) => processSponsorFee(a))
    )

  def fromObj(ctx: EvaluationContext[Environment, Id], txId: ByteStr, e: EVALUATED, version: StdLibVersion): Either[ExecutionError, ScriptResult] =
    (e, version) match {
      case (CaseObj(tpe, fields), V3) =>
        tpe.name match {
          case FieldNames.WriteSet     => processWriteSetV3(fields).map(ScriptResultV3(_, List.empty))
          case FieldNames.TransferSet  => processTransferSetV3(ctx, fields).map(ScriptResultV3(List.empty, _))
          case FieldNames.ScriptResult => processScriptResultV3(ctx, fields)
          case f                       => err(f, version)
        }
      case (ARR(actions), V4 | V5) =>
        processScriptResultV4(ctx, txId, actions)

      case c => err(c.toString, version)
    }

}
