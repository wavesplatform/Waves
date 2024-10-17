package com.wavesplatform.lang.v1.evaluator

import cats.Id
import cats.implicits.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.*
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4, V5}
import com.wavesplatform.lang.v1.compiler.ScriptResultSource.CallableFunction
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, Types}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}

sealed trait ScriptResult {
  def returnedValue: EVALUATED                                                    = unit
  def invokes: Seq[(Address, String, Seq[EVALUATED], Seq[CaseObj], ScriptResult)] = Nil
  def unusedComplexity: Int
  def actions: List[CallableAction]
}

case class ScriptResultV3(ds: List[DataItem[_]], ts: List[AssetTransfer], unusedComplexity: Int) extends ScriptResult {
  override lazy val actions: List[CallableAction] = ds ++ ts
}

case class ScriptResultV4(
    actions: List[CallableAction],
    unusedComplexity: Int,
    override val returnedValue: EVALUATED = unit
) extends ScriptResult

case class IncompleteResult(expr: EXPR, unusedComplexity: Int) extends ScriptResult {
  override val actions: List[CallableAction] = Nil
}

object ScriptResult {
  type ActionInput    = (EvaluationContext[Environment, Id], ByteStr, Map[String, EVALUATED])
  type ActionResult   = Either[ExecutionError, CallableAction]
  type ActionHandlers = Map[String, ActionInput => ActionResult]

  private def err[A](actual: AnyRef, version: StdLibVersion, expected: String = ""): Either[ExecutionError, A] =
    Types
      .callableReturnType(version)
      .leftMap(CommonError(_))
      .flatMap(t =>
        Left(
          callableResultError(t, actual, CallableFunction) + (if (expected.isEmpty) "" else s" instead of $expected")
        )
      )

  private def processDataEntryV3(fields: Map[String, EVALUATED]): Either[ExecutionError, DataItem[_]] =
    (processIntEntry orElse processBoolEntry orElse processBinaryEntry orElse processStringEntry)
      .lift((fields.get(FieldNames.Key), fields.get(FieldNames.Value)))
      .fold(err[DataItem[_]](s"can't reconstruct ${FieldNames.DataEntry} from $fields", V3))(Right(_))

  private def processDataEntry(
      fields: Map[String, EVALUATED],
      dataType: String,
      entryHandler: PartialFunction[(Option[EVALUATED], Option[EVALUATED]), DataItem[_]],
      version: StdLibVersion
  ): Either[ExecutionError, DataItem[_]] =
    entryHandler
      .lift((fields.get(FieldNames.Key), fields.get(FieldNames.Value)))
      .fold(err[DataItem[_]](s"can't reconstruct $dataType from $fields", version))(Right(_))

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

  private def processDeleteEntry(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, DataItem.Delete] =
    fields.get(FieldNames.Key) match {
      case Some(CONST_STRING(key)) => Right(DataItem.Delete(key))
      case other                   => err(other, version, FieldNames.DeleteEntry)
    }

  private def processScriptTransfer(
      ctx: EvaluationContext[Environment, Id],
      fields: Map[String, EVALUATED],
      version: StdLibVersion
  ): Either[ExecutionError, AssetTransfer] =
    (fields(FieldNames.Recipient), fields(FieldNames.Amount), fields(FieldNames.Asset)) match {
      case (recipient: CaseObj, CONST_LONG(b), maybeToken) =>
        for {
          token <- maybeToken match {
            case CONST_BYTESTR(tokenId)     => Right(Some(tokenId))
            case CaseObj(_, m) if m.isEmpty => Right(None)
            case other                      => err(s"can't reconstruct token from $other", version)
          }
          recipient <- processRecipient(recipient, ctx, version)
          address <- recipient match {
            case a: Address  => Right(a)
            case Alias(name) => ctx.environment.resolveAlias(name).leftMap(CommonError(_))
          }
        } yield AssetTransfer(address, recipient, b, token)
      case other =>
        err(other, version, FieldNames.ScriptTransfer)
    }

  private def processRecipient(obj: CaseObj, ctx: EvaluationContext[Environment, Id], version: StdLibVersion): Either[ExecutionError, Recipient] =
    if (obj.caseType.name == Types.addressType.name)
      obj.fields("bytes") match {
        case CONST_BYTESTR(addBytes) => Right(Address(addBytes))
        case other                   => err(s"can't reconstruct address from $other", version)
      }
    else if (obj.caseType.name == Types.aliasType.name && ctx.environment.multiPaymentAllowed)
      obj.fields("alias") match {
        case CONST_STRING(alias) => Right(Alias(alias))
        case other               => err(s"can't reconstruct alias from $other", version)
      }
    else
      err(obj, version, FieldNames.Recipient)

  private def processWriteSetV3(fields: Map[String, EVALUATED]): Either[ExecutionError, List[DataItem[_]]] =
    fields(FieldNames.Data) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(tpe, fields) if tpe.name == FieldNames.DataEntry => processDataEntryV3(fields)
          case other                                                    => err(other, V3, FieldNames.DataEntry)
        }
      case other => err(other, V3, s"List(${FieldNames.Data})")
    }

  private def processTransferSetV3(
      ctx: EvaluationContext[Environment, Id],
      fields: Map[String, EVALUATED]
  ): Either[ExecutionError, List[AssetTransfer]] =
    fields(FieldNames.Transfers) match {
      case ARR(xs) =>
        xs.toList.traverse {
          case CaseObj(t, fields) if t.name == FieldNames.ScriptTransfer => processScriptTransfer(ctx, fields, V3)
          case other                                                     => err(other, V3, FieldNames.TransferSet)
        }
      case other => err(other, V3, s"List(${FieldNames.Transfers})")
    }

  private def processActionV3(
      ctx: EvaluationContext[Environment, Id],
      fields: Map[String, EVALUATED],
      unusedComplexity: Int
  ): Either[ExecutionError, ScriptResultV3] = {
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
    } yield ScriptResultV3(w, p, unusedComplexity)
  }

  private def processScriptResultV3(
      ctx: EvaluationContext[Environment, Id],
      tpe: CASETYPEREF,
      fields: Map[String, EVALUATED],
      unusedComplexity: Int
  ) =
    tpe.name match {
      case FieldNames.WriteSet     => processWriteSetV3(fields).map(ScriptResultV3(_, List.empty, unusedComplexity))
      case FieldNames.TransferSet  => processTransferSetV3(ctx, fields).map(ScriptResultV3(List.empty, _, unusedComplexity))
      case FieldNames.ScriptResult => processActionV3(ctx, fields, unusedComplexity)
      case f                       => err(f, V3)
    }

  private def processIssue(input: ActionInput): Either[ExecutionError, Issue] = {
    val (_, parentId, fields) = input
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

  private def processReissue(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, Reissue] =
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
        err(other, version, FieldNames.Reissue)
    }

  private def processBurn(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, Burn] =
    (fields.get(FieldNames.BurnAssetId), fields.get(FieldNames.BurnQuantity)) match {
      case (Some(CONST_BYTESTR(assetId)), Some(CONST_LONG(quantity))) =>
        Right(Burn(assetId, quantity))
      case other =>
        err(other, version, FieldNames.Burn)
    }

  private def processSponsorFee(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, SponsorFee] =
    (fields.get(FieldNames.SponsorFeeAssetId), fields.get(FieldNames.SponsorFeeMinFee)) match {
      case (Some(CONST_BYTESTR(assetId)), Some(minFeeOpt)) =>
        val minFeeValueOpt = minFeeOpt match {
          case CONST_LONG(minFee) => Right(Some(minFee))
          case `unit`             => Right(None)
          case other              => err(s"can't reconstruct ${FieldNames.SponsorFeeMinFee} from $other", V4)
        }
        minFeeValueOpt.map(v => SponsorFee(assetId, v))
      case other =>
        err(other, version, FieldNames.SponsorFee)
    }

  private def processLease(
      ctx: EvaluationContext[Environment, Id],
      fields: Map[String, EVALUATED],
      version: StdLibVersion
  ): Either[ExecutionError, Lease] =
    (fields.get(FieldNames.LeaseRecipient), fields.get(FieldNames.LeaseAmount), fields.get(FieldNames.LeaseNonce)) match {
      case (Some(recipient: CaseObj), Some(CONST_LONG(quantity)), Some(CONST_LONG(nonce))) =>
        processRecipient(recipient, ctx, version)
          .map(Lease(_, quantity, nonce))
      case other =>
        err(other, version, FieldNames.Lease)
    }

  private def processLeaseCancel(fields: Map[String, EVALUATED], version: StdLibVersion): Either[ExecutionError, LeaseCancel] =
    fields.get(FieldNames.LeaseId) match {
      case Some(CONST_BYTESTR(leaseId)) =>
        Right(LeaseCancel(leaseId))
      case other =>
        err(other, version, FieldNames.LeaseCancel)
    }

  private def processScriptResult(
      ctx: EvaluationContext[Environment, Id],
      txId: ByteStr,
      actions: Seq[EVALUATED],
      handlers: ActionHandlers,
      version: StdLibVersion,
      unusedComplexity: Int,
      ret: EVALUATED = unit
  ): Either[ExecutionError, ScriptResultV4] =
    actions.toList
      .traverse {
        case obj @ CaseObj(actionType, fields) =>
          handlers
            .get(actionType.name)
            .map(_((ctx, txId, fields)))
            .getOrElse(err(obj, version))

        case other => err(other, version)
      }
      .map(ScriptResultV4(_, unusedComplexity, ret))

  private def fromV4ActionHandlers(v: StdLibVersion): ActionHandlers =
    Map(
      FieldNames.ScriptTransfer -> { case (ctx, _, fields) => processScriptTransfer(ctx, fields, v) },
      FieldNames.IntegerEntry   -> { case (_, _, fields) => processDataEntry(fields, FieldNames.IntegerEntry, processIntEntry, v) },
      FieldNames.BooleanEntry   -> { case (_, _, fields) => processDataEntry(fields, FieldNames.BooleanEntry, processBoolEntry, v) },
      FieldNames.StringEntry    -> { case (_, _, fields) => processDataEntry(fields, FieldNames.StringEntry, processStringEntry, v) },
      FieldNames.BinaryEntry    -> { case (_, _, fields) => processDataEntry(fields, FieldNames.BinaryEntry, processBinaryEntry, v) },
      FieldNames.DeleteEntry    -> { case (_, _, fields) => processDeleteEntry(fields, v) },
      FieldNames.Reissue        -> { case (_, _, fields) => processReissue(fields, v) },
      FieldNames.Burn           -> { case (_, _, fields) => processBurn(fields, v) },
      FieldNames.SponsorFee     -> { case (_, _, fields) => processSponsorFee(fields, v) },
      FieldNames.Issue          -> processIssue
    )

  private def fromV5ActionHandlers(v: StdLibVersion): ActionHandlers =
    Map(
      FieldNames.Lease       -> { case (ctx, _, fields) => processLease(ctx, fields, v) },
      FieldNames.LeaseCancel -> { case (_, _, fields) => processLeaseCancel(fields, v) }
    )

  private val v4ActionHandlers = fromV4ActionHandlers(V4)
  private val v5ActionHandlers = fromV4ActionHandlers(V5) ++ fromV5ActionHandlers(V5)

  def fromObj(
      ctx: EvaluationContext[Environment, Id],
      txId: ByteStr,
      e: EVALUATED,
      version: StdLibVersion,
      unusedComplexity: Int
  ): Either[ExecutionError, ScriptResult] = {
    def processResultWithValue(
        tpe: CASETYPEREF,
        fields: Map[String, EVALUATED],
        v: StdLibVersion
    ) =
      (fields.get("_1"), fields.get("_2")) match {
        case (Some(ARR(actions)), Some(ret)) => processScriptResult(ctx, txId, actions, v5ActionHandlers, v, unusedComplexity, ret)
        case _                               => err(tpe.name, version)
      }

    (e, version) match {
      case (CaseObj(tpe, fields), V3)           => processScriptResultV3(ctx, tpe, fields, unusedComplexity)
      case (ARR(actions), V4)                   => processScriptResult(ctx, txId, actions, v4ActionHandlers, V4, unusedComplexity)
      case (ARR(actions), v) if v >= V5         => processScriptResult(ctx, txId, actions, v5ActionHandlers, v, unusedComplexity)
      case (CaseObj(tpe, fields), v) if v >= V5 => processResultWithValue(tpe, fields, v)
      case c                                    => err(c.toString, version)
    }
  }
}
