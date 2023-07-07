package com.wavesplatform.transaction

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.account
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, UNIT}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2.LogKeys.*
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Bindings, Types}
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.{CommonError, ExecutionError}
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object DiffToLogConverter {

  def convert(snapshot: StateSnapshot, txId: ByteStr, funcName: String, complexityLimit: Int): Log[Id] = {
    def strToMap(str: String, fieldName: String)          = CONST_STRING(str).map(s => Map(fieldName -> s)).getOrElse(Map.empty)
    def byteStrToMap(byteStr: ByteStr, fieldName: String) = CONST_BYTESTR(byteStr).map(bs => Map(fieldName -> bs)).getOrElse(Map.empty)
    def assetToMap(asset: Asset, fieldName: String) = (asset match {
      case IssuedAsset(id) => CONST_BYTESTR(id)
      case Waves           => CaseObj(UNIT, Map.empty).asRight[CommonError]
    }).map(assetObj => Map(fieldName -> assetObj)).getOrElse(Map.empty)
    def arrToMap(items: Seq[EVALUATED], fieldName: String) =
      ARR(items.toIndexedSeq, false).map(arr => Map(fieldName -> arr)).getOrElse(Map.empty)

    def scriptResultsToObj(scriptResults: InvokeScriptResult): CaseObj = {
      CaseObj(
        CASETYPEREF("StateChanges", List.empty),
        arrToMap(
          scriptResults.data.map {
            case BooleanDataEntry(key, value) =>
              CaseObj(
                Types.booleanDataEntry,
                strToMap(key, "key") ++ Map("value" -> CONST_BOOLEAN(value))
              )
            case IntegerDataEntry(key, value) => CaseObj(Types.intDataEntry, strToMap(key, "key") ++ Map("value" -> CONST_LONG(value)))
            case StringDataEntry(key, value)  => CaseObj(Types.stringDataEntry, strToMap(key, "key") ++ strToMap(value, "value"))
            case BinaryDataEntry(key, value) =>
              CaseObj(Types.binaryDataEntry, CONST_BYTESTR(value).map(b => strToMap(key, "key") ++ Map("value" -> b)).getOrElse(Map.empty))
            case EmptyDataEntry(key) => CaseObj(Types.deleteDataEntry, strToMap(key, "key"))
          },
          "data"
        ) ++
          arrToMap(
            scriptResults.transfers.map { payment =>
              CaseObj(
                Types.scriptTransfer,
                Map(
                  "amount"    -> CONST_LONG(payment.amount),
                  "recipient" -> Bindings.senderObject(Recipient.Address(ByteStr(payment.address.bytes)))
                ) ++ assetToMap(payment.asset, "asset")
              )
            },
            "transfers"
          ) ++
          arrToMap(
            scriptResults.issues.map { issue =>
              CaseObj(
                Types.issueActionType,
                byteStrToMap(issue.id, "id") ++
                  strToMap(issue.name, "name") ++
                  strToMap(issue.description, "description") ++
                  issue.compiledScript.map(byteStrToMap(_, "script")).getOrElse(Map.empty) ++
                  Map(
                    "decimals"     -> CONST_LONG(issue.decimals),
                    "isReissuable" -> CONST_BOOLEAN(issue.isReissuable),
                    "quantity"     -> CONST_LONG(issue.quantity),
                    "nonce"        -> CONST_LONG(issue.nonce)
                  )
              )
            },
            "issues"
          ) ++
          arrToMap(
            scriptResults.reissues.map { reissue =>
              CaseObj(
                Types.reissueActionType,
                byteStrToMap(reissue.assetId, "id") ++ Map(
                  "isReissuable" -> CONST_BOOLEAN(reissue.isReissuable),
                  "quantity"     -> CONST_LONG(reissue.quantity)
                )
              )
            },
            "reissues"
          ) ++
          arrToMap(
            scriptResults.burns.map { burn =>
              CaseObj(Types.burnActionType, byteStrToMap(burn.assetId, "id") ++ Map("quantity" -> CONST_LONG(burn.quantity)))
            },
            "burns"
          ) ++
          arrToMap(
            scriptResults.sponsorFees.map { sponsor =>
              CaseObj(
                Types.sponsorFeeActionType,
                byteStrToMap(sponsor.assetId, "id") ++ sponsor.minSponsoredAssetFee
                  .map(fee => Map("minSponsoredAssetFee" -> CONST_LONG(fee)))
                  .getOrElse(Map.empty)
              )
            },
            "sponsorFees"
          ) ++
          arrToMap(
            scriptResults.leases.map { lease =>
              val recipient = lease.recipient match {
                case addr: account.Address => Bindings.senderObject(Address(ByteStr(addr.bytes)))
                case alias: account.Alias  => CaseObj(Types.aliasType, strToMap(alias.name, "alias"))
              }
              CaseObj(
                Types.leaseActionType,
                Map("recipient" -> recipient, "amount" -> CONST_LONG(lease.amount), "nonce" -> CONST_LONG(lease.nonce)) ++ byteStrToMap(
                  lease.id,
                  "id"
                )
              )
            },
            "leases"
          ) ++
          arrToMap(
            scriptResults.leaseCancels.map { leaseCancel =>
              CaseObj(Types.leaseCancelActionType, byteStrToMap(leaseCancel.id, "id"))
            },
            "leaseCancels"
          ) ++
          arrToMap(
            scriptResults.invokes.map { invoke =>
              CaseObj(
                CASETYPEREF("Invoke", List.empty),
                Map(
                  "dApp" -> Bindings.senderObject(Address(ByteStr(invoke.dApp.bytes))),
                  "call" -> CaseObj(
                    CASETYPEREF("Call", List.empty),
                    strToMap(invoke.call.function, "function") ++
                      arrToMap(invoke.call.args, "args")
                  ),
                  "stateChanges" -> scriptResultsToObj(invoke.stateChanges)
                ) ++
                  arrToMap(
                    invoke.payments.map { payment =>
                      CaseObj(Types.paymentType, assetToMap(payment.assetId, "assetId") ++ Map("amount" -> CONST_LONG(payment.amount)))
                    },
                    "payments"
                  )
              )
            },
            "invokes"
          )
      )
    }

    List(
      s"$funcName.$StateChanges" -> scriptResultsToObj(snapshot.scriptResults.getOrElse(txId, InvokeScriptResult.empty)).asRight[ExecutionError],
      s"$funcName.$Complexity"   -> CONST_LONG(snapshot.scriptsComplexity).asRight[ExecutionError],
      ComplexityLimit            -> CONST_LONG(complexityLimit - snapshot.scriptsComplexity).asRight[ExecutionError]
    )
  }
}
