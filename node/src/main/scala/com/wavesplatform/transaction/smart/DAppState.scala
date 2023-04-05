package com.wavesplatform.transaction.smart

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.miniev.{ComplexityLimit, Ev, Op, State}
import com.wavesplatform.lang.utils.Logging
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.{IncompleteResult, ScriptResult, ScriptResultV3, ScriptResultV4}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.{CommonError, ExecutionError, FailOrRejectError, ValidationError}
import com.wavesplatform.state.diffs.BalanceDiffValidation
import com.wavesplatform.state.diffs.invoke.*
import com.wavesplatform.state.diffs.invoke.InvokeDiffsCommon.ActionCount
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{Blockchain, InvokeScriptResult, StateSnapshot}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.validation.impl.DataTxValidator
import shapeless.Coproduct

import scala.annotation.tailrec

class DAppState(
    val root: InvokeScriptTransactionLike,
    rootDAppPK: PublicKey,
    rootVersion: StdLibVersion,
    blockchain: Blockchain,
    private var snapshot: StateSnapshot,
    inputEntity: Environment.InputEntity,
    ds: DirectiveSet,
    complexityLimit: ComplexityLimit,
    newMode: Boolean
) extends State(complexityLimit, newMode)
    with Logging {
  import DAppState.*

  private var currentBlockchain = SnapshotBlockchain(blockchain, snapshot)

  private var invocationStack: List[InvocationFrame] = List(
    InvocationFrame(
      root,
      rootDAppPK,
      rootVersion,
      CachedDAppCTX
        .get(rootVersion, currentBlockchain)
        .completeContext(
          new WavesEnvironment(inputEntity, Coproduct[Environment.Tthis](Recipient.Address(ByteStr(rootDAppPK.toAddress.bytes))), root.id(), ds) {
            override def blockchain: Blockchain = DAppState.this.blockchain()
          }
        )
    )
  )

  def currentInvocation(): InvocationFrame = invocationStack.head

  def blockchain(): Blockchain = currentBlockchain

  override def stdlibVersion: StdLibVersion = currentInvocation().version

  override def evaluationContext: EvaluationContext[Id] = currentInvocation().ec

  private var reentrancyStack: List[(PublicKey, Boolean)] = Nil

  @tailrec
  private def canReEnter(dappPK: PublicKey, stack: List[(PublicKey, Boolean)], isTopFrame: Boolean): Boolean = {
    if (stack.isEmpty) true
    else
      stack.head match {
        case (`dappPK`, reentrant) => isTopFrame || reentrant
        case _                     => canReEnter(dappPK, stack.tail, false)
      }
  }

  def invoke(invocation: InvokeScript, dAppPublicKey: PublicKey, reentrant: Boolean, version: StdLibVersion): Either[ExecutionError, this.type] = {
//    trace(s"${invocation.sender.toAddress} is ${if (reentrant) "reentrant-" else ""}invoking ${invocation.funcCall} on ${dAppPublicKey.toAddress} with ${invocation.payments
//      .mkString("[", ",", "]")}, spent=${totalSpentComplexity()}")
    for {
      _ <- Either.cond(
        canReEnter(dAppPublicKey, reentrancyStack, true),
        (),
        CommonError(
          s"The invocation stack contains multiple invocations of the dApp at address ${invocation.dApp} with invocations of another dApp between them"
        )
      )
      _ <- Either.cond(
        reentrancyStack.size < ContractLimits.MaxSyncDAppCalls(version),
        (),
        CommonError(s"DApp calls limit = ${ContractLimits.MaxSyncDAppCalls(version)} is exceeded")
      )
      paymentsDiff <- InvokeDiffsCommon
        .paymentsPart(currentBlockchain, invocation, dAppPublicKey.toAddress, Map.empty)
        .leftMap(ge => CommonError("Error extracting payments part", Some(ge)))
      validPaymentsPart <- BalanceDiffValidation
        .cond(blockchain(), _.isFeatureActivated(BlockchainFeatures.RideV6))(paymentsDiff)
        .leftMap(abe => CommonError("Error validating attached payments", Some(abe)))
      _ = appendDiff(validPaymentsPart)
    } yield {
      scriptRunCount += 1
      reentrancyStack ::= ((invocation.sender, reentrant))
      val invocationFrame = InvocationFrame(
        invocation,
        dAppPublicKey,
        version,
        CachedDAppCTX
          .get(version, blockchain())
          .completeContext(
            new WavesEnvironment(
              inputEntity,
              Coproduct[Tthis](Recipient.Address(ByteStr(dAppPublicKey.toAddress.bytes))),
              root.id(),
              ds
            ) {
              override def blockchain: Blockchain = DAppState.this.blockchain()
            }
          )
      )
      invocationStack ::= invocationFrame
      println(s"${"  " * reentrancyStack.size}> ${invocation.dApp}")
      push(new FromInvocation(rootVersion, currentScope(), this, invocationFrame))
      resetScope(Ev.Scope(Map.empty, Map.empty))
    }
  }

  private[DAppState] var assetActionCount   = 0
  private[DAppState] var balanceActionCount = 0
  private[DAppState] var dataItemCount      = 0
  private[DAppState] var dataSize           = 0
  private[DAppState] var scriptRunCount     = 1

  def assetActions: Int   = assetActionCount
  def balanceActions: Int = balanceActionCount
  def dataItems: Int      = dataItemCount
  def totalDataSize: Int  = dataSize
  def allActionCount: Int = assetActionCount + balanceActionCount

  def printInvocations(): Unit = {
    def zzz(top: InvocationFrame, offset: Int, r: InvokeScriptResult): Unit = {
      println(s"${"  " * offset}${top.dappPK.toAddress}: $r")
      top.invocations.foreach { case (f, r) =>
        zzz(f, offset + 1, r)

      }
    }
    currentInvocation().invocations.foreach { case (f, r) => zzz(f, 0, r) }
  }


  def countActions(actions: List[CallableAction]): ActionCount = {
    var ac, bc, dc, ds = 0
    actions.foreach {
      case _: Issue | _: Reissue | _: Burn | _: SponsorFee =>
        ac += 1
      case _: Lease | _: LeaseCancel | _: AssetTransfer =>
        bc += 1
      case op: DataOp =>
        dc += 1
        val value = InvokeDiffsCommon.dataItemToEntry(op)
        val size  = DataTxValidator.invokeWriteSetSize(blockchain, Seq(value))
        trace(s"$value: $size")
        ds += size
    }
    assetActionCount += ac
    balanceActionCount += bc
    dataItemCount += dc
    dataSize += ds
    InvokeDiffsCommon.ActionCount(ac, bc, dc, ds)
  }

  def scriptRuns: Int = scriptRunCount

  def currentDApp: PublicKey              = invocationStack.head.dappPK
  def currentSnapshot: StateSnapshot      = snapshot

  private[DAppState] def appendDiff(diff: StateSnapshot): StateSnapshot = {
    import cats.syntax.semigroup.*
    this.snapshot = this.snapshot.combine(diff)
    this.currentBlockchain = SnapshotBlockchain(blockchain, this.snapshot)
    this.currentSnapshot
  }

  private[DAppState] def popInvocation(isr: InvokeScriptResult): InvocationFrame = {
    val poppedFrame = invocationStack.head
    println(s"${"  " * (invocationStack.size - 1)}< ${poppedFrame.dappPK.toAddress}")
    val newHead :: newTail = invocationStack.tail
    invocationStack = newHead.copy(invocations = (newHead.invocations :+ (poppedFrame -> isr))) :: newTail
    reentrancyStack = reentrancyStack.tail
    poppedFrame
  }
}

object DAppState {
  case class InvocationFrame(
      invocation: InvokeScriptLike,
      dappPK: PublicKey,
      version: StdLibVersion,
      ec: EvaluationContext[Id],
      invocations: Seq[(InvocationFrame, InvokeScriptResult)] = Seq.empty
  )

  private def runPaymentAssetScripts(
      initialDiff: StateSnapshot,
      invocationFrame: InvocationFrame,
      blockchain: Blockchain
  ): Either[ValidationError, StateSnapshot] = {
    invocationFrame.invocation.payments.foldLeft(initialDiff.asRight[ValidationError]) {
      case (Right(d), InvokeScriptTransaction.Payment(amount, ia @ IssuedAsset(id))) =>
        blockchain.assetScript(ia).fold(d.asRight[ValidationError]) { asi =>
          InvokeDiffsCommon.validatePseudoTxWithSmartAssetScript(blockchain, invocationFrame.invocation)(
            ScriptTransfer(
              Some(id),
              Recipient.Address(ByteStr(invocationFrame.invocation.sender.toAddress.bytes)),
              invocationFrame.invocation.sender,
              Recipient.Address(ByteStr(invocationFrame.dappPK.toAddress.bytes)),
              amount,
              invocationFrame.invocation.timestamp,
              invocationFrame.invocation.txId
            ),
            id,
            d,
            asi.script,
            asi.complexity,
            Int.MaxValue,
            enableExecutionLog = false
          )
        }
      case (other, _) => other
    }
  }

  private[DAppState] class FromInvocation(rootVersion: StdLibVersion, savedScope: Ev.Scope, ds: DAppState, invocation: InvocationFrame) extends Op {
    override def ret(ev: EVALUATED): Op.Result = {
      // if actions can not be extracted from `ev`, return Left(ExecutionError)
      val scriptResultE = for {
        comp   <- ds.totalSpentComplexity()
        result <- ScriptResult.fromObj(ds.evaluationContext, ds.root.id(), ev, invocation.version, comp.toInt)
      } yield (result, comp)

      scriptResultE match {
        case Right((result, comp)) =>
          val actions = StructuredCallableActions(
            result match {
              case v3: ScriptResultV3  => v3.ds ++ v3.ts
              case v4: ScriptResultV4  => v4.actions
              case _: IncompleteResult => Nil
            },
            ds.blockchain()
          )

          val snapshotAndResultE = for {
            _ <- InvokeDiffsCommon
              .checkScriptResultFields(ds.blockchain(), result)
            _ <- InvokeDiffsCommon
              .checkCallResultLimits(
                invocation.version,
                rootVersion,
                ds.blockchain(),
                comp,
                ds.logEntries.toList,
                ds.allActionCount,
                ds.balanceActionCount,
                ds.assetActionCount,
                ds.dataItemCount,
                ds.dataSize,
                ActionLimits(
                  ContractLimits.MaxCallableActionsAmountBeforeV6(invocation.version),
                  ContractLimits.MaxBalanceScriptActionsAmountV6,
                  ContractLimits.MaxAssetScriptActionsAmountV6,
                  ContractLimits.MaxWriteSetSize,
                  ContractLimits.MaxTotalWriteSetSizeInBytes
                )
              )
              .resultE
            snapshot <- InvokeDiffsCommon
              .processActions(
                actions,
                invocation.version,
                rootVersion,
                ds.currentDApp.toAddress,
                ds.currentDApp,
                comp.toInt,
                ds.currentInvocation().invocation,
                ds.blockchain(),
                ds.blockchain().lastBlockTimestamp.get,
                true,
                false,
                Int.MaxValue,
                Seq.empty,
                false,
                Nil
              )
              .resultE
            _   <- runPaymentAssetScripts(snapshot, invocation, ds.blockchain())
            isr <- InvokeDiffsCommon.actionsToScriptResult(actions, comp.toInt, invocation.invocation, Nil).resultE
          } yield (snapshot, isr)

          snapshotAndResultE match {
            case Right((snapshot, isr)) =>
              ds.appendDiff(snapshot)
              ds.popInvocation(isr)
              Right(result.returnedValue) -> Some(savedScope)
            case Left(error) =>
              ds.popInvocation(InvokeScriptResult(error = Some(InvokeScriptResult.ErrorMessage(1, error.toString))))
              Left(FailOrRejectError(error.toString)) -> Some(savedScope)
          }

        case Left(error) =>
          ds.popInvocation(InvokeScriptResult(error = Some(InvokeScriptResult.ErrorMessage(1, error.toString))))
          Left(error) -> Some(savedScope)
      }

    }
  }
}
