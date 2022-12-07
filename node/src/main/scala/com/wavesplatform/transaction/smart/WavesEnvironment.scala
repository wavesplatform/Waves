package com.wavesplatform.transaction.smart

import cats.syntax.either.*
import com.wavesplatform.account
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.MultiPaymentPolicyProvider.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.{Log, ScriptResult}
import com.wavesplatform.lang.v1.traits.*
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.lang.v1.traits.domain.Recipient.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.invoke.{InvokeScript, InvokeScriptDiff, InvokeScriptTransactionLike}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.*
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, GenericError}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.smart.script.trace.InvokeScriptTrace
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, DiffToLogConverter, TransactionBase, TransactionType}
import monix.eval.Coeval
import shapeless.*

import scala.util.Try

object WavesEnvironment {
  type In = TransactionBase :+: Order :+: PseudoTx :+: CNil
}

class WavesEnvironment(
    nByte: Byte,
    in: Coeval[Environment.InputEntity],
    h: Coeval[Int],
    blockchain: Blockchain,
    val tthis: Environment.Tthis,
    ds: DirectiveSet,
    override val txId: ByteStr
) extends Environment[Id] {
  import com.wavesplatform.lang.v1.traits.Environment.*

  def currentBlockchain(): Blockchain = blockchain

  override def height: Long = h()

  override def multiPaymentAllowed: Boolean = blockchain.allowsMultiPayment

  override def transactionById(id: Array[Byte]): Option[Tx] =
    // There are no new transactions in currentBlockchain
    blockchain
      .transactionInfo(ByteStr(id))
      .filter(_._1.succeeded)
      .collect { case (_, tx) if tx.t.tpe != TransactionType.Ethereum => tx }
      .map(tx => RealTransactionWrapper(tx, blockchain, ds.stdLibVersion, paymentTarget(ds, tthis)).explicitGet())

  override def inputEntity: InputEntity = in()

  override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer] =
    // There are no new transactions in currentBlockchain
    blockchain
      .transferById(ByteStr(id))
      .map(t => RealTransactionWrapper.mapTransferTx(t._2))

  def toAddress(recipient: Recipient): Option[com.wavesplatform.account.Address] = {
    recipient match {
      case Address(bytes) =>
        com.wavesplatform.account.Address
          .fromBytes(bytes.arr)
          .toOption
      case Alias(name) =>
        com.wavesplatform.account.Alias
          .create(name)
          .flatMap(blockchain.resolveAlias)
          .toOption
    }
  }

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = {
    for {
      address <- toAddress(recipient)
      data <- currentBlockchain()
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (IntegerDataEntry(_, value), DataType.Long)     => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteStr(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data
  }

  override def hasData(recipient: Recipient): Boolean = {
    (for {
      address <- recipient match {
        case Address(bytes) =>
          com.wavesplatform.account.Address
            .fromBytes(bytes.arr, chainId)
            .toOption
        case Alias(name) =>
          com.wavesplatform.account.Alias
            .create(name)
            .flatMap(blockchain.resolveAlias)
            .toOption
      }
    } yield currentBlockchain()
      .hasData(address)).getOrElse(false)
  }

  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    // There are no new aliases in currentBlockchain
    blockchain
      .resolveAlias(com.wavesplatform.account.Alias.create(name).explicitGet())
      .map(a => Recipient.Address(ByteStr(a.bytes)))
      .left
      .map(_.toString)

  override def chainId: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = currentBlockchain().balance(address, Asset.fromCompatId(maybeAssetId.map(ByteStr(_))))
    } yield balance).left.map(_.toString)
  }

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails] = {
    val addressE: Either[ValidationError, account.Address] = addressOrAlias match {
      case Address(bytes) => account.Address.fromBytes(bytes.arr)
      case Alias(name)    => account.Alias.create(name).flatMap(a => blockchain.resolveAlias(a))
    }
    for {
      address <- addressE.leftMap(_.toString)
      portfolio = currentBlockchain().wavesPortfolio(address)
      effectiveBalance <- portfolio.effectiveBalance
    } yield Environment.BalanceDetails(
      portfolio.balance - portfolio.lease.out,
      portfolio.balance,
      blockchain.generatingBalance(address),
      effectiveBalance
    )
  }

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    // There are no new transactions in currentBlockchain
    blockchain.transactionMeta(ByteStr(id)).collect { case tm if tm.succeeded => tm.height.toLong }

  override def assetInfoById(id: Array[Byte]): Option[domain.ScriptAssetInfo] = {
    for {
      assetDesc <- currentBlockchain().assetDescription(IssuedAsset(ByteStr(id)))
    } yield {
      ScriptAssetInfo(
        id = ByteStr(id),
        name = assetDesc.name.toStringUtf8,
        description = assetDesc.description.toStringUtf8,
        quantity = assetDesc.totalVolume.toLong,
        decimals = assetDesc.decimals,
        issuer = Address(ByteStr(assetDesc.issuer.toAddress.bytes)),
        issuerPk = assetDesc.issuer,
        reissuable = assetDesc.reissuable,
        scripted = assetDesc.script.nonEmpty,
        minSponsoredFee = Some(assetDesc.sponsorship).filter(_ != 0)
      )
    }
  }

  override def lastBlockOpt(): Option[BlockInfo] =
    // There are no new blocks in currentBlockchain
    blockchain.lastBlockHeader
      .map(block => toBlockInfo(block.header, height.toInt, blockchain.vrf(height.toInt)))

  override def blockInfoByHeight(blockHeight: Int): Option[BlockInfo] =
    // There are no new blocks in currentBlockchain
    blockchain
      .blockHeader(blockHeight)
      .map(blockHAndSize => toBlockInfo(blockHAndSize.header, blockHeight, blockchain.vrf(blockHeight)))

  private def toBlockInfo(blockH: BlockHeader, bHeight: Int, vrf: Option[ByteStr]) = {
    // There are no new blocks in currentBlockchain
    BlockInfo(
      timestamp = blockH.timestamp,
      height = bHeight,
      baseTarget = blockH.baseTarget,
      generationSignature = blockH.generationSignature,
      generator = ByteStr(blockH.generator.toAddress.bytes),
      generatorPublicKey = blockH.generator,
      if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) vrf else None
    )
  }

  override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer] =
    PBTransactionSerializer
      .parseBytes(b)
      .toOption
      .collect { case tx: TransferTransaction =>
        RealTransactionWrapper.mapTransferTx(tx)
      }

  override def addressFromString(addressStr: String): Either[String, Address] =
    account.Address
      .fromString(addressStr, Some(chainId))
      .bimap(
        _.toString,
        address => Address(ByteStr(address.bytes))
      )

  override def addressFromPublicKey(publicKey: ByteStr): Either[String, Address] =
    Try(PublicKey(publicKey)).toEither
      .bimap(
        _.getMessage,
        pk => Address(ByteStr(pk.toAddress.bytes))
      )

  override def accountScript(addressOrAlias: Recipient): Option[Script] = {
    for {
      address <- toAddress(addressOrAlias)
      si      <- blockchain.accountScript(address)
    } yield si.script
  }

  override def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = ???
}

object DAppEnvironment {
  // Not thread safe
  final case class InvocationTreeTracker(root: DAppInvocation) {
    private var result: Either[ValidationError, ScriptResult] = Left(GenericError("No result"))
    private var log: Log[Id]                                  = Nil
    private[this] var invocations                             = Vector.empty[InvocationTreeTracker]

    def record(invocation: DAppInvocation): InvocationTreeTracker = {
      val tracker = InvocationTreeTracker(invocation)
      this.invocations :+= tracker
      tracker
    }

    def setResult(result: ScriptResult): Unit =
      this.result = Right(result)

    def setError(error: ValidationError): Unit =
      this.result = Left(error)

    def setLog(log: Log[Id]): Unit =
      this.log = log

    def toInvocationList: Seq[InvokeScriptResult.Invocation] = {
      this.invocations.to(LazyList).map { inv =>
        val call     = InvokeScriptResult.Call.fromFunctionCall(inv.root.call)
        val payments = InvokeScriptResult.AttachedPayment.fromInvokePaymentList(inv.root.payments)
        InvokeScriptResult
          .Invocation(inv.root.dAppAddress, call, payments, InvokeScriptResult(invokes = inv.toInvocationList, error = inv.getErrorMessage))
      }
    }

    def toTraceList(invocationId: ByteStr): Seq[InvokeScriptTrace] =
      this.invocations.to(LazyList).map { inv =>
        InvokeScriptTrace(invocationId, inv.root.dAppAddress, inv.root.call, inv.result, inv.log, inv.toTraceList(invocationId))
      }

    def getErrorMessage: Option[InvokeScriptResult.ErrorMessage] = {
      def isNestedError(ve: ValidationError) = invocations.exists { inv =>
        (inv.result, ve) match {
          case (Left(fte1: FailedTransactionError), fte2: FailedTransactionError) => fte1.error == fte2.error
          case (Left(ve1), ve2)                                                   => ve1 == ve2
          case _                                                                  => false
        }
      }

      this.result.left.toOption.collect {
        case ve if !isNestedError(ve) =>
          errorMessage(ve)
      }
    }

    private[this] def errorMessage(ve: ValidationError): InvokeScriptResult.ErrorMessage = {
      val fte = FailedTransactionError.asFailedScriptError(ve)
      InvokeScriptResult.ErrorMessage(fte.code, fte.message)
    }
  }

  final case class DAppInvocation(
      dAppAddress: com.wavesplatform.account.Address,
      call: FUNCTION_CALL,
      payments: Seq[InvokeScriptTransaction.Payment]
  )
}

// Not thread safe
class DAppEnvironment(
    nByte: Byte,
    in: Coeval[Environment.InputEntity],
    h: Coeval[Int],
    blockchain: Blockchain,
    tthis: Environment.Tthis,
    ds: DirectiveSet,
    tx: InvokeScriptTransactionLike,
    currentDApp: com.wavesplatform.account.Address,
    currentDAppPk: com.wavesplatform.account.PublicKey,
    calledAddresses: Set[com.wavesplatform.account.Address],
    limitedExecution: Boolean,
    totalComplexityLimit: Int,
    var remainingCalls: Int,
    var availableActions: Int,
    var availableBalanceActions: Int,
    var availableAssetActions: Int,
    var availablePayments: Int,
    var availableData: Int,
    var availableDataSize: Int,
    var currentDiff: Diff,
    val invocationRoot: DAppEnvironment.InvocationTreeTracker
) extends WavesEnvironment(nByte, in, h, blockchain, tthis, ds, tx.id()) {

  private[this] var mutableBlockchain = CompositeBlockchain(blockchain, currentDiff)

  override def currentBlockchain(): CompositeBlockchain = this.mutableBlockchain

  override def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = {

    val r = for {
      address <- traced(
        account.Address
          .fromBytes(dApp.bytes.arr)
          .ensureOr(address =>
            GenericError(
              s"The invocation stack contains multiple invocations of the dApp at address $address with invocations of another dApp between them"
            )
          )(address => currentDApp == address || !calledAddresses.contains(address))
      )
      invoke = InvokeScript(
        currentDAppPk,
        address,
        FUNCTION_CALL(User(func, func), args),
        payments.map(p => Payment(p._2, p._1.fold(Waves: Asset)(a => IssuedAsset(ByteStr(a))))),
        tx
      )
      invocationTracker = {
        // Log sub-contract invocation
        val invocation = DAppEnvironment.DAppInvocation(invoke.dApp, invoke.funcCall, invoke.payments)
        invocationRoot.record(invocation)
      }
      invocation = InvokeScriptResult.Invocation(
        address,
        InvokeScriptResult.Call(func, args),
        payments.map(p => InvokeScriptResult.AttachedPayment(p._1.fold(Asset.Waves: Asset)(a => IssuedAsset(ByteStr(a))), p._2)),
        InvokeScriptResult.empty
      )
      (diff, evaluated, remainingActions, remainingBalanceActions, remainingAssetActions, remainingPayments, remainingData, remainingDataSize) <-
        InvokeScriptDiff( // This is a recursive call
          mutableBlockchain,
          blockchain.settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp + 1,
          limitedExecution,
          totalComplexityLimit,
          availableComplexity,
          remainingCalls,
          availableActions,
          availableBalanceActions,
          availableAssetActions,
          availablePayments,
          availableData,
          availableDataSize,
          if (reentrant) calledAddresses else calledAddresses + invoke.sender.toAddress,
          invocationTracker
        )(invoke)
      fixedDiff = diff
        .withScriptResults(Map(txId -> InvokeScriptResult(invokes = Seq(invocation.copy(stateChanges = diff.scriptResults(txId))))))
        .withScriptRuns(diff.scriptsRun + 1)
      newCurrentDiff <- traced(currentDiff.combineF(fixedDiff).leftMap(GenericError(_)))
    } yield {
      currentDiff = newCurrentDiff
      mutableBlockchain = CompositeBlockchain(blockchain, currentDiff)
      remainingCalls = remainingCalls - 1
      availableActions = remainingActions
      availableBalanceActions = remainingBalanceActions
      availableAssetActions = remainingAssetActions
      availablePayments = remainingPayments
      availableData = remainingData
      availableDataSize = remainingDataSize
      (evaluated, diff.scriptsComplexity.toInt, DiffToLogConverter.convert(diff, tx.id(), func, availableComplexity))
    }

    r.v.map {
      _.resultE match {
        case Left(fte: FailedTransactionError)       => (Left(fte), fte.spentComplexity.toInt)
        case Left(e)                                 => (Left(e), 0)
        case Right((evaluated, complexity, diffLog)) => (Right((evaluated, diffLog)), complexity)
      }
    }
  }
}
