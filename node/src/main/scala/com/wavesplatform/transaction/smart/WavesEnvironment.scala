package com.wavesplatform.transaction.smart

import cats.implicits._
import com.wavesplatform.account
import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FUNCTION_CALL}
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.lang.v1.traits.domain.Recipient._
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.invoke.{InvokeScript, InvokeScriptDiff}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.transaction.Asset._
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, GenericError}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.serialization.impl.PBTransactionSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced
import com.wavesplatform.transaction.transfer.TransferTransaction
import monix.eval.Coeval
import shapeless._

object WavesEnvironment {
  type In = Transaction :+: Order :+: PseudoTx :+: CNil
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
  import com.wavesplatform.lang.v1.traits.Environment._

  def currentBlockchain(): Blockchain = blockchain

  override def height: Long = h()

  override def multiPaymentAllowed: Boolean = blockchain.allowsMultiPayment

  override def transactionById(id: Array[Byte]): Option[Tx] =
    // There are no new transactions in currentBlockchain
    blockchain
      .transactionInfo(ByteStr(id))
      .filter(_._3)
      .map(_._2)
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
            .fromBytes(bytes.arr)
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
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = currentBlockchain().balance(address, Asset.fromCompatId(maybeAssetId.map(ByteStr(_))))
    } yield balance).left.map(_.toString)
  }

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      portfolio = currentBlockchain().wavesPortfolio(address)
    } yield Environment.BalanceDetails(
      portfolio.balance - portfolio.lease.out,
      portfolio.balance,
      blockchain.generatingBalance(address),
      portfolio.effectiveBalance
    )).left.map(_.toString)
  }

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    // There are no new transactions in currentBlockchain
    blockchain.transactionMeta(ByteStr(id)).collect { case (h, true) => h.toLong }

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
      .collect {
        case tx: TransferTransaction => RealTransactionWrapper.mapTransferTx(tx)
      }

  override def addressFromString(addressStr: String): Either[String, Address] =
    account.Address
      .fromString(addressStr)
      .bimap(
        _.toString,
        address => Address(ByteStr(address.bytes))
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
  ): Coeval[(Either[ValidationError, EVALUATED], Int)] = ???
}

object DAppEnvironment {
  // Not thread safe
  final case class InvocationTreeTracker(root: DAppInvocation) {
    private[this] var error       = Option.empty[ValidationError]
    private[this] var invocations = Vector.empty[InvocationTreeTracker]

    def record(invocation: DAppInvocation): InvocationTreeTracker = {
      val tracker = InvocationTreeTracker(invocation)
      this.invocations :+= tracker
      tracker
    }

    def setError(error: ValidationError): Unit =
      this.error = Some(error)

    def toInvocationList: Seq[InvokeScriptResult.Invocation] = {
      this.invocations.to(LazyList).map { inv =>
        val call       = InvokeScriptResult.Call.fromFunctionCall(inv.root.call)
        val payments   = InvokeScriptResult.AttachedPayment.fromInvokePaymentList(inv.root.payments)
        InvokeScriptResult.Invocation(inv.root.dAppAddress, call, payments, InvokeScriptResult(invokes = inv.toInvocationList, error = inv.getErrorMessage))
      }
    }

    def getErrorMessage: Option[InvokeScriptResult.ErrorMessage] =
      this.error.map(errorMessage)

    private[this] def errorMessage(ve: ValidationError): InvokeScriptResult.ErrorMessage = {
      val fte = FailedTransactionError.asFailedScriptError(ve)
      InvokeScriptResult.ErrorMessage(fte.code, fte.message)
    }
  }

  final case class DAppInvocation(dAppAddress: com.wavesplatform.account.Address, call: FUNCTION_CALL, payments: Seq[InvokeScriptTransaction.Payment])
}

// Not thread safe
class DAppEnvironment(
    nByte: Byte,
    in: Coeval[Environment.InputEntity],
    h: Coeval[Int],
    blockchain: Blockchain,
    tthis: Environment.Tthis,
    ds: DirectiveSet,
    tx: Option[InvokeScriptTransaction],
    currentDApp: com.wavesplatform.account.Address,
    currentDAppPk: com.wavesplatform.account.PublicKey,
    calledAddresses: Set[com.wavesplatform.account.Address],
    limitedExecution: Boolean,
    totalComplexityLimit: Int,
    var remainingCalls: Int,
    var availableActions: Int,
    var availableData: Int,
    var currentDiff: Diff,
    invocationRoot: DAppEnvironment.InvocationTreeTracker
) extends WavesEnvironment(nByte, in, h, blockchain, tthis, ds, tx.map(_.id()).getOrElse(ByteStr.empty)) {

  private[this] var mutableBlockchain = CompositeBlockchain(blockchain, currentDiff)

  override def currentBlockchain(): CompositeBlockchain = this.mutableBlockchain

  override def callScript(
      dApp: Address,
      func: String,
      args: List[EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, EVALUATED], Int)] = {
    val invocation = InvokeScriptResult.Invocation(
      account.Address.fromBytes(dApp.bytes.arr).explicitGet(),
      InvokeScriptResult.Call(func, args),
      payments.map(p => InvokeScriptResult.AttachedPayment(p._1.fold(Asset.Waves: Asset)(a => IssuedAsset(ByteStr(a))), p._2)),
      InvokeScriptResult.empty
    )

    val r = for {
      invoke <- traced(
        account.Address
          .fromBytes(dApp.bytes.arr)
          .ensureOr(
            address => GenericError(s"The invocation stack contains multiple invocations of the dApp at address $address with invocations of another dApp between them")
          )(
            address => currentDApp == address || !calledAddresses.contains(address)
          )
          .map(
            InvokeScript(
              currentDApp,
              currentDAppPk,
              _,
              FUNCTION_CALL(User(func, func), args),
              payments.map(p => Payment(p._2, p._1.fold(Waves: Asset)(a => IssuedAsset(ByteStr(a))))),
              tx
            )
          )
      )
      invocationTracker = {
        // Log sub-contract invocation
        val invocation = DAppEnvironment.DAppInvocation(invoke.dAppAddress, invoke.funcCall, invoke.payments)
        invocationRoot.record(invocation)
      }
      (diff, evaluated, remainingActions, remainingData) <- InvokeScriptDiff( // This is a recursive call
        mutableBlockchain,
        blockchain.settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp + 1,
        limitedExecution,
        totalComplexityLimit,
        availableComplexity,
        remainingCalls,
        availableActions,
        availableData,
        if (reentrant) calledAddresses else calledAddresses + invoke.senderAddress,
        invocationTracker
      )(invoke).leftMap { err =>
        invocationTracker.setError(err)
        err
      }
    } yield {
      val fixedDiff = diff.copy(
        scriptResults = Map(txId -> InvokeScriptResult(invokes = Seq(invocation.copy(stateChanges = diff.scriptResults(txId))))),
        scriptsRun = diff.scriptsRun + 1
      )
      currentDiff = currentDiff |+| fixedDiff
      mutableBlockchain = CompositeBlockchain(blockchain, currentDiff)
      remainingCalls = remainingCalls - 1
      availableActions = remainingActions
      availableData = remainingData
      (evaluated, diff.scriptsComplexity.toInt)
    }

    r.v.map {
      _.resultE match {
        case Left(fte: FailedTransactionError) => (Left(fte), fte.spentComplexity.toInt)
        case Left(e)                           => (Left(e), 0)
        case Right((evaluated, complexity))    => (Right(evaluated), complexity)
      }
    }
  }
}
