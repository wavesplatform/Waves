package com.wavesplatform.it.api

import java.util.concurrent.TimeoutException

import com.google.protobuf.ByteString
import com.google.protobuf.wrappers.StringValue
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.grpc.BalanceResponse.WavesBalances
import com.wavesplatform.api.grpc.{TransactionStatus => PBTransactionStatus, _}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi.RequestAwaitTime
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.block.Block.Header
import com.wavesplatform.protobuf.block.{PBBlocks, VanillaBlock}
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, TxVersion}
import io.grpc.Status.Code
import io.grpc.StatusRuntimeException
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object SyncGrpcApi extends Assertions {

  def assertGrpcError[R](f: => R, errorRegex: String = "", expectedCode: Code = Code.INVALID_ARGUMENT): Assertion = Try(f) match {
    case Failure(GrpcStatusRuntimeException(status, _)) =>
      Assertions.assert(
        status.getCode == expectedCode
          && status.getDescription.matches(s".*$errorRegex.*"),
        s"\nexpected '$errorRegex'\nactual '${status.getDescription}'"
      )
    case Failure(e) => Assertions.fail(e)
    case Success(s) => Assertions.fail(s"Expecting bad request but handle $s")
  }

  implicit def stringAsBytes(str: String): ByteString = {
    ByteString.copyFrom(Base58.decode(str))
  }

  implicit def keyPairAsBytes(kp: KeyPair): ByteString = {
    ByteString.copyFrom(kp.toAddress.bytes)
  }

  implicit class PBTransactionOps(tx: PBSignedTransaction) {
    def id: String = PBTransactions.vanillaUnsafe(tx).id().toString
  }

  implicit class NodeExtGrpc(n: Node) {
    def grpc: NodeExtGrpc = this
    import com.wavesplatform.account.{Address => Addr}
    import com.wavesplatform.it.api.AsyncGrpcApi.{NodeAsyncGrpcApi => async}

    private[this] lazy val accounts     = AccountsApiGrpc.blockingStub(n.grpcChannel)
    private[this] lazy val assets       = AssetsApiGrpc.blockingStub(n.grpcChannel)
    private[this] lazy val transactions = TransactionsApiGrpc.blockingStub(n.grpcChannel)
    private[this] lazy val blocks       = BlocksApiGrpc.blockingStub(n.grpcChannel)

    def sync[A](awaitable: Awaitable[A], atMost: Duration = RequestAwaitTime): A =
      try Await.result(awaitable, atMost)
      catch {
        case gsre: StatusRuntimeException => throw GrpcStatusRuntimeException(gsre.getStatus, gsre.getTrailers)
        case te: TimeoutException         => throw te
        case NonFatal(cause)              => throw new Exception(cause)
      }

    def resolveAlias(alias: String): Addr = {
      val addr = accounts.resolveAlias(StringValue.of(alias))
      Addr.fromBytes(addr.value.toByteArray).explicitGet()
    }

    def stateChanges(txId: String): (VanillaTransaction, StateChangesDetails) = {
      sync(async(n).stateChanges(Seq(txId))).head
    }

    def stateChanges(address: ByteString): Seq[(VanillaTransaction, StateChangesDetails)] = {
      sync(async(n).stateChanges(address = address))
    }

    def stateChanges(request: TransactionsRequest): Seq[(com.wavesplatform.transaction.Transaction, StateChangesDetails)] = {
      sync(async(n).stateChanges(request))
    }

    def exchange(
        matcher: KeyPair,
        buyOrder: Order,
        sellOrder: Order,
        amount: Long,
        price: Long,
        buyMatcherFee: Long,
        sellMatcherFee: Long,
        fee: Long,
        timestamp: Long,
        version: Byte,
        matcherFeeAssetId: String = "WAVES",
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(
        sync(
          async(n).exchange(matcher, buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, version, matcherFeeAssetId)
        ),
        waitForTx
      )
    }

    def broadcastIssue(
        source: KeyPair,
        name: String,
        quantity: Long,
        decimals: Int,
        reissuable: Boolean,
        fee: Long,
        description: String = "",
        script: Either[Array[Byte], Option[Script]] = Right(None),
        version: Int = 2,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(
        sync(async(n).broadcastIssue(source, name, quantity, decimals, reissuable, fee, description, script, version)),
        waitForTx
      )
    }

    def broadcastTransfer(
        source: KeyPair,
        recipient: Recipient,
        amount: Long,
        fee: Long,
        version: Int = 2,
        assetId: String = "WAVES",
        feeAssetId: String = "WAVES",
        attachment: Attachment.Attachment = Attachment.Attachment.Empty,
        timestamp: Long = System.currentTimeMillis(),
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(
        sync(async(n).broadcastTransfer(source, recipient, amount, fee, version, assetId, feeAssetId, attachment, timestamp)),
        waitForTx
      )
    }

    def broadcastReissue(
        source: KeyPair,
        fee: Long,
        assetId: String,
        amount: Long,
        reissuable: Boolean = false,
        version: Int = 2,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastReissue(source, fee, assetId, amount, reissuable, version)), waitForTx)
    }

    def broadcastCreateAlias(source: KeyPair, alias: String, fee: Long, version: Int = 2, waitForTx: Boolean = false): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastCreateAlias(source, alias, fee, version)), waitForTx)
    }

    def putData(
        source: KeyPair,
        data: Seq[DataTransactionData.DataEntry],
        fee: Long,
        version: Int = 1,
        timestamp: Long = System.currentTimeMillis(),
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).putData(source, data, fee, version, timestamp)), waitForTx)
    }

    def assetsBalance(address: ByteString, assetIds: Seq[String] = Nil): Map[String, Long] = {
      val pbAssetIds = assetIds.map(a => ByteString.copyFrom(Base58.decode(a)))
      val balances   = accounts.getBalances(BalancesRequest.of(address, pbAssetIds))
      balances.map(b => Base58.encode(b.getAsset.assetId.toByteArray) -> b.getAsset.amount).toMap
    }

    def nftList(address: ByteString, limit: Int, after: ByteString = ByteString.EMPTY): Seq[NFTResponse] = {
      assets.getNFTList(NFTRequest.of(address, limit, after)).toList
    }

    def assertAssetBalance(acc: ByteString, assetIdString: String, balance: Long): Unit = {
      val actual = assetsBalance(acc, Seq(assetIdString)).getOrElse(assetIdString, 0L)
      assert(actual == balance, s"Asset balance mismatch, required=$balance, actual=$actual")
    }

    def wavesBalance(address: ByteString): WavesBalances = {
      accounts.getBalances(BalancesRequest.of(address, Seq(ByteString.EMPTY))).next().getWaves
    }

    def getTransaction(id: String, sender: ByteString = ByteString.EMPTY, recipient: Option[Recipient] = None): PBSignedTransaction = {
      sync(async(n).getTransaction(id, sender, recipient))
    }

    def getTransactionInfo(id: ByteString, sender: ByteString = ByteString.EMPTY, recipient: Option[Recipient] = None): TransactionResponse = {
      sync(async(n).getTransactionInfo(id, sender, recipient))
    }

    def getTransactionSeq(ids: Seq[String], sender: ByteString = ByteString.EMPTY, recipient: Option[Recipient] = None): List[TransactionResponse] = {
      transactions.getTransactions(TransactionsRequest(sender, recipient, ids.map(id => ByteString.copyFrom(Base58.decode(id))))).toList
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): PBSignedTransaction =
      sync(async(n).waitForTransaction(txId))

    private def maybeWaitForTransaction(tx: PBSignedTransaction, wait: Boolean): PBSignedTransaction = {
      if (wait) sync(async(n).waitForTransaction(PBTransactions.vanilla(tx).explicitGet().id().toString))
      tx
    }

    def height: Int = sync(async(n).height)

    def waitForHeight(expectedHeight: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      sync(async(n).waitForHeight(expectedHeight), requestAwaitTime)

    def waitForHeightArise(requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      sync(async(n).waitForHeight(this.height + 1), requestAwaitTime)

    def waitFor[A](desc: String)(f: Node => A, cond: A => Boolean, retryInterval: FiniteDuration): A =
      sync(async(n).waitFor(desc)(x => Future.successful(f(x.n)), cond, retryInterval))

    def broadcastBurn(
        sender: KeyPair,
        assetId: String,
        amount: Long,
        fee: Long,
        version: Int = 2,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastBurn(sender, assetId, amount, fee, version)), waitForTx)
    }

    def broadcastMassTransfer(
        sender: KeyPair,
        assetId: Option[String] = None,
        transfers: Seq[MassTransferTransactionData.Transfer],
        attachment: ByteString = ByteString.EMPTY,
        fee: Long,
        version: Int = 1,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastMassTransfer(sender, assetId, transfers, attachment, fee, version)), waitForTx)
    }

    def setAssetScript(
        sender: KeyPair,
        assetId: String,
        script: Either[Array[Byte], Option[Script]],
        fee: Long,
        timestamp: Long = System.currentTimeMillis(),
        version: Int = 1,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).setAssetScript(sender, assetId, script, fee, timestamp, version)), waitForTx)
    }

    def getDataByKey(address: ByteString, key: String): List[DataTransactionData.DataEntry] = {
      accounts.getDataEntries(DataRequest.of(address, key)).toList.map(res => res.getEntry)
    }

    def getData(address: ByteString): List[DataTransactionData.DataEntry] = {
      accounts.getDataEntries(DataRequest(address)).toList.map(res => res.getEntry)
    }

    def setScript(
        sender: KeyPair,
        script: Either[Array[Byte], Option[Script]],
        fee: Long,
        timestamp: Long = System.currentTimeMillis(),
        version: Int = 1,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).setScript(sender, script, fee, timestamp, version)), waitForTx)
    }

    def scriptInfo(address: ByteString): ScriptData = {
      accounts.getScript(AccountRequest.of(address))
    }

    def signedBroadcast(tx: PBSignedTransaction, waitForTx: Boolean = false): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcast(tx.getTransaction, tx.proofs)), waitForTx)
    }

    def broadcast(tx: PBTransaction, proofs: Seq[ByteString], waitForTx: Boolean = false): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcast(tx, proofs)), waitForTx)
    }

    def broadcastSponsorFee(sender: KeyPair, minFee: Option[Amount], fee: Long, version: Int = 1, waitForTx: Boolean = false): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastSponsorFee(sender, minFee, fee, version)), waitForTx)
    }

    def broadcastInvokeScript(
        caller: KeyPair,
        dApp: Recipient,
        functionCall: Option[FUNCTION_CALL],
        payments: Seq[Amount] = Seq.empty,
        fee: Long,
        version: Int = 2,
        feeAssetId: ByteString = ByteString.EMPTY,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastInvokeScript(caller, dApp, functionCall, payments, fee, version, feeAssetId)), waitForTx)
    }

    def broadcastLease(
        source: KeyPair,
        recipient: Recipient,
        amount: Long,
        fee: Long,
        version: Int = 2,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastLease(source, recipient, amount, fee, version)), waitForTx)
    }

    def broadcastLeaseCancel(source: KeyPair, leaseId: String, fee: Long, version: Int = 2, waitForTx: Boolean = false): PBSignedTransaction = {
      maybeWaitForTransaction(sync(async(n).broadcastLeaseCancel(source, leaseId, fee, version)), waitForTx)
    }

    def getActiveLeases(address: ByteString): List[PBSignedTransaction] = {
      val leases = accounts.getActiveLeases(AccountRequest.of(address))
      leases.toList.map(resp => resp.getTransaction)
    }

    def updateAssetInfo(
        sender: KeyPair,
        assetId: String,
        updatedName: String,
        updatedDescription: String,
        fee: Long,
        feeAsset: Asset = Waves,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): PBSignedTransaction = {
      maybeWaitForTransaction(
        sync(async(n).updateAssetInfo(sender, assetId, updatedName, updatedDescription, fee, feeAsset, version)),
        waitForTx
      )
    }

    def assetInfo(assetId: String): AssetInfoResponse = sync(async(n).assetInfo(assetId))

    def blockAt(height: Int): VanillaBlock = {
      val block = blocks.getBlock(BlockRequest.of(includeTransactions = true, BlockRequest.Request.Height.apply(height))).getBlock
      PBBlocks.vanilla(block).toEither.explicitGet()
    }

    def blockHeaderAt(height: Int): Header = {
      blocks.getBlock(BlockRequest.of(includeTransactions = true, BlockRequest.Request.Height.apply(height))).getBlock.getHeader
    }

    def blockById(blockId: ByteString): VanillaBlock = {
      val block = blocks.getBlock(BlockRequest.of(includeTransactions = true, BlockRequest.Request.BlockId.apply(blockId))).getBlock
      PBBlocks.vanilla(block).toEither.explicitGet()
    }

    def blockSeq(fromHeight: Int, toHeight: Int): Seq[VanillaBlock] = {
      val blockIter = blocks.getBlockRange(BlockRangeRequest.of(fromHeight, toHeight, includeTransactions = true, BlockRangeRequest.Filter.Empty))
      blockIter.map(blockWithHeight => PBBlocks.vanilla(blockWithHeight.getBlock).toEither.explicitGet()).toSeq
    }

    def blockSeqByAddress(address: String, fromHeight: Int, toHeight: Int): Seq[VanillaBlock] = {
      val blockIter = blocks.getBlockRange(
        BlockRangeRequest
          .of(fromHeight, toHeight, includeTransactions = true, BlockRangeRequest.Filter.Generator.apply(ByteString.copyFrom(Base58.decode(address))))
      )
      blockIter.map(blockWithHeight => PBBlocks.vanilla(blockWithHeight.getBlock).toEither.explicitGet()).toSeq
    }

    def getStatuses(request: TransactionsByIdRequest): Seq[PBTransactionStatus] = sync(async(n).getStatuses(request))
  }
}
