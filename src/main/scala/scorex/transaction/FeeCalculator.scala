package scorex.transaction

import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptComplexityCalculator}
import com.wavesplatform.settings.FeesSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, BalanceSnapshot, ByteStr, DataEntry, Portfolio, VolumeAndFee, _}
import monix.eval.Coeval
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import scorex.transaction.FeeCalculator._
import scorex.transaction.Transaction.Type
import scorex.transaction.ValidationError.{GenericError, InsufficientFee}
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.BlockchainContext
import scorex.transaction.smart.script.Script
import scorex.transaction.smart.script.v1.ScriptV1

import scala.util.Failure

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings, blockchain: Blockchain) {

  private val Kb = 1024

  private val functionCosts: Map[FunctionHeader, Long] = {
    val error = new IllegalStateException("This context has no data")
    val fail  = Coeval.fromTry(Failure(error))
    val emptyBlockchain = new Blockchain {
      override def height: Int                                                                                             = throw error
      override def portfolio(a: Address): Portfolio                                                                        = throw error
      override def balance(address: Address, assetId: Option[AssetId]): Long                                               = throw error
      override def transactionInfo(id: AssetId): Option[(Int, Transaction)]                                                = throw error
      override def transactionHeight(id: AssetId): Option[Int]                                                             = throw error
      override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = throw error
      override def containsTransaction(id: AssetId): Boolean                                                               = throw error
      override def assetDescription(id: AssetId): Option[AssetDescription]                                                 = throw error
      override def resolveAlias(a: Alias): Option[Address]                                                                 = throw error
      override def leaseDetails(leaseId: AssetId): Option[LeaseDetails]                                                    = throw error
      override def filledVolumeAndFee(orderId: AssetId): VolumeAndFee                                                      = throw error
      override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot]                            = throw error
      override def accountScript(address: Address): Option[Script]                                                         = throw error
      override def accountData(acc: Address): AccountDataInfo                                                              = throw error
      override def accountData(acc: Address, key: String): Option[DataEntry[_]]                                            = throw error
      override def assetDistribution(height: Int, assetId: AssetId): Map[Address, Long]                                    = throw error
      override def wavesDistribution(height: Int): Map[Address, Long]                                                      = throw error
      override def allActiveLeases: Set[LeaseTransaction]                                                                  = throw error
      override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]                 = throw error
      override def score: BigInt                                                                                           = throw error
      override def scoreOf(blockId: AssetId): Option[BigInt]                                                               = throw error
      override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]                                             = throw error
      override def blockHeaderAndSize(blockId: AssetId): Option[(BlockHeader, Int)]                                        = throw error
      override def lastBlock: Option[Block]                                                                                = throw error
      override def blockBytes(height: Int): Option[Array[Type]]                                                            = throw error
      override def blockBytes(blockId: AssetId): Option[Array[Type]]                                                       = throw error
      override def heightOf(blockId: AssetId): Option[Int]                                                                 = throw error
      override def lastBlockIds(howMany: Int): Seq[AssetId]                                                                = throw error
      override def blockIdsAfter(parentSignature: AssetId, howMany: Int): Option[Seq[AssetId]]                             = throw error
      override def parent(block: Block, back: Int): Option[Block]                                                          = throw error
      override def approvedFeatures(): Map[Short, Int]                                                                     = throw error
      override def activatedFeatures(): Map[Short, Int]                                                                    = throw error
      override def featureVotes(height: Int): Map[Short, Int]                                                              = throw error
      override def append(diff: Diff, block: Block): Unit                                                                  = throw error
      override def rollbackTo(targetBlockId: AssetId): Seq[Block]                                                          = throw error
    }
    Context.functionCosts(BlockchainContext.build(nByte = 0, fail, fail, emptyBlockchain).functions.values)
  }

  private val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.map { v =>
        val maybeAsset = if (v.asset.toUpperCase == "WAVES") None else ByteStr.decodeBase58(v.asset).toOption
        val fee        = v.fee

        TransactionAssetFee(transactionType, maybeAsset).key -> fee
      }
    }
  }

  def enoughFee(tx: Transaction): Either[ValidationError, Unit] = {
    val (txFeeAssetId, txFeeValue) = tx.assetFee
    val txAssetFeeKey              = TransactionAssetFee(tx.builder.typeId, txFeeAssetId).key
    for {
      txMinBaseFee <- Either.cond(map.contains(txAssetFeeKey), map(txAssetFeeKey), GenericError(s"Minimum fee is not defined for $txAssetFeeKey"))
      script <- Right(tx match {
        case tx: Transaction with Authorized => blockchain.accountScript(tx.sender)
        case _                               => None
      })
      _ <- Either.cond(script.isEmpty || txFeeAssetId.isEmpty,
                       (),
                       ValidationError.GenericError("Scripted accounts can accept transactions with Waves as fee only"))
      minTxFee = minFeeFor(tx, txFeeAssetId, txMinBaseFee)
      _ <- Either.cond(
        txFeeValue >= minTxFee,
        (),
        GenericError {
          s"Fee in ${txFeeAssetId.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minTxFee"
        }
      )

      scriptComplexity <- script match {
        case Some(s: ScriptV1) =>
          ScriptComplexityCalculator(s.expr, functionCosts) match {
            case Right(x) => Right(settings.smartAccount.baseExtraCharge + x)
            case Left(e)  => Left(ValidationError.GenericError(e))
          }
        case Some(x) => throw new IllegalStateException(s"Doesn't know how to calculate complexity for a script of ${x.version} version")
        case None    => Right(0L)
      }

      totalRequiredFee = minTxFee + (scriptComplexity * settings.smartAccount.extraChargePerOp).toLong
      _ <- Either.cond(
        txFeeValue >= totalRequiredFee,
        (),
        InsufficientFee(s"Scripted account requires $totalRequiredFee fee for this transaction, but given: $txFeeValue")
      )
    } yield ()
  }

  private def minFeeFor(tx: Transaction, txFeeAssetId: Option[AssetId], txMinBaseFee: Long): Long = tx match {
    case tx: DataTransaction =>
      val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
      txMinBaseFee * sizeInKb
    case tx: MassTransferTransaction =>
      val transferFeeSpec = map.getOrElse(
        TransactionAssetFee(TransferTransaction.typeId, txFeeAssetId).key,
        throw new IllegalStateException("Can't find spec for TransferTransaction")
      )
      transferFeeSpec + txMinBaseFee * tx.transfers.size
    case _ => txMinBaseFee
  }
}

object FeeCalculator {

  private case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {
    val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"
  }

}
