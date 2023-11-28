package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.*
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import org.scalamock.scalatest.MockFactory
import shapeless.Coproduct

class MatcherBlockchainTest extends PropSpec with MockFactory with WithDomain {
  property("ScriptRunner.applyGeneric() avoids Blockchain calls") {
    val blockchain: Blockchain = new Blockchain {
      override def settings: BlockchainSettings                                                             = ???
      override def height: Int                                                                              = ???
      override def score: BigInt                                                                            = ???
      override def blockHeader(height: Int): Option[SignedBlockHeader]                                      = ???
      override def hitSource(height: Int): Option[ByteStr]                                                  = ???
      override def carryFee(refId: Option[ByteStr]): Long                                                   = ???
      override def heightOf(blockId: ByteStr): Option[Int]                                                  = ???
      override def approvedFeatures: Map[Short, Int]                                                        = ???
      override def activatedFeatures: Map[Short, Int]                                                       = ???
      override def featureVotes(height: Int): Map[Short, Int]                                               = ???
      override def blockReward(height: Int): Option[Long]                                                   = ???
      override def blockRewardVotes(height: Int): Seq[Long]                                                 = ???
      override def wavesAmount(height: Int): BigInt                                                         = ???
      override def transferById(id: ByteStr): Option[(Int, TransferTransaction)]                            = ???
      override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)]                              = ???
      override def transactionInfos(ids: Seq[BlockId]): Seq[Option[(TxMeta, Transaction)]]                  = ???
      override def transactionMeta(id: ByteStr): Option[TxMeta]                                             = ???
      override def containsTransaction(tx: Transaction): Boolean                                            = ???
      override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription]                        = ???
      override def resolveAlias(a: Alias): Either[ValidationError, Address]                                 = ???
      override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]                                     = ???
      override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee                                       = ???
      override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)]      = ???
      override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = ???
      override def accountScript(address: Address): Option[AccountScriptInfo]                               = ???
      override def hasAccountScript(address: Address): Boolean                                              = ???
      override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo]                              = ???
      override def accountData(acc: Address, key: String): Option[DataEntry[?]]                             = ???
      override def hasData(address: Address): Boolean                                                       = ???
      override def leaseBalance(address: Address): LeaseBalance                                             = ???
      override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance]                       = ???
      override def balance(address: Address, mayBeAssetId: Asset): Long                                     = ???
      override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long]                        = ???
      override def wavesBalances(addresses: Seq[Address]): Map[Address, Long]                               = ???
      override def effectiveBalanceBanHeights(address: Address): Seq[Int]                                   = ???
      override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset]                    = ???
      override def lastStateHash(refId: Option[ByteStr]): BlockId                                           = ???
    }

    val tx = TransferTransaction.selfSigned(1.toByte, accountGen.sample.get, accountGen.sample.get.toAddress, Waves, 1, Waves, 1, ByteStr.empty, 0)
    val scripts =
      Seq(
        TestCompiler(V5).compileExpression("true"),
        TestCompiler(V5).compileContract(
          """
            |@Callable(i)
            |func foo() = []
            |""".stripMargin
        ),
        TestCompiler(V5).compileContract(
          """
            |@Callable(i)
            |func foo() = []
            |
            |@Verifier(tx)
            |func bar() = true
            |""".stripMargin
        )
      )

    scripts.foreach { script =>
      ScriptRunner
        .applyGeneric(
          Coproduct(tx.explicitGet()),
          blockchain,
          script,
          isAssetScript = false,
          Coproduct(Recipient.Address(ByteStr.empty)),
          defaultLimit = 2000,
          default = null,
          useCorrectScriptVersion = true,
          fixUnicodeFunctions = true,
          useNewPowPrecision = true,
          checkEstimatorSumOverflow = true,
          newEvaluatorMode = true,
          checkWeakPk = true,
          enableExecutionLog = false,
          fixBigScriptField = true,
          fixedThrownError = true
        )
        ._3 shouldBe Right(CONST_BOOLEAN(true))
    }
  }
}
