package com.wavesplatform.state.diffs

import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.utils.EthConverters.*
import org.web3j.crypto.Bip32ECKeyPair

class EthereumTransactionDiffTest extends PropSpec with WithDomain {

  property(
    s"public keys with leading zeros and shortened byte representation are allowed only after ${BlockchainFeatures.ConsensusImprovements} activation"
  ) {
    val senderAcc = Bip32ECKeyPair.generateKeyPair("i1".getBytes)
    senderAcc.getPublicKey.toByteArray.length shouldBe <(EthereumKeyLength)

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 3),
      Seq(AddrWithBalance(senderAcc.toWavesAddress))
    ) { d =>
      val transfer = EthTxGenerator.generateEthTransfer(senderAcc, senderAcc.toWavesAddress, 1, Waves)
      d.appendAndCatchError(transfer) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid public key"),
        transfer
      )
      d.appendBlock()
      d.appendAndAssertSucceed(transfer)
    }

    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(BlockchainFeatures.ConsensusImprovements -> 4),
      Seq(AddrWithBalance(senderAcc.toWavesAddress), AddrWithBalance(TxHelpers.defaultAddress))
    ) { d =>
      val invoke = EthTxGenerator.generateEthInvoke(senderAcc, TxHelpers.defaultAddress, "test", Nil, Nil)

      val dApp = TestCompiler(V6).compileContract("""
                                                    |@Callable(i)
                                                    |func test() = []
                                                    |""".stripMargin)

      d.appendBlock(TxHelpers.setScript(TxHelpers.defaultSigner, dApp))

      d.appendAndCatchError(invoke) shouldBe TransactionDiffer.TransactionValidationError(
        GenericError("Invalid public key"),
        invoke
      )
      d.appendBlock()
      d.appendAndAssertSucceed(invoke)
    }
  }
}
