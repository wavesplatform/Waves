package com.wavesplatform.transaction

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction

class RideGeneratingBalanceSpec extends FreeSpec with WithDomain {
  "generating balance is updated too early (TransactionStateSnapshot, STDLIB_VERSION 8)" in {
    val dAppAccount    = TxHelpers.signer(999)
    val anotherAccount = TxHelpers.signer(1)

    val balances = Seq(AddrWithBalance(dAppAccount.toAddress, 100.01.waves), AddrWithBalance(anotherAccount.toAddress, 500.waves))

    withDomain(DomainPresets.TransactionStateSnapshot, balances) { d =>
      // Arrange
      val dAppScript = TxHelpers.script(
        s"""{-# STDLIB_VERSION 8 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable(i)
           |func assertBalances(
           |  expectedRegularBalance: Int,
           |  expectedAvailableBalance: Int,
           |  expectedEffectiveBalance: Int,
           |  expectedGeneratingBalance: Int
           |) = {
           |  let actualRegularBalance = wavesBalance(this).regular
           |  let actualAvailableBalance = wavesBalance(this).available
           |  let actualEffectiveBalance = wavesBalance(this).effective
           |  let actualGeneratingBalance = wavesBalance(this).generating
           |
           |  strict checkRegular = if (actualRegularBalance != expectedRegularBalance)
           |    then throw("Expected Regular balance to be: " + toString(expectedRegularBalance) + ", But got: " + toString(actualRegularBalance))
           |    else unit
           |
           |  strict checkAvailable = if (actualAvailableBalance != expectedAvailableBalance)
           |    then throw("Expected Available balance to be: " + toString(expectedAvailableBalance) + ", But got: " + toString(actualAvailableBalance))
           |    else unit
           |
           |  strict checkEffective = if (actualEffectiveBalance != expectedEffectiveBalance)
           |    then throw("Expected Effective balance to be: " + toString(expectedEffectiveBalance) + ", But got: " + toString(actualEffectiveBalance))
           |    else unit
           |
           |  strict checkGenerating = if (actualGeneratingBalance != expectedGeneratingBalance)
           |    then throw("Expected Generating balance to be: " + toString(expectedGeneratingBalance) + ", But got: " + toString(actualGeneratingBalance))
           |    else unit
           |  ([], unit)
           |}""".stripMargin
      )

      def assertBalancesInRide(regular: Long, available: Long, effective: Long, generating: Long): InvokeScriptTransaction =
        TxHelpers.invoke(
          dAppAccount.toAddress,
          Some("assertBalances"),
          Seq(CONST_LONG(regular), CONST_LONG(available), CONST_LONG(effective), CONST_LONG(generating)),
          payments = Seq(),
          invoker = anotherAccount
        )

      // Act, assert
      d.blockchain.height shouldBe 1

      // Block 1
      d.appendBlock(
        TxHelpers.setScript(dAppAccount, dAppScript), // Note: setScript costs 0.01.waves
        assertBalancesInRide(100.waves, 100.waves, 100.waves, 100.waves)
      )
      d.blockchain.height shouldBe 2

      // Block 2
      d.appendBlock(TxHelpers.transfer(anotherAccount, dAppAccount.toAddress, 10.waves))
      d.blockchain.height shouldBe 3

      // Fast-forward to block 1000
      Range.inclusive(3, 999).foreach(_ => d.appendBlock())
      d.blockchain.height shouldBe 1000

      // Block 1001
      // This assertion tells us that the generating balance
      // is not being updated until the block 1002, which is expected,
      // because `10.waves` was sent on height = 3, 
      // and until height 1002 the balance is not updated
      // (...the lowest of the last 1000 blocks, including 3 and 1002)
      d.appendBlock(assertBalancesInRide(110.waves, 110.waves, 110.waves, 100.waves))
      d.blockchain.height shouldBe 1001

      // Block 1002
      d.appendBlock(
        // This assertion tells us that the generating balance
        // was already updated after `10.waves` was sent on height = 3
        assertBalancesInRide(110.waves, 110.waves, 110.waves, 110.waves),
        TxHelpers.transfer(dAppAccount, anotherAccount.toAddress, 50.waves),
        // This assertion tells us that the generating balance
        // was updated by a transaction in this block.
        // We (probably) expect the generating balance to be 110.waves here...
        assertBalancesInRide(59.99.waves, 59.99.waves, 59.99.waves, 59.99.waves)
      )
    }
  }
}
