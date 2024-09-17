package com.wavesplatform.transaction

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.test.{FreeSpec, NumericExt}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction

class RideGeneratingBalanceSpec extends FreeSpec with WithDomain {
  "generating balance is updated too early" in {
    val dAppAccount    = TxHelpers.signer(999)
    val anotherAccount = TxHelpers.signer(1)

    val balances = Seq(AddrWithBalance(dAppAccount.toAddress, 123.01.waves), AddrWithBalance(anotherAccount.toAddress, 456.waves))

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
      d.solidStateHeight shouldBe 0

      // Block 1
      d.appendBlock(
        TxHelpers.setScript(dAppAccount, dAppScript), // Note: setScript costs 1_000_000L
        assertBalancesInRide(123.waves, 123.waves, 123.waves, 123.waves)
      )
      d.solidStateHeight shouldBe 1

      // Block 2
      d.appendBlock(TxHelpers.transfer(anotherAccount, dAppAccount.toAddress, 1.waves))
      d.solidStateHeight shouldBe 2

      // Fast-forward to block 999
      Range.inclusive(3, 999).foreach(_ => d.appendBlock())
      d.solidStateHeight shouldBe 999

      // Block 1000
      d.appendBlock(assertBalancesInRide(124.waves, 124.waves, 124.waves, 123.waves))
      d.solidStateHeight shouldBe 1000

      // Block 1001
      d.appendBlock(
        assertBalancesInRide(124.waves, 124.waves, 124.waves, 124.waves),
        TxHelpers.transfer(dAppAccount, anotherAccount.toAddress, 10.waves),
        // Note: we expected the Generating balance to be 124.waves here
        assertBalancesInRide(113.99.waves, 113.99.waves, 113.99.waves, 113.99.waves)
      )
    }
  }
}
