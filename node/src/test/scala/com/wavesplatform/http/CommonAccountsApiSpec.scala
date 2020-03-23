package com.wavesplatform.http

import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.{AccountDataInfo, Diff, StringDataEntry}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FreeSpec, Matchers}

class CommonAccountsApiSpec extends FreeSpec with Matchers with WithDomain {
  "Data stream" - {
    "handles non-existent address" in {
      withDomain() { d =>
        val address           = KeyPair.fromSeed("matcher").right.get.toAddress
        val dataEntry         = StringDataEntry("test", "test")
        val diff              = Diff.stateOps(accountData = Map(address -> AccountDataInfo(Map(dataEntry.key -> dataEntry))))
        val commonAccountsApi = CommonAccountsApi(diff, d.db, d.blockchainUpdater)
        val data              = commonAccountsApi.dataStream(address, None).toListL.runSyncUnsafe()
        data shouldBe Seq(dataEntry)
      }
    }
  }
}
