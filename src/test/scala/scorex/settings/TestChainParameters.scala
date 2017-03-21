package scorex.settings

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import scala.concurrent.duration._

object TestFunctionalitySettings {
  val Disabled = FunctionalitySettings(Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue,
    Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue, Long.MaxValue)
  val Enabled = FunctionalitySettings(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
}

object TestBlockchainSettings {
  private val initialBalance = 100000000000000L

  private val initialBaseTarget = 153722867L

  private val testGenesisSettings = GenesisSettings(0L, 0L, initialBalance, "",
    List(
      GenesisTransactionSettings("3N3rfWUDPkFsf2GEZBCLw491A79G46djvQk", initialBalance / 3),
      GenesisTransactionSettings("3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K", initialBalance / 3),
      GenesisTransactionSettings("3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY", initialBalance / 3)
    ), initialBaseTarget, 5.seconds)

  val Enabled = BlockchainSettings("", 'T', TestFunctionalitySettings.Enabled, testGenesisSettings)

  val Disabled = BlockchainSettings("", 'T', TestFunctionalitySettings.Disabled, testGenesisSettings)
}
