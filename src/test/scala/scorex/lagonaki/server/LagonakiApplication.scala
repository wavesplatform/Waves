package scorex.lagonaki.server

import akka.actor.ActorSystem
import com.wavesplatform.Application
import com.wavesplatform.settings.WavesSettings
import scorex.settings.Settings
import scorex.waves.UnitTestNetParams

class LagonakiApplication(val settingsFilename: String)
  extends Application(ActorSystem("test"), new WavesSettings(Settings.readSettingsJson(settingsFilename)) {
    override lazy val chainParams = UnitTestNetParams
    override lazy val walletDirOpt = None
    override lazy val dataDirOpt = None
  })