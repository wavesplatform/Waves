package scorex.perma

import scorex.lagonaki.server.LagonakiApplication
import scorex.utils.ScorexLogging


object Application extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  val filename = if (args.length > 0) args(0) else "settings.json"

  val application = new LagonakiApplication(filename)

  log.debug("PermaScorex has been started")
  application.run()
  require(application.blockStorage.state(application.blockStorage.history.lastBlock.uniqueId).isDefined)

  if (application.wallet.privateKeyAccounts().isEmpty) application.wallet.generateNewAccounts(1)

}
