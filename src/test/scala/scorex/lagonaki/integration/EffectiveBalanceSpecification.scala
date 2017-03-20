package scorex.lagonaki.integration

import org.scalatest.{FunSuite, Matchers}

class EffectiveBalanceSpecification extends FunSuite with Matchers with scorex.waves.TestingCommons {
  private def accounts = application.wallet.privateKeyAccounts()

  private def addresses = accounts.map(_.address)

//  private waitForNextBlock()
}
