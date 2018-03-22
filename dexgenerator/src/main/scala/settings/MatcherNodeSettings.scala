package settings

import cats.Show
import scorex.account.PublicKeyAccount
import settings.MatcherNodeSettings.Settings

case class MatcherNodeSettings(setting: Settings,
                               endpoint: String,
                               matcherKey: PublicKeyAccount) {

}

object MatcherNodeSettings {

  case class Settings(endpoint: String,
                      matcherKey: PublicKeyAccount)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._

      s"""Matcher address:
         |$endpoint
         |$matcherKey""".stripMargin

    }
  }

}
