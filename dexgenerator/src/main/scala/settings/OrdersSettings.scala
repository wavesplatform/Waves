package settings

import cats.Show
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.dexgen.utils.GenOrderType
import settings.OrdersSettings.Settings

class OrdersSettings(settings: Settings, val accounts: Seq[PrivateKeyAccount], val assets: Int) {}

object OrdersSettings {

  case class Settings(orders: Int, assets: Int, probabilities: Map[GenOrderType.Value, Double])

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""orders per iteration: $orders
         |orderbooks: $assets
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}
