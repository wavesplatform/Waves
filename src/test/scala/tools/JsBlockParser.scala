package tools

import play.api.libs.json._
import scorex.account.{Address, AddressScheme, PublicKeyAccount}
import scorex.api.http.assets.SignedTransferRequest
import scorex.transaction.assets.TransferTransaction
import com.wavesplatform.state2._
import scorex.block.Block

import scala.io.Source

object JsBlockParser extends App {

  def parseTx(str: String): TransferTransaction = {
    val js = Json.parse(str)
    val sig = ByteStr.decodeBase58((js \ "signature").get.as[String]).get
    val senderPk = ByteStr.decodeBase58((js \ "senderPublicKey").get.as[String]).get
    val recipient = (js \ "recipient").get.as[String]
    val fee = (js \ "fee").get.as[Long]
    val timestamp = (js \ "timestamp").get.as[Long]
    val amount = (js \ "amount").get.as[Long]
    TransferTransaction(None, PublicKeyAccount(senderPk.arr), Address.fromString(recipient).explicitGet(), amount, timestamp, None, fee, Array.emptyByteArray, sig)
  }

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'
  }

  private val txs: List[TransferTransaction] = Source.fromFile("C:\\Users\\ilyas\\Desktop\\is2.json").getLines
    .toList
    .map(_.trim)
    .map(parseTx)

  val block = ???
}
