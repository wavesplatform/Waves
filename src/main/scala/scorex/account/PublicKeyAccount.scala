package scorex.account

import controller.Controller
import scorex.crypto.Crypto

class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Crypto.getAddress(publicKey)){
  def generatingBalance: BigDecimal = Controller.blockchainStorage.generationBalance(address)
}