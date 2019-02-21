package com.wavesplatform.transactions.capnp
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.WritableByteChannel

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transactions.capnp.Transactions.Transaction
import org.capnproto.{MessageBuilder, Serialize}

object CAPNPTest extends App {
  val defaultRecipient = PublicKeyAccount(Array.fill(32)(0: Byte))


  val msg = new MessageBuilder()

  val transaction = msg.initRoot(Transaction.factory)
  transaction.setChainId(100)
  transaction.setTimestamp(System.currentTimeMillis())
  transaction.setVersion(1)

  val data = transaction.initData().initGenesis()
  data.setAmount(1000)

  val recipient = data.initRecipient()
  recipient.setAddress(defaultRecipient.bytes.arr)


  val buffer = ByteBuffer.allocate(Serialize.computeSerializedSizeInWords(msg).toInt * 8)
  val outputStream = new org.capnproto.ArrayOutputStream(buffer)
  Serialize.write(outputStream, msg)
  outputStream.close()

  val bytes = ByteStr(buffer.array())
  println(bytes)
  println(bytes.toString == "1111JCbt7hujK9qHboWCAJiqo6RGecABtMfh4BptmQdG2KKdfE2DHXN1T4NqpRefR5yN37knY4Frb5b6vo8XomDSKYYyKBnzzStGd1tTdBM4pXU6EtXrKXkLuoDrRrJiVtFDnyTUXXdmdZh72xjiXvpKFML4Qsjc4P")
  println(bytes.hashCode())
}
