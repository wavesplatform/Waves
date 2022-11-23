package com.wavesplatform.database

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream}
import java.net.URL

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.{Transaction, TransactionFactory}
import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable
import scala.util.Using

class TXDB(baseDirectory: String) extends StrictLogging {
  private val entries: mutable.AnyRefMap[ByteStr, Transaction] =  {
    val pairs = Json
      .parse(new BufferedInputStream(new FileInputStream(new File(baseDirectory, "txdb.json"))))
      .as[Seq[JsValue]]
      .map { txJson =>
        val tx = TransactionFactory.fromSignedRequest(txJson).explicitGet()
        tx.id() -> tx
      }
      .toMap
    val builder = mutable.AnyRefMap.newBuilder[ByteStr, Transaction]
    builder ++= pairs
    builder.result()
  }

  private def loadFromPool(id: ByteStr): Transaction = {
    logger.debug(s"Loading $id")
    val url  = new URL(s"https://nodes.wavesnodes.com/transactions/info/${id}")
    val json = Json.parse(url.openStream())
    Using(new FileOutputStream(s"$baseDirectory/${id}.json")) { fos =>
      val bos = new BufferedOutputStream(fos)
        bos.write(Json.toBytes(json))
      bos.flush()
    }
    TransactionFactory.fromSignedRequest(json).explicitGet()
  }

  def load(id: ByteStr): Transaction = entries.getOrElseUpdate(id, loadFromPool(id))
}
