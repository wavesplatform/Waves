package com.wavesplatform

import java.io.{FileOutputStream, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.utils.ScorexLogging

import scala.util.Try

object ResponsivenessLogs extends ScorexLogging {
  def writeEvent(height: Int, txType: Int, txId: ByteStr, eventType: String): Unit = Try(synchronized {
    val date = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
    val fileStream = new FileOutputStream(s"${sys.props("waves.directory")}/tx-events-$date.csv", true)
    val pw = new PrintWriter(fileStream)
    val logLine = s"$txId,$eventType,$height,$txType,${System.currentTimeMillis()}"
    log.info(logLine)
    try pw.println(logLine)
    finally pw.close()
  })
}
