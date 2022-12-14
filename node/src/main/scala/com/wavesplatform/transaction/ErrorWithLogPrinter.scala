package com.wavesplatform.transaction

import cats.Id
import com.wavesplatform.lang.CommonError
import com.wavesplatform.lang.v1.compiler.TermPrinter
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.transaction.TxValidationError.FailedTransactionError

import scala.annotation.tailrec

object ErrorWithLogPrinter {

  def logToString(log: Log[Id], limit: Int, depth: Int = 1): String = {
    @tailrec
    def loop(log: Log[Id], limit: Int, sb: StringBuilder): String = {
      log match {
        case _ if limit < 0 => sb.append("\n...").toString()
        case Nil            => sb.toString()
        case (name, Right(v)) :: tail =>
          val logStrItem = s"\n${"\t" * depth}$name = ${TermPrinter(true).prettyString(v, depth)}"
          val length     = logStrItem.length
          if (length <= limit) sb.append(logStrItem)
          loop(tail, limit - length, sb)
        case (name, Left(CommonError(_, Some(fte: FailedTransactionError)))) :: _ =>
          val logStrItem = s"\n${"\t" * depth}$name = ${fte.errorDetails}"
          val length     = logStrItem.length
          if (length <= limit) sb.append(logStrItem)
          val closingBracket = s"${"\t" * depth})"
          sb.append(s"${logToString(fte.log, limit - length - closingBracket.length, depth + 1)}$closingBracket").toString()
        case (name, Left(err)) :: _ =>
          val logStrItem = s"\n${"\t" * depth}$name = $err"
          if (logStrItem.length <= limit) sb.append(logStrItem)
          sb.toString()
      }
    }

    s"${loop(log, limit - 1, new StringBuilder)}\n"
  }
}
