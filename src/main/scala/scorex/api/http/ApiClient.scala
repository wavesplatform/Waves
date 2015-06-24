package scorex.api.http

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import play.libs.Json
import scorex.settings.Settings

import scala.util.Try


object ApiClient {

  def executeCommand(command: String): String = {
    if (command.equals("help")) {
      "<method> <url> <data> \n Type quit to stop."
    } else Try {
      val args = command.split(" ")
      val method = args.head.toUpperCase
      val path = args(1)

      val content = if (method.equals("POST")) {
        command.substring((method + " " + path + " ").length())
      } else ""

      val url = new URL("http://127.0.0.1:" + Settings.rpcPort + "/" + path)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod(method)

      if (method.equals("POST")) {
        connection.setDoOutput(true)
        connection.getOutputStream.write(content.getBytes)
        connection.getOutputStream.flush()
        connection.getOutputStream.close()
      }

      val stream = connection.getResponseCode match {
        case 200 => connection.getInputStream
        case _ => connection.getErrorStream
      }

      val isReader = new InputStreamReader(stream)
      val br = new BufferedReader(isReader)
      val result = br.readLine()

      Try(Json.parse(result)).map(_.toString).getOrElse(result)
    }.getOrElse("Invalid command! \n Type help to get a list of commands.")
  }
}
