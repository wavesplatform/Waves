package scorex.api.client

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import play.libs.Json
import scorex.settings.Settings

import scala.io.StdIn
import scala.util.{Failure, Success, Try}


class ApiClient(settings: Settings) {

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

      val url = new URL("http://127.0.0.1:" + settings.rpcPort + "/" + path)
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

      Try(Json.parse(result)).map(_.toString)
    }.flatten match {
      case Success(result) => result
      case Failure(e) =>
        s"Problem occurred $e! \n Type help to get a list of commands."
    }
  }
}

object ApiClient {


  def main(args: Array[String]): Unit = {
    val settingsFilename = args.headOption.getOrElse("settings.json")
    val settings = Settings(settingsFilename)
    val apiClient = new ApiClient(settings)

    println("Welcome to the Scorex command-line client...")
    Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
      println(s"[$command RESULT] " + apiClient.executeCommand(command))
    }
  }
}
