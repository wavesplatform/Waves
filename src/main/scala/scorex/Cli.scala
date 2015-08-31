package scorex

import scorex.app.LagonakiApplication
import scorex.app.api.http.ApiClient
import scala.io.StdIn

object Cli extends App {
  val filename = if (args.length > 0) args(0) else "settings.json"
  val application = new LagonakiApplication(filename)
  val apiClient = new ApiClient(application)

  println("Welcome to the Score command-line client...")
  Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
    println(s"[$command RESULT] " + apiClient.executeCommand(command))
  }
}
