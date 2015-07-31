package scorex

import scorex.api.http.ApiClient

import scala.io.StdIn

object Cli extends App {

  println("Welcome to the Score command-line client...")
  Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
    println(s"[$command RESULT] " + ApiClient.executeCommand(command))
  }

}
