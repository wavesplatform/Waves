package api

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.JSONValue
import settings.Settings
import scala.util.Try


object ApiClient {

	def executeCommand(command:String):String = {
		if(command.equals("help")) {
			"<method> <url> <data> \n Type quit to stop."
		}else Try{
			val args = command.split(" ")
			val method = args.head.toUpperCase
			val path = args(1)

			val content = if(method.equals("POST")){
				command.substring((method + " " + path + " ").length())
			} else ""

			val url = new URL("http://127.0.0.1:" + Settings.getRpcPort + "/" + path)
			val connection = url.openConnection().asInstanceOf[HttpURLConnection]
			connection.setRequestMethod(method)

			if(method.equals("POST")){
				connection.setDoOutput(true)
				connection.getOutputStream.write(content.getBytes)
				connection.getOutputStream.flush()
				connection.getOutputStream.close()
			}

			val stream = connection.getResponseCode match {
				case 200 => connection.getInputStream
				case _   => connection.getErrorStream
			}

			val isReader = new InputStreamReader(stream)
			val br = new BufferedReader(isReader)
			val result = br.readLine()

			JSONValue.parse(result) match {
				case jarr: JSONArray => jarr.toJSONString
				case jo: JSONObject  => jo.toJSONString
				case _ => result
			}
		}.getOrElse("Invalid command! \n Type help to get a list of commands.")
	}
}
