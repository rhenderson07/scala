package problem4

import common.UserInput
import scala.util.parsing.json._

/**
 * Is it nice out today? Or should I grab my coat?
 * Using the OpenWeatherMap API at http://openweathermap.org/
 * create a program that prompts for a city name and returns the current temperature for the city.
 *
 * Constraint:
 * - Keep the processing of the weather feed separate from the part of your program that displays the results.
 *
 * Challenges: *
 * 1) The API gives the sunrise and sunset times, as well as the humidity and a description of the weather. Display that data in a meaningful way.
 * 2) The API gives the wind direction in degrees. Convert it to words such as “North,” “West,” “South,” “Southwest,” or even “South-southwest.”
 * 3)	Develop a scheme that lets the weather program tell you what kind of day it is. If it’s 70 degrees and clear skies, say that it’s a nice day out!
 * 4) Display the temperature in both Celsius and Fahrenheit.
 * 5) Based on the information, determine if the person needs a coat or an umbrella.
 */
object WeatherReporter extends App {

  val OpenWeatherBaseUrl = "http://api.openweathermap.org/data/2.5/weather?q="
  val OpenWeatherAppId = "&appid=2de143494c0b295cca9337e1e96b00e0"

  def parseWeatherJsonString(weatherJsonStr: String) = {
    // functional use of JSON
    class CC[T] { def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T]) }
    object M extends CC[Map[String, Any]]
    object D extends CC[Double]
    object S extends CC[String]

    val functionalJsonWeather = for {
      Some(M(map)) <- Some(JSON.parseFull(weatherJsonStr))
      S(name) = map("name")
      M(main) = map("main")
      D(temp) = main("temp")
      D(pressure) = main("pressure")
      D(humidity) = main("humidity")
    } yield { 
      new Weather(name, temp, pressure, humidity)
    }

    functionalJsonWeather
  }

  def buildWeatherUrl(city: String) = {
    OpenWeatherBaseUrl + city + OpenWeatherAppId
  }

  def getWeatherAsJsonStr(weatherUrl: String) = {
    val weatherJson = scala.io.Source.fromURL(weatherUrl).mkString
    weatherJson.trim() // remove trailing new line
  }

  def getWeather(city: String) = {
    val url = buildWeatherUrl(city)
    val weatherJsonStr = getWeatherAsJsonStr(url)

    if (weatherJsonStr.isEmpty)
      println("No data found for this city")
    else {
      val weatherTuple = parseWeatherJsonString(weatherJsonStr)
      println(weatherTuple)
    }
  }

  def run = {
    print("Where are you? ")
    val city = UserInput.promptString()
    getWeather(city)
  }

    run

  //  val testURL = "http://api.openweathermap.org/data/2.5/weather?q=London,uk&appid=2de143494c0b295cca9337e1e96b00e0"
//  val testResponse = """{"coord":{"lon":-0.13,"lat":51.51},"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03n"}],"base":"cmc stations","main":{"temp":283.58,"pressure":1026.19,"humidity":93,"temp_min":283.58,"temp_max":283.58,"sea_level":1036.42,"grnd_level":1026.19},"wind":{"speed":5.72,"deg":233},"clouds":{"all":44},"dt":1448995277,"sys":{"message":0.0042,"country":"GB","sunrise":1448955864,"sunset":1448985284},"id":2643743,"name":"London","cod":200}"""
//  val testData = parseWeatherJsonString(testResponse)
//  println(testData)
}