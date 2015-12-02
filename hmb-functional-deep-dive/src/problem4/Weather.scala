package problem4

class Weather(val city: String, val temperature: Double, val pressure: Double, val humidity: Double) extends Equals {

  override def toString() = {
    s"city: $city, temperature: $temperature, pressure: $pressure, humidity: $humidity"
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[problem4.Weather]
  }

  override def equals(other: Any) = {
    other match {
      case that: problem4.Weather => that.canEqual(Weather.this) && city == that.city && temperature == that.temperature && pressure == that.pressure && humidity == that.humidity
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime * (prime * (prime + city.hashCode) + temperature.hashCode) + pressure.hashCode) + humidity.hashCode
  }
}