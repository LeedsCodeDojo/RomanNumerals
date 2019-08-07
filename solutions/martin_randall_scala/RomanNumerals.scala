import scala.io.Source

object RomanNumerals extends App {

  /**
   * Load the numeral test cases
   *
   * @return
   */
  def loadNumerals: List[(Int, String)] = {
    val filename = "1-3000.txt"
    for (line <- Source.fromFile(filename).getLines) yield
      line.split("=") match {
        case Array(v1: String, v2: String) => (v1.toInt, v2)
      }
    }.toList

  // Test all cases
  val testValues: Seq[(Int, String)] = loadNumerals
  val converter = new RomanNumeralConverter()

  val passedCases = testValues.map(t => (t, converter.toRoman(t._1), converter.toDecimal(t._2)))
    .collect {
      case (t, r, d) if (t._1 == d && t._2 == r) => t
      case _ => println("failed")
    }

  println(passedCases.length == testValues.length)

  // One off unit test
  val testValue = 1989
  val converted = converter.toRoman(testValue)
  val convertedBackAgain = converter.toDecimal(converted)

  println(testValue)
  println(converted)
  println(convertedBackAgain)
}
