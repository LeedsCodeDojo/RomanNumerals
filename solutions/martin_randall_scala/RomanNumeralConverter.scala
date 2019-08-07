class RomanNumeralConverter {

  private val DecimalsToNumerals = List(
    1000 -> "M",
    900 -> "CM",
    500 -> "D",
    400 -> "CD",
    100 -> "C",
    90 -> "XC",
    50 -> "L",
    40 -> "XL",
    10 -> "X",
    9 -> "IX",
    5 -> "V",
    4 -> "IV",
    1 -> "I"
  )

  def toRoman(number: Int): String = DecimalsToNumerals.foldLeft((List.empty[String], number)) {
    case ((answer, remaining), (decimal, numeral)) =>
      val count = remaining / decimal
      (answer ++ List.fill(count)(numeral), remaining - decimal * count)
  }._1.mkString


  def toDecimal(value: String, acc: Int = 0, numerals: List[(Int, String)] = DecimalsToNumerals): Int =
    if (value.isEmpty) acc
    else {
      val numeral = numerals.head
      if (value.startsWith(numeral._2))
        toDecimal(value.substring(numeral._2.length), acc + numeral._1, numerals)
      else
        toDecimal(value, acc, numerals.tail)
    }
}
