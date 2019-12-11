import scala.io.Source


val words = List("Java", "Linux", "Scala", "kava")

val  mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5'-> "JKL", '6' -> "MNO",
  '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] =
  for ((digit, str) <- mnem; ltr <- str) yield (ltr, digit)

/**
 * Matches a string to its code number in cellphone
 * */
def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

wordsForNum
words.groupBy(word => word.toUpperCase() map charCode)


/**
 * Returns all ways to encode a number as a list of words
 * */
def encode(number: String): Set[List[String]] = {
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet
}
encode("5282")

def translate(number: String): Set[String] = {
  encode(number).map(_ mkString " ")
}

translate("5282")