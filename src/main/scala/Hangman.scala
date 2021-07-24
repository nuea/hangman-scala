import io.circe.parser._

import scala.io.Source
import io.circe.generic.auto._
import io.circe.Error

import scala.io.StdIn._
import scala.util.Try

case class Category(name: String, file: String)

case class Word(word: String, hint: String)

object Hangman {

  def readCategories(file: String): Either[Error, List[Category]] = {
    val categoriesText = Source.fromResource(file).mkString
    decode[List[Category]](categoriesText)
  }

  def readWords(file: String): Either[Error, List[Word]] = {
    val wordText = Source.fromResource(file).mkString
    decode[List[Word]](wordText)
  }

  def renderWord(word: String, chars: List[Char]): String = {
    val str: String = chars.map(_.toUpper).appendedAll(chars).mkString(",")
    val regex: String = if (chars.isEmpty) "[a-zA-Z]" else s"(?![$str])[a-zA-Z]"
    word
      .replaceAll("_", " ")
      .replaceAll(regex, "_")
  }

  def play(guessWord: Word): Unit = {
    var rounds: Int = 10
    var score = 0
    var chars = Set[Char]()
    var word: String = ""
    var correctStack = 0

    println(s"Hint: ${guessWord.hint}")
    // List(e, x, q, l, p, r, k, o, d)
    val guessList = guessWord.word.filter(_.isLetter).toLowerCase().toSet

    while (rounds > 0) {
      word = renderWord(guessWord.word, chars.toList)
      println(s"$word \t Score: $score, Remaining wrong guess: $rounds, Wrong guessed: ${chars.filter(!guessList.contains(_)).mkString(", ")}")
      if (chars.intersect(guessList) == guessList) {
        println(s"You Winner. Score: $score")
        rounds = -1
      }
      if (rounds != -1) {
        val inputChar: Char = readChar().toLower

        if (guessList.contains(inputChar)) {
          if (chars.isEmpty) {
            correctStack += 1
            score += 20
          }
          else if (!chars.contains(inputChar)) {
            correctStack += 1
            if (correctStack > 1) println(s"Combo!! $correctStack")
            if (correctStack > 5) score += rounds * 5 else score += rounds * correctStack
          }

        }

        if (!guessList.contains(inputChar)) {
          rounds -= 1
          score -= 2
          correctStack = 0
        }
        chars += inputChar
      }
      if (rounds == 0) println(s"You lost. Score: $score")
    }
  }

  def main(args: Array[String]): Unit = {

    println("Select category number: ")
    // read Categories
    readCategories("categoryIndex.json") match {
      case Left(_) => println("Malformation!!")
      case Right(categories) => {
        for (i <- categories.indices)
          println("%d. %s".format(i + 1, categories.apply(i).name))

        var inputCategory: String = ""
        var facter: Boolean = true

        while ( facter ){
          inputCategory = scala.io.StdIn.readLine()
          if(Try(inputCategory.toInt).isSuccess && (1 <= inputCategory.toInt && inputCategory.toInt <= categories.length)) facter = false
          if (facter) println("Enter category number again.")
        }

        readWords(categories.apply(inputCategory.toInt - 1).file) match {
          case Left(_) => println("Malformation!!")
          case Right(words) => play(words.apply(scala.util.Random.nextInt(words.length)))
        }

      }
    }
  }
}
