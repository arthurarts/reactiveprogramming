package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength
import Signal._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /** ****************
    * * TWEET LENGTH **
    * *****************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }


  test("computedelta") {
    // Δ = b² - 4ac
    val result1 = Polynomial.computeDelta(new Signal(1D), new Signal(1D), new Signal(1D))
    assert(result1() == -3)
    val result2 = Polynomial.computeDelta(new Signal(2D), new Signal(6D), new Signal(1D))
    assert(result2() == 28)
    val result3 = Polynomial.computeDelta(new Signal(1D), new Signal(8D), new Signal(3D))
    assert(result3() == 52)
  }

  test("computesolutions") {
    // (-b + √Δ) / (2a)  (-b - √Δ) / (2a)
    //                                            a             b             c               delta
    val result1 = Polynomial.computeSolutions(new Signal(1), new Signal(1), new Signal(1), new Signal(9))
    assert(result1() == Set())
    val result2 = Polynomial.computeSolutions(new Signal(5), new Signal(6D), new Signal(1), new Signal(16))
    assert(result2() == Set(-0.2, -1))
    val result3 = Polynomial.computeSolutions(new Signal(2), new Signal(4), new Signal(2), new Signal(0))
    assert(result3() == Set(-1))
  }


}
