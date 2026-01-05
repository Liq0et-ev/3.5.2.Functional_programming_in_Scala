/*Vladislav Ebert
241RDB316*/

import scala.io.StdIn.readLine

object Main {

  case class Mezgls(v: Int, l: Option[Mezgls], r: Option[Mezgls])

  def ielikt(koks: Option[Mezgls], x: Int): Option[Mezgls] =
    koks match {
      case None => Some(Mezgls(x, None, None))
      case Some(m) =>
        if (x < m.v) Some(Mezgls(m.v, ielikt(m.l, x), m.r))
        else if (x > m.v) Some(Mezgls(m.v, m.l, ielikt(m.r, x)))
        else koks}

  def mekl(koks: Option[Mezgls], x: Int): String =
    koks match {
      case None => "not found"
      case Some(m) =>
        if (m.v == x) "*"
        else if (x < m.v)
          mekl(m.l, x) match {
            case "not found" => "not found"
            case ceļš => "*" + "L" + ceļš.drop(1)}
        else
          mekl(m.r, x) match {
            case "not found" => "not found"
            case ceļš => "*" + "R" + ceļš.drop(1)}}

  def main(args: Array[String]): Unit = {
    val n = readLine().trim.toInt
    val sk = readLine().trim.split(" ").map(_.toInt).toList
    val x = readLine().trim.toInt

    val koks = sk.foldLeft(Option.empty[Mezgls]) { (k, v) => ielikt(k, v) }
    println(mekl(koks, x))}}
