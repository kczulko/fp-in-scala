import com.github.kczulko.chapter13.TailRec$
import TailRec.{PrintLine, ReadLine}
import com.github.kczulko.chapter13.tailrec.TailRec

def fahrenheitToCelsius(v: Double): Double = {
  (v - 32)*5.0/9.0
}

def converter: TailRec[Unit] = for {
  _ <- PrintLine("Enter a temperature in degrees Fahrenheit")
  d <- ReadLine.map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()

val echo = ReadLine.flatMap(PrintLine)
val readInt = ReadLine.map(_.toInt)
val readInts = TailRec.map2(readInt, readInt)((_,_))

TailRec.replicateM(10, ReadLine)
