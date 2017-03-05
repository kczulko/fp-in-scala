import com.github.kczulko.chapter15.Process.{exists, lift}

val multiplyByTwo: Process[Int, Int] = lift[Int,Int](_ * 2)
val modifiedStream: Stream[Int] = multiplyByTwo(Stream(1,2,3))
modifiedStream.toList

val evenOnly = Process.filter((i: Int) => i % 2 == 0)
evenOnly(Stream(1,2,3,4,5)).toList

val evensPlusOne = evenOnly |> multiplyByTwo
evensPlusOne(Stream(1,2,3,4)).toList

lift[Int,Int](identity).zipWithIndex(Stream(5,4,3,2,1)).toList
