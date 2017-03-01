import com.github.kczulko.chapter15.Process
import com.github.kczulko.chapter15.Process.liftOne

val one: Process[Int, Int] = liftOne((i: Int) => i*2)
val modifiedStream: Stream[Int] = one(Stream(1,2,3))
modifiedStream.toList
