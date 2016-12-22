import java.security.MessageDigest
import scala.util.Try

val id = "ugkcyxxp" 

def md5(s: String) = MessageDigest
					.getInstance("MD5")
					.digest( s.getBytes )
					.map( "%02x".format(_) )
					.mkString

var count = 0
Range(0, Int.MaxValue).view
				.takeWhile( _ => count < 8 )
				.map(c => md5(id + c.toString))
				.filter( c => {
							val ok = c.slice(0,5).mkString == "00000"
							if (ok) count += 1
							ok
				 })			
				.map(_(5))
				.foreach(print)

//part 2
println
count = 0
var password = (for (i <- 0 to 7) yield false).toList
val res = Range(0, Int.MaxValue).view
				.takeWhile( _ => count < 8 )
				.map(c => md5(id + c.toString))
				.filter( c => {
							val pos = c(5)
							val ok = c.slice(0,5).mkString == "00000" && 
									Try(pos.toString.toInt).isSuccess && 
									pos.toString.toInt < 8 &&
									!password(pos.toString.toInt)
							if (ok) {
								count += 1
								password = password.updated(pos.toString.toInt, true)
							}
							ok
				 })			
				.map(_.slice(5,7))
				.sortWith( _ < _ )
				.map(_.tail.toString)
				.foreach(print)