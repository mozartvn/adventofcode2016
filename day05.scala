import java.security.MessageDigest

val id = "ugkcyxxp" //"abc"

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
							val ok = c.toList.slice(0,5).mkString == "00000"
							if (ok) count += 1
							ok
				 })			
				.map(_.toList(5))
				.foreach(print)