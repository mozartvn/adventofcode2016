import scala.io.Source
var count : Int = 0

def isTr(a: Int, b: Int, c: Int) : Boolean = ( a + b ) > c && ( a + c ) > b && ( b + c ) > a


for ( l <- Source.fromFile("day03.input").getLines() )
{
	val t = l.split(" ")
			 .toList
			 .filter(_ != "")
			 .map(_.toInt)

	if (isTr(t(0), t(1), t(2))) count += 1
}

println(count)

//part 2
def isSame(a: Int, b: Int, c: Int) : Boolean = ( a % 100 == b % 100) && ( a % 100 == c % 100 )

count = 0
for ( l <- Source.fromFile("day03.input").getLines() )
{
	val t = l.split(" ")
			 .toList
			 .filter(_ != "")
			 .map(_.toInt)

	if ( isTr( t(0), t(1), t(2)) && isSame( t(0), t(1), t(2) ) ) {
		println(t)
		count += 1
	}
}

println(count)