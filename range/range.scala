import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.io._

class IpRange( val start: Long, val end: Long, val title: String )

object RangeParser {
	def ip2int( address: String ) : Long = {
		var n : Long = 0
		
		for( a <- address.split("\\.") ) {
			n = n << 8 | ( a.toInt & 0xff )
		}

		return n;
	}

	def readRanges() : List[IpRange] = {
		var ranges = new ListBuffer[IpRange]()

		for (line <- Source.fromFile("ranges.tsv").getLines()) {
			var data = line.split("\t")
			var ips = data(0).split("-")
			ranges += new IpRange( ip2int(ips(0)), ip2int(ips(0)), data(1) )
		}

		return ranges.toList
	}

	def main(args: Array[String]) = {

		var ranges = readRanges()

		val pw = new PrintWriter(new File("output.tsv" ))

		for (line <- Source.fromFile("transactions.tsv").getLines()) {
			var data = line.split("\t")
			var ip = ip2int(data(1))
			
			for( r <- ranges.filter( x=> x.start >= ip && x.end <= ip ).map( _.title ) ) {
				pw.print(data(0))
				pw.print("\t")
				pw.print(r)
				pw.println()
			}
		}

		pw.close()
	}
}