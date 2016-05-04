import scala.io.Source
import scala.collection.mutable._
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
			ranges += new IpRange( ip2int(ips(0)), ip2int(ips(1)), data(1) )
		}

		return ranges.toList
	}

	/*
	def readRanges() : HashMap[Long,String] = {
		var ranges = new HashMap[Long,String]()

		for (line <- Source.fromFile("ranges.tsv").getLines()) {
			var data = line.split("\t")
			var ips = data(0).split("-")
			for ( var ip <- ( ip2int(ips(0)) to ip2int(ips(0))
			ranges += new IpRange( ip2int(ips(0)), ip2int(ips(0)), data(1) )
		}

		return ranges.toList
	}
	*/

	def printRanges( pw: PrintWriter, ip: Long, ranges: List[IpRange], userId : String ) {
		for( r <- ranges.filter( x => ip >= x.start && ip <= x.end ).map( _.title ) ) {
				pw.print(userId)
				pw.print("\t")
				pw.print(r)
				pw.println()
		}
	}

	def test() = {
		var ip1 = ip2int("63.173.67.135")
		var ip2 = ip2int("63.173.67.218")
		var ip = ip2int("63.173.67.200")

		println(ip1)
		println(ip2)
		println(ip)

		println( ip >= ip1 )
		println( ip <= ip2 )
	}

	def test1() = {
		var ip1 = ip2int("63.173.67.135")
		var ip2 = ip2int("63.173.67.218")
		var ip = ip2int("63.173.67.200")

		var r = new ListBuffer[IpRange]()
		r += new IpRange(ip1, ip2, "Network57")

		val pw = new PrintWriter(System.out)

		printRanges(pw, ip, r.toList, "1")

		pw.flush()
		//pw.close()
	}

	def main(args: Array[String]) = {

		//test()
		//test1()

		//var allRanges = new HashMap[Long,String]()

		var ranges = readRanges()

		val pw = new PrintWriter(new File("output.tsv" ))

		val t0 = System.nanoTime()

		for (line <- Source.fromFile("transactions.tsv").getLines()) {
			var data = line.split("\t")
			var ip = ip2int(data(1))

			printRanges( pw, ip, ranges, data(0))
		}

		val t1 = System.nanoTime()
		println("Elapsed time: " + (t1 - t0) + "ns")

		pw.flush()
		pw.close()
	}
}