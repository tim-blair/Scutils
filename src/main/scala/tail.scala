/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

//Use java's RandomAccessFile to read from a file without starting at the beginning...

var count = 10
var bytes = -1
var pid = -1
var files: List[(io.Source, String)] = Nil
var follow = false
var silent = false
var verbose = false
var retry = false
var delay = 0
var max = -1
var startLine = 0
var startByte = 0
val iter = args.iterator
for(arg <- iter) {
	arg.split("=") match {
		case Array("-n") => {
			val str: Seq[Char] = iter.next
			str match {
				case Seq('+', num @ _*) => startLine = num.toString.toInt
				case Seq(num @ _*) => count = num.toString.toInt
			}
		}
		case Array("--lines", n) => {
			if(n.startsWith("+"))
				startLine = n.toInt
			else
				count = n.toInt
		}
		case Array("-c") => {
			val str: Seq[Char] = iter.next
			str match {
				case Seq('+', num @ _*) => startByte = num.toString.toInt
				case Seq(num @ _*) => bytes = num.toString.toInt
			}
		}
		case Array("--bytes", n) => {
			if(n.startsWith("+"))
				startByte = n.toInt
			else
				bytes = n.toInt
		}
		case Array("--pid", n) => pid = n.toInt
		case Array("-f") => follow = true
		case Array("--follow") => follow = true
		case Array("--follow", "name") => follow = true //TODO: name
		//TODO: desc. Not sure I can copy that effectively in scala/java
		//although desc is the default in gnu
		case Array("--follow", "descriptor") => follow = true
		case Array("--max-unchanged-stats", n) => max = n.toInt
		case Array("-q") => silent = true
		case Array("-quiet") => silent = true
		case Array("-silent") => silent = true
		case Array("--retry") => retry = true
		case Array("-s") => delay = iter.next.toInt
		case Array("--sleep-interval", n) => delay = n.toInt
		case Array("-v") => verbose = true
		case Array("--verbose") => verbose = true
		//TODO: help and version (maybe)
		case Array("-") => files = (io.Source.stdin, "standard in") :: files
		case Array(x) => files = (io.Source.fromFile(x), x) :: files
		case _ => println("Usage: not what you did :P")
	}
}

val showHeader = (files.length > 1 || verbose) && !silent

files.foreach(src => {
	if(showHeader)
		println("==> " + src._2 + "<==")
	if(startByte > 0 || bytes > 0) {
		val str = src._1.mkString
		if(startByte > 0)
			print(str.drop(startByte - 1))
		else
			print(str.drop(str.length - bytes))
	} else {
		val lines = src._1.getLines.toList
		if(startLine > 0)
			lines.drop(startLine - 1).foreach(println)
		else
			lines.drop(lines.length - count).foreach(println)
	}
})




/*val str: Seq[Char] = arg
arg match {
	case Seq("-", "n") => count = iter.next.toInt
	case Seq("-", "-", "b"
}*/
/*arg match {
	case "-n" => count = iter.next.toInt
	case x if x.startsWith("--lines=") => count = x.split("=")(1)
}*/
