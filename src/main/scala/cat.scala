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
object Cat extends App {
  override def main(args: Array[String]) = {
    def checkArg(str: String) =
      args.length > 0 && args.contains(str)

    //-n means number each line
    val numbered = checkArg("-n")
    //-b means number non-empty lines, overrides -n
    val nonBlankNum = checkArg("-b")
    //-E display $ at end of each line
    val showEnds = checkArg("-E")
    //-s suppress repeated empty lines
    val suppress = checkArg("-s")

    val srcs = 
      if(args.length > 0 && args.contains("-"))
		    io.Source.stdin :: Nil
	    else
		    args filter(arg => !arg.startsWith("-")) map(io.Source.fromFile(_)) toList
    var num = 1
    var prevWasBlank = false
    srcs.foreach(src => {
      src.getLines.foreach(line => {
        if(!(suppress && prevWasBlank && line == "")) {
          prevWasBlank = line == ""
          if(nonBlankNum) {
				    // Re-use the check we already did
				    if(prevWasBlank) {
					    printf("%6d ", num)
					    num += 1
				    }
			    } else if(numbered) {
				    // real cat pads and puts a space -- assumes under 1 million lines?
				    printf("%6d ", num)
				    num += 1
			    }
			    print(line)
			    if(showEnds)
				    print("$")
			    println()
		    }
	    })
    })
  }
}