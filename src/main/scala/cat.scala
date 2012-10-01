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
    def checkArg(str: String) = args.contains(str)

    //-n means number each line
    val numbered = checkArg("-n")
    //-b means number only non-empty lines, overrides -n
    val nonBlankNum = checkArg("-b")
    //-E display $ at end of each line
    val showEnds = checkArg("-E")
    //-s suppress repeated empty lines
    val suppress = checkArg("-s")

    val srcs = 
      if(checkArg("-")) io.Source.stdin :: Nil
	    else args.filter(!_.startsWith("-")).map(io.Source.fromFile(_)).toList
		    
		def output(line: String) = {
      print(line)
      if(showEnds)
        print("$")
      println()
	  }
    
    def printLineNumber(num: Int): Int = {
      printf("%6d ", num)
      num + 1
    }
    
	  def printLines(lines: List[String], lineNum: Int, prevWasBlank: Boolean): Unit = {
      if(lines.isEmpty) {
        // nothing to do
      } else if(suppress && prevWasBlank && lines.head.isEmpty) {
        printLines(lines.tail, lineNum, true)
      } else {
        val line = lines.head
        val nextNum =
          if(nonBlankNum && !line.isEmpty) printLineNumber(lineNum)
          else if(numbered && !nonBlankNum) printLineNumber(lineNum)
          else lineNum
          
        output(line)
        printLines(lines.tail, nextNum, line.isEmpty)
      }
    }
  }
}