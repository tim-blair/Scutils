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
  
  class Printer {
    def output(line: String) = {
      print(line)
      endLine
    }
    
    protected def endLine = println()
  }
  
  class EndPrinter extends Printer {
    override def endLine = {
      print("$")
      super.endLine
    }
  }
  
  class Numberer {
    def printLineNumber(empty: Boolean, num: Int): Int = {
      printf("%6d ", num)
      num + 1
    }
  }
  
  class NoopNumberer extends Numberer {
    override def printLineNumber(empty: Boolean, num: Int): Int = num
  }
  
  class NonBlankNumberer extends Numberer {
    override def printLineNumber(empty: Boolean, num: Int): Int = {
      if(empty) num
      else super.printLineNumber(empty, num)
    }
  }
  
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
		    
    val printer = if(showEnds) new EndPrinter else new Printer
    val numberer = if(nonBlankNum) new NonBlankNumberer else if(numbered) new Numberer else new NoopNumberer
    
	  def printLines(lines: List[String], lineNum: Int, prevWasBlank: Boolean): (Int, Boolean) = {
      if(lines.isEmpty) {
        (lineNum, prevWasBlank)
      } else if(suppress && prevWasBlank && lines.head.isEmpty) {
        printLines(lines.tail, lineNum, true)
      } else {
        val line = lines.head
        val nextNum = numberer.printLineNumber(line.isEmpty, lineNum)
        
        printer.output(line)
        printLines(lines.tail, nextNum, line.isEmpty)
      }
    }
    var lineCount = 1
    var isBlank = false
    for(src <- srcs) {
      val (count, blank) = printLines(src.getLines.toList, lineCount, isBlank)
      lineCount = count
      isBlank = blank
    }
  }
}