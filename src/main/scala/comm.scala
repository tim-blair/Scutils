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

import scala.io.Source
import java.io.File

object Comm extends App {
  
  override def main(args: Array[String]) {
    val fileNames = args.takeRight(2)
    val arguments = args.dropRight(2)
    val first = readFile(fileNames(0))
    val second = readFile(fileNames(1))
    
    processFiles(first, second, arguments)
  }
  
  def output(str: Option[String]) = {
    str match {
      case None =>
      case Some(string) => println(string)
    }
  }

  def getOutputDelimiter(arguments: Array[String]): (String, List[String]) = {
    val outputDelimiterParamName = "--output-delimiter"
    val defaultDelimiter = "\t"
      
    def getActualDelimiter(args: Array[String], unusedArgs: List[String]): (String, List[String]) = { 
      if(args.isEmpty)
        (defaultDelimiter, unusedArgs)
      else if(args.head == outputDelimiterParamName)
        (args.tail.head, unusedArgs ::: args.tail.tail.toList)
      else if(args.head.startsWith(outputDelimiterParamName))
        // --long=foo style
        (args.head.substring(outputDelimiterParamName.size + 1), unusedArgs ::: args.tail.toList)
      else
        getActualDelimiter(args.tail, args.head :: unusedArgs)
    }
    getActualDelimiter(arguments, Nil)
  }
  
  def checkArg(argName: String, arguments: List[String]): (Boolean, List[String]) =
    (arguments.contains(argName), arguments.filter(str => str != argName))
    
  def getSuppressions(args: List[String]): (Boolean, Boolean, Boolean, List[String]) = {
    def getActualSuppressions(args: List[String], unusedArgs: List[String]): (Boolean, Boolean, Boolean, List[String]) = {
      if(args.isEmpty)
        (false, false, false, unusedArgs)
      else if(args.head.startsWith("-") && !args.head.startsWith("--")) {
        val arg = args.head
        (arg.contains('1'), arg.contains('2'), arg.contains('3'), args.tail)
      } else
        getActualSuppressions(args.tail, args.head :: unusedArgs)
    }
    getActualSuppressions(args, Nil)
  }
  
  
  def processFiles(first: List[String], second: List[String], args: Array[String]) = {
    val (delimiter, remainingArgs1) = getOutputDelimiter(args)
    val (isForceCheckOrder, remainingArgs2) = checkArg("--check-order", remainingArgs1)
    val (isForceNoCheckOrder, remainingArgs3) = checkArg("--nocheck-order", remainingArgs2)
    val (isCaseInsensitive, remainingArgs4) = checkArg("-i", remainingArgs3)
    val (suppressOne, suppressTwo, suppressThree, _) = getSuppressions(remainingArgs4) 
    
    def firstColumn(str: String): Option[String] = if(suppressOne) None else Some(str)
    def secondColumn(str: String): Option[String] = if(suppressTwo) None else Some(delimiter + str)
    def thirdColumn(str: String): Option[String] = if(suppressThree) None else Some(delimiter + delimiter + str)
    val compare: (String, String) => Int = {
      if(isCaseInsensitive) (left, right) => left.compareToIgnoreCase(right)
      else (left, right) => left.compareTo(right)
    }
    def checkIsOrdered(check: Boolean, strs: List[String], filenum: String): Boolean = {
        def isOrdered(): Boolean = strs.tail.isEmpty || compare(strs.head, strs.tail.head) <= 0
            
            if(check && !isOrdered) {
              System.err.println("comm: file " + filenum + " is not in sorted order")
              false
            } else
              check
    }

    def process(first: List[String], second: List[String], checkFirst: Boolean, checkSecond: Boolean): Unit = {
      def checkFirstOrder(): Boolean = checkIsOrdered(checkFirst, first, "1")
      def checkSecondOrder(): Boolean = checkIsOrdered(checkSecond, second, "2")
      
      def handleFirst() = {
        output(firstColumn(first.head))
        process(first.tail, second, checkFirstOrder, checkSecond)
      }
    	def handleSecond() = {
    	  output(secondColumn(second.head))
    	  process(first, second.tail, checkFirst, checkSecondOrder)
    	}
      
      if(first.isEmpty && second.isEmpty) {
        // End the recursion
      } else if(first.isEmpty) {
        handleSecond
      } else if(second.isEmpty) {
        handleFirst
      } else if(compare(first.head, second.head) == 0) {
        output(thirdColumn(first.head))
        process(first.tail, second.tail, checkFirstOrder, checkSecondOrder)
      } else if(compare(first.head, second.head) > 0) {
        handleSecond
      } else { // first.head < second.head
        handleFirst
      }
    }
    // check order is on by default, so we only need to consider nocheck
    process(first, second, !isForceNoCheckOrder, !isForceNoCheckOrder)
  }
  
  def readFile(file: String): List[String] =
    Source.fromFile(new File(file)).getLines.toList

}