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
    
    val output = processFiles(first, second, arguments)
    output.foreach(str => str match {
      case None =>
      case Some(string) => println(string)
    })
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

  def processFiles(first: List[String], second: List[String], args: Array[String]): List[Option[String]] = {
    val (delimiter, remainingArgs) = getOutputDelimiter(args)
    def unlessContains(target: String, result: => String): Option[String] = {
      if(remainingArgs.contains(target))
        None
      else
        Some(result)
    }
    def firstColumn(str: String): Option[String] = unlessContains("-1", str)
    def secondColumn(str: String): Option[String] = unlessContains("-2", delimiter + str)
    def thirdColumn(str: String): Option[String] = unlessContains("-3", delimiter + delimiter + str)

    def process(first: List[String], second: List[String]): List[Option[String]] = {
    	if(first.isEmpty && second.isEmpty)
        Nil
      else if(first.isEmpty)
        secondColumn(second.head) :: process(first, second.tail)
      else if(second.isEmpty)
        firstColumn(first.head) :: process(first.tail, second)
      else if(first.head == second.head)
        thirdColumn(first.head) :: process(first.tail, second.tail)
      else if(first.head > second.head)
        secondColumn(second.head) :: process(first, second.tail)
      else // first.head < second.head
        firstColumn(first.head) :: process(first.tail, second)
    }
    process(first, second)
  }
  
  def readFile(file: String): List[String] = {
    Source.fromFile(new File(file)).getLines.toList
  }

}