package parser
import scala.util.matching.Regex
import concepto._

object ParserGramatica {

  def terminales(s: String): Set[Terminal] = {
    val sinEspacios = s.replace(" ", "")
    val sinComas = sinEspacios.replace(",", "") 
    val terminalRegEx: Regex = "[a-z]|[0-9]|\\W".r
    val matches = terminalRegEx.findAllIn(sinComas).toSet
    matches.foldLeft(Set[Terminal]())((ret, y) => ret.+(Terminal(y)) )
  }
  
  def variables(s: String): Set[Variable] = {
    val terminalRegEx: Regex = "([A-Z])\\'*".r
    val matches = terminalRegEx.findAllIn(s).toSet
    matches.foldLeft(Set[Variable]())((ret, y) => ret.+(Variable(y)) )
  }
  
  def producciones(s: List[String]): List[Produccion] = {
    val sinEspacios = s.foldLeft(List[String]())((x, y) => x.++(List(y.replace(" ", ""))))
    sinEspacios.foldLeft(List[Produccion]())((x, y) => x.++(List(extraerProduccion(y)))) 
  }
    
  def simbolos(s: String): List[Simbolo] = {
    val sinComasNiEspacios = s.replace(",", "").replace(" ", "")
    val simboloRegEx: Regex = "([A-Z])|[a-z]|[0-9]|\\W".r   
    val matches = simboloRegEx.findAllIn(sinComasNiEspacios).toList
    matches.foldLeft(List[Simbolo]())((ret, y) =>
      if(esTerminal(y))
        ret.++(List(Terminal(y))) 
      else
        ret.++(List(Variable(y))) )
  }
  
  def esTerminal(s: String): Boolean = {
    val charRegEx: Regex = "[a-z]|[0-9]|\\(|\\)|\\*|\\+|\\ε".r
    val ret = charRegEx.findAllIn(s).toList
    !ret.isEmpty
  }
  
  def extraerProduccion(s: String): Produccion = {
    val g = s.split("\\→")
    if(g.size > 1){
    	val produce = simbolos(g(1))
			Produccion(variables(g(0)).head, produce)
    }
    else
      Produccion(Variable(""), List())
  }
  
  def gramatica(s: String): Gramatica = {
    val sinFlechasNiComas = s.replace("→", "").replace(",", "")
    val terminales = ParserGramatica.terminales(sinFlechasNiComas)
    val variables = ParserGramatica.variables(sinFlechasNiComas)
    val p = s.split("\\,")
    val producciones = ParserGramatica.producciones(p.toList)
    
    Gramatica(producciones, terminales, variables, Variable("S"))
  }
    
}