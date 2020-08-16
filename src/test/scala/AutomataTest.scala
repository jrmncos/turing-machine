import concepto._

class AutomataTest extends UnitSpec{

  "El Automata no deterministico generado por la grilla" must "ser equivalente a su version deterministica" in {
    val transiciones = List((1, 'r', 2), (1, 'r', 4), (1, 'b', 5),
      (2, 'r', 4), (2, 'r', 6), (2, 'b', 1), (2, 'b', 3), (2, 'b', 5),
      (3, 'r', 2), (3, 'r', 6), (3, 'b', 5),
      (4, 'r', 2), (4, 'r', 8), (4, 'b', 1), (4, 'b', 5), (4, 'b', 7),
      (5, 'r', 2), (5, 'r', 4), (5, 'r', 6), (5, 'r', 8), (5, 'b', 1), (5, 'b', 3), (5, 'b', 7), (5, 'b', 9),
      (6, 'r', 2), (6, 'r', 8), (6, 'b', 3), (6, 'b', 5), (6, 'b', 9),
      (7, 'r', 4), (7, 'r', 8), (7, 'b', 5),
      (8, 'r', 4), (8, 'r', 6), (8, 'b', 5), (8, 'b', 7), (8, 'b', 9),
      (9, 'r', 6), (9, 'r', 8), (9, 'b', 5))
    val alfabeto = Set('r', 'b')
    val estados = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val AFND = Automata(estados, alfabeto, trancisiones, 1, Set(9))

    val resultAFD = Automata.convertirAFNDaAFD(AFND)

    resultAFD.alfabeto should equal (Set('r', 'b'))
    resultAFD.estados should equal (Set(1, 24, 5, 2468, 1357, 1379, 13579))
    resultAFD.transiciones should contain allOf ((1, 'r', 24),      (1, 'b', 5),
                                                 (24, 'r', 2468),   (24, 'b', 1357),
                                                 (5, 'r', 2468),    (5, 'b', 1379),
                                                 (2468, 'r', 2468), (2468, 'b', 13579),
                                                 (1357, 'r', 2468), (1357, 'b', 13579),
                                                 (1379, 'r', 2468), (1379, 'b', 5),
                                                 (13579, 'r', 2468),(13579, 'b', 13579))
    resultAFD.estadosFinales should equal (Set(1379, 13579))
  }

}
