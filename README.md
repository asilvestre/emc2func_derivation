emc2func_derivation
===================
La propuesta Número 2 (por si alguien participo en su momento en la GJ) tiene que ver con resolución analítica de ecuaciones (un tema con el que estoy jugando en casa últimamente). El enunciado sería algo así: 

"Desarrollar una función que, dada una expresión algebráica y una variable, devuelva una función que calcula la derivada de la expresión respecto a esa variable. El formato de las expresiones es el siguiente:

expresion := operador expresion expresion | constante | variable
operador := sum | diff | div | mul | pow (opcionalmente: sqrt | sin | cos | log | neg, lo que cambiaría ligeramente la definición de expresion)
constante := <valor numérico en el lenguaje usado>
variable :=  <keyword en el lenguaje usado>
"

Ejemplo:

Para la entrada (4x^2 + 5y) dx se debería devolver la función f(x) = 8x. En cambio para (4x^2 + 5y) dy debería devolver 
la función constante f(x) = 5.

O, en clojure, la invocación:

(differentiate '(+ (* 4 (pow 2 :X)) (* 5 :Y)) :X) 

debería devolver algo como (fn [x] (* 8 x)) (se deja al juicio del lector cómo pasar los parámetros necesarios de la función). De forma que para calcular la derivada de la función objetivo en el punto x=2, podríamos hacer:

((differentiate '(+ (* 4 (pow 2 :X)) (* 5 :Y)) :X) 2) 

siendo f(x) = 16.
