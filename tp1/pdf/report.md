Compréhension
=============

###q1

Séparer le crible dans l’analyseur lexical permet d’avoir un analyseur lexical plus générique.
Dans l’hypothèse d’ajout de nouvelles unités lexicales, seul le crible sera à modifier, l’analyseur lexical restera inchangé.

###q2

Il est possible de faire reconnaitre les mots-clés `and` et `or` par notre analyseur syntaxique. Notre afd n’a pas a faire
la différence entre les mots-clés et les ident puisque c’est au crible qu’incombe ce travail.

###q3

Les types énumérés permettent une assez bonne lisibilté de code. De plus le filtrage sur les types énumérés est relativement
simple (match with). De plus, comme dans le cas de l’ident, on peut spécifier un constructeur pour un élément du type.

###q4

On applique directement le crible au lexème. Ca permet aussi de pouvoir localiser une erreur plus précisement qu’avec une liste de lexème.

### q5

Ident est considéré comme une String. Si on considère la String comme un type pseudo-primitif, on ne peut plus dériver Ident
C’est donc un terminal.

###q6

Il suffit de créer les nouvelles unités lexicales propre aux conditinelles et aux comparaisons et d’ajouter les règles qui leurs sont
propres au crible.

\newpage

Tests
=====

Les tests sont directement integrés au fichier test.ml qui est une réplique du main un peu modifiée.
Il suffit de compiler avec l’option ```test``` et de lancer ./test.native pour executer lehier de tests.
La liste des cas de tests est :

    1. toto and titi tutu or tata
    2. toto and or titi = rata
    3. tata and or <> = )( /\*aa\*/ > a2 < az2az2
    4. toto and titi/\*/\*testst \*\*\*/
    5. ok /\* ignor \*/ ok /\* ignore \*/ fin

Si le cas de test passe, le programme ecris “OK” sinon il écrit “FAIL”
