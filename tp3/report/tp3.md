---
title: TP3 - Analyse syntaxique ascendante
author: François Boschet
header-includes:
    - \usepackage[frenchb]{babel}
    - \usepackage{graphicx}
    - \usepackage{float}
    - \usepackage{hyperref}
    - \usepackage[top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
    - \usepackage{fancyhdr}
    - \usepackage{verbatim}
    - \usepackage{multirow}
    - \pagestyle{fancy}
    - \fancyhead[CO,CE]{}
    - \fancyfoot[CO,CE]{}
    - \fancyfoot[LE,RO]{\thepage}
abstract: Réalisation d’un analyseur lexical et syntaxique d’une grammaire LR.
---

Préparation
===========

Après avoir supprimé les règles 15 et 16 car elles ne concernent pas
l'analyse syntaxique, nous obtenons la grammaire suivante à partir de
la ligne 9:

\verbatiminput{grammar}

A partir de cette grammaire nous avons fait sa clôture SLR
(voir annexe \ref{app:cloture} ).
Puis nous avons obtenu cette table SLR en faisant attention aux conflits :
\input{slr_table}


Questions de compréhension du TP
================================

Q 3.1
-----

Un crible filtre la suite des lexèmes et génère les identifiants. En utilisant ocamllex,
on peut générer une hashtable pour stocker les identifiants et limiter ainsi le nombre
de transitions (ocamllex est limité à 32767 transitions).

Q 3.2
-----

Écrire une grammaire sous forme LR plutôt qur LL comporte plusieurs avantage,
la classe de grammaire couverte par un analyseur LR est plus large et la détection d’erreur est
faite plus tôt (dans le parseur)

Q 3.3
-----

Une colonne vide dans une table SLR correspond à un token inutilisé.


Sources
=======

Les sources sont toutes disponibles dans le dossier ../src/

Tests
=====

Les tests sont disponibles dans le dossier ../test/

\appendix

SLR Cloture Table
=================

\begin{figure}[H]
    \input{slr_closure}
    \caption{SLR Cloture Table}
    \label{app:cloture}
\end{figure}
