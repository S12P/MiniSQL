# Projet de DBDM fait par Guillaume Cluzel et Stéphane Pouget

## Compilation

### Dependances :
opam avec ocaml version 4.06.1

paquets nécessaires : obuild, csv

### Pour installer les modules nécessaires et pour initialiser :
make install

### Pour compiler :
make

### Pour supprimer les fichiers de compilation :
make clean

## Présentation du projet

### Travail réalisé

* Pour le rendu 1 tout à été fait. Le NOT IN semble toutefois renvoyer quelque chose d'incorrect. Sans certitude.
* Pour le rendu 2
   * Nous supprimons les colonnes non nécessaires dans une table, notamment lors de la lecture des fichiers CSV, nous ne gardons que les colonnes qui seront utiles.
   * Nous n'avons pas réussi à améliorer le produit cartésien pour le rentre plus rapide, en faisant des projections plus tôt et en réorganisant l'ordre des produits cartésiens. 
   * En outre, nous avons ajouté des opérateurs d'agrégation (AVG, COUNT, MAX, MIN) qui peuvent être lié à un GROUP BY, ou non.
   * Nous avons ajouté un opérateur ORDER BY pour trier le résultat d'une requête.
   * Pour ne pas avoir tout à refaire, nous n'avons pas à proprement parler typé minisql avec des entiers ou des chaînes de caractères. Nous analysons seulement lorsque le cas se présente si les données que nous manipulons sont des entiers ou des chaînes de caractères. C'est assez raisonnable de faire comme ça si on suppose que les fichiers sont assez homogènes.

### Organisation du travail

* **dataType.ml** : définition des types de base pour exécuter une requête SQL
* **table.ml** : contient un module définissant et permettrant de gérer des tables
* **cond.ml** : contient un module permettant des opérations sur les conditions dans le WHERE
* **requete.ml** : contient des fonctions utiles pour faire des opérations sur les requêtes (suppression des IN et NOT IN par exemple)
* **minisql** : fichier principal du projet

### Problèmes connus et futures améliorations

Problème connus :

* Le renommage des noms de variables ne fonctionne pas toujours super bien, en particulier quand on veut réutiliser ces noms dans un GROUP By par exemple. Les colonnes sont aussi renommés avec le nom de la fonction d'agrégation utilisées ce qui empêche d'utiliser un GROUP BY par dessus.
* Le produit cartésien est trop long et n'est pas optimisé.
* Le NOT IN ne semble pas renvoyer le bon résultat sur q3.


Futures améliorations:
* Les problèmes ci-dessus serait à résoudre, et notamment améliorer le produit cartésiens pour le rendre plus rapide, et pour qu'il prenne moins de place en mémoire.
* Améliorer le NOT IN pour qu'il fonctionne et qu'il prenne en compte plus de cas (pour l'instant seule les conditions sous forme DNF sont gérées).
