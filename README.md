# Reproduction de l’article « The Conservative Formula: Quantitative Investing Made Easy »

## Description

Ce projet a été réalisé dans le cadre du cours **GSF-3120 Séminaire en finance quantitative** à l’Université Laval.  
L’objectif principal est de reproduire et analyser la stratégie d’investissement proposée dans l’article **The Conservative Formula** de Pim van Vliet et David Blitz, qui combine trois facteurs simples : faible volatilité, rendement net élevé et momentum.

Le code a été développé principalement en **R**, mais certaines fonctions critiques ont été implémentées en **C++** (via `Rcpp`) pour accélérer le traitement de grandes quantités de données.

## Objectifs du projet

- Reproduire la stratégie d’investissement conservatrice proposée dans l’article.
- Construire deux portefeuilles : conservateur et spéculatif.
- Réaliser des simulations et comparaisons avec le marché et les portefeuilles Fama-French.
- Étendre l’analyse aux données récentes (jusqu’en 2023).
- Automatiser le traitement des données et l’exécution du code.
- Présenter les résultats sous forme de rapport structuré.

## Méthodologie

1. Importer les données financières historiques (1925–2023) depuis CRSP, WRDS et Kenneth French Data Library.
2. Nettoyer, formater et filtrer les données pour constituer l’univers d’investissement.
3. Calculer les facteurs : volatilité, Net Payout Yield (NPY), momentum.
4. Construire les portefeuilles conservateur et spéculatif.
5. Réaliser des simulations et comparer les performances aux portefeuilles Fama-French.
6. Étendre l’analyse de 2017 à 2023.
7. Générer graphiques, tableaux et interprétation des résultats.

## Auteurs

- Nicolas Dansereau  
- Maxime Levasseur-Vandal

## Références

- Van Vliet, P., & Blitz, D. (2018). *The Conservative Formula: Quantitative Investing Made Easy*.  
  [Lire l’article original (JPM)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3145152)  
- WRDS, CRSP, Kenneth French Data Library
